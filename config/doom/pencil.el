;;; pencil.el --- Integrating basic NLP functions into Emacs -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;  There's this project called pen.el (https://github.com/semiosis/pen.el),
;;  which seems really cool, but also overly complicated and hard to set up.
;;  So this is my own mini version, appropriately named pencil.el.
;;
;;; Code:

(require 'request)
(require 's)
(require 'vertico)

(defvar pencil/goose-ai-key (let ((file "~/.goose-ai.key"))
                              (when (file-exists-p file)
                                (s-trim (with-temp-buffer
                                          (insert-file-contents file)
                                          (buffer-string))))))

(defvar pencil/debug-requests nil
  "When non-nil, print all request and response data to the *Messages* buffer.")

(cl-defstruct pencil-params
  "Struct for parameters of a request."
  prompt max-tokens min-tokens num-completions temperature top-p stop-sequences presence-penalty frequency-penalty echo)

(defvar pencil-request-function #'pencil-make-request-goose-ai
  "The function to use for prompting GPT.
Should take a `pencil-params' struct as well as an optional
callback function. The callback function should take a single
argument, the response data. Should also respect `pencil-debug-requests'.
Should return a list of completions.")

;; There's probably a proper way to do this with methods, but whatever.
(defun pencil-params-defaults (params)
  "Return a copy of PARAMS with default values filled in."
  (make-pencil-params :prompt (or (pencil-params-prompt params) (error "No prompt"))
                      :max-tokens (or (pencil-params-max-tokens params) 256)
                      :min-tokens (or (pencil-params-min-tokens params) 0)
                      :num-completions (or (pencil-params-num-completions params) 3)
                      :temperature (or (pencil-params-temperature params) 0.8)
                      :top-p (or (pencil-params-top-p params) 1)
                      :stop-sequences (or (pencil-params-stop-sequences params) ["<|endoftext|>"])
                      :presence-penalty (or (pencil-params-presence-penalty params) 0)
                      :frequency-penalty (or (pencil-params-frequency-penalty params) 0)
                      :echo (or (pencil-params-echo params) nil)))

(defun pencil-choose-completion (completions)
  "Choose and insert a completion from COMPLETIONS.
Preview completions using Vertico."
  (let ((ov (make-overlay (point) (point)))
        (timer nil))
    (unwind-protect
        (progn
          (setq timer (run-with-idle-timer 0.01 t (lambda () (overlay-put ov 'after-string (propertize (vertico--candidate) 'face 'shadow)))))
          (insert (completing-read "Completion: " completions)))
      (delete-overlay ov)
      (cancel-timer timer))))

(defun pencil-make-request (params append-behavior &optional callback)
  "Make a request using `pencil-request-function'.
See `pencil-request-function' for details on PARAMS and CALLBACK.
Inserts the response data into the current buffer using
`pencil-choose-completion' with APPEND-BEHAVIOR.
APPEND-BEHAVIOR should be one of the following symbols:

- `always-append' - Always append the completion to the end of the
active region, or point if the region is inactive.
- `replace-if-echo' - Replace the active region if echo is non-nil,
otherwise append. Append if the region is inactive.
- `always-replace' - Always replace the active region. Append
if the region is inactive."
  (when pencil/debug-requests
    (message "Request data: %s" params))
  (let ((completions (funcall pencil-request-function (pencil-params-defaults params) callback))
        (echo (pencil-params-echo params))
        (append-point (if (region-active-p)
                          (region-end)
                        (point))))
    (cond ((eq append-behavior 'always-append)
           (goto-char append-point))
          ((eq append-behavior 'replace-if-echo)
           (if (and echo (region-active-p))
               (progn
                 (goto-char (region-beginning))
                 (delete-region (region-beginning) (region-end)))
             (goto-char append-point)))
          ((eq append-behavior 'always-replace)
           (if (region-active-p)
               (progn
                 (goto-char (region-beginning))
                 (delete-region (region-beginning) (region-end)))
             (goto-char append-point)))
          (t (error "Invalid append-behavior: %s" append-behavior)))
    (pencil-choose-completion completions)))

(defun pencil-make-request-goose-ai (params &optional callback)
  "Make a request to the Goose AI API.
See `pencil-request-function' for details on PARAMS and CALLBACK."
  (message "Making request to Goose AI...")
  (let ((data
         (request-response-data
          (request
            "https://api.goose.ai/v1/engines/gpt-neo-20b/completions"
            :type "POST"
            :headers `(("Authorization" . ,(concat "Bearer " pencil/goose-ai-key))
                       ("Content-Type" . "application/json"))
            :data (json-encode `((prompt            . ,(pencil-params-prompt params))
                                 (max_tokens        . ,(pencil-params-max-tokens params))
                                 (min_tokens        . ,(pencil-params-min-tokens params))
                                 (n                 . ,(pencil-params-num-completions params))
                                 (temperature       . ,(pencil-params-temperature params))
                                 (top_p             . ,(pencil-params-top-p params))
                                 (stop              . ,(pencil-params-stop-sequences params))
                                 (presence_penalty  . ,(pencil-params-presence-penalty params))
                                 (frequency_penalty . ,(pencil-params-frequency-penalty params))
                                 (echo              . ,(pencil-params-echo params))))
            :parser 'json-read
            :error (cl-function
                    (lambda (&key error-thrown &allow-other-keys)
                      (error "Goose AI request threw error: %S" error-thrown)))
            :success (cl-function
                      (lambda (&key data &allow-other-keys)
                        (message "Goose AI request succeeded.")
                        (when pencil/debug-requests
                          (message "Response from Goose AI: %s" data))
                        (when callback
                          (funcall callback data))))
            :sync (not callback)))))
    (mapcar (lambda (completion)
              (alist-get 'text completion))
            (alist-get 'choices data))))

(defun pencil/complete-line (arg)
  "Complete the current line using GPT.
With a prefix argument ARG, prompt the user for a prompt
to replace/prepend to the current line/region, respectively."
  (interactive "P")
  (let* ((extra (when arg (concat (read-string "Prompt: ") "\n\n")))
         (prompt (cond ((region-active-p)
                        (concat extra (buffer-substring-no-properties (region-beginning) (region-end))))
                       (extra)
                       ((progn
                          (beginning-of-line)
                          (set-mark-command nil)
                          (end-of-line)
                          (buffer-substring-no-properties (region-beginning) (region-end))))))
         (params (make-pencil-params :prompt prompt
                                     :max-tokens 128
                                     :stop-sequences ["\n" "<|endoftext|>"]
                                     :echo arg)))
    (pencil-make-request params 'replace-if-echo)))

(defun pencil/complete-long ()
  "Complete using GPT, with more context and no stop sequences.
If a region is not active, use the last 2000 characters of the
buffer as prompt."
  (interactive)
  (let* ((prompt (if (region-active-p)
                     (buffer-substring-no-properties (region-beginning) (region-end))
                   (buffer-substring-no-properties (max (- (point) 2000) (point-min)) (point))))
         (params (make-pencil-params :prompt prompt
                                     :max-tokens 512)))
    (pencil-make-request params 'always-append)))

(provide 'pencil)
;;; pencil.el ends here

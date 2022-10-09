;;; pencil.el --- Integrating basic NLP functions into Emacs -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;  There's this project called pen.el (https://github.com/semiosis/pen.el),
;;  which seems really cool, but also overly complicated and hard to set up.
;;  So this is my own mini version simply taking advantage of pre-existing API
;;  endpoints provided by NLP Cloud. Appropriately, I've named it pencil.el.
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

(defun pencil-make-request (params &optional callback)
  "Make a request to the Goose AI API with PARAMS.
If CALLBACK is present, make the request asynchronously and call
CALLBACK on success; otherwise, make the request synchronously."
  (message "Making request to Goose AI...")
  (when pencil/debug-requests
    (message "Request to Goose AI made with parameters: %s" params))
  (request-response-data
   (request
     "https://api.goose.ai/v1/engines/gpt-neo-20b/completions"
     :type "POST"
     :headers `(("Authorization" . ,(concat "Bearer " pencil/goose-ai-key))
                ("Content-Type" . "application/json"))
     :data (json-encode params)
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
     :sync (not callback))))

(defun pencil/complete-line ()
  "Complete the current line using the Goose AI API.
If a region is selected, use that as the prompt instead.
Request three completions and allow the user to choose one.
Insert the chosen completion at the end of the line/region."
  (interactive)
  (let* ((prompt (if (region-active-p)
                     (buffer-substring-no-properties (region-beginning) (region-end))
                   (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
         (params `(("prompt"      . ,prompt)
                   ("max_tokens"  . 128)
                   ("temperature" . 0.8)
                   ("n"           . 3)
                   ("stop"        . "\n")))
         (completions (mapcar (lambda (completion)
                                (alist-get 'text completion))
                              (alist-get 'choices (pencil-make-request params)))))
    (goto-char (if (region-active-p)
                   (region-end)
                 (line-end-position)))
    ;; Preview choices as they're selected
    (let ((ov (make-overlay (point) (point)))
          (timer nil))
      (unwind-protect
          (progn
            (setq timer (run-with-idle-timer 0.01 t (lambda () (overlay-put ov 'after-string (propertize (vertico--candidate) 'face 'shadow)))))
            (insert (completing-read "Completion: " completions)))
        (delete-overlay ov)
        (cancel-timer timer)))))

(provide 'pencil)
;;; pencil.el ends here

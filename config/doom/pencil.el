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

(defvar pencil/nlp-cloud-key (let ((file (expand-file-name ".nlp-cloud.key" doom-user-dir)))
                               (when (file-exists-p file)
                                 (s-trim (with-temp-buffer
                                           (insert-file-contents file)
                                           (buffer-string))))))

(defvar pencil/debug-requests nil
  "When non-nil, print all request and response data to the *Messages* buffer.")

(defun pencil-make-request (model endpoint params &optional callback)
  "Make a request to the NLP Cloud API ENDPOINT for MODEL with PARAMS.
If CALLBACK is present, make the request asynchronously and call
CALLBACK on success; otherwise, make the request synchronously
and return the response."
  (message "Making request to NLP Cloud...")
  (when pencil/debug-requests
    (message "Request to %s/%s made with parameters: %s" model endpoint params))
  (request-response-data
   (request
     (format "https://api.nlpcloud.io/v1/gpu/%s/%s" model endpoint)
     :type "POST"
     :headers `(("Authorization" . ,(concat "Token " pencil/nlp-cloud-key))
                ("Content-Type" . "application/json"))
     :data (json-encode params)
     :parser 'json-read
     :error (cl-function
             (lambda (&key error-thrown &allow-other-keys)
               (error "NLP Cloud request threw error: %S" error-thrown)))
     :success (cl-function
               (lambda (&key data &allow-other-keys)
                 (message "NLP Cloud request succeeded.")
                 (when pencil/debug-requests
                   (message "Response from NLP Cloud: %s" data))
                 (when callback
                   (funcall callback data))))
     :sync (not callback))))

;;;###autoload
(defun pencil/prompt (arg)
  "Send the active region to the generation API and insert the response.
If no region is active, ask the user if they would like to use the current
line. Additionally, ask the user if they would like to prepend a prompt.
The response is inserted at the end of the region/line.

With a prefix argument ARG, prompt the user for a custom temperature (default
0.7), top p (default 1.0), min tokens (default 16), max tokens (default 256),
and stop sequence (default dependent upon scenario)."
  (interactive "P")
  (unless (use-region-p)
    (when (y-or-n-p "No region active; use current line? ")
      (move-beginning-of-line nil)
      (set-mark-command nil)
      (move-end-of-line nil)))
  (let* ((text (when (use-region-p)
                 (s-trim (buffer-substring-no-properties
                          (region-beginning)
                          (region-end)))))
         (prompt (when (or (not text)
                           (y-or-n-p "Prepend prompt? "))
                   (concat (read-string "Prompt: ") (if text "\n\n" ""))))
         (input (progn
                  (or text
                      prompt
                      (user-error "No text to send"))
                  (concat prompt text)))
         (params `(("text"         . ,input)
                   ("temperature"  . ,(if arg (read-number "Temperature: " 0.7) 0.7))
                   ("top_p"        . ,(if arg (read-number "Top p: " 1.0) 1.0))
                   ("min_length"   . ,(if arg (read-number "Min tokens: " 16) 16))
                   ("max_length"   . ,(if arg (read-number "Max tokens: " 256) 256))
                   ("end_sequence" . ,(if arg (read-string "Stop sequence: ")
                                        (if (and (not prompt)
                                                 (= (count-lines (region-beginning) (region-end)) 1))
                                            "\n\n"
                                          nil)))
                   ;; Other params
                   ("length_no_input"     . nil)
                   ("remove_end_sequence" . t)
                   ("remove_input"        . ,(not prompt))))
         (response (pencil-make-request "finetuned-gpt-neox-20b" "generation" params))
         (body (alist-get 'generated_text response)))
    (if (and prompt text)
        (delete-region (region-beginning) (region-end))
      (goto-char (region-end)))
    (insert body)))

;;;###autoload
(defun pencil/summarize ()
  "Send the active region to the summarization API and insert the response.
If no region is active, ask the user if they would like to use the current
line. The response is inserted at the end of the region/line."
  (interactive)
  (unless (use-region-p)
    (if (y-or-n-p "No region active; use current line? ")
        (progn
          (move-beginning-of-line nil)
          (set-mark-command nil)
          (move-end-of-line nil))
      (user-error "No text to send")))
  (let* ((input (s-trim (buffer-substring-no-properties
                         (region-beginning)
                         (region-end))))
         (response (pencil-make-request "finetuned-gpt-neox-20b" "summarization" `(("text" . ,input))))
         (text (alist-get 'summary_text response)))
    (deactivate-mark)
    (goto-char (region-end))
    (insert "\n\n" text)))

;;;###autoload
(defun pencil/answer ()
  "Send the active region to the question answering API and insert the response.
Ask the user for a question. If the region is active, send it as the context;
otherwise, ask the user if they would like to add a context. The response is
echoed in the minibuffer."
  (interactive)
  (let* ((question (read-string "Question: "))
         (context (if (use-region-p)
                      (s-trim (buffer-substring-no-properties
                               (region-beginning)
                               (region-end)))
                    (when (y-or-n-p "No region active; add context? ")
                      (read-string "Context: "))))
         (response (pencil-make-request "finetuned-gpt-neox-20b" "question"
                                        `(("question" . ,question)
                                          ("context" . ,context))))
         (answer (alist-get 'answer response)))
    (message "Answer: %s" answer)))

;;;###autoload
(defun pencil/paraphrase ()
  "Send the active region to the paraphrasing API and insert the response.
If no region is active, ask the user if they would like to use the current
line. The response replaces the region/line."
  (interactive)
  (unless (use-region-p)
    (if (y-or-n-p "No region active; use current line? ")
        (progn
          (move-beginning-of-line nil)
          (set-mark-command nil)
          (move-end-of-line nil))
      (user-error "No text to send")))
  (let* ((input (s-trim (buffer-substring-no-properties
                         (region-beginning)
                         (region-end))))
         (response (pencil-make-request "finetuned-gpt-neox-20b" "paraphrasing" `(("text" . ,input))))
         (text (alist-get 'paraphrased_text response)))
    (deactivate-mark)
    (goto-char (region-beginning))
    (delete-region (region-beginning) (region-end))
    (insert text)))

;;;###autoload
(defun pencil/spelling-and-grammar ()
  "Send the active region to the spelling and grammar correction API
and insert the response. If no region is active, ask the user if they
would like to use the current line. The response replaces the region/line."
  (interactive)
  (unless (use-region-p)
    (if (y-or-n-p "No region active; use current line? ")
        (progn
          (move-beginning-of-line nil)
          (set-mark-command nil)
          (move-end-of-line nil))
      (user-error "No text to send")))
  (let* ((input (s-trim (buffer-substring-no-properties
                         (region-beginning)
                         (region-end))))
         (response (pencil-make-request "finetuned-gpt-neox-20b" "spelling-and-grammar"
                                        `(("text" . ,input))))
         (text (alist-get 'corrected response)))
    (deactivate-mark)
    (goto-char (region-beginning))
    (delete-region (region-beginning) (region-end))
    (insert text)))

(defvar pencil-chat-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'pencil-chat-send)
    map)
  "Keymap for `pencil-chat-mode'.")

(defvar-local pencil-chat-context nil
  "The context for the current chat buffer.")

(defvar-local pencil-chat-history nil
  "History of the current chat buffer.")

(define-derived-mode pencil-chat-mode text-mode "Virtual Chat"
  "Major mode for having a virtual conversation with AI."
  (erase-buffer)
  (insert "Human: ")
  (goto-char (point-max)))

(defun pencil-chat-send ()
  "Send the current input to the chat API and insert the response."
  (interactive)
  (let* ((input (s-trim (buffer-substring-no-properties
                         ;; Skip the "Human: " prompt
                         (+ 7 (line-beginning-position))
                         (line-end-position))))
         (params `(("input"   . ,input)
                   ("context" . ,pencil-chat-context)
                   ("history" . ,pencil-chat-history)))
         (response (pencil-make-request "finetuned-gpt-neox-20b" "chatbot" params))
         (text (alist-get 'response response))
         (history (alist-get 'history response)))
    (goto-char (point-max))
    (insert "\nAI: " text "\nHuman: ")
    (setq pencil-chat-history history)))

;;;###autoload
(defun pencil/chat ()
  "Start a virtual conversation with AI."
  (interactive)
  (switch-to-buffer (generate-new-buffer "*Virtual Chat*"))
  (pencil-chat-mode)
  (setq pencil-chat-context "This is a casual conversation between a human and an AI. The AI is knowledgeable, creative, and humorous."))

(provide 'pencil)
;;; pencil.el ends here

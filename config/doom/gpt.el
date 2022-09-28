;;; gpt.el -*- lexical-binding: t; -*-
;;;
;;; Commentary:
;;; A set of functions for interacting with the OpenAI GPT-3 API.
;;; Includes interactive functions for prompting, editing, and
;;; inserting text via the Davinci model.
;;;
;;; Code:

(require 'request)

(defvar gpt/openai-key (s-trim (with-temp-buffer
                                 (insert-file-contents "~/.openai.key")
                                 (buffer-string))))

(defun gpt-make-request (endpoint params &optional callback)
  "Make a request to the OpenAI API endpoint ENDPOINT with PARAMS.
If CALLBACK is present, make the request asynchronously and call
CALLBACK on success; otherwise, make the request synchronously
and return the response."
  (message "Making request to OpenAI...")
  (request-response-data
   (request
     (concat "https://api.openai.com/v1/" endpoint)
     :type "POST"
     :headers `(("Authorization" . ,(concat "Bearer " gpt/openai-key))
                ("Content-Type" . "application/json"))
     :data (json-encode params)
     :parser 'json-read
     :error (cl-function
             (lambda (&key error-thrown &allow-other-keys)
               (error "OpenAI request threw error: %S" error-thrown)))
     :success (cl-function
               (lambda (&key data &allow-other-keys)
                 (message "OpenAI request succeeded.")
                 (when callback
                   (funcall callback data))))
     :sync (not callback))))

;;;###autoload
(defun gpt/prompt (arg)
  "Send a prompt to the OpenAI completion API and insert
the response. If the region is active, ask the user if
they would like to prepend a prompt, and if so ask the
user for a prompt and prepend it to the region with two
newlines. Insert the response after and deactivate the
region. Otherwise, always ask the user for a prompt and
insert the response at point. Move point to the end of the
response.

With a prefix argument ARG, prompt the user for a custom
temperature (default 0.7), max tokens (default 256),
stop sequence (default none), and frequency penalty
(default 0.0)."
  (interactive "P")
  (let* ((prompt (if (use-region-p)
                     (concat
                      (if (y-or-n-p "Prepend a prompt? ")
                          (read-string "Prompt: ")
                        "")
                      "\n\n"
                      (buffer-substring-no-properties
                       (region-beginning)
                       (region-end)))
                   (read-string "Prompt: ")))
         (params `(("prompt"            . ,(s-trim prompt))
                   ("max_tokens"        . ,(if arg (read-number "Max tokens: " 256) 256))
                   ("temperature"       . ,(if arg (read-number "Temperature: " 0.7) 0.7))
                   ("stop"              . ,(if arg (read-string "Stop sequence: ") ""))
                   ("frequency_penalty" . ,(if arg (read-number "Frequency penalty: " 0.0) 0.0))
                   ("model"             . "text-davinci-002")
                   ("echo"              . t))))
    ;; Warn if prompt is greater than 1000 characters
    (when (> (length prompt) 1000)
      (unless (y-or-n-p (format "Prompt is %d characters. Continue? " (length prompt)))
        (user-error "Aborted")))
    (let* ((response (gpt-make-request "completions" params))
           (text (alist-get 'text (aref (alist-get 'choices response) 0))))
      (if (use-region-p)
          (progn
            (delete-active-region)
            (insert text))
        (insert text)))))

;;;###autoload
(defun gpt/edit (arg)
  "Send text to the OpenAI edit API and replace
the text with the response. If the region is active,
use the region as input; otherwise, prompt the user if
they would like to use the entire buffer as input (if
not then abort). Then prompt for an instruction and
send the text to the API. Replace the sent text with
the response and deactivate the region if active.

Choose between the text and code model based on the
major mode of the current buffer.

With a prefix argument ARG, prompt the user for a custom
temperature (default 0.7 for text and 1.0 for code) and
top-p (default 1.0 for text and 0.1 for code)."
  (interactive "P")
  (let* ((input (if (use-region-p)
                    (buffer-substring-no-properties (region-beginning) (region-end))
                  (if (y-or-n-p "Use entire buffer as input? ")
                      (buffer-string)
                    (user-error "Aborted"))))
         (instruction (read-string "Instruction: "))
         (model (concat (if (derived-mode-p 'prog-mode) "code" "text") "-davinci-edit-001"))
         (default-temp (if (derived-mode-p 'prog-mode) 1.0 0.7))
         (default-top-p (if (derived-mode-p 'prog-mode) 0.1 1.0))
         (params `(("input"       . ,(s-trim input))
                   ("instruction" . ,instruction)
                   ("model"       . ,model)
                   ("temperature" . ,(if arg (read-number "Temperature: " default-temp) default-temp))
                   ("top_p"       . ,(if arg (read-number "Top-P: " default-top-p) default-top-p)))))
    (let* ((response (gpt-make-request "edits" params))
           (text (alist-get 'text (aref (alist-get 'choices response) 0))))
      (if (use-region-p)
          (progn
            (delete-active-region)
            (insert text))
        (delete-region (point-min) (point-max))
        (insert text)))))

;;;###autoload
(defun gpt/insert (arg)
  "Send text to the OpenAI insert API with the
insertion location at point, then insert the response.
Prompt the user for the number of surrounding lines
to use as input (default 1).

With a prefix argument ARG, prompt the user for a custom
temperature (default 0.7), max tokens (default 256),
stop sequence (default none), and frequency penalty
(default 0.0)."
  (interactive "P")
  (let* ((lines (read-number "Number of surrounding lines to use: " 1))
         (prompt (buffer-substring-no-properties
                  (save-excursion
                    (forward-line (- lines))
                    (point))
                  (point)))
         (suffix (buffer-substring-no-properties
                  (point)
                  (save-excursion
                    (forward-line lines)
                    (end-of-line)
                    (point))))
         (params `(("prompt"           . ,(s-trim prompt))
                   ("suffix"           . ,(s-trim suffix))
                   ("max_tokens"       . ,(if arg (read-number "Max tokens: " 256) 256))
                   ("temperature"      . ,(if arg (read-number "Temperature: " 0.7) 0.7))
                   ("stop"             . ,(if arg (read-string "Stop sequence: ") ""))
                   ("frequency_penalty". ,(if arg (read-number "Frequency penalty: " 0.0) 0.0))
                   ("model"            . "text-davinci-002"))))
    (let* ((response (gpt-make-request "completions" params))
           (text (alist-get 'text (aref (alist-get 'choices response) 0))))
      (insert text))))

(provide 'gpt)

(defvar-local gpt-chat-prompt ""
  "The prompt for the current chat buffer.
Prepended to all requests.")

(defvar-local gpt-chat-human-prefix "Human:"
  "The prefix inserted for the human's response.")

(defvar-local gpt-chat-ai-prefix "GPT:"
  "The prefix inserted for the AI's response.")

(defvar gpt-chat-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'gpt-chat-send)
    map)
  "Keymap for `gpt-chat-mode'.")

(define-derived-mode gpt-chat-mode text-mode "GPT-Chat"
  "Major mode for having a virtual chat with GPT-3."
  ;; Delete all text in case the buffer is old
  (delete-region (point-min) (point-max))
  (insert (concat
           gpt-chat-human-prefix
           " Hello, who are you?\n"
           gpt-chat-ai-prefix
           " I am an AI named GPT. How can I help you?\n"
           gpt-chat-human-prefix
           " "))
  (goto-char (point-max)))

(defun gpt/chat ()
  "Start a chat with GPT-3. Opens a `gpt-chat-mode'
buffer and inserts some initial text. The last 4 lines
of the buffer, along with the current line,
are appended to the prompt and then sent to the API
when the user hits return. The result is appended to the
end of the buffer."
  (interactive)
  (switch-to-buffer (generate-new-buffer "*GPT-Chat*"))
  (setq gpt-chat-prompt "The following is a conversation with an AI named GPT. GPT is helpful and knowledgeable.")
  (gpt-chat-mode))


(defun gpt-chat-send ()
  "Send the current line along with the last four lines
(for context) appended to the prompt to the OpenAI API
asychronously and append the result to the end of the
chat buffer, as well as a new prompt for the human."
  (interactive)
  ;; Append a new prompt for the AI first
  (insert "\n" gpt-chat-ai-prefix)
  (let* ((context (buffer-substring-no-properties
                   (save-excursion
                     ;; A newline was appended, so the last four becomes five
                     (forward-line -5)
                     (point))
                   (point)))
         (params `(("prompt"           . ,(concat gpt-chat-prompt "\n\n" context))
                   ("max_tokens"       . 128)
                   ("temperature"      . 0.8)
                   ("top_p"            . 1.0)
                   ("presence_penalty" . 0.5)
                   ("stop"             . `(,(concat "\n" gpt-chat-human-prefix)
                                           ,(concat "\n" gpt-chat-ai-prefix)))
                   ("model"            . "text-davinci-002")))
         (buffer (current-buffer))
         (callback (lambda (response)
                     (let* ((text (alist-get 'text (aref (alist-get 'choices response) 0))))
                       (with-current-buffer buffer
                         ;; GPT likes to sometimes start with two newlines,
                         ;; so we get rid of those. We still need to prepend
                         ;; a space since the prompt doesn't include one after
                         ;; the colon.
                         (insert " " (s-trim text) "\n" gpt-chat-human-prefix " ")
                         (goto-char (point-max)))))))
    (gpt-make-request "completions" params callback)))

;;; gpt.el ends here

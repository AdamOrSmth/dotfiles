;;; Native Compilation
;; Enable native compilation of Elisp and silence compilation warnings.
(setq native-comp-deferred-compilation t
      warning-suppress-types '((comp)))

;;; Variables
(setq auto-save-default t ; No one likes losing work
      user-full-name "Ad"
      user-mail-address "me@adamorsomething.xyz"
      langtool-bin "/run/current-system/sw/bin/languagetool-commandline" ; Fix for langtool on NixOS
      evil-want-fine-undo t) ; Instead of undo-ing the entire insert operation
(global-subword-mode t) ; Word navigation respects CamelCase

;;; Functions
(defun ad/get-org-buffer-title (buffer)
  "Given an 'org-mode' buffer BUFFER, return its TITLE property."
  (with-current-buffer buffer
    (nth 1 (car (org-collect-keywords '("TITLE"))))))
(defun ad/custom-agenda-prefix (len)
  "Create a custom 'org-agenda' prefix of LEN."
  (if buffer-file-name ; Check if the item if a file
      ;; Get the `org-mode' title, remove prefix, I forgot how the hell this code
      ;; works, good luck trying to maintain it.
      (let ((len (if (string-empty-p time) len (- len (length time) 3)))
            (title (ad/get-org-buffer-title (find-file-noselect buffer-file-name))))
        (concat (if (> (length title) len) ; Pad or truncate the title, depending on
                                        ; whether it's longer or shorter than the desired length.
                    (s-truncate len title "…")
                  (s-pad-right len " " title))
                (unless (string-empty-p time) "  ") ; Add timestamp divider, if it's present.
                time))
    (concat (make-string (- len (length time)) ? ) time))) ; It must be a timestamp, prepend it with padding.

;;; Appearance settings
(setq display-line-numbers-type t
      doom-theme 'doom-nord-plus
      doom-font (font-spec :family "Comic Code Ligatures" :size 18)
      doom-variable-pitch-font (font-spec :family "Comic Neue" :size 18)
      doom-big-font-increment 6)
;; Enable italics for comments and keywords.
(custom-set-faces!
  '(font-lock-comment-face :slant italic)
  '(font-lock-keyword-face :slant italic))
;; Enable beacon.el for cursor blinking
(use-package! beacon
  :custom
  (beacon-blink-when-point-moves-vertically 10)
  (beacon-blink-when-point-moves-horizontally 20)
  (beacon-color "#D8DEE9")
  :config
  (beacon-mode t)
  (advice-add 'evil-scroll-up :after (lambda (&rest _) (beacon-blink)))
  (advice-add 'evil-scroll-down :after (lambda (&rest _) (beacon-blink))))

;;; Advice
;; Replace 'sudo' with 'doas'
(define-advice doom--sudo-file-path (:around (orig-fun file))
  (s-replace "sudo" "doas" (apply orig-fun file nil)))
;; Make 'j' and 'k' navigate through visual lines instead of
;; logical lines by default.
(after! evil
  (define-advice evil-line-move (:around (orig-fun count &optional noerror))
    (let ((line-move-visual t))
      (apply orig-fun count noerror))))

;;; Org-Mode
;; Must be set before org loads!
(setq org-directory "/home/ad/Sync/APP/")
(after! org
  ;; Appearance
  (setq org-hide-emphasis-markers t
        org-startup-folded 'content
        org-ellipsis " […] "
        org-list-demote-modify-bullet '(("+" . "-") ("-" . "+") ("*" . "+"))
        org-reverse-note-order t)
  ;; Custom keybindings for common functions I use.
  (map! (:map org-mode-map
         (:localleader
          :desc "org-edit-src-code" "E" #'org-edit-src-code
          :desc "org-latex-preview" "L" #'org-latex-preview)))
  ;; Org-Agenda
  (setq org-agenda-files `(,(concat org-directory "gtd/"))
        org-agenda-skip-scheduled-if-done t
        org-agenda-skip-deadline-if-done t
        org-agenda-todo-ignore-scheduled t
        org-agenda-skip-deadline-prewarning-if-scheduled 'pre-scheduled
        org-agenda-prefix-format '((agenda . " %i %(ad/custom-agenda-prefix 32)  %s%b") (todo . " %i %-32:(ad/custom-agenda-prefix 32)  %b") (tags . " %i %-12:c") (search . " %i %-12:c"))
        ;; Default appearance is kinda ugly.
        org-agenda-time-grid
        '((daily today require-timed)
          (800 1000 1200 1400 1600 1800 2000)
          "" "----------------")
        org-agenda-time-leading-zero t
        org-agenda-current-time-string "———————————————— now")
  (setq org-export-with-section-numbers nil)
  ;; Org-Export
  (setq org-export-with-toc nil
        org-export-with-tags nil
        ;; LaTeX requires the 'titletoc' package for proper table of contents export,
        ;; and it must load before 'hyperref', which is what this spaghetti does
        ;; ('hyperref' is last in the list by default).
        org-latex-default-packages-alist (append (butlast org-latex-default-packages-alist 1)
                                                 '(("" "titletoc" nil ("pdflatex")))
                                                 (last org-latex-default-packages-alist)))
                                        ; Default export sub-directory, see 'https://stackoverflow.com/questions/9559753/emacs-org-mode-export-to-another-directory'.
  (define-advice org-export-output-file-name (:around (orig-fun extension &optional subtreep pub-dir))
    (unless pub-dir
      (setq pub-dir (concat org-directory "exports/"))
      (unless (file-directory-p pub-dir)
        (make-directory pub-dir)))
    (apply orig-fun extension subtreep pub-dir nil))
  ;; TODO keywords, mostly ripped off Doom's default keywords, just with logging and some
  ;; custom ones. '@' adds a note when changing to the specified state, and '!' adds a
  ;; timestamp. Putting it after a '/' adds it when moving away from the state, and only
  ;; if the target does not having logging. See 'https://orgmode.org/manual/Tracking-TODO-state-changes.html' for more details.
  (setq org-todo-keywords '((sequence
                             "TODO(t/!)"   ; Ready to start.
                             "PROJ(p/!)"   ; Project, containing sub-tasks.
                             "LOOP(r/!)"   ; A repeating task.
                             "STRT(s!/!)"  ; Started and in progress.
                             "WAIT(w@/@)"  ; Something external is holding up this task.
                             "HOLD(h@/@)"  ; This task is on hold because of me.
                             "IDEA(i/@)"   ; An idea.
                             "LOOK(l/@)"   ; Something to look into/check out.
                             "|"
                             "DONE(d)"     ; Successfully completed — can't have logging because otherwise an entry that
                                        ; has note logging after switch won't take effect. Logging set on individual
                                        ; keywords instead.
                             "KILL(k@/@)") ; Task canceled, aborted, or no longer applicable.
                            (sequence
                             "[ ](T/!)"
                             "[-](S!)"
                             "[?](W@/@)"
                             "|"
                             "[X](D)")
                            (sequence
                             "|"
                             "OKAY(o@)"
                             "YES(y@)"
                             "NO(n@)"))
        ;; Custom faces for custom keywords.
        org-todo-keyword-faces (append org-todo-keyword-faces '(("LOOK" . +org-todo-active)))
        ;; Log notes and timestamps into drawers.
        org-log-into-drawer t
        org-log-done 'time)
  ;; Prompt to clock into a task when it's marked as 'STRT'.
  (add-hook! 'org-after-todo-state-change-hook
    (when (and (string-equal org-state "STRT")
               (y-or-n-p "Clock into this task?"))
      (org-clock-in)))
  ;; Capture templates for my inbox.
  (setq org-capture-templates
        '(("u" "unsorted" entry
           (file "inbox.org")
           "* %?\n"
           :prepend t)
          ("e" "event" entry
           (file "inbox.org")
           "* %?\n<%(org-read-date)>\n"
           :prepend t)
          ("t" "todo")
          ("tt" "no time" entry
           (file "inbox.org")
           "* TODO %?\n"
           :prepend t)
          ("ts" "with scheduled" entry
           (file "inbox.org")
           "* TODO %?\nSCHEDULED: <%(org-read-date)>\n"
           :prepend t)
          ("td" "with deadline" entry
           (file "inbox.org")
           "* TODO %?\nDEADLINE: <%(org-read-date)>\n"
           :prepend t)
          ("i" "idea" entry
           (file "inbox.org")
           "* IDEA %?\n"
           :prepend t)
          ("c" "check out/investigate" entry
           (file "inbox.org")
           "* LOOK %?\n"
           :prepend t)))
  ;; Personal spellcheck dictionary.
  (add-hook 'spell-fu-mode-hook
            (lambda () (spell-fu-dictionary-add
                        (spell-fu-get-personal-dictionary "personal" (concat org-directory ".aspell.pws"))))))

;;; Org-Roam
(after! org-roam
  (setq org-roam-directory org-directory
        org-roam-dailies-directory (concat org-roam-directory "journals/")
        org-roam-db-location (concat org-roam-directory ".org-roam.db")
        +org-roam-open-buffer-on-find-file nil
        ;; Capture templates.
        org-roam-capture-templates
        `(("n" "note" plain
           (file ,(concat org-directory "templates/note.org"))
           :target (file "notes/%<%Y%m%d%H%M%S>-${slug}.org")
           :unnarrowed t)
          ("w" "work")
          ("ww" "default" plain
           (file ,(concat org-directory "templates/document.org"))
           :target (file "works/%<%Y%m%d%H%M%S>-${slug}.org")
           :unnarrowed t)
          ("wl" "lab report" plain
           (file ,(concat org-directory "templates/aet-lab-report.org"))
           :target (file "works/%<%Y%m%d%H%M%S>-${slug}.org")
           :unnarrowed t)
          ("t" "topic" plain
           (file ,(concat org-directory "templates/topic.org"))
           :target (file "topics/%<%Y%m%d%H%M%S>-${slug}.org")
           :unnarrowed t)
          ("p" "project" plain
           (file ,(concat org-directory "templates/project.org"))
           :target (file "projects/%<%Y%m%d%H%M%S>-${slug}.org")
           :unnarrowed t))
        org-roam-dailies-capture-templates
        `(("d" "default" entry
           (file "templates/journal.org")
           :target (file+head
                    "%<%Y-%m-%d>.org"
                    "#+title: %<%Y-%m-%d %a>\n\n")
           :clock-in
           :clock-resume)))
  ;; Icons for my sub-directory file "types".
  (defvar ad/org-roam-icons
    '(("notes"    . "📑")
      ("topics"   . "🏷")
      ("projects" . "📂")
      ("works"    . "✏")
      ("journals" . "📖")))
  ;; Add type prefix to 'org-roam-find-file'.
  (cl-defmethod org-roam-node-doom-prefix ((node org-roam-node))
    (cdr (assoc (org-roam-node-doom-type node)
                ad/org-roam-icons)))
  (setq org-roam-node-display-template #("${doom-prefix} ${doom-hierarchy:*} ${todo:8} ${doom-type:12} ${doom-tags:24}" 20 35
                                         (face font-lock-keyword-face)
                                         36 51
                                         (face org-tag)))
  ;; Update zettel file names when title changes to new slug.
  (defun ad/update-roam-filename ()
    (interactive)
    (when (and (org-roam-file-p) ; Ensure it's a roam file of the 'zettel' type.
               (string-equal (concat org-directory "zettel/") (file-name-directory buffer-file-name)))
      (let
          ((file-location ; Location that file should be at.
            (concat
             (file-name-directory buffer-file-name)
             (s-replace-regexp "^\\([0-9]\\{14\\}\\).*" "\\1" (file-name-base buffer-file-name))
             "-"
             (-> (org-roam-node-at-point)
                 (org-roam-node-file-title)
                 (org-roam-node-from-title-or-alias)
                 (org-roam-node-slug))
             ".org")))
        (unless (string-equal buffer-file-name file-location)
          (doom/move-this-file file-location)))))
  (add-hook 'before-save-hook #'ad/update-roam-filename)
  ;; Org-Roam-UI
  (use-package! org-roam-ui
    :bind (:map doom-leader-notes-map ("r u" . org-roam-ui-mode))
    :requires (org-roam)
    :init
    (use-package! websocket)
    :custom
    (org-roam-ui-sync-theme t)
    (org-roam-ui-follow t)
    (org-roam-ui-update-on-save t)
    (org-roam-ui-open-on-start nil))
  ;; Keybind to visit a random node, excluding dailies.
  (defun ad/org-roam-random-excluding-dailies (&optional other-window)
    "Visit a random `org-roam` node, excluding dailies."
    (interactive "P")
    (org-roam-node-random
     other-window
     (lambda (node)
       (->> node
            (org-roam-node-file)
            (file-name-directory)
            (string-equal org-roam-dailies-directory)
            (not)))))
  (map! (:leader
         :desc "Random node (no dailies)" "n r A" #'ad/org-roam-random-excluding-dailies)))

;;; Org-Auto_tangle
(use-package! org-auto-tangle
  :hook (org-mode . org-auto-tangle-mode)
  :custom (org-auto-tangle-default t))

;;; Anki-Editor
(use-package! anki-editor
  :hook (org-mode . anki-editor-mode)
  :config
  (map! (:map org-mode-map
         (:localleader
          (:prefix ("F" . "anki")
           :desc "cloze" :nv "c" #'anki-editor-cloze-dwim
           :desc "push" "p" #'anki-editor-push-notes
           :desc "retry failed" "r" #'anki-editor-retry-failure-notes
           :desc "insert note" "i" #'anki-editor-insert-note)))))

;;; Partial Window Transparency
;; It may be useless and impractical, but it's hot.
;; Background-only transparency requires this patch:
;; 'https://github.com/TheVaffel/emacs/blob/master/emacs_background_transparency.patch'.
(setq default-frame-alist (append default-frame-alist '((alpha-background . 0.85))))
;; Toggle transparency.
(map! :leader
      (:prefix "t"
       :desc "Transparency" "t"
       (cmd!
        (set-frame-parameter
         nil 'alpha-background
         (let* ((parameter (frame-parameter nil 'alpha-background))
                (alpha (or (car-safe parameter) parameter)))
           (if (or (= alpha 1.0) (= alpha 100))
               0.85
             1.0))))))

;;; 'Langtool' keybindings.
(map! (:leader
       (:prefix ("l" . "langtool")
        :desc "langtool-check" "c" #'langtool-check
        :desc "langtool-correct-buffer" "l" #'langtool-correct-buffer
        :desc "langtool-check-done" "d" #'langtool-check-done)))

;;; 'titlecase.el' setup.
(use-package! titlecase
  :after evil
  :config
  (map! :nv "g`" (evil-define-operator evil-titlecase (beg end)
                   (interactive "<r>")
                   (save-excursion
                     (set-mark beg)
                     (goto-char end)
                     (titlecase-dwim)))))

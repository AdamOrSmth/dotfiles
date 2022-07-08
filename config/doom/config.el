(setq native-comp-deferred-compilation t)

(setq auto-save-default t
      user-full-name "Ad"
      user-mail-address "me@adamorsomething.xyz"
      langtool-bin "/run/current-system/sw/bin/languagetool-commandline" ; (ref:langtool-fix)
      evil-want-fine-undo t)                                             ; (ref:fine-undo)
(global-subword-mode t)                                                  ; (ref:subword-mode)

(defun ad/get-org-buffer-title (&optional buffer)
  (with-current-buffer (or buffer (current-buffer))
    (nth 1 (car (org-collect-keywords '("TITLE"))))))

(defun ad/custom-agenda-prefix (len)
  (if buffer-file-name
      (let ((len (if (string-empty-p time) len (- len (length time) 3)))
            (title (ad/get-org-buffer-title (find-file-noselect buffer-file-name))))
        (concat (if (> (length title) len)
                    (s-truncate len title "…")
                  (s-pad-right len " " title))
                (unless (string-empty-p time) "  ")
                time))
    (concat (make-string (- len (length time)) ? ) time)))

(setq display-line-numbers-type t
      doom-theme 'doom-one
      doom-font (font-spec :family "Comic Code Ligatures" :size 16)
      doom-variable-pitch-font (font-spec :family "Comic Neue" :size 16)
      doom-big-font-increment 6)

(custom-set-faces!
  '(font-lock-comment-face :slant italic)
  '(font-lock-keyword-face :slant italic))

(custom-set-faces!
  `(org-document-info  :inherit variable-pitch :height 1.0  :weight light  :slant italic)
  `(org-document-title :inherit variable-pitch :height 1.80 :weight bold   :slant italic)
  `(org-level-1        :inherit variable-pitch :height 1.70 :weight medium :foreground ,(doom-color 'blue))
  `(org-level-2        :inherit variable-pitch :height 1.60 :weight medium :foreground ,(doom-color 'magenta))
  `(org-level-3        :inherit variable-pitch :height 1.50 :weight medium :foreground ,(doom-color 'cyan))
  `(org-level-4        :inherit variable-pitch :height 1.40 :weight medium :foreground ,(doom-color 'violet))
  `(org-level-5        :inherit variable-pitch :height 1.30 :weight medium :foreground ,(doom-color 'teal))
  `(org-level-6        :inherit variable-pitch :height 1.20 :weight medium :foreground ,(doom-color 'green))
  `(org-level-7        :inherit variable-pitch :height 1.10 :weight medium :foreground ,(doom-color 'yellow))
  `(org-level-8        :inherit variable-pitch :height 1.00 :weight medium :foreground ,(doom-color 'orange)))

(use-package! beacon
  :custom
  (beacon-blink-when-point-moves-vertically 10)
  (beacon-blink-when-point-moves-horizontally 20)
  (beacon-color "#D8DEE9")
  :config
  (beacon-mode t)
  (advice-add 'evil-scroll-up :after (lambda (&rest _) (beacon-blink)))
  (advice-add 'evil-scroll-down :after (lambda (&rest _) (beacon-blink))))

(define-advice doom--sudo-file-path (:around (orig-fun file))
  (s-replace "sudo" "doas" (apply orig-fun file nil)))

(after! evil
  (define-advice evil-line-move (:around (orig-fun count &optional noerror))
    (let ((line-move-visual t))
      (apply orig-fun count noerror))))

(setq org-directory "/home/ad/Sync/APP/")

(setq org-hide-emphasis-markers t
      org-startup-folded 'content
      org-ellipsis " […] "
      org-list-demote-modify-bullet '(("+" . "-") ("-" . "+") ("*" . "+"))
      org-reverse-note-order t)

(map! (:map org-mode-map
       (:localleader
        :desc "org-edit-src-code" "E" #'org-edit-src-code
        :desc "org-latex-preview" "L" #'org-latex-preview
        :desc "org-babel-demarcate-block" "D" #'org-babel-demarcate-block)))

(setq org-agenda-files `(,(expand-file-name "gtd/" org-directory))
      org-agenda-skip-scheduled-if-done t
      org-agenda-skip-deadline-if-done t
      org-agenda-todo-ignore-scheduled t
      org-agenda-skip-deadline-prewarning-if-scheduled 'pre-scheduled
      org-agenda-prefix-format '((agenda . " %i %(ad/custom-agenda-prefix 32)  %s%b") (todo . " %i %-32:(ad/custom-agenda-prefix 32)  %b") (tags . " %i %-12:c") (search . " %i %-12:c"))
      org-agenda-time-grid
      '((daily today require-timed)
        (800 1000 1200 1400 1600 1800 2000)
        "" "----------------")
      org-agenda-time-leading-zero t
      org-agenda-current-time-string "———————————————— now")

(setq org-export-with-section-numbers nil
      org-export-with-toc nil
      org-export-with-tags nil)

(define-advice org-export-output-file-name (:around (orig-fun extension &optional subtreep pub-dir))
  (unless pub-dir
    (setq pub-dir (expand-file-name "export/" org-directory))
    (unless (file-directory-p pub-dir)
      (make-directory pub-dir)))
  (apply orig-fun extension subtreep pub-dir nil))

(setq org-latex-compiler "lualatex"
      org-latex-default-class "report")

(setq org-latex-classes '(("article" "\\documentclass[11pt]{article}"
                           ("\\section{%s}"       . "\\section*{%s}")
                           ("\\subsection{%s}"    . "\\subsection*{%s}")
                           ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                           ("\\paragraph{%s}"     . "\\paragraph*{%s}")
                           ("\\subparagraph{%s}"  . "\\subparagraph*{%s}"))
                          ("report" "\\documentclass[11pt]{report}"
                           ("\\chapter{%s}"       . "\\chapter*{%s}")
                           ("\\section{%s}"       . "\\section*{%s}")
                           ("\\subsection{%s}"    . "\\subsection*{%s}")
                           ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                           ("\\paragraph{%s}"     . "\\paragraph*{%s}"))
                          ("book" "\\documentclass[11pt]{book}"
                           ("\\part{%s}"          . "\\part*{%s}")
                           ("\\chapter{%s}"       . "\\chapter*{%s}")
                           ("\\section{%s}"       . "\\section*{%s}")
                           ("\\subsection{%s}"    . "\\subsection*{%s}")
                           ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))))

(after! org
  (setq org-todo-keywords '((sequence
                             "TODO(t/!)"
                             "PROJ(p/!)"
                             "STRT(s!/!)"
                             "WAIT(w@/@)"
                             "HOLD(h@/@)"
                             "IDEA(i/@)"
                             "READ(r/@)"
                             "|"
                             "DONE(d)"
                             "KILL(k@/@)")
                            (sequence
                             "[ ](T/!)"
                             "[-](S!)"
                             "[?](W@/@)"
                             "|"
                             "[X](D)"))
        org-todo-keyword-faces (append org-todo-keyword-faces
                                       '(("LOOK" . +org-todo-active)))))

(setq org-log-into-drawer t
      org-log-done 'time)

(add-hook! 'org-after-todo-state-change-hook
  (when (and (string-equal org-state "STRT")
             (y-or-n-p "Clock into this task?"))
    (org-clock-in)))

(setq org-default-notes-file (expand-file-name "inbox.org" org-directory)
      org-capture-templates
      '(("d" "default" entry
         (file "inbox.org")
         "* %?\n"
         :prepend t)
        ("e" "event" entry
         (file "inbox.org")
         "* %?\n<%(org-read-date)>\n"
         :prepend t)
        ("t" "todo")
        ("tt" "default" entry
         (file "inbox.org")
         "* TODO %?\n"
         :prepend t)
        ("ts" "scheduled" entry
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
        ("r" "read later" entry
         (file "inbox.org")
         "* READ %?\n"
         :prepend t)))

(add-hook 'spell-fu-mode-hook
          (lambda () (spell-fu-dictionary-add
                      (spell-fu-get-personal-dictionary "personal" (expand-file-name ".aspell.pws" org-directory)))))

(setq org-roam-directory org-directory
      org-roam-dailies-directory (expand-file-name "journal/" org-roam-directory)
      org-roam-db-location (expand-file-name ".org-roam.db" org-roam-directory)
      +org-roam-open-buffer-on-find-file nil)

(defvar ad/org-roam-icons
  '(("gtd"     . "🗓")
    ("journal" . "📖")
    ("log"     . "🗃")
    ("ref"     . "📚")
    ("work"    . "✏")
    ("zettel"  . "🗒")
    (nil       . "📥")))

(after! org-roam
   (cl-defmethod org-roam-node-doom-prefix ((node org-roam-node))
     (cdr (assoc (org-roam-node-doom-type node)
                 ad/org-roam-icons)))
  (setq org-roam-node-display-template #("${doom-prefix} ${doom-hierarchy:*} ${todo:8} ${doom-type:12} ${doom-tags:24}" 20 35
                                         (face font-lock-keyword-face)
                                         36 51
                                         (face org-tag))))

(defun ad/update-roam-filename ()
  (interactive)
  (when (and (org-roam-file-p)
             (-contains-p '("log" "ref" "work" "zettel") (f-filename (f-parent buffer-file-name))))
    (let
        ((new-file-location
          (concat
           (file-name-directory buffer-file-name)
           (s-replace-regexp "^\\([0-9]\\{14\\}\\).*" "\\1" (file-name-base buffer-file-name))
           "-"
           (-> (org-roam-node-at-point)
               (org-roam-node-file-title)
               (org-roam-node-from-title-or-alias)
               (org-roam-node-slug))
           ".org")))
      (unless (string-equal buffer-file-name new-file-location)
        (doom/move-this-file new-file-location)))))
(add-hook 'before-save-hook #'ad/update-roam-filename)

(let ((template (lambda (template)
                  (expand-file-name (concat template ".org")
                                    (expand-file-name "template/" org-roam-directory)))))
  (setq org-roam-capture-templates
        `(("z" "zettel" plain
           (file ,(apply template '("zettel")))
           :target (file "zettel/%<%Y%m%d%H%M%S>-${slug}.org")
           :unnarrowed t)
          ("o" "outline" plain
           (file ,(apply template '("outline")))
           :target (file "outline/%<%Y%m%d%H%M%S>-${slug}.org")
           :unnarrowed t)
          ("w" "work")
          ("ww" "default" plain
           (file ,(apply template '("work")))
           :target (file "work/%<%Y%m%d%H%M%S>-${slug}.org")
           :unnarrowed t)
          ("wl" "lab report" plain
           (file ,(apply template '("lab-report")))
           :target (file "work/%<%Y%m%d%H%M%S>-${slug}.org")
           :unnarrowed t))))

(setq org-roam-dailies-capture-templates
      `(("d" "default" entry
         (file "template/journal.org")
         :target (file+head
                  "%<%Y-%m-%d>.org"
                  "#+title: %<%Y-%m-%d %a>\n\n")
         :clock-in
         :clock-resume)))

(use-package! org-roam-ui
  :after (org-roam)
  :bind (:map doom-leader-notes-map ("r u" . org-roam-ui-mode))
  :init
  (use-package! websocket)
  :custom
  (org-roam-ui-sync-theme t)
  (org-roam-ui-follow t)
  (org-roam-ui-update-on-save t)
  (org-roam-ui-open-on-start nil))

(use-package! org-auto-tangle
  :hook (org-mode . org-auto-tangle-mode))

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

(setq default-frame-alist (append default-frame-alist '((alpha-background . 0.75))))
(map! :leader
      (:prefix "t"
       :desc "Transparency" "t"
       (cmd!
        (set-frame-parameter
         nil 'alpha-background
         (let* ((parameter (frame-parameter nil 'alpha-background))
                (alpha (or (car-safe parameter) parameter)))
           (if (or (= alpha 1.0) (= alpha 100))
               0.75
             1.0))))))

(map! (:leader
       (:prefix ("l" . "langtool")
        :desc "langtool-check" "c" #'langtool-check
        :desc "langtool-correct-buffer" "l" #'langtool-correct-buffer
        :desc "langtool-check-done" "d" #'langtool-check-done)))

(use-package! titlecase
  :after evil
  :config
  (map! :nv "g`" (evil-define-operator evil-titlecase (beg end)
                   (interactive "<r>")
                   (save-excursion
                     (set-mark beg)
                     (goto-char end)
                     (titlecase-dwim)))))

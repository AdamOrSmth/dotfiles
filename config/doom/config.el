(setq native-comp-deferred-compilation t)

(setq auto-save-default t
      user-full-name "Ad"
      user-mail-address "me@adamorsomething.xyz"
      langtool-bin "/run/current-system/sw/bin/languagetool-commandline"
      evil-want-fine-undo t)
(global-subword-mode t)

(setq display-line-numbers-type t
      doom-theme 'doom-dracula-plus
      doom-font (font-spec :family "Comic Code Ligatures" :size 16)
      doom-variable-pitch-font (font-spec :family "Comic Neue" :size 16)
      doom-big-font-increment 6
      vertico-posframe-width 164)

(custom-set-faces!
  '(font-lock-comment-face :slant italic)
  '(font-lock-keyword-face :slant italic))

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
      org-reverse-note-order t
      org-cite-global-bibliography `(,(expand-file-name "refs.bib" org-directory))
      org-archive-location "%s.archive::datetree/"
      org-todo-repeat-to-state t
      org-priority-start-cycle-with-default nil)

(map! (:map org-mode-map
       (:localleader
        :desc "org-edit-src-code" "E" #'org-edit-src-code
        :desc "org-latex-preview" "L" #'org-latex-preview
        :desc "org-babel-demarcate-block" "D" #'org-babel-demarcate-block)
       (:leader
        :prefix "i"
        :desc "org-attach-dir" "a" (cmd! (insert (org-attach-dir-get-create))))))

(after! (org)
  (custom-declare-face 'ad/org-todo-scheme '((t :inherit (bold font-lock-builtin-face org-todo))) "")
  (custom-declare-face 'ad/org-todo-needy  '((t :inherit (bold font-lock-preprocessor-face org-todo))) "")
  (setq org-todo-keywords '((sequence
                             "MISSION(m/!)"
                             "QUEST(q/!)"
                             "ACTIVE(a!/!)"
                             "WAITING(w@/@)"
                             "SCHEME(s/@)"
                             "INVESTIGATE(i/@)"
                             "NEEDY(n/!)"
                             "|"
                             "COMPLETE(c)"
                             "FAILED(f@)")
                            (sequence
                             "[ ](M)"
                             "[-](A!)"
                             "[?](W@/@)"
                             "|"
                             "[X](C!)"
                             "[#](F@)"))
        org-todo-keyword-faces '(("QUEST"   . +org-todo-project)
                                 ("ACTIVE"  . +org-todo-active)
                                 ("WAITING" . +org-todo-onhold)
                                 ("SCHEME"  . ad/org-todo-scheme)
                                 ("NEEDY"   . ad/org-todo-needy)
                                 ("FAILED"  . +org-todo-cancel)
                                 ("[-]"     . +org-todo-active)
                                 ("[?]"     . +org-todo-onhold)
                                 ("[#]"     . +org-todo-cancel))))

(setq org-log-into-drawer t
      org-log-done 'time)

(add-hook! 'org-after-todo-state-change-hook
  (when (and (or (string-equal org-state "ACTIVE")
                 (string-equal org-state "[-]"))
             (y-or-n-p "Clock into this task?"))
    (org-clock-in)))
(advice-add #'org-clock-in :after
            (lambda (&rest _)
              (when (and (string-equal (org-get-todo-state) "MISSION")
                         (y-or-n-p "Change this task to active?"))
                (org-todo "ACTIVE"))
              (when (and (string-equal (org-get-todo-state) "[ ]")
                         (y-or-n-p "Change this task to active?"))
                (org-todo "[-]"))))

(after! org
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
           ("m" "mission")
           ("mm" "default" entry
            (file "inbox.org")
            "* MISSION %?\n"
            :prepend t)
           ("ms" "scheduled" entry
            (file "inbox.org")
            "* MISSION %?\nSCHEDULED: <%(org-read-date)>\n"
            :prepend t)
           ("md" "with deadline" entry
            (file "inbox.org")
            "* MISSION %?\nDEADLINE: <%(org-read-date)>\n"
            :prepend t)
           ("s" "scheme" entry
            (file "inbox.org")
            "* SCHEME %?\n"
            :prepend t)
           ("i" "investigate" entry
            (file "inbox.org")
            "* INVESTIGATE %?\n"
            :prepend t))))

(setq org-roam-directory org-directory
      org-roam-dailies-directory (expand-file-name "journal/" org-roam-directory)
      +org-roam-open-buffer-on-find-file nil)

(defvar ad/org-roam-icons
  '(("fc"      . "🎴")
    ("gtd"     . "🗓")
    ("journal" . "📖")
    ("log"     . "🗃")
    ("outline" . "📂")
    ("ref"     . "📚")
    ("work"    . "✏")
    ("zettel"  . "🗒")
    (nil       . "📥")))

(after! org-roam
  (cl-defmethod org-roam-node-doom-prefix ((node org-roam-node))
    (cdr (assoc (org-roam-node-doom-type node)
                ad/org-roam-icons)))
  (setq org-roam-node-display-template #("${doom-prefix} ${doom-hierarchy:96} ${todo:8} ${doom-type:12} ${doom-tags:24}" 20 35
                                         (face font-lock-keyword-face)
                                         36 51
                                         (face org-tag))))

(defun ad/update-roam-filename ()
  "Update org-roam file names when their titles change."
  (interactive)
  (when (and (org-roam-file-p)
             (-contains-p '("fc" "log" "outline" "ref" "work" "zettel") (f-filename (f-parent buffer-file-name))))
    (let ((new-file-location
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
(add-hook 'after-save-hook #'ad/update-roam-filename)

(defun ad/org-attach-dir-get-create (id)
  "Return existing or new directory associated with the given ID."
  (let ((attach-dir (org-attach-dir-from-id id)))
    (unless (file-directory-p attach-dir)
      (make-directory attach-dir t))
    attach-dir))

(defun ad/get-html-title (url)
  "Retrieve a URL and return the HTML title. "
  (with-current-buffer (url-retrieve-synchronously url)
    (goto-char (point-min))
    (re-search-forward "<title>\\([^<]*\\)</title>")
    (match-string 1)))

(defun ad/roam-template-path (template)
  "Given a template name, return the org-roam template path."
  (expand-file-name (concat template ".org")
                    (expand-file-name "template/" org-roam-directory)))
(setq org-roam-capture-templates
      `(("f" "fc" plain
         (file ,(ad/roam-template-path "fc"))
         :target (file "fc/%<%Y%m%d%H%M%S>-${slug}.org")
         :unnarrowed t)
        ("l" "log" plain
         (file ,(ad/roam-template-path "log"))
         :target (file "log/%<%Y%m%d%H%M%S>-${slug}.org")
         :unnarrowed t)
        ("o" "outline" plain
         (file ,(ad/roam-template-path "outline"))
         :target (file "outline/%<%Y%m%d%H%M%S>-${slug}.org")
         :unnarrowed t)
        ("r" "ref")
        ("rw" "website" plain
         (file ,(ad/roam-template-path "website"))
         :target (file "ref/%<%Y%m%d%H%M%S>-${slug}.org")
         :unnarrowed t)
        ("rc" "citekey" plain
         (file ,(ad/roam-template-path "citekey"))
         :target (file "ref/%<%Y%m%d%H%M%S>-${slug}.org")
         :unnarrowed t)
        ("w" "work")
        ("ww" "default" plain
         (file ,(ad/roam-template-path "work"))
         :target (file "work/%<%Y%m%d%H%M%S>-${slug}.org")
         :unnarrowed t)
        ("wl" "lab report" plain
         (file ,(ad/roam-template-path "lab-report"))
         :target (file "work/%<%Y%m%d%H%M%S>-${slug}.org")
         :unnarrowed t)
        ("z" "zettel" plain
         (file ,(ad/roam-template-path "zettel"))
         :target (file "zettel/%<%Y%m%d%H%M%S>-${slug}.org")
         :unnarrowed t)))

(setq org-roam-dailies-capture-templates
      `(("d" "default" entry
         (file "template/journal.org")
         :target (file+head
                  "%<%Y-%m-%d>.org"
                  "#+title: %<%Y-%m-%d %a>\n\n")
         :clock-in
         :clock-resume)))

(use-package! org-roam-ui
  :after org-roam
  :bind (:map doom-leader-notes-map
              ("r u" . org-roam-ui-mode))
  :init
  (use-package! websocket)
  :custom
  (org-roam-ui-sync-theme t)
  (org-roam-ui-follow t)
  (org-roam-ui-update-on-save t)
  (org-roam-ui-open-on-start nil))

(setq org-agenda-files `(,(expand-file-name "gtd/" org-directory))
      org-agenda-skip-scheduled-if-done t
      org-agenda-skip-deadline-if-done t
      org-agenda-todo-ignore-scheduled 'future
      org-agenda-skip-deadline-prewarning-if-scheduled 'pre-scheduled
      org-agenda-prefix-format '((agenda . " %i %(ad/custom-agenda-prefix 32) → %s%b") (todo . " %i %-32:(ad/custom-agenda-prefix 32) → %b") (tags . " %i %-12:c") (search . " %i %-12:c"))
      org-agenda-time-grid
      '((daily today require-timed)
        (800 1000 1200 1400 1600 1800 2000)
        "" "----------------")
      org-agenda-time-leading-zero t
      org-agenda-current-time-string "———————————————— now")

(defun ad/get-org-buffer-title (&optional buffer)
  "Given an org-mode BUFFER, return its title property."
  (with-current-buffer (or buffer (current-buffer))
    (nth 1 (car (org-collect-keywords '("TITLE"))))))

(defun ad/custom-agenda-prefix (len)
  "Create a customized agenda prefix of LEN."
  (if buffer-file-name
      (let ((len (if (string-empty-p time) len (- len (length time) 3)))
            (title (ad/get-org-buffer-title (find-file-noselect buffer-file-name))))
        (concat (if (> (length title) len)
                    (s-truncate len title "…")
                  (s-pad-right len " " title))
                (unless (string-empty-p time) " → ")
                time))
    (concat (make-string (- len (length time)) ? ) time)))

(setq org-export-with-section-numbers nil
      org-export-with-toc nil
      org-export-with-tags nil)

(define-advice org-export-output-file-name (:around (orig-fun extension &optional subtreep pub-dir))
  (unless pub-dir
    (setq pub-dir (expand-file-name "export/" org-directory))
    (unless (file-directory-p pub-dir)
      (make-directory pub-dir)))
  (apply orig-fun extension subtreep pub-dir nil))

(use-package! org-auto-tangle
  :hook (org-mode . org-auto-tangle-mode))

(map! :map org-noter-doc-mode-map
      "u" #'org-noter-insert-note)

(setq org-latex-compiler "lualatex")

(add-hook
 'org-mode-hook
 (lambda ()
   (when (and (org-roam-node-at-point)
          (string-equal (org-roam-node-doom-type (org-roam-node-at-point)) "work"))
      (turn-on-org-cdlatex))))

(setq! org-latex-classes '(("apa" "\\documentclass[11pt]{apa7}"
                            ("\\section{%s}"       . "\\section{%s}")
                            ("\\subsection{%s}"    . "\\subsection{%s}")
                            ("\\subsubsection{%s}" . "\\subsubsection{%s}")
                            ("\\paragraph{%s}"     . "\\paragraph{%s}")
                            ("\\subparagraph{%s}"  . "\\subparagraph{%s}"))
                           ("article" "\\documentclass[11pt]{article}"
                            ("\\section{%s}"       . "\\section*{%s}")
                            ("\\subsection{%s}"    . "\\subsection*{%s}")
                            ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                            ("\\paragraph{%s}"     . "\\paragraph*{%s}")
                            ("\\subparagraph{%s}"  . "\\subparagraph*{%s}"))
                           ("report" "\\documentclass[11pt]{report}"
                            ("\\part{%s}"          . "\\part*{%s}")
                            ("\\chapter{%s}"       . "\\chapter*{%s}")
                            ("\\section{%s}"       . "\\section*{%s}")
                            ("\\subsection{%s}"    . "\\subsection*{%s}")
                            ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))
                           ("book" "\\documentclass[11pt]{book}"
                            ("\\part{%s}"          . "\\part*{%s}")
                            ("\\chapter{%s}"       . "\\chapter*{%s}")
                            ("\\section{%s}"       . "\\section*{%s}")
                            ("\\subsection{%s}"    . "\\subsection*{%s}")
                            ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))))

(setq ispell-personal-dictionary (expand-file-name ".pws" org-directory))

(use-package! org-anki
  :custom
  (org-anki-inherit-tags nil)
  :config
  (map! (:map org-mode-map
         (:localleader
          (:prefix ("F" . "org-anki")
           :desc "cloze" :nv "c" #'org-anki-cloze-dwim
           :desc "sync" "s" #'org-anki-sync-entry
           :desc "sync all" "S" #'org-anki-sync-all
           :desc "update all" "u" #'org-anki-update-all
           :desc "delete" "d" #'org-anki-delete-entry
           :desc "delete all" "D" #'org-anki-delete-all)))))

(setq default-frame-alist (append default-frame-alist '((alpha-background . 0.75))))
(map! :map doom-leader-toggle-map
      :desc "Transparency" "t"
      (cmd!
       (set-frame-parameter
        nil 'alpha-background
        (let* ((parameter (frame-parameter nil 'alpha-background))
               (alpha (or (car-safe parameter) parameter)))
          (if (or (= alpha 1.0) (= alpha 100))
              0.75
            1.0)))))

(after! (evil-org)
  (setq evil-org-movement-bindings '((left  . "m")
                                     (down  . "n")
                                     (up    . "e")
                                     (right . "i"))))
(evil-collection-translate-key nil '(evil-normal-state-map evil-motion-state-map evil-visual-state-map evil-operator-state-map)
  "m" "h"
  "n" "j"
  "e" "k"
  "i" "l"
  "h" "e"
  "j" "m"
  "k" "n"
  "l" "u"
  "u" "i"
  "M" "H"
  "N" "J"
  "E" "K"
  "I" "L"
  "H" "E"
  "J" "M"
  "K" "N"
  "L" "U"
  "U" "I")
(evil-collection-translate-key nil '(evil-window-map)
  "m" "h"
  "n" "j"
  "e" "k"
  "i" "l"
  "h" "m"
  "k" "n")
(map! :after magit
      :map magit-mode-map
      :nmv "n" #'evil-next-line
      :nv  "j" #'magit-ediff-dwim
      :nmv "e" #'evil-previous-line
      :nmv "k" #'evil-ex-search-next
      :nmv "K" #'evil-ex-search-previous
      :nmv "E" #'+lookup/documentation)

(load! "mu4e.el.crypt")

(map! :map doom-leader-open-map
      :desc "Elfeed" "e" #'elfeed)
(add-hook 'elfeed-search-mode-hook #'elfeed-update)

(use-package! copilot
  :bind (:map doom-leader-toggle-map
         ("C" . #'copilot-mode)
         :map copilot-mode-map
         ("M-RET" . #'copilot-accept-completion)))

(use-package! pencil
  :load-path doom-user-dir
  :commands (pencil/complete-line pencil/complete-long))

(use-package! titlecase
  :after evil
  :config
  (map! :nv "g`" (evil-define-operator evil-titlecase (beg end)
                   (interactive "<r>")
                   (save-excursion
                     (set-mark beg)
                     (goto-char end)
                     (titlecase-dwim)))))

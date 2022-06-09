(setq user-full-name "Nick Salesky"
      user-mail-address "nicksalesky@gmail.com")

(setq doom-font (font-spec :family "JetBrainsMono" :size 16)
      doom-variable-pitch-font (font-spec :family "SourceSans3" :size 15)
      doom-big-font (font-spec :family "JetBrainsMono" :size 24))
(after! doom-themes
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t))
(custom-set-faces!
  '(font-lock-comment-face :slant italic)
  '(font-lock-keyword-face :slant italic))

(setq doom-theme 'doom-moonlight)
(map! :leader
      :desc "Load new theme" "h t" #'counsel-load-theme)

(set-face-attribute 'mode-line nil :font "JetBrainsMono")
(setq doom-modeline-height 30     ;; sets modeline height
      doom-modeline-bar-width 5   ;; sets right bar width
      doom-modeline-persp-name t  ;; adds perspective name to the modeline
      doom-modeline-persp-icon t) ;; adds a folder icon next to the perspective name

(setq frame-title-format
      '(""
        (:eval
         (if (s-contains-p org-roam-directory (or buffer-file-name ""))
             (replace-regexp-in-string
              ".*/[0-9]*-?" "☰ "
              (subst-char-in-string ?_ ? buffer-file-name))
           "%b"))
        (:eval
         (let ((project-name (projectile-project-name)))
           (unless (string= "-" project-name)
             (format (if (buffer-modified-p) " ◉ %s" "  ●  %s") project-name))))))

;; (if (eq initial-window-system 'x)
;;     (toggle-frame-maximized)
;;   (toggle-frame-fullscreen))

(defun ns/toggle-window-transparency ()
  "Toggle transparency."
  (interactive)
  (let ((alpha-transparency 75))
    (if (equal alpha-transparency (frame-parameter nil 'alpha-background))
        (set-frame-parameter nil 'alpha-background 100)
      (set-frame-parameter nil 'alpha-background alpha-transparency))))

(evil-global-set-key 'motion "j" 'evil-next-visual-line)
(evil-global-set-key 'motion "k" 'evil-previous-visual-line)

(map! :leader
      :desc "Org babel tangle" "m B" #'org-babel-tangle)
(after! org
  (setq org-directory "~/notes"
        org-agenda-files '("~/notes/agenda.org")
        org-default-notes-file (expand-file-name "notes.org" org-directory)
        org-return-follows-link t  ;; pressing RET follows links
        org-ellipsis " ▼ "
        org-superstar-headline-bullets-list '("◉" "●" "○" "◆" "●" "○" "◆")
        org-log-done 'time
        org-hide-emphasis-markers t))
        ;; org-todo-keywords ; This overwrites the default Doom org-todo-keywords
        ;; '((sequence
        ;;    "TODO(t)" ; A task that is ready to be tackled
        ;;    "WAIT(w)" ; Some thing that is holding up this task
        ;;    "HOLD(h)" ; Something that has been put on hold
        ;;    "|" ; Seperator between "active" and "inactive" states
        ;;    "DONE(d)" ; Task has been completed
        ;;    "CANCELLED(c)" ; Task has been canceled

(use-package org-modern
  :hook
  (org-mode . org-modern-mode)
  (org-agenda-finalize . org-modern-agenda))

;; (defun ns/org-mode-visual-fill ()
;;   (setq visual-fill-column-width 200
;;         visual-fill-column-center-text t)
;;   (visual-fill-column-mode 1))

;; (use-package visual-fill-column
;;   :hook (org-mode . ns/org-mode-visual-fill))

(custom-set-faces
  '(org-level-1 ((t (:inherit outline-1 :height 1.4))))
  '(org-level-2 ((t (:inherit outline-2 :height 1.3))))
  '(org-level-3 ((t (:inherit outline-3 :height 1.2))))
  '(org-level-4 ((t (:inherit outline-4 :height 1.1))))
  '(org-level-5 ((t (:inherit outline-5 :height 1.0))))
)

;(add-hook 'org-mode 'variable-pitch-mode)

(setq org-journal-dir "~/org/journal/"
      org-journal-date-prefix "* "
      org-journal-time-prefix "** "
      org-journal-date-format "%B %d, %Y (%A) "
      org-journal-file-format "%Y-%m-%d.org"
      org-journal-enable-agenda-integration t)

(after! org-roam
  (setq org-roam-directory "~/notes/roam"))

(after! org-roam
  (setq org-roam-capture-templates
       '(("d" ; the "key", a letter that you press to choose the template
          "default" ; the full name of the template
          plain ; the type of text being inserted, always =plain= for note templates
          "%?" ; the text that will be inserted into the new note, can be anything
          :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n") ; this list describes how the note file will be created
          :unnarrowed t) ; ensures that the full file will be displayed when captured

         ;; A capture template for a programming language
         ("l" "programming language" plain
          (file "~/notes/roam/templates/programming-language-template.org")
          :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
          :unnarrowed t)

         ;; A capture template for a project I'm working on
         ("p" "project" plain
          (file "~/notes/roam/templates/project-template.org")
          :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+filetags: Project")
          :unnarrowed t)
                        )))

(use-package org-remark
  :init
  (map! :leader
        (:prefix ("r" . "remark")
         ;; custom pen bindings
         :desc "Mark yellow" "y" #'org-remark-mark-yellow
         :desc "Mark green" "g" #'org-remark-mark-green
         :desc "Mark red" "r" #'org-remark-mark-red

         ;; general bindings
         :desc "Open current remark" "o" #'org-remark-open
         :desc "View current remark" "v" #'org-remark-view
         :desc "View previous remark" "p" #'org-remark-view-prev
         :desc "View next remark" "n" #'org-remark-view-next
         :desc "Delete remark" "d" #'org-remark-remove))
  :config

  ;; set up my pens
  (org-remark-create "orange"
                     '(:underline "gold" :background "dark orange")
                     '(CATEGORY "important"))
  (org-remark-create "green"
                     '(:background "lime green" :inherit shadow)
                     '(CATEGORY "vocab"))
  (org-remark-create "red"
                     '(:background "tomato" :underline "dark red" :inherit shadow)
                     '(CATEGORY "important"))

  (org-remark-global-tracking-mode))

(setq display-line-numbers-type t)
(map! :leader
      :desc "Comment or uncomment lines" "TAB TAB" #'comment-line
      (:prefix ("t" . "toggle")
       :desc "Toggle line numbers" "l" #'doom/toggle-line-numbers
       :desc "Toggle line highlight in frame" "h" #'hl-line-mode
       :desc "Toggle line highlight globally" "H" #'global-hl-line-mode
       :desc "Toggle truncate lines" "t" #'toggle-truncate-lines))

(setq confirm-kill-emacs nil)

(setq auto-save-default t
      auto-save-timeout 30)

(map! :leader
      (:prefix ("e". "evaluate")
       :desc "Evaluate elisp in buffer" "b" #'eval-buffer
       :desc "Evaluate defun" "d" #'eval-defun
       :desc "Evaluate elisp expression" "e" #'eval-expression
       :desc "Evaluate last sexpr" "l" #'eval-last-sexp
       :desc "Evaluate elisp in region" "r" #'eval-region))

(map! :leader
      (:prefix ("d" . "dired")
       :desc "Open dired" "d" #'dired
       :desc "Dired jump to current" "j" #'dired-jump)
      (:after dired
       (:map dired-mode-map
        :desc "Peep-dired image previews" "d p" #'peep-dired
        :desc "Dired view file" "d v" #'dired-view-file)))

(evil-define-key 'normal dired-mode-map
  (kbd "M-RET") 'dired-display-file
  (kbd "h") 'dired-up-directory
  (kbd "l") 'dired-open-file ; use dired-find-file instead of dired-open.
  (kbd "m") 'dired-mark
  (kbd "t") 'dired-toggle-marks
  (kbd "u") 'dired-unmark
  (kbd "C") 'dired-do-copy
  (kbd "D") 'dired-do-delete
  (kbd "J") 'dired-goto-file
  (kbd "M") 'dired-do-chmod
  (kbd "O") 'dired-do-chown
  (kbd "P") 'dired-do-print
  (kbd "R") 'dired-do-rename
  (kbd "T") 'dired-do-touch
  (kbd "Y") 'dired-copy-filenamecopy-filename-as-kill ; copies filename to kill ring.
  (kbd "+") 'dired-create-directory
  (kbd "-") 'dired-up-directory
  (kbd "% l") 'dired-downcase
  (kbd "% u") 'dired-upcase
  (kbd "; d") 'epa-dired-do-decrypt
  (kbd "; e") 'epa-dired-do-encrypt)

;; Get file icons in dired
(add-hook 'dired-mode-hook 'all-the-icons-dired-mode)

;; With dired-open plugin you can launch external programs for certain extensions
;;(setq dired-open-extensions '(("jpg" . "gimp")
;;                              ("mp4" . "vlc" )))

(evil-define-key 'normal peep-dired-mode-map
  (kbd "j") 'peep-dired-next-file
  (kbd "k") 'peep-dired-prev-file)
(add-hook 'peep-dired-hook 'evil-normalize-keymaps)

;; (map! :leader
;;       :desc "Switch to perspective NAME" "DEL" #'persp-switch
;;       :desc "Switch to buffer in perspective" "," #'persp-switch-to-buffer
;;       :desc "Switch to next perspective" "]" #'persp-next
;;       :desc "Switch to previous perspective" "[" #'persp-prev
;;       :desc "Add a buffer current perspective" "+" #'persp-add-buffer
;;       :desc "Remove perspective by name" "-" #'persp-remove-by-name)

(use-package wakatime-mode
  :ensure t
  :config
  (global-wakatime-mode))

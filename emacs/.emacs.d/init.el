(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(setq byte-compile-warnings '(not free-vars unresolved noruntime lexical make-local)
      native-comp-async-report-warnings-errors nil)

(setq gc-cons-threshold 100000000
      read-process-output-max (* 1024 1024))

(scroll-bar-mode -1)

(tooltip-mode -1)

(setq ring-bell-function 'ignore)

(set-fringe-mode 10)

(setq inhibit-startup-message t)

(setq use-short-answers t)

(setq confirm-nonexistent-file-or-buffer nil)

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(setq straight-host-usernames
      '((github . "nsalesky")))

(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

(use-package exec-path-from-shell
  :config
  (exec-path-from-shell-initialize))

(setq user-full-name "Nick Salesky"
      user-mail-address "nicksalesky@gmail.com")

(use-package saveplace
  :unless noninteractive
  :config
  (setq save-place-limit 1000)
  (save-place-mode))

(use-package savehist
  :unless noninteractive
  :defer 1
  :config
  (setq savehist-additional-variables '(compile-command kill-ring regexp-search-ring))
  (savehist-mode 1))

;; (use-package time
;;   :defer t
;;   :config
;;   (setq display-time-24hr-format nil))

;; TODO look into displaying the current time in the modeline

(global-auto-revert-mode 1)

(let ((backup-dir (concat user-emacs-directory "backups"))
      (auto-saves-dir (concat user-emacs-directory "auto-saves")))
  (dolist (dir (list backup-dir auto-saves-dir))
    (when (not (file-directory-p dir))
      (make-directory dir t)))
  (setq backup-directory-alist `(("." . ,backup-dir))
        auto-save-file-name-transforms `((".*" ,auto-saves-dir t))
        tramp-backup-directory-alist `((".*" . ,backup-dir))
        tramp-auto-save-directory auto-saves-dir))

(setq backup-by-copying t   ; Don't delink hardlinks
      delete-old-versions t ; Clean up the backups
      version-control t     ; Use version numbers on backups
      kept-new-versions 2   ; Keep some new versions of backups
      kept-old-versions 1)  ; Keep some old backups too

(setq backup-directory-alist
      `(("." . ,(concat user-emacs-directory "backups/"))))

(winner-mode 1)

;; (setq-default frame-title-format '("%b [%m]"))
(setq-default frame-title-format '("GNU Emacs"))

(global-visual-line-mode 1)

;; Enable line numbers
(column-number-mode)
;; (global-display-line-numbers-mode t)

;; Disable line numbers for some modes
(dolist (mode
         '(prog-mode-hook
           markdown-mode-hook
        ))
(add-hook mode (lambda () (display-line-numbers-mode 1))))

(use-package diminish
  :config
  (diminish 'buffer-face-mode))

(defvar ns/default-font "JetBrainsMono Nerd Font"
  "My custom default font choice.")

(defvar ns/fixed-pitch-font "JetBrainsMono Nerd Font"
  "My custom fixed pitch font choice.")

(defvar ns/variable-pitch-font "Iosevka Aile"
  "My custom variable pitch font choice.")

(custom-set-faces
 `(default ((t (:family ,ns/default-font :slant normal :weight regular :height 120 :width normal :foundry "JB  "))))
 `(fixed-pitch ((t (:family ,ns/fixed-pitch-font))))
 `(variable-pitch ((t (:family ,ns/variable-pitch-font)))))

(use-package all-the-icons)

;; (use-package autothemer)

(use-package doom-themes
  :config
  (load-theme 'doom-moonlight t))

(use-package ef-themes)
  ;; :config
  ;; (load-theme 'ef-summer t))

;; (use-package catppuccin-theme
;;   :straight (:type git :host github
;;                    :repo "catppuccin/emacs")
;;   :after autothemer)
  ;; :config (load-theme 'catppuccin-macchiato t))

(use-package modus-themes)
  ;; :custom
  ;; (modus-themes-italic-constructs t)     ; use italics for comments
  ;; (modus-themes-bold-constructs t)       ; use bold
  ;; (modus-themes-syntax '(faint))
  ;; (modus-themes-mixed-fonts t)           ; Enable fixed and variable pitched fonts
  ;; (modus-themes-prompts '(italic))
  ;; ;; (modus-themes-mode-line '(accented borderless))
  ;; (modus-themes-mode-line '())
  ;; (modus-themes-subtle-line-numbers t)

(use-package doom-modeline
  :init
  (setq doom-modeline-height 35
        doom-modeline-support-imenu t)
  (doom-modeline-mode 1))

;; Necessary for dashboard in order to get nice seperators between sections
(use-package page-break-lines)

(use-package dashboard
    :custom
    (dashboard-image-banner-max-width 256)
    (dashboard-startup-banner (expand-file-name "emacs.png" user-emacs-directory))
    (dashboard-center-content t)
    (dashboard-set-heading-icons t)
    (dashboard-set-file-icons t)
    (dashboard-projects-backend 'project-el)
    ;; (dashboard-projects-switch-function 'projectile-persp-switch-project)
    (dashboard-items '((recents . 5)
                          (projects . 5)
                          (agenda . 5)))
    (initial-buffer-choice (lambda () (get-buffer-create "*dashboard*")))
    ;; :hook (after-init-hook . dashboard-refresh-buffer)
    :config
    (dashboard-setup-startup-hook))

(pixel-scroll-mode)
(setq scroll-margin 5)

(use-package helpful
  :bind
  (("C-h f" . helpful-callable)
   ("C-h v" . helpful-variable)
   ("C-h k" . helpful-key)))

(use-package hl-todo
  :config
  (global-hl-todo-mode))

;; (add-hook 'prog-mode-hook 'hl-line-mode)

(defun ns/toggle-window-transparency ()
  "Toggle transparency."
  (interactive)
  (let ((alpha-transparency 90))
    (if (equal alpha-transparency (frame-parameter nil 'alpha-background))
        (set-frame-parameter nil 'alpha-background 100)
      (set-frame-parameter nil 'alpha-background alpha-transparency))))

;; Make the frame transparent when launched
;; (ns/toggle-window-transparency)

(use-package discover)

(use-package vertico
  :init
  (vertico-mode))

(use-package consult
  :bind
  (;; C-c bindings
   ("C-c h" . consult-history)
   ("C-c m" . consult-mode-command)
   ("C-c k" . consult-kmacro)
   ; M-g bindings
   ("M-g g" . consult-goto-line)
   ("M-g M-g" . consult-goto-line)

   ; Buffers
   ("C-x b" . consult-buffer)
   ("C-x 4 b" . consult-buffer-otther-window)
   ("C-x p b" . consult-project-buffer)

   ; Random
   ("C-x r b" . consult-bookmark)
   ("M-y" . consult-yank-pop)

   ; Special search bindings
   ("C-c q" . consult-line)
   ("C-c w" . consult-ripgrep)
   
   ; M-s bindings (search-map)
   ("M-s d" . consult-find)
   ("M-s r" . consult-ripgrep)
   ("M-s l" . consult-line)
   ("M-s L" . consult-line-multi)
   ("M-s k" . consult-keep-lines)
   ("M-s u" . consult-focus-lines)

   ; Isearch integration
   ("M-s e" . consult-isearch-history)
   :map isearch-mode-map
   ("M-e" . consult-isearch-history)
   ("M-s e" . consult-isearch-history)
   ("M-s l" . consult-line)
   ("M-s L" . consult-line-multi)

   ; Minibuffer history
   :map minibuffer-local-map
   ("M-s" . consult-history)
   ("M-r" . consult-history))
  :custom
  (consult-narrow-key (kbd "<")))

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '(
                                   (file (styles basic partial-completion))
                                   (eglot (styles orderless)))))

(use-package marginalia
  :bind
  (:map minibuffer-local-map
        ("M-A" . marginalia-cycle))
  :custom
  (marginalia-align 'right)
  :init
  (marginalia-mode))

(use-package all-the-icons-completion
  :after (marginalia all-the-icons)
  :hook (marginalia-mode . all-the-icons-completion-marginalia-setup)
  :init
  (all-the-icons-completion-mode))

(use-package embark
  ;; TODO: set up bindings for embark-act and embark-dwim
  :bind
  (("C-." . embark-act)
   ("M-." . embark-dwim))

  :config

  ;; Hide the mode line for Embark buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(use-package embark-consult
  :after (embark consult)
  :demand t
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package aggressive-indent
  :hook
  (emacs-lisp-mode-hook . aggressive-indent-mode))

(use-package rainbow-delimiters
    :hook (prog-mode . rainbow-delimiters-mode))

(use-package yasnippet
  :diminish yas-minor-mode
  :config
  (yas-global-mode))

(use-package which-key
  ;; :after (ivy)
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.3))

(use-package olivetti
  :custom
  (olivetti-body-width 110)
  (olivetti-style t))
  ;; :hook
  ;; (org-mode . olivetti-mode))

(use-package iedit
  :bind
  ("C-;" . iedit-mode))

(setq tramp-default-method "ssh") ;; Use SSH by default for remote files

(use-package expand-region
  :bind
  ("C-=" . er/expand-region))

(use-package mwim
  :bind
  ("C-a" . mwim-beginning)
  ("C-e" . mwim-end))

(setq mail-user-agent 'message-user-agent
      user-mail-address "nicksalesky@gmail.com"
      user-full-name "Nicholas Salesky"
      )

(use-package smtpmail-multi
  :config
  (setq smtpmail-multi-accounts
        '((gmail-main . ("nicksalesky@gmail.com" "smtp.gmail.com" 587 "nicksalesky@gmail.com" nil nil nil nil))))

  (setq smtpmail-multi-associations
        '(("nicksalesky@gmail.com" gmail-main)))

  (setq smtpmail-multi-default-account 'gmail-main)
  (setq message-send-mail-function 'smtpmail-multi-send-it)

  (setq smtpmail-debug-info t)
  (setq smtpmail-debug-verbose t)

  (when (>= emacs-major-version 25)
    (setq smtpmail-local-domain (car (split-string (shell-command-to-string "hostname -f"))))))

(use-package notmuch)

(use-package auth-source-pass
  :diminish t
  :config
  (auth-source-pass-enable))

(use-package dired
  :straight (:type built-in)
  :custom
  (dired-kill-when-opening-new-dired-buffer t))

;; (defun ns/toggle-between-implementation-and-tests ()
;;   (interactive)
  
;;   )

;; (use-package project
;;   :bind
;;   ("C-x p t" . ns/toggle-between-implementation-and-tests))

;; (use-package projectile
;;   :diminish projectile-mode
;;   :config (projectile-mode)
;;   ;; :custom ((projectile-completion-system 'ivy))
;;   :bind-keymap
;;   ("C-c p" . projectile-command-map)
;;   :init
;;   ;(when (file-directory-p "~/Documents")
;;     ;(setq projectile-project-search-path '("~/Documents")))
;;   (setq projectile-switch-project-action #'projectile-dired))

;; (use-package ripgrep)

(use-package treemacs
  :custom
  (treemacs-width 25)
  :bind
  ("M-0" . treemacs-select-window)
  ("C-c t 1" . treemacs-delete-other-windows)
  ("C-c t t" . treemacs)
  ("C-c t d" . treemacs-select-directory)
  ("C-c t B" . treemacs-bookmark)
  ("C-c t f" . treemacs-find-file))
;; (use-package treemacs-projectile
;;   :config
;;   (treemacs-project-follow-mode 1))
(use-package treemacs-icons-dired
    :hook (dired-mode . treemacs-icons-dired-enable-once))
;; (use-package treemacs-perspective
;;   :after (treemacs perspective))
(use-package treemacs-magit
    :after (treemacs magit))
(use-package treemacs-all-the-icons
  :config
  (treemacs-load-theme "all-the-icons"))

;; (defun ns/tab-bar-switch-or-create (name func)
;;   (if (ns/tab-bar-tab-exists name)
;;       (tab-bar-switch-to-tab name)
;;     (ns/tab-bar-new-tab name func)))

;; (defun ns/tab-bar-tab-exists (name)
;;   (member name
;;           (mapcar #'(lambda (tab) (alist-get 'name tab))
;;                   (tab-bar-tabs))))

;; (defun ns/tab-bar-new-tab (name func)
;;   (when (eq nil tab-bar-mode)
;;     (tab-bar-mode))
;;   (tab-bar-new-tab)
;;   (tab-bar-rename-tab name)
;;   (when func ;; If func is nil, don't try to run it
;;       (funcall func)))

(use-package tab-bar
  :straight (:type built-in)
  :custom
  (tab-bar-show nil))

  ;; :config
  ;; (setf mode-line-misc-info ;; I got this from the Hammy README.md
  ;;       ;; When the tab-bar is active, don't show global-mode-string
  ;;       ;; in mode-line-misc-info, because we now show that in the
  ;;       ;; tab-bar using `tab-bar-format-align-right' and
  ;;       ;; `tab-bar-format-global'.
  ;;       (remove '(global-mode-string ("" global-mode-string))
  ;;               mode-line-misc-info))
  ;; (unless (member 'tab-bar-format-global tab-bar-format)
  ;;   ;; Show `global-mode-string' in the tab bar.
  ;;   (setf tab-bar-format (append tab-bar-format '(tab-bar-format-align-right tab-bar-format-global)))))

(use-package tabspaces
  :straight (:type git :host github :repo "mclear-tools/tabspaces")
  :hook (after-init . tabspaces-mode)
  :commands (tabspaces-switch-or-create-workspace
             tabspaces-open-or-create-project-and-workspace)
  :custom
  (tabspaces-use-filtered-buffers-as-default t)
  (tabspaces-default-tab "Default")
  (tabspaces-remove-to-default t)
  (tabspaces-include-buffers '("*scratch*"))

  :bind (:map tabspaces-mode-map
            ("C-c TAB r" . tab-bar-rename-tab))

  ; sessions
  ; (tabspaces-session t)
  ; (tabspaces-session-auto-restore t))

  ;; Filter buffers for consult-buffer
  :config
  (with-eval-after-load 'consult
    ;; hide full buffer list
    (consult-customize consult--source-buffer :hidden t :default nil)
    ;; set consult-workspace buffer list
    (defvar consult--source-workspace
      (list :name     "Workspace Buffers"
            :narrow   ?w
            :history  'buffer-name-history
            :category 'buffer
            :state    #'consult--buffer-state
            :default  t
            :items    (lambda () (consult--buffer-query
                                  :predicate #'tabspaces--local-buffer-p
                                  :sort 'visibility
                                  :as #'buffer-name)))
      "Set workspace buffer list for consult-buffer.")
    (add-to-list 'consult-buffer-sources 'consult--source-workspace)))

(setq disabled-command-function nil)

(use-package lispy
  :hook ((emacs-lisp-mode . lispy-mode)
         (clojure-mode . lispy-mode))
  :custom
  (lispy-compat '(edebug cider magit-blame-mode)))

(electric-pair-mode 1)
(setq electric-pair-inhibit-predicate
      (lambda (char)
        (member major-mode '(org-mode))))

(global-unset-key (kbd "ESC ESC"))

(delete-selection-mode 1)

(use-package hydra)

;; Set the default tab settings
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)
(setq-default c-basic-offset 4)
(setq-default python-indent-offset 4)

;; Make the backspace properly erase the whole tab instead of removing
;; 1 space at a time
(setq backward-delete-char-untabify-method 'hungry)

;; Keep track of recently-opened files
(recentf-mode 1)
(setq recentf-max-menu-items 25)
(setq recentf-max-saved-items 25)
(global-set-key (kbd "C-x C-r") 'consult-recent-file)

(define-key global-map (kbd "M-o") 'ace-window)

(use-package dumb-jump
  :config
  (defhydra dumb-jump-hydra (:color blue :columns 3)
    "Dumb Jump"
    ("j" dumb-jump-go "Go")
    ("o" dumb-jump-go-other-window "Other window")
    ("e" dumb-jump-go-prefer-external "Go external")
    ("x" dumb-jump-go-prefer-external-other-window "Go external other window")
    ("i" dumb-jump-go-prompt "Prompt")
    ("l" dumb-jump-quick-look "Quick look")
    ("b" dumb-jump-back "Back"))
  (keymap-global-set "M-g j" 'dumb-jump-hydra/body))

(use-package magit
  :after blamer
  :bind
  (:prefix-map ns/git-prefix-map
               :prefix "C-x g"
               ("g" . magit-status)
               ("b" . blamer-mode)))

;; (use-package forge
;;   :after magit)
;; TODO set up personal access token personal to work with pull requests from Emacs  :after magit)

(use-package blamer)

(defun ns/org-mode-setup ()
   (org-indent-mode)
   ;; (variable-pitch-mode 1)
   (visual-line-mode 1))

(defun ns/org-font-setup ()
  ;; Make sure that anything that should be fixed pitch in Org files actually appears that way
    (set-face-attribute 'org-block nil :foreground nil :inherit
                        'fixed-pitch)
    (set-face-attribute 'org-code nil :inherit '(shadow fixed-pitch))
    (set-face-attribute 'org-table nil :inherit '(shadow fixed-pitch))
    ;; (set-face-attribute 'org-indent nil :inherit '(org-hide fixed-pitch))
    (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
    (set-face-attribute 'org-special-keyword nil :inherit
                    '(font-lock-comment-face fixed-pitch))
    (set-face-attribute 'org-meta-line nil :inherit
                        '(font-lock-comment-face fixed-pitch))
    (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch))

;; Org Mode
(use-package org
  :straight (:type built-in)
  :bind
  ("C-c l" . org-store-link)
  :hook (org-mode . ns/org-mode-setup)
  :config
  ;; (ns/org-font-setup)
  :custom
  (org-ellipsis "…")
  (org-pretty-entities t)
  (org-pretty-entities-include-sub-superscripts nil)
  (org-hide-emphasis-markers t)

  (org-use-property-inheritance t)

  (org-directory "~/Documents/notes")
  (org-default-notes-file "~/Documents/notes/notes.org")

  (org-src-tab-acts-natively t)
  (org-src-preserve-indentation t))

(use-package org-appear
  :straight (org-appear :type git :host github :repo "awth13/org-appear")
  :hook (org-mode . org-appear-mode))

(add-hook 'org-mode-hook 'variable-pitch-mode)

(require 'org-faces)

;; Resize Org headings
(dolist (face '((org-level-1 . 1.2)
                (org-level-2 . 1.1)
                (org-level-3 . 1.05)
                (org-level-4 . 1.0)
                (org-level-5 . 1.1)
                (org-level-6 . 1.1)
                (org-level-7 . 1.1)
                (org-level-8 . 1.1)))
  (set-face-attribute (car face) nil :font ns/variable-pitch-font :weight 'medium :height (cdr face)))

;; Make the document title a bit bigger
(set-face-attribute 'org-document-title nil :font ns/variable-pitch-font :weight 'bold :height 1.3)

;; Make sure certain org faces continue to use fixed-pitch face even whenn variable-pitch-mode is on
(set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
(set-face-attribute 'org-table nil :inherit 'fixed-pitch)
(set-face-attribute 'org-formula nil :inherit 'fixed-pitch)
(set-face-attribute 'org-code nil :inherit '(shadow fixed-pitch))
(set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
(set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
(set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
(set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch)

(org-babel-do-load-languages 'org-babel-load-languages
    '((emacs-lisp . t)
      (python . t)
      (clojure . t)
      (C . t)
      ;; (cpp . t)
      (shell . t)
      (eshell . t)
      (java . t)
      (js . t)
      (ruby . t)
      (sql . t)))

(setq org-confirm-babel-evaluate nil)

(setq
 org-agenda-files (directory-files-recursively "~/Documents/notes/" "\\.org$")

 org-agenda-todo-ignore-scheduled 'all
 org-agenda-todo-ignore-deadlines 'all
 org-agenda-todo-ignore-with-date 'all
 org-agenda-tags-todo-honor-ignore-options t

 org-todo-keywords '((sequence "TODO(t)" "WIP(w!)" "HOLD(h!)" "|" "DONE(d!)" "KILL(k!)"))

 org-log-done 'time    ; log the time when a task is *DONE*
 org-log-reschedule 'time
 org-log-redeadline 'time

 org-agenda-deadline-leaders '("DUE:       " "In %3d d.: " "%2d d. ago: ")
 org-agenda-scheduled-leaders '("DO:       " "Sched. %2dx: ")

 org-agenda-sticky t
 org-agenda-dim-blocked-tasks nil
 org-agenda-time-grid (quote
                       ((daily today remove-match)
                        (800 1200 1600 2000)
                        "......" "----------------")))
      
      ;org-agenda-hide-tags-regexp "."     ; hide all tags in the agenda

(add-to-list 'org-tags-exclude-from-inheritance "project")
(add-to-list 'org-tags-exclude-from-inheritance "rez")

(require 'cl)
(defun cmp-date-property-stamp (prop)
  "Compare two `org-mode' agenda entries, `A' and `B', by some date property.
If a is before b, return -1. If a is after b, return 1. If they
are equal return nil."
  (lexical-let ((prop prop))
	#'(lambda (a b)

		(let* ((a-pos (get-text-property 0 'org-marker a))
			   (b-pos (get-text-property 0 'org-marker b))
			   (a-date (or (org-entry-get a-pos prop)
						   (format "<%s>" (org-read-date t nil "now"))))
			   (b-date (or (org-entry-get b-pos prop)
						   (format "<%s>" (org-read-date t nil "now"))))
			   (cmp (compare-strings a-date nil nil b-date nil nil))
			   )
		  (if (eq cmp t) nil (signum cmp))
		  ))))

(with-eval-after-load "org-roam"
  ;; Got this from https://d12frosted.io/posts/2021-01-16-task-management-with-roam-vol5.html
  (defun ns/org-roam-files-by-tag (tag)
    "Finds the org roam files with the given TAG."
    (seq-uniq
     (seq-map
      #'car
      (org-roam-db-query
       [:select [nodes:file]
                :from tags
                :left-join nodes
                :on (= tags:node_id nodes:id)
                :where (= tag $s1)]
       tag))))

  (setq org-agenda-custom-commands
        '(
          ("r" "Resonance Cal" tags "Type={.}"
	       ((org-agenda-files (ns/org-roam-files-by-tag "rez"))
	        (org-overriding-columns-format
		     "%35Item %Type %Start %Fin %Rating")
	        (org-agenda-cmp-user-defined
		     (cmp-date-property-stamp "Start"))
	        (org-agenda-sorting-strategy
		     '(user-defined-down))
            (org-agenda-overriding-header "C-u r to re-run Type={.}")
            (org-agenda-mode-hook
	         (lambda ()
	           (visual-line-mode -1)
	           (setq truncate-lines 1)
	           (setq display-line-numbers-offset -1)
	           (display-line-numbers-mode 1)))
	        (org-agenda-view-columns-initially t)))
          ("u" "Super view"
           ((agenda "" ((org-agenda-span 'day)
                        (org-agenda-overriding-header "Time-Sensitive")
                        (org-super-agenda-groups
                         '(
                           (:discard (:todo ("DONE")))
                           (:name "Today"
                                  :tag ("bday" "ann" "hols" "cal" "today")
                                  :scheduled today
                                  :time-grid t
                                  ;; :todo ("WIP" "TODO")
                                  :order 0)
                           ;; (:name "Due Today"
                           ;;        :deadline today
                           ;;        :order 2)
                           ;; (:name "Overdue"
                           ;;        :deadline past)
                           ;; (:name "Reschedule"
                           ;;        :scheduled past)
                           (:name "Personal"
                                  :tag "perso")
                           (:name "School"
                                  :tag "school")
                           (:name "Work"
                                  :tag "work")))))
                           ;; (:name "Due Soon"
                           ;;        :deadline future
                           ;;        :scheduled future)
                           ;; ))))
            (tags
             (concat "w" (format-time-string "%V"))
             ((org-agenda-overriding-header
               (concat "Todos Week " (format-time-string "%V")))
              (org-super-agenda-groups
               '(
                 (:discard (:deadline t))
                 (:discard (:scheduled t))
                 (:discard (:todo ("DONE")))
                 (:name "Someday" :tag "someday")
                 (:name "Personal"
                        :and (:tag "perso" :not (:tag "someday")))
                 (:name "School"
                        :and (:tag "school" :not (:tag "someday")))
                 (:name "Work"
                        :and (:tag "work" :not (:tag "someday")))
                 ))))))
          ("t" "Todo View"
           (
            (todo "" ((org-agenda-overriding-header "")
                      (org-super-agenda-groups
                       '(
                         (:auto-category t :order 9)
                         )))))))))

(use-package org-super-agenda
  :after org-agenda
  :init
  ;; (setq org-agenda-skip-scheduled-if-done t
  ;;       org-agenda-skip-deadline-if-done t
  ;;       org-agenda-include-deadlines t
  ;;       org-agenda-block-separator nil
  ;;       org-agenda-compact-blocks t
  ;;       org-agenda-start-day nil
  ;;       org-agenda-span 1
  ;;       org-agenda-start-on-weekday nil)
  :config
  (org-super-agenda-mode))

(keymap-global-set "C-c c" 'org-capture)
(keymap-global-set "C-c a" 'org-agenda)

(defun ns/org-agenda-reload-files ()
  (interactive)
  (message "Reloading agenda files")
  (setq org-agenda-files (directory-files-recursively "~/Documents/notes/" "\\.org$")))

(keymap-global-set "C-c r" 'ns/org-agenda-reload-files)

(defun ns/org-present-begin ()
  (setq-local ns/olivetti-mode-enabled (bound-and-true-p olivetti-mode)) ;; remember if olivetti was already enabled or not
  (olivetti-mode 1)                                                      ;; enable olivetti-mode regardless

  ;; Tweak the font sizes
  (setq-local face-remapping-alist '((default (:height 1.5) variable-pitch)
                                     (header-line (:height 4.0) variable-pitch)
                                     (org-document-title (:height 1.75) org-document-title)
                                     (org-code (:height 1.55) org-code)
                                     (org-verbatim (:height 1.55) org-verbatim)
                                     (org-block (:height 1.25) org-block)
                                     (org-block-begin-line (:height 0.7) org-block)))

  ;; Set a blank header line string to create some blank space at the top
  (setq-local header-line-format " ")

  (message "Starting presentation. Good luck!"))

(defun ns/org-present-end ()
  (unless (symbol-value 'ns/olivetti-mode-enabled)
    (olivetti-mode 0))                                                   ;; disable olivetti-mode only if it wasn't open before the presentation

  ;; Reset the font customizations
  (setq-local face-remapping-alist '((default variable-pitch default)))

  ;; Reset the header line back to nothing
  (setq-local header-line-format nil)

  (message "Ending presentation. Nice job!"))

(use-package org-present
  :config
  (add-hook 'org-present-mode-hook 'ns/org-present-begin)
  (add-hook 'org-present-mode-quit-hook 'ns/org-present-end))

(use-package emacsql-sqlite-builtin)

(use-package org-roam
  :diminish
  :bind
  (:prefix-map ns/notes-prefix-map
               :prefix "C-c n"
               ("l" . org-roam-buffer-toggle)
               ("f" . org-roam-node-find)
               ("g" . org-roam-graph)
               ("i" . org-roam-node-insert)
               ("c" . org-roam-capture)
               ;; Dailies
               ("d" . org-roam-dailies-goto-today)
               ("j" . org-roam-dailies-capture-today))
  :custom
  (org-roam-directory (file-truename "~/Documents/notes/"))
  (org-roam-file-extensions '("org" "md"))
  (org-roam-dailies-directory "logs")
  (org-roam-database-connector 'sqlite-builtin)
  (org-roam-capture-templates
   '(("d" "default" plain (file "~/Documents/notes/capture-templates/default.org")
      :target (file "${slug}.org")
      :unnarrowed t)
     ("r" "Rez" plain (file "~/Documents/notes/capture-templates/rez.org")
      :target (file "${slug}.org")
      :unnarrowed t)
     ("p" "Project" plain (file "~/Documents/notes/capture-templates/project.org")
      :target (file "${slug}.org")
      :unnarrowed t)
     ("7" "Weekly" plain (file "~/Documents/notes/capture-templates/weekly.org")
      :target (file "logs/${slug}.org")
      :unnarrowed t)))
  (org-roam-dailies-capture-templates
      '(("d" "default" plain
         (file "~/Documents/notes/capture-templates/daily.org")
         :target (file "%<%Y-%m-%d>.org"))))
  :init
  (setq org-roam-v2-ack t)
  :config
  (setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
  (org-roam-db-autosync-enable))
(require 'org-roam) ;; Force org-roam to load

(use-package consult-org-roam
   :after org-roam
   :init
   (require 'consult-org-roam)
   (consult-org-roam-mode 1)
   :custom
   (consult-org-roam-grep-func #'consult-ripgrep)
   ;; Configure a custom narrow key for `consult-buffer'
   (consult-org-roam-buffer-narrow-key ?r)
   ;; Display org-roam buffers right after non-org-roam buffers
   ;; in consult-buffer (and not down at the bottom)
   (consult-org-roam-buffer-after-buffers t)
   :config
   ;; Eventually suppress previewing for certain functions
   (consult-customize
    consult-org-roam-forward-links
    :preview-key (kbd "M-."))
   :bind
   ;; Define some convenient keybindings as an addition
   ("C-c n f" . consult-org-roam-file-find)
   ("C-c n b" . consult-org-roam-backlinks)
   ("C-c n l" . consult-org-roam-forward-links)
   ("C-c n s" . consult-org-roam-search))

(use-package org-roam-ui
  :straight
    (:host github :repo "org-roam/org-roam-ui" :branch "main" :files ("*.el" "out"))
    :after org-roam
;;  :hook (after-init . org-roam-ui-mode)
    :custom
    (org-roam-ui-sync-theme t)
    (org-roam-ui-follow t)
    (org-roam-ui-update-on-save t)
    (org-roam-ui-open-on-start t))

(keymap-global-set "M-&" 'with-editor-async-shell-command)

(use-package term
  :custom
  (explicit-shell-file-name "/usr/bin/fish"))

(use-package eterm-256color
  :hook
  (term-mode . eterm-256color-mode))

(use-package vterm
  :custom
  (vterm-shell "fish")
  (vterm-max-scrollback 10000))

(use-package multi-vterm
  :bind
  (:prefix-map ns/multi-vterm-prefix-map
               :prefix "C-c v"
               ("v" . multi-vterm)
               ("C-p" . multi-vterm-prev)
               ("p" . multi-vterm-prev)
               ("C-n" . multi-vterm-next)
               ("n" . multi-vterm-next)
               ("t" . multi-vterm-dedicated-toggle)
               ("p" . multi-vterm-project)
               ("r" . multi-vterm-rename-buffer)))

;; (use-package lsp-mode
;;     :commands (lsp lsp-deferred)
;;     :custom
;;     (lsp-keymap-prefix "C-c l")
;;     (lsp-enable-which-key-integration t)
;;     (lsp-lens-enable t)
;;     (lsp-signature-auto-activate nil)
;;     (lsp-ui-doc-mode t))
;;     :custom

;;     ;; Enable/disable type hints as you type for Rust
;;     (lsp-rust-analyzer-server-display-inlay-hints t)
;;     (lsp-rust-analyzer-display-lifetime-elision-hints-enable "skip_trivial")
;;     (lsp-rust-analyzer-display-chaining-hints nil)
;;     (lsp-rust-analyzer-display-lifetime-elision-hints-use-parameter-names nil)
;;     (lsp-rust-analyzer-display-closure-return-type-hints t)
;;     (lsp-rust-analyzer-display-parameter-hints t)
;;     (lsp-rust-analyzer-display-reborrow-hints nil))

;; ;; (use-package lsp-ivy)

;; (use-package lsp-ui
;;     :hook (lsp-mode . lsp-ui-mode)
;;     :custom
;;     (lsp-ui-peek-always-show t)
;;     (lsp-ui-sideline-show-hover t)
;;     (lsp-ui-doc-position 'bottom)
;;     (lsp-ui-doc-enable nil))

(use-package treesit-auto
  :straight (:type git :host github :repo "renzmann/treesit-auto")
  :custom
  (treesit-auto-install 'prompt)
  :config
  (global-treesit-auto-mode))

;; (require 'treesit)
;; (defun ns/tree-sitter-compile-grammar (destination &optional path)
;;   "Compile grammar at PATH, and place the resulting shared library in DESTINATION."
;;   (interactive "fWhere should we put the shared library? \nfWhat tree-sitter grammar are we compiling? \n")
;;   (make-directory destination 'parents)

;;   (let* ((default-directory
;;           (expand-file-name "src/" (or path default-directory)))
;;          (parser-name
;;           (thread-last (expand-file-name "grammar.json" default-directory)
;;                        (json-read-file)
;;                        (alist-get 'name)))
;;          (emacs-module-url
;;           "https://raw.githubusercontent.com/casouri/tree-sitter-module/master/emacs-module.h")
;;          (tree-sitter-lang-in-url
;;           "https://raw.githubusercontent.com/casouri/tree-sitter-module/master/tree-sitter-lang.in")
;;          (needs-cpp-compiler nil))
;;     (message "Compiling grammar at %s" path)

;;     (url-copy-file emacs-module-url "emacs-module.h" :ok-if-already-exists)
;;     (url-copy-file tree-sitter-lang-in-url "tree-sitter-lang.in" :ok-if-already-exists)

;;     (with-temp-buffer
;;       (unless
;;           (zerop
;;            (apply #'call-process
;;                   (if (file-exists-p "scanner.cc") "c++" "cc") nil t nil
;;                   "parser.c" "-I." "--shared" "-o"
;;                   (expand-file-name
;;                    (format "libtree-sitter-%s%s" parser-name module-file-suffix)
;;                    destination)
;;                   (cond ((file-exists-p "scanner.c") '("scanner.c"))
;;                         ((file-exists-p "scanner.cc") '("scanner.cc")))))
;;         (user-error
;;          "Unable to compile grammar, please file a bug report\n%s"
;;          (buffer-string))))
;;     (message "Completed compilation")))

;; (use-package tree-sitter-rust
;;   :straight (:type git :host github :repo "tree-sitter/tree-sitter-rust"
;;              :post-build
;;              (ns/tree-sitter-compile-grammar
;;               (expand-file-name "ts-grammars" user-emacs-directory))))

;; (require 'treesit)
;; (setq treesit-extra-load-path (list (expand-file-name "ts-grammars" user-emacs-directory)))

;; (defun ns/compile-tree-sitter-grammar
;;     (language destination)
;;   (make-directory destination 'parents)
;;   (let ((shared-lib-name (format "libtree-sitter-%s.so" language)))
;;     (shell-command (concat "./build.sh" language))))
;;     ;; (f-move (concat "./dist/" shared-lib-name)
;;     ;;         (expand-file-name shared-lib-name destination))))

;; (defun ns/compile-tree-sitter-grammars
;;     (destination)
;;   (make-directory destination 'parents)
  
;;   ;; (async-start
;;    ;; (lambda ()
;;      (shell-command "./batch.sh")
;;      (f-move "./dist" destination))

;; (ns/compile-tree-sitter-grammars (expand-file-name "ts-grammars" user-emacs-directory))

;; (use-package tree-sitter-module
;;   :straight (:type git :host github :repo "casouri/tree-sitter-module"
;;                    :post-build (ns/compile-tree-sitter-grammars
;;                                 (expand-file-name "ts-grammars" user-emacs-directory))))

;; (use-package company
;;     :hook (prog-mode . company-mode)
;;     :bind (:map company-active-map
;;         ("<tab>" . company-complete-selection))
;;         ;; (:map lsp-mode-map
;;         ;; ("<tab>" . company-indent-or-complete-common))
;;     :custom
;;     (company-minimum-prefix-length 1)
;;     (company-idle-delay 0.0))

;; ;; Adds colors and icons to company-mode
;; (use-package company-box
;;     :hook (company-mode . company-box-mode))

(use-package eglot
  :bind
  (:prefix-map ns/eglot-actions-map
               :prefix "C-c e"
               ("a" . eglot-code-actions)
               ("f" . eglot-format-buffer))
  :custom
  (eglot-events-buffer-size 0) ; Disable the events buffer for performance
  (eglot-send-changes-idle-time 0.5)

  ;; TODO: (hopefully) temporary hack for Treesitter support
  :config
  (add-to-list 'eglot-server-programs
               '(python-ts-mode . ("pylsp"))))

(use-package corfu
  :straight (corfu :files (:defaults "extensions/*")
                   :includes (corfu-info corfu-history))
  :bind
  (:map corfu-map
        ("C-n" . corfu-next)
        ("C-p" . corfu-previous)
        ("<escape>" . corfu-quit)
        ("C-g" . corfu-quit)
        ("<return>" . corfu-insert)
        ("M-d" . corfu-show-documentation)
        ("M-l" . corfu-show-location))

  :custom
  (corfu-auto t)
  (corfu-auto-prefix 3) ; Minimum length of prefix for auto-complete
  (corfu-auto-delay 0) ; Immediately start auto-completion

  (corfu-popupinfo-delay 0)

  (corfu-min-width 80) ; Min width of popup, I like to have it consistent
  (corfu-max-width corfu-min-width) ; Always have the same width
  (corfu-count 14) ; Max number of candidates to show
  (corfu-scroll-margin 4)
  ;; (corfu-cycle nil)

  ;; (corfu-quit-at-boundary nil)
  ;; (corfu-seperator ?\s)            ; Use space
  ;; (corfu-quit-no-match 'seperator) ; Don't quit if there is 'corfu-seperator' inserted
  ;; (corfu-quit-no-match t)
  (corfu-preview-current 'insert)  ; Preview first candidate
  (corfu-preselect-first t)        ; Preselect first candidate?

  ;; Enable indentation+completion using the TAB key instead of M-TAB
  (tab-always-indent 'complete)
  ;; (completion-cycle-threshold nil)

  (corfu-excluded-modes '(eshell-mode))

  :init
  (global-corfu-mode)
  (corfu-popupinfo-mode))

(use-package cape)

(use-package kind-icon
  :custom
  (kind-icon-default-face 'corfu-default)
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(use-package eldoc-box
  :hook
  (eglot-managed-mode . eldoc-box-hover-mode))

(use-package format-all)
  ;:hook
  ;(prog-mode . format-all-mode)

(use-package flymake
  :bind
  (:map flymake-mode-map
        ("M-p" . flymake-goto-prev-error)
        ("M-n" . flymake-goto-next-error)))

(use-package realgud)

(use-package verb
  :after (org)
  :config
  (define-key org-mode-map (kbd "C-c C-r") verb-command-map))

(use-package wakatime-mode
  :diminish wakatime-mode
  :config
  (global-wakatime-mode))

(use-package ws-butler
  :diminish ws-butler-mode
  :hook
  (prog-mode . ws-butler-mode))

(use-package imenu-list)

(use-package re-builder
  :custom
  (reb-re-syntax 'string))

(use-package yaml-mode
  :mode "\\.yml\\'")

;; (add-hook 'c-mode-hook 'lsp)
;; (add-hook 'c++-mode-hook 'lsp)
(add-hook 'c-mode-hook 'eglot-ensure)
(add-hook 'c++-mode-hook 'eglot-ensure)

(use-package d-mode
  :straight (:type git :host github :repo "nsalesky/Emacs-D-Mode")
  :mode "\\.d\\'"
  :hook
  (d-mode . eglot-ensure)
  :config
  (add-to-list 'org-src-lang-modes '("d" . d))
  (add-to-list 'eglot-server-programs '(d-mode . ("/home/nsalesky/bin/serve-d"))))

;; (use-package dockerfile-mode
;;   :mode "Dockerfile\\'")

(defun ns/setup-cider-format-hook
    ()
  (add-hook 'before-save-hook 'cider-format-buffer nil t))

(use-package clojure-mode
  :mode "\\.clj\\'")

(use-package cider
  :hook
  (clojure-mode . ns/setup-cider-format-hook)
  (clojurescript-mode . ns/setup-cider-format-hook)
  (clojurec-mode . ns/setup-cider-format-hook))

(use-package glsl-mode
  :mode ("\\.glsl\\'" "\\.vert\\'" "\\.frag\\'" "\\.geom\\'"))

(use-package go-mode
  :mode "\\.go\\'"
  :hook (go-mode . eglot-ensure))

(defun ns/compile-tex-doc ()
  "Asynchronously compile the current tex buffer to a pdf."
  (start-process "pdflatex" nil "pdflatex" (buffer-file-name)))
  ;; (async-shell-command (concat "pdflatex " (buffer-file-name))))

(use-package tex-mode
  :hook (latex-mode . (lambda () (add-hook 'after-save-hook #'ns/compile-tex-doc nil t))))

(use-package markdown-mode
  :mode "\\.md\\'")

(use-package protobuf-mode
  :mode "\\.proto\\'")

(use-package python-mode
  :hook (python-mode . eglot-ensure)
  ;; :hook (python-mode . (lambda ()
                         ;; (eglot-ensure)
                         ;; (setq tab-width 4)))
  :custom
  (python-shell-interpreter "python3")
  :config
  (setq python-ts-mode-hook python-mode-hook))

(use-package pipenv
  :hook (python-mode . pipenv-mode))

(use-package typescript-mode
  :mode ("\\.ts\\'" "\\.tsx\\'" "\\.js\\'" "\\.jsx\\'")
  :hook (typescript-mode . eglot-ensure)
  :config
  (setq typescript-indent-level 4))

;; (defun tide-completion-at-point ()
;;   (let ((prefix (progn (looking-back "[a-zA-Z_$]\*" 50 t) (match-string 0))))
;;     (tide-command:completions
;;      prefix
;;      `(lambda (response)
;;         (completion-in-region (- (point) (length ',prefix)) (point)
;;                               (loop for completion in response
;;                                     if (string-prefix-p ',prefix completion)
;;                                     collect completion))))))

;; (defun ns/setup-tide-mode ()
;;   (interactive)
;;   (tide-setup)
;;   (tide-hl-identifier-mode +1)
;;   (add-hook 'before-save-hook #'tide-format-before-save nil t)
;;   (add-hook 'completion-at-point-functions #'tide-completion-at-point nil t))
;;   ;; (add-hook 'completion-at-point-functions (cape-company-to-capf #'company-tide) nil t))

;; (use-package tide
;;   :after web-mode
;;   :init
;;   :hook
;;   (typescript-mode . ns/setup-tide-mode)
;;   :custom
;;   (tide-format-options '(:insertSpaceAfterFunctionKeywordForAnonymousFunctions t :placeOpenBraceOnNewLineForFunctions nil)))
  ;; (tide-completion-setup-company-backend t))

(use-package ruby-mode
  :hook (ruby-mode . eglot-ensure))

(use-package inf-ruby) ;; Interact with a Ruby REPL

;; (defun ns/setup-eglot-rust ()
;;   (setq-local eglot-workspace-configuration
;;               '(:rust-analyzer
;;                 (:procMacro (:attributes (:enable t)
;;                                          :enable t)
;;                             :cargo (:buildScripts (:enable t))
;;                             :diagnostics (:disabled ["unresolved-proc-macro"
;;                                                      "unresolved-macro-call"])))))

;; (defclass eglot-rust-analyzer (eglot-lsp-server) ()
;;   :documentation "A custom class for rust-analyzer.")

;; (cl-defmethod eglot-initialization-options ((server eglot-rust-analyzer))
;;   eglot-workspace-configuration)

(use-package rustic
  :custom
  (rustic-lsp-client 'eglot)
  :hook
  (rustic-mode . (lambda () (flycheck-mode -1)))
  ;; (rustic-mode . ns/setup-eglot-rust)
  (rustic-mode . eglot-ensure))
  ;; :config
  ;; (add-to-list 'eglot-server-programs
  ;;              '(rustic-mode . (eglot-rust-analyzer "rust-analyzer"))))

  ;comment to disable rustfmt on save
  ;(setq rustic-format-on-save t))

;; (defun ns/toggle-web-mode ()
;;   "Toggles web-mode on or off, switching back to the previous major mode when disabled."
;;   (interactive)
;;   (if (eq 'web-mode major-mode)
;;       (funcall (symbol-value 'ns/prev-major-mode))
;;     (progn
;;       ;; (setq-local ns/prev-major-mode major-mode)
;;       (set (make-local-variable 'ns/prev-major-mode) major-mode)
;;       (web-mode))))

(use-package web-mode
    :commands (web-mode)
    :mode (("\\.html" . web-mode)
            ("\\.htm" . web-mode)
            ;; ("\\.tsx\\'" . web-mode)
            ;; ("\\.jsx\\'" . web-mode)
            ("\\.mustache\\'" . web-mode)
            ("\\.phtml\\'" . web-mode)
            ("\\.as[cp]x\\'" . web-mode)
            ("\\.erb\\'" . web-mode)
            ("\\.sgml\\'" . web-mode)))
    ;; :bind
    ;; ("C-c h" . ns/toggle-web-mode))

(use-package yuck-mode
  :mode "\\.yuck\\'")

(require 'transient)

(define-prefix-command 'ns/files-map)
(keymap-global-set "C-c f" 'ns/files-map)

(transient-define-prefix ns/visit-note-transient ()
  "Visit common note files."
  ["Visit common note files"
   ["Agenda"
    ("a" "agenda.org" (lambda () (interactive) (find-file (expand-file-name "agenda.org" org-directory))))
    ("p" "projects.org" (lambda () (interactive) (find-file (expand-file-name "projects.org" org-directory))))
    ("i" "inbox.org" (lambda () (interactive) (find-file (expand-file-name "inbox.org" org-directory))))
    ]
   ["Config"
    ("c" "config.org" (lambda () (interactive) (find-file (expand-file-name "config.org" user-emacs-directory))))
    ("I" "init.el" (lambda () (interactive) (find-file (expand-file-name "init.el" user-emacs-directory))))
    ]
   ])

(define-key 'ns/files-map (kbd "f") 'ns/visit-note-transient)

;; (require 'erc-sasl)

;; (add-to-list 'erc-sasl-server-regexp-list "irc\\.libera\\.chat")

;; ;; Redefine/Override the erc-login() function from the erc package, so that
;; ;; it now uses SASL
;; (defun erc-login ()
;;   "Perform user authentication at the IRC server. (PATCHED)"
;;   (erc-log (format "login: nick: %s, user: %s %s %s :%s"
;;            (erc-current-nick)
;;            (user-login-name)
;;            (or erc-system-name (system-name))
;;            erc-session-server
;;            erc-session-user-full-name))
;;   (if erc-session-password
;;       (erc-server-send (format "PASS %s" erc-session-password))
;;     (message "Logging in without password"))
;;   (when (and (featurep 'erc-sasl) (erc-sasl-use-sasl-p))
;;     (erc-server-send "CAP REQ :sasl"))
;;   (erc-server-send (format "NICK %s" (erc-current-nick)))
;;   (erc-server-send
;;    (format "USER %s %s %s :%s"
;;        ;; hacked - S.B.
;;        (if erc-anonymous-login erc-email-userid (user-login-name))
;;        "0" "*"
;;        erc-session-user-full-name))
;;   (erc-update-mode-line))

(setq erc-server "irc.libera.chat"
      erc-nick "abcd987"              ; change this
      erc-autojoin-channels-alist '((Libera.Chat
                                     "#systemcrafters"
                                     "#emacs"
                                     "#go-nuts"
                                     "##rust"))
      erc-track-shorten-start 8
      erc-kill-buffer-on-part t
      erc-auto-query 'bury)

;; (use-package mastodon
;;   :custom
;;   (mastodon-instance-url "https://emacs.ch")
;;   (mastodon-active-user "nsalesky")
;;   :config
;;   (mastodon-discover))

(use-package tablist)

(use-package pdf-tools
  :config
  (pdf-loader-install))

(use-package saveplace-pdf-view)

(use-package request)
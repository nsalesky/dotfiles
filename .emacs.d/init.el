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

(use-package async)

;; (setq-default frame-title-format '("%b [%m]"))
(setq-default frame-title-format '("GNU Emacs"))

(global-visual-line-mode 1)

;; Enable line numbers
(column-number-mode)
(global-display-line-numbers-mode t)

;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
        term-mode-hook
        shell-mode-hook
        eshell-mode-hook
        treemacs-mode-hook
        pdf-view-mode-hook
        vterm-mode-hook
        ))
(add-hook mode (lambda () (display-line-numbers-mode 0))))

(set-face-attribute 'default nil :font "JetBrains Mono" :height 120)
(set-face-attribute 'fixed-pitch nil :font "JetBrains Mono" :height 120)
(set-face-attribute 'variable-pitch nil :font "Iosevka Aile" :height 140)

;; (set-face-attribute 'default nil :font "JetBrains Mono" :height 120)
;; (set-face-attribute 'default nil :font "Rec Mono Semi Casual" :height 120)
;; (set-face-attribute 'fixed-pitch nil :font "Rec Mono Semi Casual" :height 120)

(use-package all-the-icons)

;; (use-package emojify
;;   :config
;;   (global-emojify-mode))

(use-package autothemer)

(use-package doom-themes
  :config
  (load-theme 'doom-palenight t))

(use-package ef-themes)
  ;; :config
  ;; (load-theme 'ef-summer t))

(use-package catppuccin-theme
  :straight (:type git :host github
                   :repo "catppuccin/emacs")
  :after autothemer)
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


  
  ;; :custom
  ;; (counsel-describe-function-function #'helpful-callable)
  ;; (counsel-describe-variable-function #'helpful-variable)
  ;; :bind
  ;; ([remap describe-function] . counsel-describe-function)
  ;; ([remap describe-command] . helpful-command)
  ;; ([remap describe-variable] . counsel-describe-variable)
  ;; ([remap describe-key] . helpful-key))

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

   ; M-s bidnings (search-map)
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

(setq tramp-default-method "ssh") ;; Use SSH by default for remote files

(use-package expand-region
  :bind
  ("C-=" . er/expand-region))

(use-package mwim
  :bind
  ("C-a" . mwim-beginning)
  ("C-e" . mwim-end))

(use-package flyspell
  :hook ((prog-mode . flyspell-prog-mode)
        ((org-mode markdown-mode) . flyspell-mode)))

(use-package flyspell-correct
  :after (flyspell)
  :bind
  (:map flyspell-mode-map
        ("C-;" . flyspell-correct-wrapper)))

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

;; (use-package hammy
;;   :config
;;   (hammy-mode 1))

(setq disabled-command-function nil)

(use-package lispy
  :hook ((emacs-lisp-mode . lispy-mode)
         (clojure-mode . lispy-mode))
  :custom
  (lispy-compat '(edebug cider magit-blame-mode)))

(electric-pair-mode 1)
(setq electric-pair-inhibit-predicate
      (lambda (char)
        (not (member major-mode '(rustic-mode
                                  go-mode
                                  python-mode)))))

;; (global-set-key (kbd "<escape>") 'keyboard-escape-quit)

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

; TODO: convert this to Emacs keybindings

;; (general-define-key
;;  :states 'normal
;;  "s" 'avy-goto-char-timer
;;  "S" 'avy-pop-mark)

;; (general-define-key
;;  :states '(normal emacs)
;;  "C-s" 'consult-line)

;; (my-leader
;;   "s" '(:ignore t :which-key "search")
;;   "s b" '(consult-line :which-key "Search buffer"))

;; (use-package ag
;;   :general
;;   (my-leader
;;     "s p" '(projectile-ag :which-key "Search project")))

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

(use-package magit)

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

;; Got this from https://stackoverflow.com/questions/10969617/hiding-markup-elements-in-org-mode
;; (defun ns/org-toggle-emphasis ()
;;   "Toggle hiding/showing of org emphasis markers"
;;   (interactive)
;;   (if org-hide-emphasis-markers
;;       (set-variable 'org-hide-emphasis-markers nil)
;;     (set-variable 'org-hide-emphasis-markers t)))

;; (use-package org-contrib)

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
  (org-hide-emphasis-markers t)

  (org-directory "~/Documents/notes")
  (org-default-notes-file "~/Documents/notes/notes.org")

  (org-src-tab-acts-natively t)
  (org-src-preserve-indentation t)

  (org-log-done 'time)    ; log the time when a task is *DONE*

  (org-todo-keywords
        '((sequence "TODO(t)" "NEXT(n)" "ACTIVE(a!)" "HOLD(h)" "|" "DONE(d!)"))))
            ;; (sequence "BACKLOG(b)" "PLAN(p)" "READY(r)" "ACTIVE(a)" "REVIEW(v)"
                ;; "WAIT(w@/!)" "HOLD(h)" "|" "COMPLETED(c)" "CANC(k@)"))))

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
  (set-face-attribute (car face) nil :font "Iosevka Aile" :weight 'medium :height (cdr face)))

;; Make the document title a bit bigger
(set-face-attribute 'org-document-title nil :font "Iosevka Aile" :weight 'bold :height 1.3)

;; Make sure certain org faces continue to use fixed-pitch face even whenn variable-pitch-mode is on
(set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
(set-face-attribute 'org-table nil :inherit 'fixed-pitch)
(set-face-attribute 'org-formula nil :inherit 'fixed-pitch)
(set-face-attribute 'org-code nil :inherit '(shadow fixed-pitch))
(set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
(set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
(set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-facee fixed-pitch))
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

(setq org-agenda-files (list "inbox.org"
                             "agenda.org"
                             "projects.org"))
      ;org-agenda-hide-tags-regexp "."     ; hide all tags in the agenda
      
      ;; org-agenda-compact-blocks nil)

;; (setq org-capture-templates
;;        `(("i" "Inbox" entry  (file "agenda/inbox.org")
;;         ,(concat "* TODO %?\n"
;;                  "/Entered on/ %U"))
;;          ("m" "Meeting entry" entry (file+headline "agenda.org" "Future")
;;           ,(concat "* %? :meeting:\n"
;;                    "<%<%Y-%m-%d %a %H:00>>"))
;;          ("n" "Note" entry (file "notes.org")
;;           ,(concat "* Note (%a)\n"
;;                    "/Entered on/ %U\n" "\n" "%?"))))

(setq org-capture-templates
      '(("t" "todo" entry (file+headline "inbox.org" "Todo")
	     "* TODO %?\n%u\n%a\n" :clock-in t :clock-resume t)
        ;; ("m" "Meeting" entry (file "agenda/inbox.org")
        ;;  "* MEETING with %? :MEETING:\n%t" :clock-in t :clock-resume t)
        ("s" "Schedule an Appointment" entry (file+headline "agenda.org" "Future")
         "*** TODO %? \nSCHEDULED: %t")
        ("d" "Diary" entry (file+olp+datetree "diary.org")
         "* %?\n%U\n" :clock-in t :clock-resume t)
        ("i" "Idea" entry (file "inbox.org")
         "* %? :IDEA: \n%t" :clock-in t :clock-resume t)
        ("n" "High Priority Task" entry (file+headline "inbox.org" "Tasks")
         "** NEXT %? \nDEADLINE: %t") ))

(setq org-refile-targets
      '(("projects.org" :regexp . "\\(?:\\(?:Note\\|Task\\)s\\)"))
      org-refile-use-outline-path 'file
      org-outline-path-complete-in-steps nil)

(defun ns/org-agenda-save-buffers ()
  "Save `org-agenda-files` buffers without user confirmation."
  (interactive)
  (message "Saving org-agenda-files buffers...")
  (save-some-buffers t
                     (lambda ()
                       (when (member (buffer-file-name) (org-agenda-files))
                         t)))
  (message "Saving org-agenda-files buffers... done"))

;; Automatically save after refile
(advice-add 'org-refile :after
            (lambda (&rest _)
              (ns/org-agenda-save-buffers)))

(defun log-todo-next-creation-date (&rest ignore)
  "Log NEXT creation time in the property drawer under the key 'ACTIVATED'"
  (when (and (string= (org-get-todo-state) "NEXT")
             (not (org-entry-get nil "ACTIVATED")))
    (org-entry-put nil "ACTIVATED" (format-time-string "[%Y-%m-%d]"))))
(add-hook 'org-after-todo-state-change-hook #'log-todo-next-creation-date)

(setq org-agenda-sticky t
      org-agenda-dim-blocked-tasks nil
      org-agenda-time-grid (quote
                            ((daily today remove-match)
                             (800 1200 1600 2000)
                             "......" "----------------")))

;; Variables for ignoring tasks with deadlines
(defvar ns/hide-deadline-next-tasks t)
(setq org-agenda-tags-todo-honor-ignore-options t
      org-deadline-warning-days 10)

;;;; Task and project filter functions

(defun ns/select-with-tag-function (select-fun-p)
  (save-restriction
    (widen)
    (let ((next-headline
           (save-excursion (or (outline-next-heading)
                               (point-max)))))
      (if (funcall select-fun-p) nil next-headline))))

;; Some helper functions for agenda views
(defun ns/org-agenda-add-location-string ()
  "Gets the value of the LOCATION property."
  (let ((loc (org-entry-get (point) "LOCATION")))
    (if (> (length loc) 0)
        (concat "{" loc "} ")
      "")))

;;;; Agenda block definitions

(defvar ns-org-agenda-block--today-schedule
  '(agenda "" ((org-agenda-overriding-header "Today's Schedule:")
               (org-agenda-span 'day)
               (org-agenda-ndays 1)
               (org-agenda-start-on-weekday nil)
               (org-agenda-start-day "+0d")))
  "A block showing a 1 day schedule.")

(defvar ns-org-agenda-block--weekly-log
  '(agenda "" ((org-agenda-overriding-header "Weekly Log")))
  "A block showing my schedule and logged tasks for this week.")

(defvar ns-org-agenda-block--previous-calendar-data
  '(agenda "" ((org-agenda-overriding-header "Previous Calendar Data (last 3 weeks)")
               (org-agenda-start-day "-21d")
               (org-agenda-span 21)
               (org-agenda-start-on-weekday nil)))
  "A block showing my schedule and logged tasks for the last few weeks.")

(defvar ns-org-agenda-block--upcoming-calendar-data
  '(agenda "" ((org-agenda-overriding-header "Upcoming Calendar Data (next 2 weeks)")
               (org-agenda-start-day "0d")
               (org-agenda-span 14)
               (org-agenda-start-on-weekday nil)))
  "A block showing my schedule for the next couple weeks.")

(defvar ns-org-agenda-block--refile
  '(tags "REFILE-ARCHIVE-REFILE=\"nil\"|INFO"
         ((org-agendda-overriding-header "Headings needing refiling or other info:")
          (org-tags-match-list-sublevels nil)))
  "Headings needing refiling or other info.")

(defvar ns-org-agenda-block--next-tasks
  '(tags-todo "-INACTIVE-SOMEDAY-CANCELLED-ARCHIVE/!NEXT"
              ((org-agenda-overriding-header "Next Tasks:")
               ))
  "Next tasks.")

(defvar ns-org-agenda-block--end-of-agenda
  '(tags "ENDOFAGENDA"
         ((org-agenda-overriding-header "End of Agenda")
          (org-tags-match-list-sublevels nil)))
  "End of the agenda.")

(defvar ns-org-agenda-display-settings
  '((org-agenda-start-with-log-mode t)
    (org-agenda-log-mode-items '(clock))
    (org-agenda-prefix-format '((agenda . "  %-12:c%?-12t %(ns/org-agenda-add-location-string)% s")
				(timeline . "  % s")
				(todo . "  %-12:c ")
				(tags . "  %-12:c ")
				(search . "  %i %-12:c")))
    (org-agenda-todo-ignore-deadlines 'near)
    (org-agenda-todo-ignore-scheduled t))
  "Display settings for my agenda views.")

(defvar ns-org-agenda-entry-display-settings
  '(,ns-org-agenda-display-settings
    (org-agenda-entry-text-mode t))
  "Display settings for my agenda views with entry text.")

(setq org-agenda-custom-commands
      `((" " "Export Schedule"
         (,ns-org-agenda-block--today-schedule
          ,ns-org-agenda-block--refile
          ,ns-org-agenda-block--next-tasks
          ,ns-org-agenda-block--end-of-agenda)
          ,ns-org-agenda-display-settings)
        ("L" "Weekly Log"
         (,ns-org-agenda-block--weekly-log)
         ,ns-org-agenda-display-settings)
        ("r " "Agenda Review (all)"
         (,ns-org-agenda-block--next-tasks
          ,ns-org-agenda-block--refile
          ,ns-org-agenda-block--end-of-agenda)
         ,ns-org-agenda-display-settings)
        ("rn" "Agenda Review (next tasks)"
         (,ns-org-agenda-block--next-tasks
          ,ns-org-agenda-block--end-of-agenda)
         ,ns-org-agenda-display-settings)
        ("rp" "Agenda Review (previous calendar data)"
         (,ns-org-agenda-block--previous-calendar-data
          ,ns-org-agenda-block--end-of-agenda)
         ,ns-org-agenda-display-settings)
        ("ru" "Agenda Review (upcoming calendar data)"
         (,ns-org-agenda-block--upcoming-calendar-data
          ,ns-org-agenda-block--end-of-agenda)
         ,ns-org-agenda-display-settings)
        ))

;; (setq org-agenda-custom-commands
;;       '(("g" "Get Things Done (GTD)"
;;          ((agenda ""
;;                   ((org-agenda-span 1) ; limit display to a single day
;;                    (org-agenda-skip-function
;;                     '(org-agenda-skip-entry-if 'deadline))
;;                    (org-deadline-warning-days 0)))
;;           (todo "NEXT"
;;                 ((org-agenda-skip-function
;;                   '(org-agenda-skip-entry-if 'deadline))
;;                  (org-agenda-prefix-format "  %i %-12:c [%e] ")
;;                  (org-agenda-overriding-header "\nTasks\n")))
;;           (agenda nil
;;                   ((org-agenda-entry-types '(:deadline))
;;                    (org-agenda-span 1)
;;                    (org-agenda-format-date "")
;;                    (org-deadline-warning-days 7)
;;                    (org-agenda-skip-function
;;                     '(org-agenda-skip-entry-if 'notregexp "\\* NEXT"))
;;                    (org-agenda-overriding-header "\nDeadlines")))
;;           (tags-todo "inbox"
;;                      ((org-agenda-prefix-format "  %?-12t% s")
;;                       (org-agenda-overriding-header "\nInbox\n")))
;;           (tags "CLOSED>=\"<today>\""
;;                 ((org-agenda-overriding-header "\nCompleted today\n")))))))

(keymap-global-set "C-c c" 'org-capture)
(keymap-global-set "C-c a" 'org-agenda)

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
               ("j" . org-roam-dailies-capture-today))
  :custom
  (org-roam-directory "~/Documents/notes/org-roam/")
  (org-roam-database-connector 'sqlite-builtin)
  (org-roam-capture-templates
   '(("d" "default" plain "%?"
      :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                         "#+title: ${title}\n#+date: %U\n")
      :unnarrowed t)
     ("r" "recipe" plain (file "~/notes/org-roam/templates/recipe-template.org")
      :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                         "#+title: ${title}\n#+date: %U\n#+filetags: Recipe\n")
      :unnarrowed t)))
  :init
  (setq org-roam-v2-ack t)
  :config
  (setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
  (org-roam-db-autosync-enable))

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
               ("p" . multi-vterm-project)))

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

(require 'treesit)
(setq treesit-extra-load-path (list (expand-file-name "ts-grammars" user-emacs-directory)))

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
  (eglot-send-changes-idle-time 0.5))
  ;(eglot-send-changes-idle-time (* 60 60))) ; Delay the automatic syntax checking to improve lag and stutters while typing
  ;; :config
  ;; (add-hook 'eglot-managed-mode-hook
            ;; (lambda ()
              ;; (eldoc-mode -1)
              ;; (flymake-mode -1)))) ; Disable doc popups in the minibuffer

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

(use-package format-all)
  ;:hook
  ;(prog-mode . format-all-mode)

(use-package flymake
  :bind
  (:map flymake-mode-map
        ("M-p" . flymake-goto-prev-error)
        ("M-n" . flymake-goto-next-error)))

;; (use-package realgud)

(use-package verb
  :after (org)
  :config
  (define-key org-mode-map (kbd "C-c C-r") verb-command-map))

(use-package wakatime-mode
  :config
  (global-wakatime-mode))

(use-package ws-butler
  :hook
  (prog-mode . ws-butler-mode))

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
  :after eglot
  :mode "\\.d\\'"
  :hook
  (d-mode . eglot-ensure)
  :config
  (add-to-list 'org-src-lang-modes '("d" . c))
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

;; (use-package lsp-pyright)

(use-package python-mode
  :hook (python-mode . eglot-ensure)
  ;; :hook (python-mode . (lambda ()
                         ;; (eglot-ensure)
                         ;; (setq tab-width 4)))
  :custom
  (python-shell-interpreter "python3"))
  ;;(dap-python-debugger 'debugpy))

;; (require 'lsp-pyright)
;; (require 'dap-python)

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

(use-package rustic
  ;; :hook (rustic-mode . eglot-ensure)
  :custom
  (rustic-lsp-client 'eglot)
  :hook
  (rustic-mode . (lambda () (flycheck-mode -1)))
  (rustic-mode . eglot-ensure))

  ;; ;;uncomment for less flashiness
  ;; (setq lsp-eldoc-hook nil)
  ;; (setq lsp-enable-symbol-highlighting nil)
  ;; (setq lsp-signature-auto-activate nil)

  ;comment to disable rustfmt on save
  ;(setq rustic-format-on-save t))

;; TODO set up keybindings
;; (use-package rust-auto-use)
  

;; (use-package rust-mode
;;   :mode "\\.rs\\'"
;;   :hook (rust-mode . eglot-ensure))

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

(add-to-list 'load-path "~/.dotfiles/.emacs.d/lisp")

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

(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

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

(setq-default frame-title-format '("%b [%m]"))

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

(use-package emojify
  :config
  (global-emojify-mode))

(use-package doom-themes)

(use-package ef-themes
  :config
  (load-theme 'ef-summer t))

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

  ;; :config
  ;; (modus-themes-load-vivendi))

(use-package doom-modeline
  :init
  (setq doom-modeline-height 35
        doom-modeline-support-imenu t)
  (doom-modeline-mode 1))

;; Necessary for dashboard in order to get nice seperators between sections
(use-package page-break-lines)

(use-package dashboard
    :init
    (setq
        dashboard-image-banner-max-width 256
        dashboard-startup-banner "~/.dotfiles/.emacs.d/emacs.png"
        dashboard-center-content t
        dashboard-set-heading-icons t
        dashboard-set-file-icons t
        ;; dashboard-projects-switch-function 'projectile-switch-project
        dashboard-items '((recents . 5)
                          (projects . 5)
                          (agenda . 5)))
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
  (let ((alpha-transparency 75))
    (if (equal alpha-transparency (frame-parameter nil 'alpha-background))
        (set-frame-parameter nil 'alpha-background 100)
      (set-frame-parameter nil 'alpha-background alpha-transparency))))

(use-package vertico
  :init
  (vertico-mode))

(use-package consult
  :bind
  ("C-s" . consult-line))

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

(use-package smartparens
  :hook
  (prog-mode . smartparens-mode)

  :config
  ;; Don't insert paired single quotes in Elisp mode
  (sp-local-pair 'emacs-lisp-mode "'" nil :actions nil))

(use-package yasnippet
  :config
  (yas-global-mode))

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

(use-package corfu
  :bind
  (:map corfu-map
        ("C-n" . corfu-next)
        ("C-p" . corfu-previous)
        ("<escape>" . corfu-quit)
        ("<return>" . corfu-insert)
        ("M-d" . corfu-show-documentation)
        ("M-l" . corfu-show-location))

  :custom
  (corfu-auto t)
  (corfu-auto-prefix 3) ; Minimum length of prefix for auto-complete
  (corfu-auto-delay 0) ; Immediately start auto-completion

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

  (corfu-echo-documentation nil) ; Use 'corfu-doc' instead

  ;; Enable indentation+completion using the TAB key instead of M-TAB
  (tab-always-indent 'complete)
  ;; (completion-cycle-threshold nil)

  (corfu-excluded-modes '(eshell-mode))

  :init
  (global-corfu-mode))

  ;; :config
  ;; (general-add-advice '(corfu--setup corfu--teardown) :after 'evil-normalize-keymaps)
  ;; (evil-make-overriding-map corfu-map))


(use-package kind-icon
  :custom
  (kind-icon-default-face 'corfu-default)
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(use-package corfu-doc
  :hook (corfu-mode . corfu-doc-mode))

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
  :config
  (defun iedit-dwim (arg)
    "Starts iedit but uses \\[narrow-to-defun] to limit its scope."
    (interactive "P")
    (if arg
        (iedit-mode)
      (save-excursion
        (save-restriction
          (widen)
          (if (bound-and-true-p iedit-mode)
              (iedit-done)
            (narrow-to-defun)
            (iedit-start (current-word) (point-min) (point-max)))))))
  (keymap-global-set "C-;" 'iedit-dwim))

(setq tramp-default-method "ssh") ;; Use SSH by default for remote files

;; (use-package flyspell
;;   :bind
;;   (:map flyspell-mode-map
;;         ("C-;" . nil)) ;; unbind this key so I can use it for iedit-dwim
  
;;   :hook ((prog-mode . flyspell-prog-mode)
;;         ((org-mode markdown-mode) . flyspell-mode)))

;; (use-package flyspell-correct
;;   :after (flyspell)
;;   :config
;;   (setq flyspell-correct-interface #'flyspell-correct-ivy))

(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  ;; :custom ((projectile-completion-system 'ivy))
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  ;(when (file-directory-p "~/Documents")
    ;(setq projectile-project-search-path '("~/Documents")))
  (setq projectile-switch-project-action #'projectile-dired))

(use-package treemacs)
;; (use-package treemacs-evil
;;     :after (treemacs evil))
(use-package treemacs-projectile
    :after (treemacs projectile))
(use-package treemacs-icons-dired
    :hook (dired-mode . treemacs-icons-dired-enable-once))
;;(use-package treemacs-perspective
;;  :after (treemacs perspective))
(use-package treemacs-magit
    :after (treemacs magit))
;; (use-package lsp-treemacs
;;     :after (treemacs lsp-mode)
;;     :config (lsp-treemacs-sync-mode 1))
(use-package treemacs-all-the-icons
  :config
  (treemacs-load-theme "all-the-icons"))

(use-package perspective
  :custom
  (persp-mode-prefix-key (kbd "C-c w"))
  :init
  (persp-mode))
  ;; :config
  ;; (consult-customize consult--source-buffer :hidden t :default nil)
  ;; (add-to-list 'consult-buffer-sources persp-consult-source))

(defun ns/tab-bar-switch-or-create (name func)
  (if (ns/tab-bar-tab-exists name)
      (tab-bar-switch-to-tab name)
    (ns/tab-bar-new-tab name func)))

(defun ns/tab-bar-tab-exists (name)
  (member name
          (mapcar #'(lambda (tab) (alist-get 'name tab))
                  (tab-bar-tabs))))

(defun ns/tab-bar-new-tab (name func)
  (when (eq nil tab-bar-mode)
    (tab-bar-mode))
  (tab-bar-new-tab)
  (tab-bar-rename-tab name)
  (when func ;; If func is nil, don't try to run it
      (funcall func)))

(use-package emacs
  :custom
  (tab-bar-show nil))

;; (defun ns/escape ()
;;   (interactive)
;;   (if (and (>= (recursion-depth) 1) (active-minibuffer-window))
;;       (abort-recursive-edit)
;;     (god-mode-all)))

;; (use-package god-mode
;;   :bind
;;   (:map god-local-mode-map
;;         ("i" . god-local-mode))
;;   :config
;;   (god-mode-all)
;;   (global-set-key (kbd "<escape>") 'ns/escape))

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

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

;(use-package forge)

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
(defun ns/org-toggle-emphasis ()
  "Toggle hiding/showing of org emphasis markers"
  (interactive)
  (if org-hide-emphasis-markers
      (set-variable 'org-hide-emphasis-markers nil)
    (set-variable 'org-hide-emphasis-markers t)))


;; (use-package org-contrib :pin nongnu)
(use-package org-contrib)

;; Org Mode
(use-package org
    :hook (org-mode . ns/org-mode-setup)
    :config
    ;; (ns/org-font-setup)
    (setq
     ;; org-hide-emphasis-markers nil
        org-ellipsis " ▾"
        org-pretty-entities t

        org-directory "~/notes"

        org-src-tab-acts-natively t
        org-src-preserve-indentation t

        org-todo-keywords
        '((sequence "TODO(t)" "NEXT(n)" "HOLD(h)" "|" "DONE(d!)")
            (sequence "BACKLOG(b)" "PLAN(p)" "READY(r)" "ACTIVE(a)" "REVIEW(v)"
                "WAIT(w@/!)" "HOLD(h)" "|" "COMPLETED(c)" "CANC(k@)"))))

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
    (python . t)))

(setq org-confirm-babel-evaluate nil)

(defun ns/org-babel-tangle-config ()
  (when (string-equal (buffer-file-name)
                      (expand-file-name "~/.dotfiles/.emacs.d/config.org"))
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle))))

;(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'ns/org-babel-tangle-config)))

(setq org-agenda-files (list "agenda/inbox.org"
                             "agenda/agenda.org"
                             "agenda/projects.org") ; add any files to be pulled from
      org-agenda-hide-tags-regexp "."     ; hide all tags in the agenda
      org-log-done 'time             ; log the time when a task is *DONE*
      org-agenda-compact-blocks nil
      org-agenda-block-separator nil
      )

(setq org-capture-templates
       `(("i" "Inbox" entry  (file "agenda/inbox.org")
        ,(concat "* TODO %?\n"
                 "/Entered on/ %U"))
         ("m" "Meeting entry" entry (file+headline "agenda.org" "Future")
          ,(concat "* %? :meeting:\n"
                   "<%<%Y-%m-%d %a %H:00>>"))
         ("n" "Note" entry (file "notes.org")
          ,(concat "* Note (%a)\n"
                   "/Entered on/ %U\n" "\n" "%?"))))

(setq org-refile-targets
      '(("agenda/projects.org" :regexp . "\\(?:\\(?:Note\\|Task\\)s\\)"))
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

(setq org-agenda-custom-commands
      '(("g" "Get Things Done (GTD)"
         ((agenda ""
                  ((org-agenda-span 1) ; limit display to a single day
                   (org-agenda-skip-function
                    '(org-agenda-skip-entry-if 'deadline))
                   (org-deadline-warning-days 0)))
          (todo "NEXT"
                ((org-agenda-skip-function
                  '(org-agenda-skip-entry-if 'deadline))
                 (org-agenda-prefix-format "  %i %-12:c [%e] ")
                 (org-agenda-overriding-header "\nTasks\n")))
          (agenda nil
                  ((org-agenda-entry-types '(:deadline))
                   (org-agenda-span 1)
                   (org-agenda-format-date "")
                   (org-deadline-warning-days 7)
                   (org-agenda-skip-function
                    '(org-agenda-skip-entry-if 'notregexp "\\* NEXT"))
                   (org-agenda-overriding-header "\nDeadlines")))
          (tags-todo "inbox"
                     ((org-agenda-prefix-format "  %?-12t% s")
                      (org-agenda-overriding-header "\nInbox\n")))
          (tags "CLOSED>=\"<today>\""
                ((org-agenda-overriding-header "\nCompleted today\n")))))))

(keymap-global-set "C-c c" 'org-capture)
(keymap-global-set "C-c a" 'org-agenda)

;; (use-package org-roam
;;   :custom
;;   (org-roam-directory "~/notes/roam/")
;;   :config
;;   (setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
;;   (org-roam-db-autosync-mode)
;;   :general
;;   (my-leader
;;     "n r" '(:ignore t :which-key "roam")
;;     ;;"n r" '(:keymap org-roam-mode-map :which-key "roam")
;;     "n r f" '(org-roam-node-find :which-key "Find Node")
;;     "n r i" '(org-roam-node-insert :which-key "Insert Node")
;;     "n r o" '(org-roam-node-open :which-key "Open Node")
;;     "n r g" '(org-roam-graph :which-key "Graph")))

;; (use-package org-journal
;;   :general
;;   (my-leader
;;     "n j" '(:ignore t :which-key "journal")
;;     "n j j" '(org-journal-new-entry :which-key "New entry")
;;     "n j r" '(org-journal-read-entry :which-key "Read entry")
;;     "n j s" '(org-journal-search :which-key "Search journal"))
  
;;   :custom
;;   (org-journal-dir "~/notes/journal")
;;   (org-journal-file-format "%Y-%m-%d.org")
;;   (org-journal-date-format "%B %d, %Y (%A) ")
;;   (org-journal-date-prefix "* ")
;;   (org-journal-time-prefix "** "))

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

;; (use-package org-modern
;;     :config
;;     (add-hook 'org-mode-hook #'org-modern-mode)
;;     (add-hook 'org-agenda-finalize #'org-modern-agenda))

(use-package denote
  :straight (denote :type git :host gitlab
                    :repo "protesilaos/denote")
  :custom
  (denote-directory "~/notes")
  (denote-known-keywords
    '("emacs" "personal" "journal")))

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

(use-package format-all)
  ;:hook
  ;(prog-mode . format-all-mode)

(use-package flycheck)
  ;; :config
  ;; (global-flycheck-mode))

(use-package eglot
  :custom
  (eglot-events-buffer-size 0)) ;; Disable the events buffer for performance

  ;; :config
  ;; (add-hook 'eglot-managed-mode-hook (lambda () (flymake-mode -1))))

;; (use-package lsp-mode
;;     :commands (lsp lsp-deferred)
;;     :init
;;     (setq lsp-lens-enable t
;;           lsp-signature-auto-activate nil
;;           lsp-ui-doc-mode t)
;;     :general
;;     (evil-define-key 'normal lsp-mode-map (kbd "/") lsp-command-map)
;;     :config
;;     (lsp-enable-which-key-integration t)
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

;; (use-package dap-mode
;;   :config
;;   (dap-auto-configure-mode))

;; (use-package realgud)

(use-package wakatime-mode
  :config
  (global-wakatime-mode))

(use-package yaml-mode
  :mode "\\.yml\\'")

;; (add-hook 'c-mode-hook 'lsp)
;; (add-hook 'c++-mode-hook 'lsp)
(add-hook 'c-mode-hook 'eglot-ensure)
(add-hook 'c++-mode-hook 'eglot-ensure)

(use-package dockerfile-mode
  :mode "Dockerfile\\'")

(use-package clojure-mode
  :mode "\\.clj\\'"
  :hook ((clojure-mode . eglot-ensure)
         (clojurescript-mode . eglot-ensure)
         (clojurec-mode . eglot-ensure)))

(use-package cider)

(use-package glsl-mode
  :mode ("\\.glsl\\'" "\\.vert\\'" "\\.frag\\'" "\\.geom\\'"))

(use-package go-mode
  :mode "\\.go\\'"
  :hook (go-mode . eglot-ensure))

;; (use-package lsp-pyright)

(use-package python-mode
  :hook (python-mode . (lambda ()
                         (eglot-ensure)
                         (setq tab-width 4)))
  :custom
  (python-shell-interpreter "python3"))
  ;;(dap-python-debugger 'debugpy))

;; (require 'lsp-pyright)
;; (require 'dap-python)

(use-package typescript-mode
  :mode "\\.ts\\'"
  ;; :hook (typescript-mode . lsp-deferred)
  :config
  (setq typescript-indent-level 4))

;; (use-package ruby-mode
;;   :hook (ruby-mode . eglot-ensure))

(use-package inf-ruby) ;; Interact with a Ruby REPL

;; (use-package rustic
;;   :hook (rustic-mode . eglot-ensure)
;;   :custom
;;   (rustic-lsp-client 'eglot)
;;   ;; :bind (:map rustic-mode-map
;;               ;; ("M-j" . lsp-ui-imenu)
;;               ;; ("M-?" . lsp-find-references)))
;;   :config
;;   (remove-hook 'rustic-mode-hook 'flycheck-mode))

  ;; uncomment for less flashiness
  ;; (setq lsp-eldoc-hook nil)
  ;; (setq lsp-enable-symbol-highlighting nil)
  ;; (setq lsp-signature-auto-activate nil)

  ;; comment to disable rustfmt on save
  ;; (setq rustic-format-on-save t))

(use-package rust-mode
  :mode "\\.rs\\'"
  :hook (rust-mode . eglot-ensure))

(use-package web-mode
    :commands (web-mode)
    :mode (("\\.html" . web-mode)
            ("\\.htm" . web-mode)
;           ("\\.tsx$" . web-mode)
            ("\\.mustache\\'" . web-mode)
            ("\\.phtml\\'" . web-mode)
            ("\\.as[cp]x\\'" . web-mode)
            ("\\.erb\\'" . web-mode)
            ("\\.sgml\\'" . web-mode)))

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

(use-package tablist)

(use-package pdf-tools
  :config
  (pdf-loader-install))

(use-package saveplace-pdf-view)

(use-package nov
  :mode "\\.epub\\'")

(use-package request)

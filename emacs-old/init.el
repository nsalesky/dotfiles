(setq native-comp-deferred-compilation t)

(setq byte-compile-warnings '(not free-vars unresolved noruntime lexical make-local)
      native-comp-async-report-warnings-errors nil)

(setq gc-cons-threshold 100000000
      read-process-output-max (* 1024 1024))

(tool-bar-mode -1)
(menu-bar-mode 1)
(scroll-bar-mode -1)
(tooltip-mode -1)
(set-fringe-mode 10)

(setq inhibit-startup-message t)

(setq use-short-answers t)

(setq confirm-nonexistent-file-or-buffer nil)

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(require 'use-package-ensure)
(setq use-package-always-pin  "melpa"
      use-package-always-ensure t
      use-package-compute-statistics t)

(use-package exec-path-from-shell
  :config
  (exec-path-from-shell-initialize))

(setq user-full-name "Nick Salesky"
      user-mail-address "nicksalesky@gmail.com")

(when (eq system-type 'darwin)
  (setq mac-option-key-is-meta t
        mac-command-key-is-meta t
        mac-command-modifier 'meta
        mac-option-modifier 'meta))

(setq disabled-command-function nil)

(electric-pair-mode 1)
(setq electric-pair-inhibit-predicate
      (lambda (char)
        (member major-mode '(org-mode))))

(global-unset-key (kbd "ESC ESC"))

(delete-selection-mode 1)

;; Set the default tab settings
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)
(setq-default c-basic-offset 4)
(setq-default python-indent-offset 4)

;; Make the backspace properly erase the whole tab instead of removing
;; 1 space at a time
(setq backward-delete-char-untabify-method 'hungry)

  ;; Keep track of recently-opened files
  (use-package recentf
    :ensure nil
    :config
    (recentf-mode 1)
    :custom
    (recentf-max-menu-items 5000)
    (recentf-max-saved-items 10000)
    :bind
    ("C-x C-r" . consult-recent-file))
    ;; :general
    ;; (ns/leader-def "fr" 'consult-recent-file))

(use-package ace-window
  :bind ("M-o" . ace-window))

(use-package hydra)

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

(use-package emacs
  :ensure nil
  :custom
  (scroll-margin 5)
  (ring-bell-function 'ignore)
  :config
  (pixel-scroll-mode 1)
  (global-visual-line-mode 1))

(use-package saveplace
  :ensure nil
  :unless noninteractive
  :config
  (setq save-place-limit 1000)
  (save-place-mode))

(use-package savehist
  :ensure nil
  :unless noninteractive
  :defer 1
  :config
  (setq savehist-additional-variables '(compile-command kill-ring regexp-search-ring))
  (savehist-mode 1))

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

(use-package display-line-numbers-mode
  :ensure nil
  :custom
  (display-line-numbers-type 'relative)
  :hook
  (prog-mode . display-line-numbers-mode)
  (markdown-mode . display-line-numbers-mode))

(setq-default frame-title-format '("GNU Emacs"))

(use-package diminish
  :init
  (diminish 'buffer-face-mode)
  (diminish 'visual-line-mode)
  (diminish 'abbrev))

(defvar ns/default-font "JetBrainsMono NF"
  "My custom default font choice.")

(defvar ns/fixed-pitch-font "JetBrainsMono NF"
  "My custom fixed pitch font choice.")

(defvar ns/variable-pitch-font "JetBrainsMono NF"
  "My custom variable pitch font choice.")

(custom-set-faces
 `(default ((t (:family ,ns/default-font :slant normal :weight regular :height 110 :width normal :foundry "JB  "))))
 `(fixed-pitch ((t (:family ,ns/fixed-pitch-font :height 110))))
 `(variable-pitch ((t (:family ,ns/variable-pitch-font)))))

(use-package all-the-icons)
;; (use-package nerd-icons)

(use-package doom-themes
  :config
  (load-theme 'doom-one t))

(use-package ef-themes :disabled t
  :config
  (setq ef-themes-headings ; read the manual's entry or the doc string
      '((0 . (variable-pitch light 1.9))
        (1 . (variable-pitch light 1.8))
        (2 . (variable-pitch regular 1.7))
        (3 . (variable-pitch regular 1.6))
        (4 . (variable-pitch regular 1.5))
        (5 . (variable-pitch 1.4)) ; absence of weight means `bold'
        (6 . (variable-pitch 1.3))
        (7 . (variable-pitch 1.2))
        (t . (variable-pitch 1.1))))
  (setq ef-themes-to-toggle '(ef-summer ef-cherie)
        ef-themes-mixed-fonts t
        ef-themes-variable-pitch-ui t))
  
  ;; (ef-themes-select 'ef-summer))

(use-package modus-themes)
  ;; :init
  ;; (setq modus-themes-mode-line '(moody)))
  ;; :config
  ;; (load-theme 'modus-vivendi t))

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
    ;; (dashboard-startup-banner "/Users/nsalesky/Pictures/wallpapers/forest-small.jpg")
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

(use-package helpful
  :bind
  (("C-h f" . helpful-callable)
   ("C-h v" . helpful-variable)
   ("C-h k" . helpful-key)))

  ;; :general
  ;; (ns/leader-def
  ;;   "h" '(:ignore t :which-key "help")
  ;;   "hv" '(helpful-variable :which-key "describe variable")
  ;;   "hf" '(helpful-callable :which-key "describe function")
  ;;   "hk" '(helpful-key :which-key "describe key")))

(use-package hl-todo
  :config
  (global-hl-todo-mode))

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
   ("M-g i" . consult-imenu)

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

  ;; :general
  ;; (ns/leader-def
  ;;   "s" '(:ignore t :which-key "search")
  ;;   "sr" '(consult-ripgrep :which-key "ripgrep")
  ;;   "sl" '(consult-line :which-key "line search")
  ;;   "si" '(consult-imenu :which-key "imenu"))

  :init
  (setq consult-narrow-key (kbd "<"))

  ;; Projectile
  ;; (autoload 'projectile-project-root "projectile")
  ;; (setq consult-project-function (lambda (_) (projectile-project-root)))

  ;; Project.el
  (setq consult-project-function #'consult--default-project-function))

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
  (marginalia-mode)
  :config
  (setq marginalia-command-categories
        (append '((projectile-find-file . project-file)
                  (projectile-find-dir . project-file)
                  (projectile-switch-project . file))
                marginalia-command-categories)))

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

(use-package rainbow-delimiters
    :hook (prog-mode . rainbow-delimiters-mode))

(use-package yasnippet
  :diminish yas-minor-mode
  :custom
  (yas-verbosity 1)
  :config
  (yas-global-mode))

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.3))

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

(use-package dired
  :ensure nil
  :custom
  (dired-kill-when-opening-new-dired-buffer t))

(use-package tab-bar
  :ensure nil
  :init
  (tab-bar-mode)
  :custom
  (tab-bar-show nil))

(use-package magit)

;; (use-package forge
;;   :after magit)
;; TODO set up personal access token personal to work with pull requests from Emacs  :after magit)

(use-package org
  :ensure nil
  :bind
  ("C-c l" . org-store-link)
  :hook (org-mode . org-indent-mode)
  :hook (org-mode . variable-pitch-mode)
  :hook (org-mode . visual-line-mode)
  :config
  ;; (add-to-list 'org-tags-exclude-from-inheritance "project")
  ;; (add-to-list 'org-tags-exclude-from-inheritance "rez")
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
      (plantuml . t)
      (ruby . t)
      (sql . t)))

  :custom
  (org-ellipsis "…")
  (org-pretty-entities t)
  (org-pretty-entities-include-sub-superscripts nil)
  (org-hide-emphasis-markers t)
  (org-hide-leading-stars nil)

  (org-todo-keywords '((sequence "TODO(t)" "WIP(w!)" "HOLD(h!)" "|" "DONE(d!)" "KILL(k!)")))
  (org-use-property-inheritance t)
  (org-log-done 'time)    ; log the time when a task is *DONE*
  (org-log-reschedule 'time)
  (org-log-redeadline 'time)

  (org-directory "~/Documents/notes")
  (org-default-notes-file "~/Documents/notes/notes.org")

  ;; Org-babel
  (org-confirm-babel-evaluate nil)
  (org-plantuml-exec-mode 'jar)
  (org-plantuml-jar-path "~/.local/bin/plantuml.jar")

  (org-latex-pdf-process '("pdflatex -interaction nonstopmode -output-directory %o %f" "bibtex %b" "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f" "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))

  (org-src-tab-acts-natively t)
  (org-src-preserve-indentation t))

  ;; :general
  ;; (:keymaps 'org-mode-map :states '(normal emacs visual)
  ;;   "SPC m t" '(org-babel-tangle :which-key "Tangle current file")))

(defun ns/org-agenda-reload-files ()
  (interactive)
  (message "Reloading agenda files")
  (setq org-agenda-files (directory-files-recursively "~/Documents/notes/agenda/" "\\.org$")))

(use-package org-agenda
  :ensure nil
  :custom
  (org-agenda-files (directory-files-recursively "~/Documents/notes/agenda/" "\\.org$"))

  (org-agenda-todo-ignore-scheduled 'all)
  (org-agenda-todo-ignore-deadlines 'all)
  (org-agenda-todo-ignore-with-date 'all)
  (org-agenda-tags-todo-honor-ignore-options) 

  (org-agenda-deadline-leaders '("DUE:       " "In %3d d.: " "%2d d. ago: "))
  (org-agenda-scheduled-leaders '("DO:       " "Sched. %2dx: "))

  (org-agenda-sticky t)
  (org-agenda-dim-blocked-tasks nil)
  (org-agenda-time-grid (quote
                         ((daily today remove-match)
                          (800 1200 1600 2000)
                          "......" "----------------")))
  ;; (org-agenda-hide-tags-regexp ".")     ; hide all tags in the agenda

  :bind
  ;; ("C-c c" . org-capture)
  ("C-c a" . org-agenda)
  ("C-c r" . ns/org-agenda-reload-files))

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

(use-package org-superstar
  :custom
  (org-superstar-headline-bullets-list '("⟶"))
  (org-superstar-leading-bullet ?\s)
  :hook
  (org-mode . org-superstar-mode))

(keymap-global-set "M-&" 'with-editor-async-shell-command)

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
    ;; :general
    ;; (ns/leader-def
    ;;   "v" '(:ignore t :which-key "terminal")
    ;;   "vv" '(multi-vterm :which-key "open new term")
    ;;   "vp" '(multi-vterm-prev :which-key "prev term")
    ;;   "vn" '(multi-vterm-next :which-key "next term")
    ;;   "vr" '(multi-vterm-rename-buffer :which-key "rename term")))

(setq treesit-language-source-alist
   '((bash "https://github.com/tree-sitter/tree-sitter-bash")
     (cmake "https://github.com/uyha/tree-sitter-cmake")
     (css "https://github.com/tree-sitter/tree-sitter-css")
     (dockerfile "https://github.com/camdencheek/tree-sitter-dockerfile")
     (elisp "https://github.com/Wilfred/tree-sitter-elisp")
     (go "https://github.com/tree-sitter/tree-sitter-go")
     (gomod "https://github.com/camdencheek/tree-sitter-go-mod")
     (html "https://github.com/tree-sitter/tree-sitter-html")
     (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
     (json "https://github.com/tree-sitter/tree-sitter-json")
     (make "https://github.com/alemuller/tree-sitter-make")
     (markdown "https://github.com/ikatyang/tree-sitter-markdown")
     (python "https://github.com/tree-sitter/tree-sitter-python")
     (rust "https://github.com/tree-sitter/tree-sitter-rust")
     (toml "https://github.com/tree-sitter/tree-sitter-toml")
     (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
     (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
     (typst "https://github.com/uben0/tree-sitter-typst")
     (yaml "https://github.com/ikatyang/tree-sitter-yaml")))

(customize-set-variable 'treesit-font-lock-level 4)

(use-package jsonrpc)

(use-package eglot
  :bind
  (:prefix-map ns/eglot-actions-map
               :prefix "C-c e"
               ("a" . eglot-code-actions)
               ("f" . eglot-format-buffer))
  :custom
  (eglot-events-buffer-size 0) ; Disable the events buffer for performance
  (eglot-send-changes-idle-time 0.5)
  :hook
  (eglot-managed-mode . eglot-inlay-hints-mode)
  :config
  (add-to-list 'eglot-server-programs
               '(python-ts-mode . ("pyright")))
  ;; (add-to-list 'eglot-server-programs
               ;; `(rustic-mode . ("/home/nsalesky/.rustup/toolchains/stable-x86_64-unknown-linux-gnu/bin/rust-analyzer"
                                ;; :initializationOptions
                                ;; (:procMacro (:attributes (:enable t)
                                ;;                          :enable t)
                                ;;  :cargo (:buildScripts (:enable t))
                                ;;  :diagnostics (:disabled ["unresolved-proc-macro"
                                ;;                           "unresolved-macro-call"])))))
  (add-to-list 'eglot-server-programs
               '(svelte-mode . ("svelteserver" "--stdio"))))

(use-package corfu
  ;; :elpaca (corfu :files (:defaults "extensions/*")
  ;;                  :includes (corfu-info corfu-history))
  :bind
  (:map corfu-map
        ("C-n" . corfu-next)
        ("C-p" . corfu-previous)
        ("<tab>" . corfu-next)
        ("S-<tab>" . corfu-previous)
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

;; Example 4: Define a defensive Dabbrev Capf, which accepts all inputs.  If you
;; use Corfu and `corfu-auto=t', the first candidate won't be auto selected even
;; if `corfu-preselect=first'. You can use this instead of `cape-dabbrev'.
(defun my-cape-dabbrev-accept-all ()
  (cape-wrap-accept-all #'cape-dabbrev))

(use-package cape
  :init
  (add-to-list 'completion-at-point-functions #'my-cape-dabbrev-accept-all))
  

(use-package kind-icon :disabled t
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

(use-package verb
  :after (org)
  :config
  (define-key org-mode-map (kbd "C-c C-r") verb-command-map))

(use-package ws-butler
  :diminish ws-butler-mode
  :hook
  (prog-mode . ws-butler-mode))

(use-package re-builder
  :ensure nil
  :custom
  (reb-re-syntax 'string))

(use-package yaml-mode
  :mode "\\.yml\\'")

;; (add-hook 'c-mode-hook 'lsp)
;; (add-hook 'c++-mode-hook 'lsp)
(add-hook 'c-mode-hook 'eglot-ensure)
(add-hook 'c++-mode-hook 'eglot-ensure)

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

(use-package sly
  :custom
  (inferior-lisp-program "sbcl"))

(use-package go-ts-mode
  :ensure nil
  :mode "\\.go\\'"
  :hook (go-ts-mode . eglot-ensure)
  :custom
  (go-ts-mode-indent-offset 4))

(defun ns/compile-tex-doc ()
  "Asynchronously compile the current tex buffer to a pdf."
  (start-process "pdflatex" nil "pdflatex" (buffer-file-name)))
  ;; (async-shell-command (concat "pdflatex " (buffer-file-name))))

(use-package tex-mode
  :ensure nil
  :hook (latex-mode . (lambda () (add-hook 'after-save-hook #'ns/compile-tex-doc nil t))))

(use-package markdown-mode
  :mode "\\.md\\'")

(use-package nix-mode
  :mode "\\.nix\\'")

(defun opam-env ()
  (interactive nil)
  (dolist (var (car (read-from-string (shell-command-to-string "opam config env --sexp"))))
    (setenv (car var) (cadr var))))

(use-package tuareg
  :mode (("\\.ocamlinit\\'" . tuareg-mode)))

(use-package dune)

(use-package merlin
  :hook
  (tuareg-mode . merlin-mode)
  :custom
  (merlin-error-after-save nil))

(use-package merlin-eldoc
  :hook (tuareg-mode . merlin-eldoc-setup))

(use-package utop
  :hook
  (tuareg-mode . utop-minor-mode))

(use-package plantuml-mode
  :mode "\\.plantuml\\'"
  :custom
  (plantuml-jar-path "~/.local/bin/plantuml.jar")
  (plantuml-default-exec-mode 'jar)
  :config
  (add-to-list
   'org-src-lang-modes
   '("plantuml" . plantuml)))

(use-package python-mode
  :hook (python-mode . eglot-ensure)
  ;; :hook (python-mode . (lambda ()
                         ;; (eglot-ensure)
                         ;; (setq tab-width 4)))
  :custom
  (python-shell-interpreter "python3")
  :config
  (setq python-ts-mode-hook python-mode-hook))

(defun ns/setup-racket-mode ()
  (add-to-list 'completion-at-point-functions
               #'racket-complete-at-point))

(use-package racket-mode
  :mode "\\.rkt\\'"
  :hook
  (racket-mode . ns/setup-racket-mode))

(define-derived-mode svelte-mode
  web-mode "Svelte"
  "Major mode for Svelte.")

(use-package svelte-mode :ensure nil
  :hook (svelte-mode . eglot-ensure)
  :mode "\\.svelte\\'")

(use-package typescript-mode
  :mode ("\\.ts\\'" "\\.tsx\\'" "\\.js\\'" "\\.jsx\\'")
  :hook (typescript-mode . eglot-ensure)
  :config
  (setq typescript-indent-level 4))

(use-package ruby-mode
  :ensure nil)

(use-package inf-ruby) ;; Interact with a Ruby REPL

(use-package robe
  :hook (ruby-mode . robe-mode)
  :hook (ruby-ts-mode . robe-mode))

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
  (rustic-format-on-save t)
  :hook
  (rustic-mode . (lambda () (flycheck-mode -1)))
  (rustic-mode . eglot-ensure))

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
            ("\\.sgml\\'" . web-mode)
            ("\\.svelte\\'" . web-mode)))
    ;; :bind
    ;; ("C-c h" . ns/toggle-web-mode))

(defun ns/sudo-find-file (filename)
  (interactive "F")
  (find-file (concat "/sudo::"
                     (expand-file-name filename))))

(use-package tablist)

(use-package pdf-tools
  :config
  (pdf-loader-install))

(use-package saveplace-pdf-view)

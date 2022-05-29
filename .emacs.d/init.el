(setq byte-compile-warnings '(not free-vars unresolved noruntime lexical make-local))

(scroll-bar-mode -1)

(tooltip-mode -1)

(setq ring-bell-function 'ignore)

(set-fringe-mode 10)

(setq inhibit-startup-message t)

(setq use-short-answers t)

(setq confirm-nonexistent-file-or-buffer nil)

(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
			 ;("melpa-stable" . "https://stable.melpa.org/packages/")
			 ("elpa" . "https://elpa.gnu.org/packages/")
             ("nongnu" . "https://elpa.nongnu.org/nongnu/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Initalize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t) ;; Always installs packages that you use if they're not already installed

;; Make sure PATH is correct
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

(use-package general
    :config
    (general-override-mode)
    (general-evil-setup t)
    (general-create-definer my-leader
        :keymaps '(normal visual emacs)
            :prefix "SPC")
    (general-create-definer my-local-leader
        :keymaps '(normal insert visual emacs)
        :which-key "local-leader"
        :prefix "C-q"))

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
        ))
(add-hook mode (lambda () (display-line-numbers-mode 0))))

(set-face-attribute 'default nil :font "JetBrains Mono" :height 120)
(set-face-attribute 'fixed-pitch nil :font "JetBrains Mono" :height 120)
(set-face-attribute 'variable-pitch nil :font "SourceSans3" :height 140)

;; (set-face-attribute 'default nil :font "Iosevka Nerd Font" :height 120)
;; (set-face-attribute 'fixed-pitch nil :font "Iosevka Nerd Font" :height 120)
;; (set-face-attribute 'default nil :font "Rec Mono Semi Casual" :height 120)
;; (set-face-attribute 'fixed-pitch nil :font "Rec Mono Semi Casual" :height 120)

(use-package all-the-icons)

(use-package emojify
  :config
  (global-emojify-mode))

(use-package doom-themes
  :init
  (load-theme 'doom-moonlight t))

(use-package doom-modeline
  :custom ((doom-modeline-height 35))
  :init (doom-modeline-mode 1))

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

(use-package smooth-scrolling
  :init
  (setq smooth-scroll-margin 5)
  :config
  (smooth-scrolling-mode))

(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

(use-package hl-todo
  :config
  (global-hl-todo-mode))

;; (add-hook 'prog-mode-hook 'hl-line-mode)

; TODO

(use-package rainbow-delimiters
    :hook (prog-mode . rainbow-delimiters-mode))

(use-package smartparens
  :hook
  (prog-mode . smartparens-mode))

(use-package evil-smartparens
  :hook
  (smartparens-enabled . evil-smartparens-mode))

(use-package yasnippet
  :config
  (yas-global-mode))

(use-package company
    :after lsp-mode
    :hook (lsp-mode . company-mode)
    :bind (:map company-active-map
        ("<tab>" . company-complete-selection))
        (:map lsp-mode-map
        ("<tab>" . company-indent-or-complete-common))
    :custom
    (company-minimum-prefix-length 1)
    (company-idle-delay 0.0))

;; Adds colors and icons to company-mode
(use-package company-box
    :hook (company-mode . company-box-mode))

(use-package which-key
  :after (ivy)
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.3))

(use-package flyspell
  :hook ((prog-mode . flyspell-prog-mode)
        ((org-mode markdown-mode) . flyspell-mode)))

;; (use-package flyspell-correct
;;   :after (flyspell)
;;   :config
;;   (setq flyspell-correct-interface #'flyspell-correct-ivy))

(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :custom ((projectile-completion-system 'ivy))
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  ;(when (file-directory-p "~/Documents")
    ;(setq projectile-project-search-path '("~/Documents")))
  (setq projectile-switch-project-action #'projectile-dired)

  :general
  (my-leader
      "SPC" '(projectile-find-file :which-key "Find file in project")
      "p" '(:ignore t :which-key "projects")
      "p p" '(projectile-switch-project :which-key "Switch project")))

(use-package counsel-projectile
  :config (counsel-projectile-mode))

(use-package treemacs)
(use-package treemacs-evil
    :after (treemacs evil))
(use-package treemacs-projectile
    :after (treemacs projectile))
(use-package treemacs-icons-dired
    :hook (dired-mode . treemacs-icons-dired-enable-once))
(use-package treemacs-magit
    :after (treemacs magit))
(use-package lsp-treemacs
    :after (treemacs lsp-mode)
    :config (lsp-treemacs-sync-mode 1))
(use-package treemacs-all-the-icons
  :config
  (treemacs-load-theme "all-the-icons"))

(use-package ivy
    :diminish
    :bind (
    :map ivy-minibuffer-map
    ("TAB" . ivy-alt-done)
    ("C-l" . ivy-alt-done)
    ("C-j" . ivy-next-line)
    ("C-k" . ivy-previous-line)
    :map ivy-switch-buffer-map
    ("C-k" . ivy-previous-line)
    ("C-l" . ivy-done)
    ("C-d" . ivy-switch-buffer-kill)
    :map ivy-reverse-i-search-map
    ("C-k" . ivy-previous-line)
    ("C-d" . ivy-reverse-i-search-kill))
    :init
    (ivy-mode 1))

(use-package ivy-rich
    :init
    (ivy-rich-mode 1))

(use-package swiper
  :bind (("C-s" . swiper)))

;; (use-package ivy-posframe
;;     :init
;;     (setq ivy-posframe-display-functions-alist
;;         '((counsel-M-x . ivy-display-function-fallback)
;;         (counsel-find-file . ivy-display-function-fallback)
;;         (swiper . ivy-display-function-fallback)
;;         (counsel-switch-buffer . ivy-display-function-fallback)
;;         (t . ivy-posframe-display)))
;;     :config
;;     (ivy-posframe-mode 1))

(use-package counsel
    :bind
    (("M-x" . counsel-M-x)
     ("M-y" . counsel-yank-pop-selection)
     ("M-i" . counsel-imenu)
     ("C-s" . counsel-grep-or-swiper)
     ("C-x b" . counsel-ibuffer)
     ("C-x C-f" . counsel-find-file)
     :map minibuffer-local-map
     ("C-r" . 'counsel-minibuffer-history)))

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; Insert newlines when you C-n at the end of the buffer
(setq next-line-add-newlines t)

(use-package hydra)

(defhydra hydra-text-scale (:timeout 4)
  "scale text"
  ("j" text-scale-increase "up")
  ("k" text-scale-decrease "down")
  ("f" nil "finished" :exit t))

(my-leader
 "t k" '(hydra-text-scale/body :which-key "Scale text"))

(use-package evil
  :init
  (setq evil-want-integration t
	evil-want-keybinding nil
	evil-want-C-u-scroll t
	evil-want-C-i-jump nil)
  :config
  (evil-mode 1)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)
  (define-key evil-insert-state-map (kbd "TAB") 'tab-to-tab-stop)

  ;; use visual line motions even outside of visual-line-mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  ;; set the initial state for certain special modes
  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal)
  ;; disable Evil-mode for certain buffers
  (evil-set-initial-state 'eshell-mode 'emacs))

;; Gives us default Evil configurations for a lot of other modes
(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

;; (defhydra my-mc-hydra (:color pink
;;                        :hint nil
;;                        :pre (evil-mc-pause-cursors))
;;   "
;; ^Match^            ^Line-wise^           ^Manual^
;; ^^^^^^----------------------------------------------------
;; _Z_: match all     _J_: make & go down   _z_: toggle here
;; _m_: make & next   _K_: make & go up     _r_: remove last
;; _M_: make & prev   ^ ^                   _R_: remove all
;; _n_: skip & next   ^ ^                   _p_: pause/resume
;; _N_: skip & prev

;; Current pattern: %`evil-mc-pattern

;; "
;;   ("Z" #'evil-mc-make-all-cursors)
;;   ("m" #'evil-mc-make-and-goto-next-match)
;;   ("M" #'evil-mc-make-and-goto-prev-match)
;;   ("n" #'evil-mc-skip-and-goto-next-match)
;;   ("N" #'evil-mc-skip-and-goto-prev-match)
;;   ("J" #'evil-mc-make-cursor-move-next-line)
;;   ("K" #'evil-mc-make-cursor-move-prev-line)
;;   ("z" #'+multiple-cursors/evil-mc-toggle-cursor-here)
;;   ("r" #'+multiple-cursors/evil-mc-undo-cursor)
;;   ("R" #'evil-mc-undo-all-cursors)
;;   ("p" #'+multiple-cursors/evil-mc-toggle-cursors)
;;   ("q" #'evil-mc-resume-cursors "quit" :color blue)
;;   ("<escape>" #'evil-mc-resume-cursors "quit" :color blue))


;; (use-package evil-mc
;;   :config
;;   (global-evil-mc-mode)
;;   (general-define-key
;;     :states '(normal visual)
;;     :prefix "g"
;;     "z" 'my-mc-hydra/body))

(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)
(setq-default c-basic-offset 4)
(setq-default evil-shift-width 4)

;; (setq-default electric-indent-inhibit t)

;; Make the backspace properly erase the whole tab instead of removing
;; 1 space at a time
(setq backward-delete-char-untabify-method 'hungry)

;; Make Evil mode backspace delete a whole tab's worth of spaces at a time
(general-define-key
    :states 'insert
    "<backspace>" 'backward-delete-char-untabify)

(my-leader
    ;; Line formatting
    "TAB TAB" '(smart-comment :which-key "Comment or uncomment lines"))

;; Keep track of recently-opened files
(recentf-mode 1)
(setq recentf-max-menu-items 25)
(setq recentf-max-saved-items 25)

(my-leader
    "." '(counsel-find-file :which-key "Find file")

    "f" '(:ignore t :which-key "files")
    "f r" '(counsel-recentf :which-key "Open Recent Files")
    "f c" '((lambda () (interactive)(find-file "~/.dotfiles/.emacs.d/config.org")) :which-key "Open config.org"))

(my-leader
     "t"  '(:ignore t :which-key "toggle")
     "t s" '(counsel-load-theme :which-key "Choose theme")

     "t t" '(treemacs :which-key "Treemacs")
     "t y" '(lsp-treemacs-symbols :which-key "Treemacs Symbols"))

(my-leader
    "o" '(:ignore t :which-key "open")
    "o e" '(eshell :which-key "Open EShell"))

(my-leader
     "w" '(:ignore t :which-key "window")
     "wc" '(delete-window :which-key "Close window")
     "wv" '(split-window-right :which-key "Vertical split")
     "ws" '(split-window-below :which-key "Horizontal split")
     "wh" '(windmove-left :which-key "Select left window")
     "wj" '(windmove-down :which-key "Select down window")
     "wk" '(windmove-up :which-key "Select up window")
     "wl" '(windmove-right :which-key "Select right window"))

(my-leader
      ;"," '(counsel-switch-buffer :which-key "Switch buffer")

      "b" '(:ignore t :which-key "buffers")
      "b k" '(kill-buffer :which-key "Kill buffer"))

(general-define-key
 :states 'normal
 "s" 'avy-goto-char-timer
 "S" 'avy-pop-mark)

(my-leader
  "s" '(:ignore t :which-key "search")
  "s b" '(swiper :which-key "Search buffer"))

(use-package ag
  :general
  (my-leader
    "s p" '(projectile-ag :which-key "Search project")))

(use-package multiple-cursors
  :general
  (general-define-key
    :states '(normal visual)
    "R" 'mc/mark-all-like-this
    "L" 'mc/edit-lines)
  ;; keybindings for when multiple cursors are active
  (general-define-key
   :states '(normal visual emacs)
   :keymaps 'mc/keymap
    "C-n" 'mc/mark-more-like-this-extended))

(use-package magit
  :general
  (my-leader
    "g" '(:ignore t :which-key "git")
    "g g" '(magit-status :which-key "Magit Status")))

;(use-package forge)

(use-package blamer
  :general
  (my-leader
    "g b" '(global-blamer-mode :which-key "Toggle blamer mode")))

(use-package format-all)
  ;:hook
  ;(prog-mode . format-all-mode)

(use-package flycheck)

(use-package lsp-mode
    :commands (lsp lsp-deferred)
    :init
    (setq lsp-keymap-prefix "C-l"
          lsp-lens-enable t
          lsp-signature-auto-activate nil)
    :config
    (lsp-enable-which-key-integration t)
    ;; :general
    ;; TODO figure this out
    ;; (my-leader
    ;;   "c" '(:ignore t :which-key "code")))
    ;; (add-hook 'lsp-after-open-hook
    ;;     (lambda ()
    ;;       (when (lsp-find-workspace 'rust-analyzer nil)
    ;;         (lsp-rust-analyzer-inlay-hints-mode))))
    :custom

    ;; Enable/disable type hints as you type for Rust
    (lsp-rust-analyzer-server-display-inlay-hints t)
    (lsp-rust-analyzer-display-lifetime-elision-hints-enable "skip_trivial")
    (lsp-rust-analyzer-display-chaining-hints nil)
    (lsp-rust-analyzer-display-lifetime-elision-hints-use-parameter-names nil)
    (lsp-rust-analyzer-display-closure-return-type-hints t)
    (lsp-rust-analyzer-display-parameter-hints t)
    (lsp-rust-analyzer-display-reborrow-hints nil))

(use-package lsp-ivy)

(use-package lsp-ui
    :hook (lsp-mode . lsp-ui-mode)
    :custom
    (lsp-ui-peek-always-show t)
    (lsp-ui-sideline-show-hover t)
    (lsp-ui-doc-position 'bottom)
    (lsp-ui-doc-enable nil))

(use-package dap-mode
  :config
  (dap-auto-configure-mode))

(use-package tree-sitter
  :config
  (global-tree-sitter-mode)
  :hook
  (tree-sitter-mode . tree-sitter-hl-mode))

(use-package tree-sitter-langs)

(use-package wakatime-mode
  :config
  (global-wakatime-mode))

(use-package lsp-pyright
  :hook
  (python-mode . (lambda ()
                   (require 'lsp-pyright)
                   (lsp-deferred))))

(use-package typescript-mode
  :mode "\\.ts\\'"
  :hook (typescript-mode . lsp-deferred)
  :config
  (setq typescript-indent-level 4))

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

(use-package rustic
  :bind (:map rustic-mode-map
              ("M-j" . lsp-ui-imenu)
              ("M-?" . lsp-find-references)))
  ;; :config
  ;; uncomment for less flashiness
  ;; (setq lsp-eldoc-hook nil)
  ;; (setq lsp-enable-symbol-highlighting nil)
  ;; (setq lsp-signature-auto-activate nil)

  ;; comment to disable rustfmt on save
  ;; (setq rustic-format-on-save t))

(add-hook 'c-mode-hook 'lsp)
(add-hook 'c++-mode-hook 'lsp)

;; (use-package meghanada
;;   :hook
;;   (java-mode . meghanada-mode)
;;   (java-mode . flycheck-mode))

;; (setq meghanada-java-path "java"
;;       meghanada-maven-path "mvn")

(use-package lsp-java
  :hook
  (java-mode . lsp))

(use-package clojure-mode
  :mode "\\.clj\\'"
  :hook ((clojure-mode . lsp-deferred)
         (clojurescript-mode . lsp-deferred)
         (clojurec-mode . lsp-deferred)))


(use-package cider)

(use-package glsl-mode
  :mode "\\.glsl\\'")

(use-package ca65-mode
  :mode "\\.s\\'")

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


(use-package org-contrib :pin nongnu)

;; Org Mode
(use-package org
    :pin elpa
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
        '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!)")
            (sequence "BACKLOG(b)" "PLAN(p)" "READY(r)" "ACTIVE(a)" "REVIEW(v)"
                "WAIT(w@/!)" "HOLD(h)" "|" "COMPLETED(c)" "CANC(k@)")))

    :general
    (my-leader
      "n" '(:ignore t :which-key "notes")))

    ;; local-leader stuff
    ;; (my-local-leader
    ;;   :keymaps 'org-mode-map
    ;;   "b" '(org-babel-tangle :which-key "Org babel tangle")
    ;;   "t" '(

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

;; (defun ns/org-mode-visual-fill ()
;;     (setq visual-fill-column-width 120
;;     visual-fill-column-center-text t)
;;     (visual-fill-column-mode 1))

;; (use-package visual-fill-column
;;     :hook (org-mode . ns/org-mode-visual-fill))

(use-package toc-org
  :hook
  (org-mode . toc-org-mode))

;; (use-package org-modern
;;     :config
;;     (add-hook 'org-mode-hook #'org-modern-mode)
;;     (add-hook 'org-agenda-finalize #'org-modern-agenda))

(use-package imenu-list
  :init
  (setq imenu-list-position 'left)
  :general
  (my-leader
   "t i" '(imenu-list-smart-toggle :which-key "Imenu")))

(use-package org-roam
  :custom
  (org-roam-directory "~/notes/roam/")
  :config
  (setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
  (org-roam-db-autosync-mode)
  :general
  (my-leader
    "n r" '(:ignore t :which-key "roam")
    ;;"n r" '(:keymap org-roam-mode-map :which-key "roam")
    "n r f" '(org-roam-node-find :which-key "Find Node")
    "n r i" '(org-roam-node-insert :which-key "Insert Node")
    "n r o" '(org-roam-node-open :which-key "Open Node")
    "n r g" '(org-roam-graph :which-key "Graph")))

(use-package hide-mode-line)

(defun ns/presentation-setup ()
    (setq text-scale-mode-amount 2)
    (org-display-inline-images)
    (text-scale-mode 1)
    (hide-mode-line-mode 1))

(defun ns/presentation-end ()
    (text-scale-mode 0)
    (hide-mode-line-mode 0))

(use-package org-tree-slide
    :hook ((org-tree-slide-play . ns/presentation-setup)
           (org-tree-slide-stop . ns/presentation-end))
    :custom
    (org-tree-slide-slide-in-effect nil)
    (org-tree-slide-activate-message "Presentation started!")
    (org-tree-slide-deactivate-message "Presentation finished!")
    (org-tree-slide-header t)
    (org-image-actual-width nil)
    :bind
    (:map org-mode-map
            ("<f8>" . org-tree-slide-mode)
        :map org-tree-slide-mode-map
            ("<f9>" . org-tree-slide-move-previous-tree)
            ("<f10>" . org-tree-slide-move-next-tree)
        ))

(use-package olivetti
  :init
  (setq olivetti-body-width 80
        olivetti-style t)
  :hook
  (org-mode . olivetti-mode))

(use-package perspective
    :init
    (setq persp-state-default-file "~/.dotfiles/.emacs.d/perspective-state"
          persp-mode-prefix-key (kbd "C-c M-p"))

    :config
    (persp-mode)

    :general
    (my-leader
      "," '(persp-ivy-switch-buffer :which-key "Switch buffer")
      "b k" '(persp-remove-buffer :which-key "Remove buffer")

      "TAB" '(:ignore t :which-key "workspace")
      "TAB ." '(persp-switch :which-key "Switch to or create a workspace")
      "TAB r" '(persp-rename :which-key "Rename workspace")
      "TAB s" '(persp-state-save :which-key "Save workspaces")
      "TAB l" '(persp-state-load :which-key "Load saved workspaces")
      "TAB k" '(persp-kill :which-key "Kill workspace")
      "TAB 1" '((lambda () (interactive)(persp-switch-by-number 1)) :which-key "Switch to workspace 1")
      "TAB 2" '((lambda () (interactive)(persp-switch-by-number 2)) :which-key "Switch to workspace 2")
      "TAB 3" '((lambda () (interactive)(persp-switch-by-number 3)) :which-key "Switch to workspace 3")
      "TAB 4" '((lambda () (interactive)(persp-switch-by-number 4)) :which-key "Switch to workspace 4")
      "TAB 5" '((lambda () (interactive)(persp-switch-by-number 5)) :which-key "Switch to workspace 5")
      "TAB 6" '((lambda () (interactive)(persp-switch-by-number 6)) :which-key "Switch to workspace 6")
      "TAB 7" '((lambda () (interactive)(persp-switch-by-number 7)) :which-key "Switch to workspace 7")
      "TAB 8" '((lambda () (interactive)(persp-switch-by-number 8)) :which-key "Switch to workspace 8")
      "TAB 9" '((lambda () (interactive)(persp-switch-by-number 9)) :which-key "Switch to workspace 9")))

(use-package tablist)

(use-package pdf-tools
  :config
  (pdf-loader-install))

(use-package nov
  :mode "\\.epub\\'")

(use-package smart-comment)

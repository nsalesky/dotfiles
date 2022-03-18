;; Basic UI settings
(setq inhibit-startup-message t)
(scroll-bar-mode -1) ; Disable visible scrollbar
(tool-bar-mode -1)   ; Disable the toolbar
(tooltip-mode -1)    ; Disable tooltips
(set-fringe-mode 10) ; Give some breathing room
(menu-bar-mode -1)   ; Disable the menu bar
(setq ring-bell-function 'ignore) ; Disable alarms

;; Font settings
(set-face-attribute 'default nil :font "Iosevka Nerd Font" :height 120)
(set-face-attribute 'fixed-pitch nil :font "Iosevka Nerd Font" :height 120)
(set-face-attribute 'variable-pitch nil :font "Source Sans Pro" :height 140)

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; Initialize package sources
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
			 ;("melpa-stable" . "https://stable.melpa.org/packages/")
			 ("org" . "https://orgmode.org/elpa/")
			 ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Initalize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t) ;; Installs packages that you use if they're not already installed

;; Install ivy for completion
(use-package ivy
  :diminish
  :bind (("C-s" . swiper)
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

;; NOTE: The first time you run this on a new machine, you'll need to run this
;; command interactively
;;
;; M-x all-the-icons-install-fonts
(use-package all-the-icons)

(use-package doom-modeline
  :custom ((doom-modeline-height 35))
  :init (doom-modeline-mode 1))

;; Enable line numbers
(column-number-mode)
(global-display-line-numbers-mode t)

;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
		term-mode-hook
		shell-mode-hook
		eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(use-package doom-themes
  :init
  (load-theme 'doom-moonlight t))

;; Rainbox delimiters for all programming modes
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; which-key
(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.3))

(use-package ivy-rich
  :init
  (ivy-rich-mode 1))

(use-package counsel
  :bind (("M-x" . counsel-M-x)
	 ("C-x b" . counsel-ibuffer)
	 ("C-x C-f" . counsel-find-file)
	 :map minibuffer-local-map
	 ("C-r" . 'counsel-minibuffer-history)))

(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

(use-package general
  :config
  (general-evil-setup t)
  (general-create-definer my-leader
			  :keymaps '(normal insert visual emacs)
			  :prefix "SPC"
			  :global-prefix "C-SPC")

  ;; Global Keybindings
  (my-leader
    ;; Projectile
    "SPC" '(projectile-find-file :which-key "Find file in project")
    "," '(counsel-switch-buffer :which-key "Switch buffer")
    "." '(counsel-find-file :which-key "Find file")
    ;"p" (:ignore t :which-key "project")
    

    ;; Toggle
   "t"  '(:ignore t :which-key "toggle")
   "tt" '(counsel-load-theme :which-key "Choose theme")

   ;; Line formatting
   "TAB TAB" '(smart-comment :which-key "Comment or uncomment lines")

   ;; Window
   "w" '(:ignore t :which-key "window")
   "wc" '(delete-window :which-key "Close window")
   "wv" '(split-window-right :which-key "Vertical split")
   "ws" '(split-window-below :which-key "Horizontal split")
   "wh" '(windmove-left :which-key "Select left window")
   "wj" '(windmove-down :which-key "Select down window")
   "wk" '(windmove-up :which-key "Select up window")
   "wl" '(windmove-right :which-key "Select right window")
  ))

;; Better commenting
(use-package smart-comment)

;; Evil mode
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
  
  ;; use visual line motions even outside of visual-line-mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(use-package hydra)

(defhydra hydra-text-scale (:timeout 4)
  "scale text"
  ("j" text-scale-increase "up")
  ("k" text-scale-decrease "down")
  ("f" nil "finished" :exit t))

(my-leader
 "ts" '(hydra-text-scale/body :which-key "scale text"))

;; Projectile
(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :custom ((projectile-completion-system 'ivy))
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  ;(when (file-directory-p "~/Documents")
    ;(setq projectile-project-search-path '("~/Documents")))
  (setq projectile-switch-project-action #'projectile-dired))

(use-package counsel-projectile
  :config (counsel-projectile-mode))

;; Magit
(use-package magit)

;(use-package forge)

(defun ns/org-mode-setup ()
  (org-indent-mode)
  ;; (variable-pitch-mode 1)
  (visual-line-mode 1))

(defun ns/org-font-setup ()
  ;; Make sure that anything that should be fixed pitch in Org files actually appears that way
  (set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-table nil :inherit '(shadow fixed-pitch))
  ;; (set-face-attribute 'org-indent nil :inherit '(org-hide fixed-pitch))
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch))

;; Org Mode
(use-package org
  :hook (org-mode . ns/org-mode-setup)
  :config
  ;; (ns/org-font-setup)
  (setq org-hide-emphasis-markers t
	org-ellipsis " ▾"
	org-pretty-entities t

	org-directory "~/org"

	org-todo-keywords
	'((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!)")
	  (sequence "BACKLOG(b)" "PLAN(p)" "READY(r)" "ACTIVE(a)" "REVIEW(v)"
		    "WAIT(w@/!)" "HOLD(h)" "|" "COMPLETED(c)" "CANC(k@)"))))


(use-package org-modern
  :config
  (add-hook 'org-mode-hook #'org-modern-mode)
  (add-hook 'org-agenda-finalize #'org-modern-agenda))

(defun ns/org-mode-visual-fill ()
  (setq visual-fill-column-width 100
	visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :hook (org-mode . ns/org-mode-visual-fill))

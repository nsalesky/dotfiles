;; Initialize package sources
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
(setq use-package-always-ensure t) ;; Installs packages that you use if they're not already installed

;; Make sure PATH is correct
(use-package exec-path-from-shell
  :config
  (exec-path-from-shell-initialize))

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

  (use-package ivy-rich
    :init
    (ivy-rich-mode 1))

  (use-package ivy-posframe
    :init
    (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display)))
    :config
    (ivy-posframe-mode 1))

  (use-package counsel
    :bind (("M-x" . counsel-M-x)
       ("C-x b" . counsel-ibuffer)
       ("C-x C-f" . counsel-find-file)
       :map minibuffer-local-map
       ("C-r" . 'counsel-minibuffer-history)))

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.3))

  (setq inhibit-startup-message t)
  (scroll-bar-mode -1) ; Disable visible scrollbar
  (tool-bar-mode -1)   ; Disable the toolbar
  (tooltip-mode -1)    ; Disable tooltips
  (set-fringe-mode 10) ; Give some breathing room
  (menu-bar-mode -1)   ; Disable the menu bar
  (setq ring-bell-function 'ignore) ; Disable alarms

  ;; Enable line numbers
  (column-number-mode)
  (global-display-line-numbers-mode t)

  ;; Disable line numbers for some modes
  (dolist (mode '(org-mode-hook
          term-mode-hook
          shell-mode-hook
          eshell-mode-hook
          treemacs-mode-hook
          ))
    (add-hook mode (lambda () (display-line-numbers-mode 0))))


  ;; Rainbox delimiters for all programming modes
  (use-package rainbow-delimiters
    :hook (prog-mode . rainbow-delimiters-mode))

  ;; Better commenting
  (use-package smart-comment)

(set-face-attribute 'default nil :font "Iosevka Nerd Font" :height 120)
(set-face-attribute 'fixed-pitch nil :font "Iosevka Nerd Font" :height 120)
(set-face-attribute 'variable-pitch nil :font "Source Sans Pro" :height 140)

;; NOTE: The first time you run this on a new machine, you'll need to run this
;; command interactively
;;
;; M-x all-the-icons-install-fonts
(use-package all-the-icons)

;; Enable global visual line mode to wrap lines properly.
(global-visual-line-mode 1)
;; Highlight the current line in prog mode
(add-hook 'prog-mode-hook 'hl-line-mode)

(use-package doom-themes
  :init
  (load-theme 'doom-moonlight t))

    ;(use-package page-break-lines)

        ;(use-package dashboard
        ;:config
  ;(dashboard-setup-startup-hook))

(use-package doom-modeline
  :custom ((doom-modeline-height 35))
  :init (doom-modeline-mode 1))

(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

(use-package emojify
  :config
  (global-emojify-mode))

(use-package general
    :config
    (general-override-mode)
    (general-evil-setup t)
    (general-create-definer my-leader
        :keymaps '(normal insert visual emacs)
            :prefix "SPC"
            :global-prefix "C-SPC"))

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

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

(use-package hydra)

(defhydra hydra-text-scale (:timeout 4)
  "scale text"
  ("j" text-scale-increase "up")
  ("k" text-scale-decrease "down")
  ("f" nil "finished" :exit t))

(my-leader
 "ts" '(hydra-text-scale/body :which-key "scale text"))

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

(use-package org-contrib :pin nongnu)

;; Org Mode
(use-package org
    :pin elpa
    :hook (org-mode . ns/org-mode-setup)
    :config
    ;; (ns/org-font-setup)
    (setq org-hide-emphasis-markers t
    org-ellipsis " ▾"
    org-pretty-entities t

    org-directory "~/org"

    org-src-tab-acts-natively t
    org-src-preserve-indentation t

    org-todo-keywords
    '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!)")
        (sequence "BACKLOG(b)" "PLAN(p)" "READY(r)" "ACTIVE(a)" "REVIEW(v)"
            "WAIT(w@/!)" "HOLD(h)" "|" "COMPLETED(c)" "CANC(k@)")))

    :general
    (my-leader
      "n" '(:ignore t :which-key "notes")))

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

(defun ns/org-mode-visual-fill ()
    (setq visual-fill-column-width 120
    visual-fill-column-center-text t)
    (visual-fill-column-mode 1))

(use-package visual-fill-column
    :hook (org-mode . ns/org-mode-visual-fill))

(use-package org-modern
    :config
    (add-hook 'org-mode-hook #'org-modern-mode)
    (add-hook 'org-agenda-finalize #'org-modern-agenda))

(use-package org-roam
  :custom
  (org-roam-directory "~/org/roam/")
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

(use-package magit
  :general
  (my-leader
    "g" '(magit-status :which-key "Git Status")))

;(use-package forge)

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

  (use-package lsp-mode
    :commands (lsp lsp-deferred)
    :init
    (setq lsp-keymap-prefix "C-l")
    :config
    (lsp-enable-which-key-integration t)
    :general
    ;; TODO figure this out
    (my-leader
      "c" '(:keymap lsp-mode-map :which-key "code")))

  (use-package lsp-ivy)

  (use-package lsp-ui
    :hook (lsp-mode . lsp-ui-mode)
    :custom
    (lsp-ui-doc-position 'bottom))

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

(use-package company-box
  :hook (company-mode . company-box-mode))

(use-package format-all)
  ;:hook
  ;(prog-mode . format-all-mode)

(use-package yasnippet
  :config
  (yas-global-mode))

(use-package wakatime-mode
  :config
  (global-wakatime-mode))

(use-package perspective
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
      "TAB 3" '((lambda () (interactive)(persp-switch-by-number 2)) :which-key "Switch to workspace 3")
      "TAB 4" '((lambda () (interactive)(persp-switch-by-number 2)) :which-key "Switch to workspace 4")
      "TAB 5" '((lambda () (interactive)(persp-switch-by-number 2)) :which-key "Switch to workspace 5")
      "TAB 6" '((lambda () (interactive)(persp-switch-by-number 2)) :which-key "Switch to workspace 6")
      "TAB 7" '((lambda () (interactive)(persp-switch-by-number 2)) :which-key "Switch to workspace 7")
      "TAB 8" '((lambda () (interactive)(persp-switch-by-number 2)) :which-key "Switch to workspace 8")
      "TAB 9" '((lambda () (interactive)(persp-switch-by-number 2)) :which-key "Switch to workspace 9")))

(use-package typescript-mode
  :mode "\\.ts\\'"
  :hook (typescript-mode . lsp-deferred)
  :config
  (setq typescript-indent-level 4))

(use-package rustic)

(add-hook 'c-mode-hook 'lsp)
(add-hook 'c++-mode-hook 'lsp)



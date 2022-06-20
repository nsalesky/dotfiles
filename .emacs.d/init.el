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

;; (require 'package)

;; (setq package-archives '(("melpa" . "https://melpa.org/packages/")
;; 			 ;("melpa-stable" . "https://stable.melpa.org/packages/")
;; 			 ("elpa" . "https://elpa.gnu.org/packages/")
;;              ("nongnu" . "https://elpa.nongnu.org/nongnu/")))

;; (package-initialize)
;; (unless package-archive-contents
;;   (package-refresh-contents))

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

;; Initalize use-package on non-Linux platforms
;; (unless (package-installed-p 'use-package)
;;   (package-install 'use-package))

;; (require 'use-package)
;; (setq use-package-always-ensure t) ;; Always installs packages that you use if they're not already installed

;; ;; Make sure PATH is correct
;; (use-package exec-path-from-shell
;;   :config
;;   (exec-path-from-shell-initialize))

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

(use-package general
    :config
    (general-override-mode)
    (general-evil-setup t)
    (general-create-definer my-leader
      :states '(normal visual emacs)
      :keymaps 'override
      :prefix "SPC"))

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
  :config
  (load-theme 'doom-shades-of-purple t))

;; (use-package nano-theme
;;   :straight (nano-theme :type git :host github
;;                         :repo "rougier/nano-theme")
;;   :config
;;   (nano-light))

;; (use-package modus-themes
;;   :init
;;   (setq modus-themes-italic-constructs t     ; use italics for comments
;;         modus-themes-bold-constructs t       ; use bold
;;         modus-themes-syntax '()
;;         modus-themes-mixed-fonts t           ; Enable fixed and variable pitched fonts
;;         modus-themes-prompts '(italic)
;;         ;; modus-themes-mode-line '(accented borderless)
;;         modus-themes-mode-line '()
;;         )
;;   :config
;;   (modus-themes-load-vivendi))

(use-package doom-modeline
  :init
  (setq doom-modeline-height 35
        doom-modeline-support-imenu t)
  (doom-modeline-mode 1))

(use-package nano-modeline
  :straight (nano-modeline :type git :host github
                           :repo "rougier/nano-modeline"))
  ;; :init
  ;; (nano-modeline-mode 1))

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

(use-package consult)

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '(
                                   (file (styles basic partial-completion))
                                   (eglot (styles orderless)))))

(use-package all-the-icons-completion
  :config
  (all-the-icons-completion-mode))

(use-package aggressive-indent
  :hook
  (emacs-lisp-mode-hook . aggressive-indent-mode))

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

;; (use-package company
;;     :after lsp-mode
;;     :hook (lsp-mode . company-mode)
;;     :bind (:map company-active-map
;;         ("<tab>" . company-complete-selection))
;;         (:map lsp-mode-map
;;         ("<tab>" . company-indent-or-complete-common))
;;     :custom
;;     (company-minimum-prefix-length 1)
;;     (company-idle-delay 0.0))

;; ;; Adds colors and icons to company-mode
;; (use-package company-box
;;     :hook (company-mode . company-box-mode))

(use-package corfu
  :general
  (:keymaps 'corfu-map
            :states 'insert
            "C-n" #'corfu-next
            "C-p" #'corfu-previous
            "<escape>" #'corfu-quit
            "<return>" #'corfu-insert
            "M-d" #'corfu-show-documentation
            "M-l" #'corfu-show-location)
  
  :custom
  (corfu-auto t) ; Only use 'corfu' when calling 'completion-at-point' or 'indent-for-tab-command'

  (corfu-auto-prefix 3)             ; Minimum length of prefix for auto-complete
  (corfu-auto-delay 0.25)

  (corfu-min-width 80)
  (corfu-max-width corfu-min-width) ; Always have the same width
  (corfu-count 14) ; Max number of candidates to show
  (corfu-scroll-margin 4)
  (corfu-cycle nil)

  (corfu-quit-at-boundary nil)
  (corfu-seperator ?\s)            ; Use space
  (corfu-quit-no-match 'seperator) ; Don't quit if there is 'corfu-seperator' inserted
  (corfu-preview-current 'insert)  ; Preview first candidate. Insert on input if only one candidate
  (corfu-preselect-first t)        ; Preselect first candidate?

  (corfu-echo-documentation nil) ; Use 'corfu-doc' instead

  ;; Enable indentation+completion using the TAB key instead of M-TAB
  (tab-always-indent 'complete)
  (completion-cycle-threshold nil)

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

    ;; set up for Consult
    (consult-customize consult--source-buffer :hidden t :default nil)
    (add-to-list 'consult-buffer-sources persp-consult-source)

    :general
    (my-leader
      ;; "," '(persp-switch-buffer :which-key "Switch buffer")
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
  ;; :custom ((projectile-completion-system 'ivy))
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

;; (use-package counsel-projectile
;;   :config (counsel-projectile-mode))

(use-package treemacs)
(use-package treemacs-evil
    :after (treemacs evil))
(use-package treemacs-projectile
    :after (treemacs projectile))
(use-package treemacs-icons-dired
    :hook (dired-mode . treemacs-icons-dired-enable-once))
(use-package treemacs-magit
    :after (treemacs magit))
;; (use-package lsp-treemacs
;;     :after (treemacs lsp-mode)
;;     :config (lsp-treemacs-sync-mode 1))
(use-package treemacs-all-the-icons
  :config
  (treemacs-load-theme "all-the-icons"))

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; Insert newlines when you C-n at the end of the buffer
;; (setq next-line-add-newlines t)

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
  (evil-set-initial-state 'dashboard-mode 'normal))
  ;; disable Evil-mode for certain buffers
  ;; (evil-set-initial-state 'eshell-mode 'emacs))

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

;; Set the default tab settings
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)
(setq-default c-basic-offset 4)
(setq-default python-indent-offset 4)
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
    "." '(find-file :which-key "Find file")

    "f" '(:ignore t :which-key "files")
    "f r" '(consult-recent-file :which-key "Open Recent Files")
    "f c" '((lambda () (interactive)(find-file "~/.dotfiles/.emacs.d/config.org")) :which-key "Open config.org"))

(my-leader
     "t"  '(:ignore t :which-key "toggle")
     "t s" '(consult-theme :which-key "Choose theme")

     "t t" '(treemacs :which-key "Treemacs"))
     ;; "t y" '(lsp-treemacs-symbols :which-key "Treemacs Symbols"))

(my-leader
    "o" '(:ignore t :which-key "open"))

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
      "," '(consult-buffer :which-key "Switch buffer")

      "b" '(:ignore t :which-key "buffers")
      "b k" '(kill-buffer :which-key "Kill buffer"))

(general-define-key
 :states 'normal
 "s" 'avy-goto-char-timer
 "S" 'avy-pop-mark)

(general-define-key
 :states '(normal emacs)
 "C-s" 'consult-line)

(my-leader
  "s" '(:ignore t :which-key "search")
  "s b" '(consult-line :which-key "Search buffer"))

(use-package ag
  :general
  (my-leader
    "s p" '(projectile-ag :which-key "Search project")))

(my-leader
  "h" '(:ignore t :which-key "help")
  "h v" '(helpful-variable :which-key "Describe variable")
  "h f" '(helpful-callable :which-key "Describe function")
  "h k" '(helpful-key :which-key "Describe key"))

;; Simple keybinding to open dired if C-x d is too hard :)
(my-leader "d" '(dired :which-key "Dired"))

;; Set up some custom keybindings for Dired
(general-define-key
    :states 'normal
    :keymaps 'dired-mode-map
    "h" 'dired-up-directory
    "l" 'dired-find-file)

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

(setq org-agenda-files (list "agenda/inbox.org"
                             "agenda/agenda.org"
                             "agenda/projects.org") ; add any files to be pulled from
      org-agenda-hide-tags-regexp "."     ; hide all tags in the agenda
      org-log-done 'time             ; log the time when a task is *DONE*
      org-agenda-compact-blocks nil
      org-agenda-block-separator nil
      )

(setq org-capture-templates
       `(("i" "Inbox" entry  (file "inbox.org")
        ,(concat "* TODO %?\n"
                 "/Entered on/ %U"))
         ("m" "Meeting entry" entry (file+headline "agenda.org" "Future")
          ,(concat "* %? :meeting:\n"
                   "<%<%Y-%m-%d %a %H:00>>"))
         ("n" "Note" entry (file "notes.org")
          ,(concat "* Note (%a)\n"
                   "/Entered on/ %U\n" "\n" "%?"))))

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

(my-leader
  "c" '(org-capture :which-key "Capture")
  "a" '(org-agenda :which-key "Agenda"))

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

;; (use-package org-modern
;;     :config
;;     (add-hook 'org-mode-hook #'org-modern-mode)
;;     (add-hook 'org-agenda-finalize #'org-modern-agenda))

(use-package toc-org
  :hook
  (org-mode . toc-org-mode))

(use-package imenu-list
  :init
  (setq imenu-list-position 'left)
  :general
  (my-leader
   "t i" '(imenu-list-smart-toggle :which-key "Imenu")))

(use-package denote
  :straight (denote :type git :host gitlab
                    :repo "protesilaos/denote")
  :custom
  (denote-directory "~/notes")
  (denote-known-keywords
    '("emacs" "personal" "journal")))

(use-package eshell-toggle
  :straight (eshell-toggle :type git :host github
                           :repo "4DA/eshell-toggle")
  :custom
  (eshell-toggle-size-fraction 3)         ; use 30% of the frame (?)
  (eshell-toggle-use-projectile-root t)   ; use projectile root if it exists
  (eshell-toggle-default-directory "~")   ; default to home directory
  :general
  (my-leader
    "o e" '(eshell-toggle :which-key "Open Eshell")))

(use-package eshell-git-prompt
  :straight (eshell-git-prompt :type git :host github
                               :repo "xuchunyang/eshell-git-prompt")
  :config
  (eshell-git-prompt-use-theme 'multiline2))

;; (require 'dash)
;; (require 's)
;; (require 'magit)

;; (defmacro with-face (STR &rest PROPS)
;;   "Return STR propertized with PROPS."
;;   `(propertize ,STR 'face (list ,@PROPS)))

;; (defmacro esh-section (NAME ICON FORM &rest PROPS)
;;   "Build eshell section NAME with ICON prepended to evaled FORM with PROPS."
;;   `(setq ,NAME
;;         (lambda () (when ,FORM
;;                      (-> ,ICON
;;                          (concat esh-section-delim ,FORM)
;;                          (with-face ,@PROPS))))))

;; (defun esh-acc (acc x)
;;   "Accumulator for evaluating andd concatenating esh-sections."
;;   (--if-let (funcall x)
;;       (if (s-blank? acc)
;;           it
;;         (concat acc esh-sep it))
;;     acc))

;; (defun esh-prompt-func ()
;;   "Build `eshell-prompt-function`."
;;   (concat esh-header
;;           (-reduce-from 'esh-acc "" eshell-funcs)
;;           "\n"
;;           eshell-prompt-string))

;; ;; Seperator between esh-sections
;; (setq esh-sep "  ")

;; ;; Seperator between an esh-section icon and form
;; (setq esh-section-delim " ")

;; ;; Eshell prompt header
;; (setq esh-header "\n┌─")

;; ;; Eshell prompt regexp and string
;; (setq eshell-prompt-regexp "└─> ")
;; (setq eshell-prompt-string "└─> ")

;; (esh-section esh-dir
;;              (all-the-icons-faicon "folder-open")
;;              (abbreviate-file-name (eshell/pwd))
;;              '(:foreground "gold" :bold ultra-bold :underline t))

;; (esh-section esh-git
;;              (all-the-icons-all-the-icon "git")
;;              (magit-get-current-branch)
;;              '(:foreground "pink"))

;; (esh-section esh-clock
;;              (all-the-icons-octicon "clock")
;;              (format-time-string "%H:%M" (current-time))
;;              '(:foreground "forest green"))

;; ;; Choose which eshell-funcs to enable
;; (setq eshell-funcs (list esh-dir esh-git esh-clock))

;; ;; Enable the new eshell prompt
;; (setq eshell-prompt-function 'esh-prompt-func)

(use-package format-all)
  ;:hook
  ;(prog-mode . format-all-mode)

(use-package flycheck)

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

(use-package eglot
  :custom
  (eglot-events-buffer-size 0)) ;; Disable the events buffer for performance

(use-package dap-mode
  :config
  (dap-auto-configure-mode))

(use-package realgud)

(use-package tree-sitter
  :config
  (global-tree-sitter-mode)
  :hook
  (tree-sitter-mode . tree-sitter-hl-mode))

(use-package tree-sitter-langs)

(use-package wakatime-mode
  :config
  (global-wakatime-mode))

;; (add-hook 'c-mode-hook 'lsp)
;; (add-hook 'c++-mode-hook 'lsp)
(add-hook 'c-mode-hook 'eglot-ensure)
(add-hook 'c++-mode-hook 'eglot-ensure)

(use-package clojure-mode
  :mode "\\.clj\\'"
  :hook ((clojure-mode . eglot-ensure)
         (clojurescript-mode . eglot-ensure)
         (clojurec-mode . eglot-ensure)))

(use-package cider)

(use-package glsl-mode
  :mode ("\\.glsl\\'" "\\.vert\\'" "\\.frag\\'" "\\.geom\\'"))

;; (use-package company-glsl
;;   :after glsl-mode
;;   :config (add-to-list 'company-backends 'company-glsl))

(use-package go-mode
  :mode "\\.go\\'"
  :hook (go-mode . eglot-ensure))

;; (use-package meghanada
;;   :hook
;;   (java-mode . meghanada-mode)
;;   (java-mode . flycheck-mode))

;; (setq meghanada-java-path "java"
;;       meghanada-maven-path "mvn")

;; (use-package lsp-java
;;   :hook
;;   (java-mode . lsp))

(use-package lsp-pyright)

(use-package python-mode
  :hook (python-mode . (lambda ()
                         (eglot-ensure)
                         (setq tab-width 4)))
  :custom
  (python-shell-interpreter "python3")
  (dap-python-debugger 'debugpy))

(require 'lsp-pyright)
(require 'dap-python)

(use-package typescript-mode
  :mode "\\.ts\\'"
  ;; :hook (typescript-mode . lsp-deferred)
  :config
  (setq typescript-indent-level 4))

(use-package ca65-mode
  :mode "\\.s\\'")

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

(use-package tablist)

(use-package pdf-tools
  :config
  (pdf-loader-install))

(use-package saveplace-pdf-view)

(use-package nov
  :mode "\\.epub\\'")

(use-package smart-comment)

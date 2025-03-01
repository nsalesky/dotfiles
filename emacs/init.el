;;; package.el

;; Enable Melpa
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
(package-refresh-contents)

;;; Set up PATH
(when (memq window-system '(mac ns x))
  (use-package exec-path-from-shell
    :ensure t
    :config
    (exec-path-from-shell-initialize)))

;;; Basic Emacs settings

;; Startup
(use-package emacs
  :ensure nil
  :custom
  (inhibit-startup-screen t))

;; Don't clobber certain directories
(use-package emacs
  :ensure nil
  :custom
  (custom-file (expand-file-name "custom.el" user-emacs-directory))
  :config
  (load-file custom-file))

;; Backup files
(use-package emacs
  :ensure nil
  :custom
  ;; Avoid generating backups or lockfiles
  (create-lockfiles nil)
  (make-backup-files nil)
  ;; Configure backups
  (backup-directory-alist `(("." . ,(expand-file-name "backup" user-emacs-directory))))
  (tramp-backup-directory-alist backup-directory-alist)
  (backup-by-copying t) ; Backup by copying rather than renaming
  (backup-by-copying-when-linked t)
  (delete-old-versions t) ; Delete excess backup versions silently
  (version-control t) ; Use version numbers for backup files
  (kept-new-versions 5)
  (kept-old-versions 5))

;; Disable bell
(use-package emacs
  :ensure nil
  :custom
  (visible-bell t)
  (ring-bell-function 'ignore))

;;; Minibuffer

(use-package emacs
  :ensure nil
  :custom
  (enable-recursive-minibuffers t)
  ;; Hide commands in M-x which do not apply to the current mode
  (read-extended-command-predicate #'command-completion-default-include-p))

(use-package vertico
  :ensure t
  :config
  (vertico-mode))

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

(use-package consult
  :ensure t
  :bind
  ("C-x b" . consult-buffer)
  ("M-y" . consult-yank-pop)
  ("M-s r" . consult-ripgrep)
  ("M-g i" . consult-imenu)
  ("C-x C-r" . consult-recent-file))

(use-package marginalia
  :ensure t
  :config
  (marginalia-mode))

(use-package which-key
  :ensure nil
  :config
  (which-key-mode))

(use-package recentf
  :ensure nil
  :config
  (recentf-mode))

;;; User interface

(use-package emacs
  :ensure nil
  :hook
  ;; Show line numbers
  (prog-mode . display-line-numbers-mode)
  :custom
  (idle-update-delay 1.0) ; Update the UI a bit slower
  (use-short-answers t) ; use "y"/"n" instead of "yes"/"no"
  :config
  ;; Configure fonts
  (set-face-attribute 'default nil :font "JetBrainsMono Nerd Font" :height 160))

(use-package catppuccin-theme
  :ensure t
  :custom
  (catppuccin-flavor 'macchiato)
  :config
  (load-theme 'catppuccin t))

(use-package rainbow-delimiters
  :ensure t
  :hook
  (prog-mode . rainbow-delimiters-mode))

;;; Indent and formatting

(use-package emacs
  :ensure nil
  :custom
  ;; Indentation
  (indent-tabs-mode nil)
  (tab-width 4)
  (require-final-newline t)
  (comment-empty-lines t)
  (tab-stop-list (number-sequence 4 200 4))
  (indent-line-function 'insert-tab)
  ;; Kill ring
  (kill-do-not-save-duplicates t))

;;; Repeat-mode

(use-package emacs
  :ensure nil
  :config
  (repeat-mode))

(defun repeatize (keymap)
  "Add `repeat-mode' support to a KEYMAP."
  (map-keymap
   (lambda (_key cmd)
     (when (symbolp cmd)
       (put cmd 'repeat-map keymap)))
   (symbol-value keymap)))

(use-package smerge-mode
  :ensure nil
  :config
  (repeatize 'smerge-basic-map))

(defvar-keymap flymake-repeat-map
  :repeat t
    "n" #'flymake-goto-next-error
    "p" #'flymake-goto-prev-error
    "M-n" #'flymake-goto-next-error
    "M-p" #'flymake-goto-prev-error)

(use-package flymake
  :ensure nil
  ;; :custom
  ;; (flymake-show-diagnostics-at-end-of-line nil)
  :bind
  (:map flymake-mode-map
   ("M-p" . flymake-goto-prev-error)
   ("M-n" . flymake-goto-next-error)))

(defvar-keymap isearch-repeat-map
  :repeat t
  "s" #'isearch-repeat-forward
  "r" #'isearch-repeat-backward)


;;; Editor

(use-package emacs
  :ensure nil
  :config
  (delete-selection-mode))

(use-package mwim
  :ensure t
  :bind
  ("C-a" . mwim-beginning)
  ("C-e" . mwim-end))

(use-package avy
  :ensure t
  :bind
  ("C-;" . avy-goto-char-timer)
  ("C-:" . avy-goto-char))

;; Completion

(use-package corfu
  :ensure t
  :custom
  (corfu-popupinfo-delay '(1.0 . 0.5))
  :init
  (global-corfu-mode)
  (corfu-popupinfo-mode))

(use-package emacs
  :ensure nil
  :custom
  (tab-always-indent 'complete)
  (text-mode-ispell-word-completion nil))

;;; Snippets

(use-package yasnippet
  :ensure t
  :config
  (yas-global-mode))

;;; Notes
(use-package howm
  :ensure t
  :init
  (setq
   howm-directory "~/Documents/howm"
   ;; howm-file-name-format "%Y-%m-%d-%H%M%S.md"
   howm-view-title-header "#"))

(use-package markdown-mode
  :ensure t
  :mode ("README\\.md\\'" . gfm-mode))
  

;;; Version control

(use-package magit
  :ensure t)

;; Programming

(use-package eglot
  :ensure nil
  :config
  (add-to-list 'eglot-server-programs
               '((python-mode python-ts-mode) . ("basedpyright-langserver" "--stdio"))))

;;; Python

(use-package python-mode
  :ensure nil
  :hook (python-mode . eglot))

;;; C
(use-package emacs
  :ensure nil
  :custom
  (c-default-style "linux")
  (c-basic-offset 4))

;; Terminal emulator

(use-package eat
  :ensure t)

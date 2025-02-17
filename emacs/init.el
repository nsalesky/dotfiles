
;;; package.el

(package-initialize)
(package-refresh-contents)


;;; Basic Emacs settings

;; Don't clobber certain directories
(use-package emacs
  :ensure nil
  :custom
  (custom-file (expand-file-name "custom.el" user-emacs-directory))
  :config
  (load-file custom-file))

;; Backup files
(use-package emacs
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
  ("M-g i" . consult-imenu))

(use-package marginalia
  :ensure t
  :config
  (marginalia-mode))

;;; User interface

(use-package emacs
  :ensure nil
  :hook
  (prog-mode . display-line-numbers-mode)
  :custom
  (idle-update-delay 1.0) ; Update the UI a bit slower
  (use-short-answers t)) ; use "y"/"n" instead of "yes"/"no"
  

; Show line numbers
(add-hook 'prog-mode-hook #'display-line-numbers-mode)

(load-theme 'modus-vivendi)

;;; Indent and formatting

(use-package emacs
  :ensure nil
  :custom
  ;; Indentation
  (indent-tabs-mode nil)
  (tab-width 4)
  (require-final-newline t)
  (comment-empty-lines t)
  ;; Kill ring
  (kill-do-not-save-duplicates t))

;;; Editor

;; Completion

(use-package corfu
  :ensure t
  :custom
  (corfu-popupinfo-delay (1.0 . 0.5))
  :init
  (global-corfu-mode)
  (corfu-popupinfo-mode))

(use-package emacs
  :ensure nil
  :custom
  (tab-always-indent 'complete)
  (text-mode-ispell-word-completion nil)
  

;;; Version control

(use-package magit
  :ensure t)

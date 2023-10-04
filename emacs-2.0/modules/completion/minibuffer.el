;; Vertico - vertical completions with extra info
(use-package vertico
  :init
  (vertico-mode))

;; Orderless - match different parts of completion candidates in any order
(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-cat(completion-category-overrides '(
                                   (file (styles basic partial-completion))
                                   (eglot (styles orderless))))))

;; Marginalia - decorate minibuffer candidates with useful metadata
(use-package marginalia
  ;; :bind
  ;; (:map minibuffer-local-map
  ;; 	("M-A" . marginalia-cycle))
  :custom
  (marginalia-align 'left)
  :init
  (marginalia-mode)
  :config
  ;; Fix marginalia annotations for Projectile
  (setq marginalia-command-categories
        (append '((projectile-find-file . project-file)
                  (projectile-find-dir . project-file)
                  (projectile-switch-project . file))
                marginalia-command-categories)))

;; All-the-icons-completion - add icons to completion candidates
(use-package all-the-icons-completion
  :after (marginalia all-the-icons)
  :hook (marginalia-mode . all-the-icons-completion-marginalia-setup)
  :init
  (all-the-icons-completion-mode))

;; Embark
(use-package embark
  :bind
  (("C-." . embark-act)
   ("M-." . embark-dwim)))

;; Consult - add helpful functions with completion
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

  :init
  (setq consult-narrow-key (kbd "<"))

  ;; Projectile
  ;; (autoload 'projectile-project-root "projectile")
  ;; (setq consult-project-function (lambda (_) (projectile-project-root)))

  ;; Project.el
  (setq consult-project-function #'consult--default-project-function))

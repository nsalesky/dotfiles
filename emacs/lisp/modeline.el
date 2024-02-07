(defun ns-mode-line-fill (reserve)
  "Return empty space leaving RESERVE space on the right."
  (unless reserve
    (setq reserve 20))
  (when (and window-system (eq 'right (get-scroll-bar-mode)))
    (setq reserve (- reserve 3)))
  (propertize " "
              'display `((space :align-to (- (+ right right-fringe right-margin) ,reserve)))))

(defun ns-mode-line-reserve-right ()
  (+ 1 (length (format-mode-line ns/mode-line-right-format))))




(defvar nick-modeline/default-mode-line mode-line-format)








;; Taken from doom-modeline
(defun ns/modeline-eldoc-minibuffer-message (format-string &rest args)
  (setq eldoc-mode-line-string
        (when (stringp format-string)
          (apply #'format-message format-string args)))
  (force-mode-line-update))

(setq eldoc-message-function #'ns/modeline-eldoc-minibuffer-message)








(defvar ns/mode-line-left-format
  '((eldoc-mode-line-string (" " eldoc-mode-line-string " ")))
  "Elements to display in the modeline on the left.")

(defvar ns/mode-line-right-format
  '(""
    mode-line-misc-info
    " %l : %C" ;; line : column
    )
  "Elements to display in the modeline on the right.")

(defvar nick-modeline-format
        (list
       ns/mode-line-left-format
       '(:eval (ns-mode-line-fill
                (ns-mode-line-reserve-right)))
       ns/mode-line-right-format))


(define-minor-mode nick-modeline-mode
  "Toggle `nick-modeline' on or off."
  :lighter nil
  :global t
  (if nick-modeline-mode
      (setq-default mode-line-format nick-modeline-format)
    (setq-default mode-line-format nick-modeline/default-mode-line)))

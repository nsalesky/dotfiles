;; Set up straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))
(setq straight-use-package-by-default t)


(setq user-full-name "Nicholas Salesky"
      user-mail-address "nicksalesky@gmail.com")


;; Add the core directory to the load-path for use with `require'.
(add-to-list 'load-path
	     (expand-file-name "core"
			       user-emacs-directory))

(require 'ns-start)

(nick! :completion
       minibuffer

       :ui
       themes
       modeline

       :tools
       magit)

(nick-load-modules)

;;; ns-modules.el --- module & package management system  -*- lexical-binding: t; -*-

;;; Inspiration and examples taken from Doom Emacs.

;; Author: Nicholas Salesky <nicksalesky@gmail.com>
;; Keywords:

(defvar nick-modules (make-hash-table :test 'equal)
  "A hash table of the enabled modules with metadata that will be initialized.")

(defvar nick--module-dirs (list (expand-file-name "modules" user-emacs-directory))
  "List of directories to be searched for modules in order.")

(defun ->string (value)
  "Converts the given VALUE, that might be a keyword, symbol, or string, to a string."
  (cond ((stringp value)
	 value)
	((keywordp value)
	 (substring (symbol-name value)
		    1))
	((symbolp value)
	 (symbol-name value))))

(defun nick-module-locate-path (category module)
  "Locates the file path for the MODULE falling under the CATEGORY or returns
  nil if it cannot be found. MODULE is expected to refer to an ELisp file, but
  it should not end in .el"
  (let ((category (->string category))
	(module (->string module)))
    (cl-loop for module-dir in nick--module-dirs
	     if (file-exists-p
		 (file-name-concat module-dir
				   category
				   (concat module
					   ".el")))
	     return
	     (file-name-concat module-dir
			       category
			       module))))

(defun nick-module-mplist-map (fn mplist)
  "Apply FN to each module in MPLIST along with its category."
  (let ((mplist (copy-sequence mplist))
	results
	category
	m)
    (while mplist
      (setq m (pop mplist))
      (cond ((keywordp m)
	     (setq category m))
	    ((null category)
	     (error "No module category specified for %s" m))
	    (t
	      (push (funcall fn category m)
		    results))))
    results))

(defun nick-enable-module (category module &rest plist)
  "Enables a module by adding it to `nick-modules'.

CATEGORY is a keyword, MODULE is a symbol, PLIST is a plist that accepts the
following properties:

  :path STRING
    Path to the file where this module lives."
  (let ((plist (copy-sequence plist)))
    (plist-put plist :idx (hash-table-count nick-modules))
    (puthash (cons category module) plist nick-modules)))

(defmacro nick! (&rest modules)
  "Bootstrap enabled MODULES into `nick-modules'."
  `(progn
     (nick-module-mplist-map
      (lambda (category module &rest plist)
	(let ((path (nick-module-locate-path category module)))
	  (if path
	      (nick-enable-module category module
		     :path path)
	    (warn "Failed to locate a module for '%s %s'" category module))))
      ,@(if (keywordp (car modules))
	    (list (list 'quote modules))
	  modules))
     nick-modules))

(defun nick-load-modules ()
  "Load the modules previously configured and enabled with `nick!'."
  (let ((module-plists (make-vector
			(hash-table-count nick-modules)
			    nil)))
    (maphash
     (lambda (key plist)
       (aset module-plists
	     (plist-get plist :idx)
	     plist))
     nick-modules)
    (seq-do
     (lambda (module-plist)
       (load (plist-get module-plist :path)))
     module-plists)))

(provide 'ns-modules)
;;; ns-modules.el ends here

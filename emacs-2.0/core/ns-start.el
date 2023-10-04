;;; ns-start.el --- bootstraps interactive sessions  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;;
;;; Set up sensible defaults


;; Disable certain native-comp byte compiler warnings to cut down on the noise for
;; usually harmless errors
(setq byte-compile-warnings '(not free-vars unresolved noruntime lexical make-local)
      native-comp-async-report-warnings-errors nil)


;; `gcmh' attempts to schedule GC collections when Emacs is idle to make it
;; more responsive while in use.
;; I got these settings from doom-emacs
(use-package gcmh
  :init
  (setq gcmh-idle-delay 'auto                      ; default is 15s
	gcmh-auto-idle-delay-factor 10
	gcmh-high-cons-threshold (* 16 1024 1024)) ; 16mb
  (gcmh-mode))

;;
;;; Load defaults and DSL

(setq use-short-answers t)       ; Use short answers ("y" or "n") for prompts
(setq confirm-nonexistent-file-or-buffer nil) ; Don't confirm when creating a new file


(require 'ns-modules)


(provide 'ns-start)
;;; ns-start.el ends here

;; Disable package.el loading any packages before init.el
(setq package-enable-at-startup nil)

;; Disable some UI elements before they get rendered
;; I took the first three from Doom Emacs. It's a faster way to disable them this early in the
;; startup process.
(push '(menu-bar-lines . 0)   default-frame-alist)        ; Disable the menu bar
(push '(tool-bar-lines . 0)   default-frame-alist)        ; Disable the tool bar
(push '(vertical-scroll-bars) default-frame-alist)        ; Disable the scrollbar
(setq menu-bar-mode nil
      tool-bar-mode nil
      scroll-bar-mode nil)
(tooltip-mode -1)                                         ; Disable the tooltip
(setq ring-bell-function 'ignore)                         ; Disable alarms
(set-fringe-mode 10)             ; Add a fringe around the screen to make it more readable
(setq inhibit-startup-message t) ; Disable the startup message

;; Store customization options in a separate file from init.el
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

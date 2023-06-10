;;; crates.el --- View latest Rust crate versions from Cargo.toml and automatically upgrade dependencies  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Nicholas Salesky

;; Author: Nicholas Salesky <nicksalesky@gmail.com>
;; Package-Requires: ((emacs "27.1") (posframe "1.1.7"))
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary: This package is heavily based on the great blamer.el

;;

;;; Code:

(require 'posframe)

(defvar crates--overlays '()
  "Current active overlays for Crate version messages.")


(defface crates-pretty-meta-data-face
  `((t :inherit font-lock-variable-name-face
       :italic nil
       :height ,(face-attribute 'default :height)
       :font ,(face-attribute 'default :font)))
  "Face for pretty meta information."
  :group 'crates)


(defun crates--prettify-meta-data (meta-data)
  "Apply font face for META-DATA of pretty popup."
  (propertize meta-data 'face 'crates-pretty-meta-data-face))


(defun crates--format-pretty-tooltip (msgs)
  "Draw a pretty tooltip from MSGS alist with respecting max possible length. Return cons of result and count of lines."
  (cl-first msgs)) ;; TODO


(defun crates--create-popup-text-content (crate-info)
  "Create the content of the popup from CRATE-INFO.
Return list of strings."
  (let* ((latest-version (plist-get crate-info :latest-version))
         (msg-list `(,(concat "Version"
                              (crates--prettify-meta-data latest-version)))))
    msg-list))




(defun crates--render-overlay-popup (crate-info)
  "Render CRATE-INFO as pretty overlay with border."
  (let* ((msg-list (crates--create-popup-text-content crate-info))
         (msg (crates--format-pretty-tooltip msg-list))
         (beg (save-excursion (beginning-of-line) (point)))
         (ov (save-excursion (move-end-of-line nil)
                             (make-overlay beg (point) nil t t))))
    (overlay-put ov 'after-string msg)
    (overlay-put ov 'cursor-intangible t)
    (overlay-put ov 'window (get-buffer-window))
    (add-to-list 'crates--overlays ov)))


(defun crates--build-pretty-content (crate-info)
  "Build pretty content inside separated buffer from CRATE-INFO."
  (let* ))


(defun crates--clear-overlay ()
  "Clear last overlay."
  (dolist (ov crates--overlays)
    (delete-overlay ov))
  (setq crates--overlays '()))


(provide 'crates)
;;; crates.el ends here

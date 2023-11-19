;;; harpoon.el --- Quickly navigate between main files  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Nick Salesky

;; Author: Nick Salesky <nicksalesky@gmail.com>
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

;;; Commentary:

;;

;;; Code:

(defvar harpoon-persist-filename
  (expand-file-name "harpoon.json" user-emacs-directory)
  "The location where Harpoon data will be persisted")

(defvar harpoon--projects-plist
  '()
  "The list of files currently being tracked per project.")

(defvar harpoon-project-root-func
  #'harpoon-project-root-project
  "The function used to determine the project root")

(defvar-local harpoon--cached-point
    nil
  "The previously-saved position of point for the given buffer.
If this buffer's corresponding filename is not being tracked in
`harpoon--projects-plist', then this will always be nil.")

(defun harpoon-project-root-project ()
    "Determine the root directory of the current project using project.el"
  (project-root (project-current t)))

;; TODO:
(defun harpoon-project-root-projectile ()
    "Determine the root directory of the current project using projectile"
  nil)

(defun harpoon--load-from-disk ()
  "Loads the contents of `harpoon-persist-filename' to `harpoon--projects-plist'."
    nil)

(defun harpoon--save-to-disk ()
  "Saves the contents of `harpoon--projects-plist' to the location
defined by `harpoon-persist-filename'."
    (let ((json-projects-str (json-encode-plist harpoon--projects-plist)))
      (with-temp-buffer
        (insert json-projects-str)
        (write-file harpoon-persist-filename))))

(defun harpoon--string-member (elt list)
  (if (not list)
      nil
    (or (string-equal elt
                      (car (car list))) ; access string key in alist
        (harpoon--string-member elt (cdr list)))))

(defun harpoon--hook-file-existing-project (filename current-project-files)
  (if (harpoon--string-member filename current-project-files)
      current-project-files
    (append current-project-files
            `((,filename . ,(point))))))

(defun harpoon--hook-file-new-project (filename)
  `((,filename . ,(point))))

(defun harpoon-hook-file ()
  "Adds the file visited by the current buffer to its respective project in
`harpoon--projects-plist'."
  (interactive)
  (let* ((project-root (funcall harpoon-project-root-func))
         (current-filename (buffer-file-name))
         (project-files (plist-get harpoon--projects-plist project-root #'string-equal)))
    (when current-filename
      (setq harpoon--projects-plist
            (plist-put harpoon--projects-plist
                       project-root
                       (if project-files
                           (harpoon--hook-file-existing-project current-filename
                                                                project-files)
                         (harpoon--hook-file-new-project current-filename))
                       #'string-equal)))))

(defun harpoon-visit-entry (index)
  (let* ((project-root (funcall harpoon-project-root-func))
         (project-files (plist-get harpoon--projects-plist
                                   project-root
                                   #'string-equal))
         (file-info (nth index project-files)))
    (when file-info
      (let ((filename (car file-info))
            (point-pos (cdr file-info)))
        (find-file filename)
        (goto-char poient-pos)))))

(defun harpoon--update-file-pos (project-files filename point-pos)
  (if (not project-files)
      nil
    (if (string-equal filename
                      (car (car project-files)))
        (cons `(,filename . ,point-pos)
              (cdr project-files))
      (cons (car project-files)
            (harpoon--update-file-pos (cdr project-files)
                                      filename
                                      point-pos)))))

(defun harpoon-update-tracking ()
  "Compares the value of point to the value of `harpoon--cached-point'.
If the values differ, then `harpoon--cached-point' and
`harpoon--projects-plist' will be updated with the new
value of point."
  (unless (equal (point) harpoon--cached-point)
    (setq harpoon--cached-point (point))
    (let* ((current-filename (buffer-file-name))
           (project-root (funcall harpoon-project-root-func))
           (project-files (plist-get harpoon--projects-plist
                                     project-root
                                     #'string-equal)))
      (setq harpoon--projects-plist
            (plist-put harpoon--projects-plist
                       project-root
                       (harpoon--update-file-pos project-files
                                                 current-filename
                                                 harpoon--cached-point)
                       #'string-equal)))))

(define-minor-mode harpoon-tracking-mode
  "Toggle Harpoon tracking mode.
When Harpoon tracking mode is enabled, the position of point is tracked
for files added to the harpoon list and can be interactively
restored."
  :global t
  :lighter " Harpoon"
  (if harpoon-tracking-mode
      (add-hook 'post-command-hook
                #'harpoon-update-tracking
                nil
                t)
    (remove-hook 'post-command-hook
                 #'harpoon-update-tracking
                 t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TODO: testing

(json-encode-plist harpoon--projects-plist)

(harpoon--save-to-disk)

(harpoon-visit-entry 0)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'harpoon)
;;; harpoon.el ends here

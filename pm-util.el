;;; pm-util.el --- utility functions  -*- lexical-binding: t; -*-

(require 'subr-x)
(require 'dash)
(require 's)
(require 'f)
(require 'org)

(defun pm-wsl-browse-url (url &rest args)
  (let ((match (s-match "file:///mnt/\\([a-zA-Z]\\)\\(/.*\\)" url)))
    (setq url (concat "file:///" (nth 1 match) ":" (nth 2 match))))
  (shell-command (concat (executable-find "wslview") " " (pm-path url))))

(defun pm-linux-path (path)
  (and path
       (let ((match (s-match "^\\([a-zA-Z]\\):\\(/\\|\\\\\\)\\(.*\\)" path)))
         (if match
             (concat "/mnt/" (downcase (nth 1 match)) "/" (s-replace "\\" "/" (nth 3 match)))
           path))))

(defun pm-path (path)
  "Make path compatible with environment."
  (cond ((eq system-type 'gnu/linux)
         (pm-linux-path path))
        (t path)))

(defun pm-delete-line ()
  (delete-region
   (point)
   (save-excursion (move-end-of-line 1) (point)))
  (delete-char 1))

(defun pm--collect-ancestors-tags (el)
  (let ((tags (org-element-property :tags el))
        (parent (org-element-property :parent el)))
    (when parent
      (setq tags (-union tags (pm--collect-ancestors-tags parent))))
    tags))

(provide 'pm-util)

;;; pm-agenda-files-loader.el --- functions definitions for async loading  -*- lexical-binding: t; -*-

(require 'pm-util)

(defun pm--load-agenda-files-from-file (file &optional referrer)
  "Load agenda files recursively from a root org file."
  (lwarn 'PM :info "Adding agenda file %s referred in %s." file referrer)
  (unless (-contains? org-agenda-files file)
    (if (not (f-exists? file))
        (lwarn 'PM :warn "Missing agenda file %s refered from %s." file referrer)
      (with-current-buffer (find-file-noselect file)
        (org-with-wide-buffer
         (setq org-agenda-files (nconc org-agenda-files (list file)))
         (org-element-map (org-element-parse-buffer) 'link
           (lambda (el)
             (let ((type (org-element-property :type el)))
               (when (or (not type)
                         (s-equals? type "file")
                         (s-equals? type "")
                         (s-equals? type "pj"))
                 (let ((tags (pm--collect-ancestors-tags el))
                       (path (pm-path (org-element-property :path el))))
                   (when (not (-contains? tags org-archive-tag))
                     (cond
                      ((s-equals? type "pj")
                       (condition-case err
                           (pm--load-agenda-files-from-file
                            (pm-project-file path)
                            file)
                         (user-error
                          (lwarn 'PM :warn "Error collecting agenda files from %s.\n%s" file (error-message-string err)))))
                      ((s-suffix? ".org" path)
                       (when (-contains? tags "_notes")
                         (setq org-default-notes-file (f-expand path (f-dirname (buffer-file-name)))))
                       (pm--load-agenda-files-from-file (f-expand path (f-dirname (buffer-file-name))) file))
                      ((f-directory? path)
                       (--map (pm--load-agenda-files-from-file it referrer) (f-files path (lambda (f) (s-suffix? ".org" f)) t)))))))))))))))

(provide 'pm-agenda-files-loader)

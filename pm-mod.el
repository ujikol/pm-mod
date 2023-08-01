;;; pm-mod.el --- org-mode for project manager and business users  -*- lexical-binding: t; -*-

(require 'dash)
(require 's)
(require 'f)
(require 'org)
(require 'org-protocol)

(defvar pm-mnemonic-key-bindings t
  "Whether to define mnemonic key bindings.")

(defvar pm-agenda-files-root "~/agenda_files.org"
  "Path of file to be parsed recursively for file links to org-agenda-files.
If nil org-agenda-files are handled the normal org-way.")

(defvar user-match-code nil
  "Match code (acronym) of user.")

;;; Basic settings

(defun pm-apply-basics ()
  "Apply basic seeting to extend org-mode for project managers and business users."

;;;; Basic basics

  
  (setq user-match-code (or user-match-code (getenv "match_code") user-login-name))
  
  (with-eval-after-load 'org
    (when pm-agenda-files-root
      (setq org-agenda-files nil))
    (setq org-archive-tag "_archive")
    (setq org-element-archive-tag "_archive")

;;;; Editor

    (when pm-mnemonic-key-bindings
      (setq org-replace-disputed-keys t)

      (cua-mode t)
      (delete-selection-mode 1)
      (setq cua-auto-tabify-rectangles nil) ;; Don't tabify after rectangle commands
      (transient-mark-mode 1) ;; No region when it is not highlighted
      (setq cua-keep-region-after-copy t) ;; Standard Windows behaviour
      (define-key cua-global-keymap (kbd "C-S-<SPC>") nil)
      (define-key cua-global-keymap (kbd "<C-return>") nil)
      (define-key cua-global-keymap (kbd "M-y") nil)
      (define-key cua--cua-keys-keymap (kbd "M-v") nil)
      (define-key cua--rectangle-keymap (kbd "M-f") 'query-replace)
      (define-key cua--rectangle-keymap (kbd "C-f") 'cua-fill-char-rectangle)
      (define-key cua--region-keymap (kbd "<C-return>") nil)
      (define-key cua-global-keymap (kbd "<C-return>") nil)
      (define-key org-mode-map (kbd "<C-return>") 'org-ctrl-c-ctrl-c)

      (global-unset-key (kbd "C-y"))
      (global-unset-key (kbd "C-x C-q"))
      (define-key org-mode-map (kbd "C-i") nil)
      (define-key org-mode-map (kbd "C-j") nil)
      (define-key org-mode-map (kbd "C-y") nil)
      (define-key org-mode-map (kbd "C-'") nil)
      (define-key org-mode-map (kbd "C-,") nil)

      (define-key org-mode-map (kbd "<S-M-return>") 'org-insert-heading-respect-content) ;; force new heading in e.g. list 
      (define-key org-mode-map (kbd "M-c") 'org-copy-special )
      (define-key org-mode-map (kbd "M-x") 'org-cut-special  )
      (define-key org-mode-map (kbd "M-v") 'org-paste-special)

      (global-set-key (kbd "C-a") 'mark-whole-buffer)
      (global-set-key (kbd "M-a") 'org-mark-element)
      (global-set-key (kbd "C-M-a") 'org-mark-subtree)
      (global-set-key (kbd "M-<SPC>") 'cua-set-rectangle-mark)
      (global-set-key (kbd "<S-M-SPC>") 'cua-set-mark)
      (require 'expand-region)
      (global-set-key (kbd "<C-SPC>") 'er/expand-region)
      (global-set-key (kbd "<S-C-SPC>") 'er/contract-region)

      (define-key org-mode-map (kbd "C-_") 'org-table-insert-hline)
      (define-key org-mode-map (kbd "M-|") 'org-table-create-or-convert-from-region)
      (define-key org-mode-map (kbd "C-#") 'org-edit-special)
      (define-key org-mode-map (kbd "M-#") 'org-table-toggle-coordinate-overlays)
      (define-key org-table-fedit-map (kbd "<C-return>") 'org-table-fedit-finish)
      (define-key org-table-fedit-map (kbd "C-q") 'org-table-fedit-abort)

      (define-key org-src-mode-map (kbd "<C-return>") 'org-edit-src-exit)
      (define-key org-src-mode-map (kbd "C-q") 'org-edit-src-abort)
      (define-key org-src-mode-map (kbd "<C-tab>") 'completion-at-point)

      (define-key org-mode-map (kbd "M-<") 'org-toggle-narrow-to-subtree)
      (define-key org-mode-map (kbd "C-<") 'pm-focus)
      (define-key org-mode-map (kbd "C-/") 'org-sparse-tree)

      (global-set-key (kbd "C-+") (lambda () (interactive) (text-scale-adjust 1)))
      (global-set-key (kbd "C--") (lambda () (interactive) (text-scale-adjust -1)))
      )
;;;;; Cursor

    (when pm-mnemonic-key-bindings
      (define-key org-mode-map (kbd "<home>") 'org-beginning-of-line)
      (define-key org-mode-map (kbd "<end>") 'org-end-of-line)
      (global-set-key (kbd "<C-prior>") 'outline-previous-visible-heading)
      (global-set-key (kbd "<C-next>") 'outline-next-visible-heading)
      (define-key org-mode-map (kbd "<C-backspace>") 'org-up-element)
      (global-set-key (kbd "C-.") 'recenter-top-bottom)
      (define-key org-mode-map (kbd "C-b") 'org-mark-ring-goto)
      (define-key org-mode-map (kbd "M-b") 'org-mark-ring-push))

;;;;; Clipboard

    (when pm-mnemonic-key-bindings
      (require 'browse-kill-ring)
      (global-set-key (kbd "S-C-v") 'browse-kill-ring))

;;;;; Undo

    (when pm-mnemonic-key-bindings
      (require 'undo-fu)
      (global-set-key (kbd "C-z") 'undo-fu-only-undo)
      (global-set-key (kbd "M-z") 'undo-fu-only-redo))
      ;;(global-set-key (kbd "S-C-z") 'undo-tree-visualize)

;;;;; Windows

    (when pm-mnemonic-key-bindings
      (global-set-key (kbd "<C-M-left>") 'windmove-left)
      (global-set-key (kbd "<C-M-right>") 'windmove-right)
      (global-set-key (kbd "<C-M-up>") 'windmove-up)
      (global-set-key (kbd "<C-M-down>") 'windmove-down)
      (global-unset-key (kbd "C-w"))
      (global-set-key (kbd "C-w 2") 'split-window-below)
      (global-set-key (kbd "C-w 3") 'split-window-right)
      (global-set-key (kbd "C-w 0") 'delete-window)
      (global-set-key (kbd "M-q") 'delete-window)
      (global-set-key (kbd "<C-f4>") 'delete-window)
      (global-set-key (kbd "C-w 1") 'delete-other-windows)
      (require 'buffer-move)
      (global-set-key (kbd "C-w m") 'buf-move)
      (define-key org-mode-map (kbd "M-w") 'visual-line-mode))

;;;;; Sessions
    
    (when pm-mnemonic-key-bindings
      (global-set-key (kbd "C-M-r") 'recover-session))

;;;;; DirEd
    
    (when pm-mnemonic-key-bindings
      (with-eval-after-load 'dired
        (define-key dired-mode-map (kbd "<C-return>") 'browse-url-of-dired-file)))

;;;;; Git

    (when pm-mnemonic-key-bindings
      (global-set-key (kbd "C-M-g") 'magit-status)
      (with-eval-after-load "magit"
        (define-key magit-mode-map (kbd "p") 'magit-push)
        (define-key with-editor-mode-map (kbd "<C-return>") 'with-editor-finish)
        (define-key with-editor-mode-map (kbd "C-q") 'with-editor-cancel)))

;;;;; Buffers and files

    (when pm-mnemonic-key-bindings
      (require 'ibuf-ext)
      (global-set-key (kbd "C-M-b") 'ibuffer)
      (global-set-key (kbd "<f12>") (lambda () (interactive) (switch-to-buffer "*scratch*")))
      (global-set-key (kbd "<C-M-next>") 'xah-next-user-buffer)
      (global-set-key (kbd "<C-M-prior>") 'xah-previous-user-buffer)
      (global-set-key (kbd "<S-C-M-next>") 'xah-next-emacs-buffer)
      (global-set-key (kbd "<S-C-M-prior>") 'xah-previous-emacs-buffer)
      ;; does not work; changing key map for mode line is not allowed
      ;;(define-key mode-line-map (kbd "<wheel-down>") 'xah-next-user-buffer)
      ;;(define-key (mode-line-map "<wheel-up>") 'xah-previous-user-buffer)

      (global-set-key (kbd "C-M-f") (lambda () (interactive) (dired (or (and (buffer-file-name) (f-dirname (buffer-file-name))) (f-expand "~")))))
      (global-set-key (kbd "S-C-M-f") 'my-dired-recent-dirs)
      (global-set-key (kbd "C-o") 'counsel-find-file)
      (global-set-key (kbd "S-C-o") 'find-file-other-window)
      (global-set-key (kbd "M-o") 'pm-open-project)
      (global-set-key (kbd "C-q") 'kill-this-buffer)
      (global-set-key (kbd "C-M-q") 'save-buffers-kill-terminal)
      (global-set-key (kbd "<M-f4>") 'save-buffers-kill-terminal)
      (global-set-key (kbd "C-s") 'save-buffer)
      (global-set-key (kbd "M-s") 'save-some-buffers)
      (global-set-key (kbd "C-j") 'jump-to-register))

;;;;; Search replace

    (when pm-mnemonic-key-bindings
      (global-set-key (kbd "C-f") 'swiper-isearch)
      (global-set-key (kbd "M-f") 'query-replace)
      (global-set-key (kbd "M-F") 'query-replace-regexp))

;;;;; Spell checking
    
    (when pm-mnemonic-key-bindings
      (with-eval-after-load "flyspell"
        (define-key flyspell-mode-map (kbd "C-.") nil)
        (define-key flyspell-mode-map (kbd "C-,") nil)
        (define-key flyspell-mode-map (kbd "C-;") nil)
        (define-key flyspell-mode-map (kbd "C-M-i") nil))
      (global-set-key (kbd "M-?") 'ispell)
      (global-set-key (kbd "C-?") 'ispell-word))

;;;; Files hierarchy

    (advice-add 'org-entry-get :around #'org-entry-get-recursively-advice)
    ;;(advice-remove 'org-entry-get #'org-entry-get-recursively-advice)

;;;; Tasks

    (setq org-todo-keywords
          '((sequence "TODO(t)" "WAITING(w)" "QUESTION(q)" "JOB(j)" "|" "CANCELED(c)" "DONE(d)")
            (sequence "ISSUE(I)" "RISK(R)" "OPPORTUNITY(O)" "MILESTONE(M)" "|" "CLOSED(C)" "ACHIVEMENT(A)" "DECISION(D)")
            (sequence "RED(r)" "AMBER(a)" "GREEN(g)" "|" "FINISHED(f)")))

    (setq org-hierarchical-todo-statistics nil)
    (setq org-provide-todo-statistics '(("JOB" "TODO" "WAITING") ("DONE")))

    (require 'org-inlinetask)
    (setq org-inlinetask-default-state "TODO")
    (setq org-inlinetask-show-first-star t)
    (setq org-default-properties (-concat org-default-properties '("ACTION_ID_LEN")))

    (advice-add 'org-store-link :around #'pm--inlinetask-link-patch-advice)
    ;;(advice-add 'org-export-resolve-id-link :around #'pm--inlinetask-link-patch-advice) ; does not help
    ;;(advice-remove 'org-export-id-fuzzy-link #'pm--inlinetask-link-patch-advice)
    ;;(advice-add 'org-export-as :around #'pm--inlinetask-link-patch-advice)
    ;;(advice-add 'org-html-format-headline-default-function :around #'pm--inlinetask-link-patch-advice)

    (when pm-mnemonic-key-bindings
      (define-key org-mode-map (kbd "C-t c") 'org-todo)
      (define-key org-mode-map (kbd "C-t s") 'org-schedule)
      (define-key org-mode-map (kbd "C-t S") 'pm-schedule-remove)
      (define-key org-mode-map (kbd "C-t d") 'org-deadline)
      (define-key org-mode-map (kbd "C-t D") 'pm-deadline-remove)
      (define-key org-mode-map (kbd "C-t p") 'org-priority)
      (define-key org-mode-map (kbd "C-t t") 'pm-set-tags)
      (define-key org-mode-map (kbd "C-t i") 'pm-convert-inlinetask))

;;;; Properties

    (when pm-mnemonic-key-bindings
      (define-key org-mode-map (kbd "C-p") 'org-set-property)
      (define-key org-mode-map (kbd "M-p") 'org-columns)
      (define-key org-mode-map (kbd "S-C-p") 'org-property-action))

;;;; Dates

    (when pm-mnemonic-key-bindings
      (define-key org-mode-map (kbd "M-d") 'org-time-stamp)
      (define-key org-mode-map (kbd "C-d") 'org-time-stamp-inactive)
      (advice-add 'org-read-date :before #'pm-read-date-advice))

;;;; Capture

    (when pm-mnemonic-key-bindings
      (global-set-key (kbd "C-n") 'pm-capture)
      (with-eval-after-load "org-capture"
        (define-key org-capture-mode-map (kbd "C-c C-c") nil)
        (define-key org-capture-mode-map (kbd "<C-return>") 'org-capture-finalize)
        (define-key org-capture-mode-map (kbd "C-q") 'org-capture-kill)
        (define-key org-capture-mode-map (kbd "C-r") 'org-capture-refile)))

;;;; Refile & goto

    (setq org-goto-interface 'outline-path-completion)
    (setq org-refile-use-outline-path 'file)
    (setq org-outline-path-complete-in-steps nil)

    (when pm-mnemonic-key-bindings
      (global-set-key (kbd "C-r") 'pm-refile)
      (global-unset-key (kbd "M-r"))
      (global-set-key (kbd "C-g") 'pm-goto)
      (global-set-key (kbd "M-g") 'goto-line))
    
;;;; Agenda
;;;;; Agenda files

    (when pm-agenda-files-root
      ;; prevents unwanted changes in custom-file e.g. org-agenda-files (not always?)
      ;;(debug-on-variable-change 'org-agenda-files)
      (define-key org-mode-map (kbd "C-c [") nil))

;;;;; Agenda definitions

    (add-hook 'org-agenda-mode-hook (lambda () (hl-line-mode 1)))
    (add-hook 'org-agenda-finalize-hook (lambda () (remove-text-properties (point-min) (point-max) '(mouse-face t))))
    (add-hook 'org-finalize-agenda-hook '(set-face-attribute 'foo nil :weight 'bold :slant 'italic))
    (setq org-agenda-format-date 'my-org-agenda-format-date-aligned)
    (setq org-agenda-start-on-weekday 1)
    (setq calendar-week-start-day 1)

    (setq org-agenda-custom-commands
          '(("k" agenda "Project schedules" ((org-agenda-span 'month)
                                             (org-agenda-entry-types '(:timestamp :sexp))
                                             ;;    (org-agenda-time-grid nil)
                                             (org-agenda-show-all-dates nil)))
            ("c" "Current" ((todo "JOB" (
                                         (org-agenda-overriding-header "My Jobs")
                                         (org-agenda-skip-function
                                          '(or (pm-agenda-skip-if-later "SCHEDULED")
                                               (pm-skip-if-delegated)))
                                         (org-agenda-prefix-format "%9(pm-agenda-entry-get-category) %6(pm-agenda-entry-get-deadline) ")))
                            (todo "TODO|QUESTION" (
                                                   (org-agenda-overriding-header "My Tasks")
                                                   (org-agenda-skip-function
                                                    '(or (pm-agenda-skip-if-later "SCHEDULED")
                                                         (pm-skip-if-delegated)))
                                                   (org-agenda-prefix-format "%9(pm-agenda-entry-get-category) %6(pm-agenda-entry-get-deadline) ")))
                            (todo "JOB|TODO|QUESTION" (
                                                       (org-agenda-overriding-header "Delegated/Assigned")
                                                       (org-agenda-skip-function
                                                        '(or (pm-agenda-skip-if-later "SCHEDULED")
                                                             (pm-skip-if-delegated t)))
                                                       (org-agenda-prefix-format "%9(pm-agenda-entry-get-category) %6(pm-agenda-entry-get-deadline) ")))
                            (todo "OPPORTUNITY|ISSUE|RISK" (
                                                            (org-agenda-overriding-header "Opportunities / Issues / Risks")
                                                            (org-agenda-skip-function
                                                             '(pm-agenda-skip-if-later "SCHEDULED"))
                                                            (org-agenda-prefix-format "%9(pm-agenda-entry-get-category) %6(pm-agenda-entry-get-deadline) ")))))
            ("p" "Plan tomorrow" todo "JOB|TODO|QUESTION" (
                                                           (org-agenda-overriding-header "My Tasks")
                                                           (org-agenda-skip-function
                                                            '(or (pm-agenda-skip-if-later "SCHEDULED" 1)
                                                                 (and (org-agenda-skip-entry-if 'regexp ":@[a-zA-Z_]+:") ;; there is no assignment tag
                                                                      (org-agenda-skip-entry-if 'notregexp (concat ":@" user-match-code ":")))))))
            ("e" "export" todo "TODO" ((org-agenda-prefix-format "%-9c  %-4T ")
                                       (org-agenda-todo-keyword-format "")
                                       (org-agenda-use-tag-inheritanc nil)
                                       (org-agenda-use-tag-inheritance nil)
                                       (org-agenda-remove-tags t)
                                       (org-agenda-overriding-header "Todo list")
                                       (org-agenda-sorting-strategy '(category-up priority-down))))))

;;;;; Key bindings
    
    (when pm-mnemonic-key-bindings
      (global-set-key (kbd "M-t") 'org-agenda)
      (add-hook 'org-agenda-mode-hook (lambda ()
                                        (define-key org-agenda-mode-map "c" 'org-agenda-todo)
                                        (define-key org-agenda-mode-map "s" 'org-agenda-schedule)
                                        (define-key org-agenda-mode-map "d" 'org-agenda-deadline)
                                        (define-key org-agenda-mode-map "p" 'org-agenda-priority)
                                        (define-key org-agenda-mode-map "t" 'org-agenda-set-tags))))

;;;; Macros
;;;;; Patch for macro expansion in links
    
    (advice-add 'org-macro-replace-all :override #'pm-macro-replace-all)
    
;;;;; Property

    (push `("property" . ,(lambda (key &optional local search &rest _) (pm-get-property key local search))) org-export-global-macros)

;;;;; Registers
    (push '("action_plan" . "(eval (pm-action-plan $1 $2))") org-export-global-macros)
    (push '("decisions_log" . "(eval (pm-decisions-log $1))") org-export-global-macros)
    (push '("issues_log" . "(eval (pm-issues-log $1 $2))") org-export-global-macros)

;;;; Export

    (require 'ox-html)
    (with-eval-after-load 'ox
      (add-to-list 'org-export-options-alist '(:people-url "PEOPLE_URL" "people" nil t))
      (add-to-list 'org-export-filter-body-functions 'pm-html-filter-people-links)
      (setq org-html-format-headline-function 'pm-html-format-headline-function)
      (setq org-html-format-inlinetask-function 'pm--html-format-inlinetask-function)
      (add-hook 'org-export-before-processing-functions 'pm--remove-action-links)
      (advice-add 'org-html--format-toc-headline :around #'pm-html--format-toc-headline))

    (when pm-mnemonic-key-bindings
      (define-key org-mode-map (kbd "C-e") 'org-export-dispatch))
    
;;;; Project IDs, paths, and links

    (add-to-list 'org-link-abbrev-alist '("pj" . pm-project-file))

;;;; Links and protocol

    ;;(advice-remove 'org-link-open 'pm--link-open-expand-advice)
    (advice-add 'org-link-open :around #'pm--link-open-expand-advice)

    (when pm-mnemonic-key-bindings
      (define-key org-mode-map (kbd "C-l") 'org-insert-link)
      (define-key org-mode-map (kbd "M-l") 'org-store-link))

;;;; Interfaces with other apps
;;;;; MsOutlook

    (setq pm-outlook-cmd "C:/Program Files (x86)/Microsoft Office/root/Office16/OUTLOOK.EXE")
    (unless (f-exists? pm-outlook-cmd)
      (setq pm-outlook-cmd "C:/Program Files/Microsoft Office/root/Office16/OUTLOOK.EXE"))
    (unless (f-exists? pm-outlook-cmd)
      (setq pm-outlook-cmd "/mnt/c/Program Files (x86)/Microsoft Office/root/Office16/OUTLOOK.EXE"))
    (unless (f-exists? pm-outlook-cmd)
      (setq pm-outlook-cmd "/mnt/c/Program Files/Microsoft Office/root/Office16/OUTLOOK.EXE"))
    (unless (f-exists? pm-outlook-cmd)
      (lwarn 'PM :error "Outlook not found."))

    (org-link-set-parameters "pm_mail" :follow (lambda (link) (pm-create-outlook-item link 'mail)) :face 'pm-action-face)
    (org-link-set-parameters "pm_meeting" :follow (lambda (link) (pm-create-outlook-item link 'appointment)) :face 'pm-action-face)
    (org-link-set-parameters "pm_outlook" :follow 'pm-outlook-open)

;;;;; MsTeams

    ;;(setq org-default-properties '("ARCHIVE" "CATEGORY" "SUMMARY" "DESCRIPTION" "CUSTOM_ID" "LOCATION" "LOGGING" "COLUMNS" "VISIBILITY" "TABLE_EXPORT_FORMAT" "TABLE_EXPORT_FILE" "EXPORT_OPTIONS" "EXPORT_TEXT" "EXPORT_FILE_NAME" "EXPORT_TITLE" "EXPORT_AUTHOR" "EXPORT_DATE" "UNNUMBERED" "ORDERED" "NOBLOCKING" "COOKIE_DATA" "LOG_INTO_DRAWER" "REPEAT_TO_STATE" "CLOCK_MODELINE_TOTAL" "STYLE" "HTML_CONTAINER_CLASS" "ORG-IMAGE-ACTUAL-WIDTH"))
    (setq org-default-properties
          (-concat org-default-properties
                   (--map (car it)
                          (-partition 2 pm-msteams-properties))))

    (push #'pm-msteams-allowed-property-values org-property-allowed-value-functions)

;;;; Completion

    (require 'company)
    (setq company-backends '(company-capf company-dabbrev company-files))
    (add-hook 'text-mode-hook #'company-mode)
    (add-hook 'org-mode-hook #'pm-company-settings)

    (add-to-list 'ispell-skip-region-alist '("^\\b@" . "^\\b"))

    (when pm-mnemonic-key-bindings
      (define-key org-mode-map (kbd "<C-tab>") nil)
      (global-set-key (kbd "<C-tab>") 'company-complete)
      (with-eval-after-load 'company
        (define-key company-active-map (kbd "<escape>") 'company-abort))
      (with-eval-after-load 'counsel
        (define-key ivy-minibuffer-map (kbd "<C-return>") 'ivy-immediate-done)
        (define-key minibuffer-local-map (kbd "<tab>") 'minibuffer-complete)
        (define-key ivy-minibuffer-map (kbd "TAB") 'ivy-partial))
      (define-key minibuffer-local-map (kbd "<tab>") 'company-complete))

;;;; Other key bindings

    (when pm-mnemonic-key-bindings
      (when (commandp 'counsel-M-x)
        (global-set-key [remap execute-extended-command] #'counsel-M-x))
      (global-set-key (kbd "C-M-x") (lambda () (interactive) (counsel-M-x "")))
      (global-set-key (kbd "C-M-h") 'help)
      (global-set-key (kbd "C-,") 'universal-argument)
      (global-set-key (kbd "C-'") 'quoted-insert))

;;;;; Context sensitive key bindings
    
    (when pm-mnemonic-key-bindings
      (add-hook 'org-metaleft-hook 'pm-metaleft)
      (add-hook 'org-metaright-hook 'pm-metaright)
      (add-hook 'org-shiftmetaleft-hook 'pm-shiftmetaleft)
      (add-hook 'org-shiftmetaright-hook 'pm-shiftmetaright)
      (add-hook 'org-metaup-hook 'ch/org-metaup-inlinetask t)
      (add-hook 'org-metadown-hook 'ch/org-metadown-inlinetask t)
      (define-key org-mode-map (kbd "<return>") 'pm-return)
      (add-hook 'org-ctrl-c-ctrl-c-final-hook 'pm-before-ctrl-c-ctrl-c)
      (define-key org-mode-map (kbd "<C-S-return>") 'pm-ctrl-shift-return)
      (add-hook 'org-metareturn-hook 'pm-meta-return)
      (define-key org-mode-map (kbd "<tab>") 'pm-tab)
      (global-set-key (kbd "C-M-z") 'keyboard-quit) ;; Interupt command or dialog, C-g still works
      (global-set-key (kbd "<escape>") 'pm-keyboard-escape-quit)
      (define-key org-mode-map(kbd "<escape>") 'pm-keyboard-escape-quit))
      
;;;; Closing of Basic settings
    ))
;;; Default settings

  (defun pm-set-defaults ()
  "Set some options as recommended for pm-mod."
  (setq org-startup-folded t
        org-startup-indented t
        org-list-indent-offset 1
        org-hide-block-startup t
        org-hide-emphasis-markers t
        org-special-ctrl-a/e t ;; Beginning of line means after the stars
        org-support-shift-select 'always
        org-return-follows-link t
        org-link-search-must-match-exact-headline nil
        org-use-sub-superscripts nil
        org-display-custom-times t
        org-time-stamp-custom-formats '("<%Y-%m-%d>" . "<%Y-%m-%d %H:%M(%z)>"))
  (setq initial-major-mode 'org-mode)
  (setq org-fold-show-context-detail
        '((default . ancestors)))
  ;;      '((agenda . canonical) ; lineage has a bug not showing direct parent of inlinetask
  ;;        (bookmark-jump . lineage)
  ;;        (isearch . lineage)
  ;;        (default . ancestors)))
  (setq org-link-file-path-type 'relative)
  (setq org-export-with-planning t)
  (setq org-export-with-priority t)
  (setq org-treat-S-cursor-todo-selection-as-state-change nil)

;;;; Mouse

    (when pm-mnemonic-key-bindings
      (require 'org-mouse)
      (global-unset-key (kbd "<C-down-mouse-1>"))
      (setq mouse-1-click-follows-link nil)
      (define-key org-mode-map (kbd "<C-mouse-1>") 'org-open-at-mouse)
      (setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
      (setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
      (setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
      (global-set-key (kbd "<wheel-down>") 'pm-scroll-up-n-lines)
      (global-set-key (kbd "<wheel-up>") 'pm-scroll-down-n-lines)
      (global-set-key (kbd "<mouse-4>") 'pm-scroll-down-n-lines) ; for trackpad
      (global-set-key (kbd "<mouse-5>") 'pm-scroll-up-n-lines)
      (global-set-key (kbd "<mouse-3>") 'jump-ispell-word))

  )

;;; Suggested settings

(defun pm-set-suggested ()

  (setq org-log-done 'time)

  (set-register ?a (cons 'file pm-agenda-files-root))
  (set-register ?n (cons 'file  org-default-notes-file))

  (org-link-set-parameters "pmeval" :follow (lambda (link) (pm-eval-block link)) :face 'pm-action-face)

;;;; Templates
  
  (setq org-capture-templates
        '(("n" "Note" entry (file org-default-notes-file) "* %?")
          ("t" "Task" entry (file+headline org-default-notes-file "Tasks") "* TODO %?\n  %i")
          ("j" "Job" entry (file+headline org-default-notes-file "Tasks") "* JOB %?\n  %i")
          ("m" "Minutes" entry (function (lambda () (switch-to-buffer (buffer-name)) (when (or (not (org-at-heading-p)) (org-inlinetask-at-task-p) (org-inlinetask-in-task-p))  (org-previous-visible-heading 1)))) "* Minutes %(symbol-value 'pm-capture-heading)
\n** Current [[mail:{{{CONTACTS(,email,\";\")}}};{{{team(,email,\";\")}}}&for={{{MAILBOX(,email,\";\")}}}?subject=%(symbol-value 'pm-capture-buffer) - %(symbol-value 'pm-capture-heading) - Minutes&body={{{this}}}][send]] [[pmexport:minutes.html][save]]\n:PROPERTIES:
:action_id_len: 2
:EXPORT_OPTIONS: H:3 toc:nil p:t num:3
:END:\n*** Participants
- [X] @@  - 
- [X] @%(symbol-value 'user-match-code)@ - %n
@@html:<br>@@\n*** Open Issues
{{{issues_log}}}
@@html:<br>@@\n*** Action Plan
{{{action_plan(1)}}}
@@html:<br>@@\n*** %t
%i%?" :immediate-finish t)))

;;;; Speed commands
  
  (setq org-use-speed-commands
        (lambda () (and (looking-at org-outline-regexp) (looking-back "^\**"))))
  (setq org-speed-commands
        '(("Task Actions")
          ("c" . org-todo)
          ("s" . org-schedule)
          ("d" . org-deadline)
          ("p" . org-priority)
          ("t" . pm-set-tags)
          ("General Actions")
          (" " . org-display-outline-path)
          ("j" . org-goto)
          ("h" . org-speed-command-help)
          ("?" . org-speed-command-help)))
  (setq org-speed-commands-default nil)

;;;; Style

;;;;; Marked emphasize
  
  (when pm-mnemonic-key-bindings
    (define-key org-mode-map (kbd "C-;") 'org-emphasize))
  (add-to-list 'org-emphasis-alist '("=" (verbatim :background "#ff7f00")))

  (with-eval-after-load 'ox
    (add-to-list 'org-html-text-markup-alist '(verbatim . "<span style=\"background-color:#ff7f00\">%s</span>")))

;;;;; org-modern
  
  (require 'org-modern)
  (global-org-modern-mode)
  (setq org-pretty-entities t)
  (setq org-pretty-entities-include-sub-superscripts nil)
  (setq org-ellipsis "…")
  (setq org-modern-hide-stars nil) ; somehow fixes indentation of body
  ;;(setq org-modern-label-border 'auto) ; cause alignment issues (e.g. in agenda view) otherwise
  (setq org-modern-todo t)

;;;;;; Patch org-modern to remove paddings that break allignments

  (el-patch-feature org-modern--todo)
  (el-patch-feature org-modern--tag)
  (el-patch-feature org-modern--priority)
  (el-patch-feature org-modern-agenda)
  (with-eval-after-load 'org-modern
    ;; Do not add padding - prevent break of agenda allignment
    (el-patch-defun org-modern--todo ()
      "Prettify headline todo keywords."
      (let ((todo (match-string 1))
            (beg (match-beginning 1))
            (end (match-end 1)))
        (el-patch-remove
          (put-text-property beg (1+ beg) 'display
                             (format #(" %c" 1 3 (cursor t)) (char-after beg)))
          (put-text-property (1- end) end 'display (string (char-before end) ?\s)))
        (put-text-property
         beg end 'face
         (if-let ((face (or (cdr (assoc todo org-modern-todo-faces))
                            (cdr (assq t org-modern-todo-faces)))))
             `(:inherit (,face org-modern-label))
           (if (member todo org-done-keywords)
               'org-modern-done
             'org-modern-todo)))))
    ;; Do not add padding - prevent break of agenda allignment
    (el-patch-defun org-modern--tag ()
      "Prettify headline tags."
      (save-excursion
        (let* ((default-face (get-text-property (match-beginning 1) 'face))
               (colon-props `(display #(":" 0 1 (face org-hide)) face ,default-face))
               (beg (match-beginning 2))
               (end (match-end 2))
               colon-beg colon-end)
          (goto-char beg)
          (while (re-search-forward "::?" end 'noerror)
            (let ((cbeg (match-beginning 0))
                  (cend (match-end 0)))
              (when colon-beg
                (el-patch-remove
                  (put-text-property colon-end (1+ colon-end) 'display
                                     (format #(" %c" 1 3 (cursor t)) (char-after colon-end)))
                  (put-text-property (1- cbeg) cbeg 'display
                                     (string (char-before cbeg) ?\s)))
                (put-text-property colon-end cbeg 'face 'org-modern-tag))
              (add-text-properties cbeg cend colon-props)
              (setq colon-beg cbeg colon-end cend))))))
    ;; Do not add padding - breaks agenda allignment
    (el-patch-defun org-modern--priority ()
      "Prettify priorities according to `org-modern-priority'."
      (let* ((beg (match-beginning 1))
             (end (match-end 1))
             (prio (char-before (1- end))))
        (if-let ((rep (and (consp org-modern-priority)
                           (cdr (assq prio org-modern-priority)))))
            (put-text-property beg end 'display rep)
          (put-text-property beg (1+ beg) 'display (el-patch-swap " " ""))
          (put-text-property (1- end) end 'display (el-patch-swap " " ""))
          (put-text-property
           beg end 'face
           (if-let ((face (or (cdr (assq prio org-modern-priority-faces))
                              (cdr (assq t org-modern-priority-faces)))))
               `(:inherit (,face org-modern-label))
             'org-modern-priority)))))
    ;; Correct allignment for lines with priority
    (el-patch-defun org-modern-agenda ()
      "Finalize Org agenda highlighting."
      (remove-from-invisibility-spec 'org-modern)
      (add-to-invisibility-spec 'org-modern) ;; Not idempotent?!
      (add-hook 'pre-redisplay-functions #'org-modern--pre-redisplay nil 'local)
      (save-excursion
        (save-match-data
          (when org-modern-todo
            (goto-char (point-min))
            (let ((re (format " %s "
                              (regexp-opt
                               (append org-todo-keywords-for-agenda
                                       org-done-keywords-for-agenda) t)))
                  (org-done-keywords org-done-keywords-for-agenda))
              (while (re-search-forward re nil 'noerror)
                (org-modern--todo))))
          (when org-modern-tag
            (goto-char (point-min))
            (let ((re (concat "\\( \\)\\(:\\(?:" org-tag-re "::?\\)+\\)[ \t]*$")))
              (while (re-search-forward re nil 'noerror)
                (org-modern--tag))))
          (when org-modern-priority
            (goto-char (point-min))
            (while (re-search-forward "\\(\\[#.\\]\\)" nil 'noerror)
              ;; For some reason the org-agenda-fontify-priorities adds overlays?!
              (when-let ((ov (overlays-at (match-beginning 0))))
                (overlay-put (car ov) 'face nil))
              (org-modern--priority)
              (el-patch-add
                (when (re-search-forward (concat "\\( \\)\\(:\\(?:" org-tag-re "::?\\)+\\)[ \t]*$") (line-end-position) 'noerror)
                  (goto-char (match-beginning 0))
                  (insert "  ")))))))))

;;;;; Fonts

  (unless (--any? (when (member it (font-family-list))
                    (set-face-attribute 'default nil :family it :height 120 :width 'extra-condensed)
                    (set-face-attribute 'fixed-pitch nil :family it)
                    (set-face-attribute 'variable-pitch nil :family it)
                    (set-face-attribute 'org-modern-label nil :family it :height 1.0)
                    t)
                  '("Iosevka Term" "Placard Condensed" "Cascadia Code" "Courier New"))
    (lwarn 'PM :error "No expected font found."))

;;;;; Modus themes
  
  (defvar pm-modus-theme 'modus-operandi "modus-operandi or modus-vivendi")
  (require 'modus-themes)
  (setq modus-themes-mode-line '(3d)
        modus-themes-links '(neutral-underline)
        modus-themes-region '(no-extend accented)
        modus-themes-diffs 'fg-only-deuteranopia
        modus-themes-org-blocks 'tinted-background
        modus-themes-headings '((t . (rainbow)))
        modus-themes-mixed-fonts t
        modus-themes-variable-pitch-ui t
        modus-themes-variable-pitch-headings t
        modus-themes-scale-headings t
        modus-themes-scale-1 1.1
        modus-themes-scale-2 1.21
        modus-themes-scale-3 1.33
        modus-themes-scale-4 1.46
        modus-themes-scale-5 1.61
        modus-themes-intense-markup t)

  (when pm-mnemonic-key-bindings
    (global-set-key (kbd "C-M-d") 'modus-themes-toggle))

  (add-hook 'modus-themes-after-load-theme-hook #'pm-reset-faces)
  (load-theme pm-modus-theme t)

;;;;; Fix heading scaling and indentation
  
  (advice-add 'org-columns :before #'pm-scale-headings-off)
  (advice-add 'org-columns-quit :after #'pm-scale-headings-on)
  ;;(advice-add 'org-columns-redo :after #'pm-scale-headings-off)
  (add-to-list 'window-buffer-change-functions #'pm--adjust-scale-headings)
  (add-hook 'org-mode-hook #'pm-set-fixed-pitch-selectively)

  (set-face-attribute 'org-inlinetask nil :inherit 'org-level-8)

  (advice-add #'org-indent--compute-prefixes :after #'pm-org-indent--compute-prefixes-after)

;;;;; Task keyword
  
  (setq org-todo-keyword-faces
        '(("JOB"  . (:foreground "blue" :weight bold))
          ("TODO"  . (:foreground "blue" :weight bold))
          ("DONE"  . (:foreground "steelblue" :weight light))
          ("WAITING"  . (:foreground "turquoise" :weight bold))
          ("QUESTION"  . (:foreground "RosyBrown4" :weight bold))
          ("CANCELED"  . (:foreground "grey"))
          ("ISSUE"  . (:foreground "magenta" :weight bold))
          ("RISK"  . (:foreground "salmon" :weight bold))
          ("OPPORTUNITY"  . (:foreground "khaki" :weight bold))
          ("DECISION"  . (:foreground "olive" :weight bold))
          ("MILESTONE"  . (:foreground "sienna" :weight bold))
          ("CLOSED"  . (:foreground "steelblue" :weight light))
          ("RED"  . (:foreground "red" :weight bold))
          ("AMBER"  . (:foreground "orange" :weight bold))
          ("GREEN"  . (:foreground "green" :weight bold))
          ("FINISHED"  . (:foreground "steelblue" :weight light))
          ))
  (with-eval-after-load 'org-modern
    (setq org-modern-todo-faces
          (--map (nconc (list (car it)) (org-combine-plists (cdr it) '(:box '(:color "#000000" :line-width (0 . -4)))))
                 org-todo-keyword-faces)))

;;;; Inline images
  (advice-add #'org-insert-link :after #'org-display-inline-images) 

  )

;;; Context specific settings

(defun pm-set-context-specific ()

  (setq org-file-apps '(("\\.x?html?\\'" . (lambda (path link) (pm-open-externally path)))
                        ("pdf" . default)
                        ("docx" . default)
                        ("xlsx" . default)
                        (directory . emacs)
                        (auto-mode . emacs)))
  (when (and (eq system-type 'gnu/linux) (executable-find "wslview"))
    (setq browse-url-browser-function 'pm-wsl-browse-url))

  (require 'ox-html)
  (setq org-export-exclude-tags '("_noexport"))
  (pm-load-html-style)

  ;; Is it really necessary to define a backend to just add a menu entry to the export dispatcher?
  (org-export-define-derived-backend 'pm-html 'html
    :menu-entry '(?h 1 ((?p "As HTML and PDF" pm-export-as-html-and-pdf))))
  
  (org-link-set-parameters "pmexport" :follow (lambda (link) (pm-export link)) :face 'pm-action-face)
  
  (add-to-list 'org-export-filter-body-functions 'pm-html-filter-sharepoint-warning)

  (require 'which-key)
  (which-key-mode)

  (unless (member "Iosevka Term" (font-family-list))
    (message "\n\nRECOMMENDATION:
You should install the font Iosevka Term for a nicer appearance:
1. Download https://github.com/be5invis/Iosevka/releases/download/v11.0.1/ttc-sgr-iosevka-term-11.0.1.zip
2. Unzip and copy the files into C:\Windows\Fonts\.\n"))

  (with-current-buffer (messages-buffer)
    (when (save-excursion
            (search-backward "consider M-x recover-this-file" nil t))
      (message "\n\n-------------------------------------------------------------")
      (message "There are auto-saved files. Consider recover-session (C-A-r).")
      (message "-------------------------------------------------------------")))

  (server-start)

  )

;;; Functions
;;;; WSL

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

;;;; Basic functions

(defun pm-delete-line ()
  (delete-region
   (point)
   (save-excursion (move-end-of-line 1) (point)))
  (delete-char 1))

(defun pm-focus ()
  (interactive)
  (let ((points (list (point))))
    (org-fold-show-all '(headings))
    (condition-case nil
        (while t
          (org-up-element)
          (push (point) points))
      (user-error nil))
    (org-overview)
    (--each points
      (goto-char it)
      (funcall-interactively #'org-cycle))))

(require 'counsel)
(defun pm-open-externally (path)
  (counsel-locate-action-extern path))
;;  (cond ((eq system-type 'windows-nt) 
;;         (shell-command (concat "start \"\" " (shell-quote-argument (ct-conv-path path t))))) 
;;        ((equal (getenv "CT_CONTEXT") "WSL") 
;;         (shell-command (concat (executable-find "explorer.exe") " " (shell-quote-argument (ct-conv-path path t)))))))

;;;; Files hierarchy

(defun org-entry-get-recursively-advice (oldfun pom property &optional inherit literal-nil)
  (or (funcall oldfun pom property inherit literal-nil)
      (and inherit
           ;; Recursion
           (let ((parent (org-collect-keywords '("PARENT") '("PARENT"))))
             (when parent
               (setq parent (nth 3 (s-match "^[[:blank:]]*\\(\\(\\[\\[\\)?file:\\)?\\(.+?\\)\\(\\]\\[.*?\\)?\\(\\]\\]\\)?[[:blank:]]*$" (cdar parent))))
               (when parent
                 (setq parent (s-split "::" parent))
                 (let ((buf (find-file-noselect (car parent))))
                   (when buf
                     (with-current-buffer buf
                       (if (cdr parent)
                           (org-link-search (org-link-unescape (cadr parent)))
                         (goto-char (point-min)))
                       (funcall oldfun pom property inherit literal-nil))))))))))

;;(advice-add 'org-entry-get :around #'org-entry-get-recursively-advice)
;;(advice-remove 'org-entry-get #'org-entry-get-recursively-advice)

;;;; Tasks
(setq org-todo-keywords
      '((sequence "TODO(t)" "WAITING(w)" "QUESTION(q)" "JOB(j)" "|" "CANCELED(c)" "DONE(d)")
        (sequence "ISSUE(I)" "RISK(R)" "OPPORTUNITY(O)" "MILESTONE(M)" "|" "CLOSED(C)" "ACHIVEMENT(A)" "DECISION(D)")
        (sequence "RED(r)" "AMBER(a)" "GREEN(g)" "|" "FINISHED(f)")))

(setq org-hierarchical-todo-statistics nil)
(setq org-provide-todo-statistics '(("JOB" "TODO" "WAITING") ("DONE")))

(defun pm-set-tags()
  (interactive)
  (minibuffer-with-setup-hook 'pm-minibuffer-mode
    (org-set-tags (read-from-minibuffer "Tags/Actors:\n" (org-make-tag-string (org-get-tags nil t))))))

(defun pm-schedule-remove () (interactive) (org-schedule `(4)))
(defun pm-deadline-remove () (interactive) (org-deadline `(4)))

;;;;; Inline tasks
(require 'org-inlinetask)
(setq org-inlinetask-default-state "TODO")
(setq org-inlinetask-show-first-star t)
(setq org-default-properties (-concat org-default-properties '("ACTION_ID_LEN")))


;; Patch for visibility cycling and inline-tasks
(defun org-inlinetask-hide-tasks (state)
  "Hide inline tasks in buffer when STATE is `contents' or `children'.
This function is meant to be used in `org-cycle-hook'."
  (pcase state
    (`contents
     (let ((regexp (org-inlinetask-outline-regexp)))
       (save-excursion
	       (goto-char (point-min))
	       (while (re-search-forward regexp nil t)
	         (org-inlinetask-toggle-visibility)
	         (org-inlinetask-goto-end)))))
    (`children
     (save-excursion
       (while
	         (or (org-inlinetask-at-task-p)
	             (and (outline-next-heading) (org-inlinetask-at-task-p)))
	       (org-inlinetask-toggle-visibility)
         (org-inlinetask-goto-end)
         (backward-char))))))

;; Auto number inline tasks; ACTION_ID_LEN property defines scope and format
(defun pm-extract-task-id (task)
  (cadr (s-match "^#\\([[:digit:]]+\\) " task)))

(defun pm-new-action-id ()
  (let ((action_id_len (org-entry-get nil "ACTION_ID_LEN" t))
        (scope 'tree)
        (action_id 0))
    (unless action_id_len
      (setq action_id_len (org-entry-get nil "ACTION_ID_LEN" t))
      (setq scope 'file))
    (if (not action_id_len)
        ""
      (save-excursion
        (catch 'break
          (while t
            (if (or (not (org-up-heading-safe))
                    (org-entry-get nil "ACTION_ID_LEN" nil))
                (throw 'break t))))
        (org-map-entries (lambda ()
                           (when (org-entry-get nil "TODO")
                             (let ((id (org-entry-get nil "ITEM")))
                               (when (string-match (format "^#\\([[:digit:]]\\{%s\\}\\)\\($\\|[^[:digit:]]\\)" action_id_len) id)
                                 (setq id (string-to-number (match-string 1 id)))
                                 (if (> id action_id)
                                     (setq action_id id)))))
                           ) nil scope))
      (setq action_id (+ 1 action_id))
      (format (format "#%%0%sd " action_id_len) action_id))))

(defun pm-insert-inlinetask ()
  "Create an inline task with auto numbering"
  (interactive)
  (let ((reg-p (use-region-p)))
    (org-inlinetask-insert-task)
    (when reg-p
      (deactivate-mark)
      (delete-char 1)
      (save-excursion
        (search-forward-regexp (org-inlinetask-outline-regexp))
        (forward-line -1)
        (if (looking-at-p "[[:space:]]*$")
            (delete-char 1))))
    (let ((id (pm-new-action-id)))
      (when id (insert id)))))

(defun pm-convert-inlinetask ()
  (interactive)
  "Convert a task into an inlinetask, or the other way around"
  (save-excursion
    (cond ((or (org-inlinetask-at-task-p) (org-inlinetask-in-task-p))
           (org-inlinetask-goto-beginning)
           (let ((demote (- (org-outline-level) (org-current-level) 1)))
             (beginning-of-line)
             (delete-char demote)
             (org-inlinetask-goto-end)
             (forward-line -1)
             (kill-whole-line)))
          ((and (org-at-heading-p)
                (or (org-entry-is-todo-p) (org-entry-is-done-p)))
           (let ((promote (- org-inlinetask-min-level (org-current-level))))
             (beginning-of-line)
             (looking-at org-complex-heading-regexp)
             (let ((refpos (min (1+ (or (match-end 3) (match-end 2) (match-end 1)))
                                (line-end-position))))
               (goto-char refpos)
               (unless (eq ?# (char-after))
                 (insert (pm-new-action-id))))
             (beginning-of-line)
             (insert (make-string promote ?*))
             (outline-next-heading)
             (insert (concat (make-string org-inlinetask-min-level ?*) " END\n"))))
          (t (pm-insert-inlinetask)))))

(defun pm--inlinetask-link-patch-advice (oldfun &rest args)
  "Work-around to treat inline tasks like normal tasks, e.g. to enable linking to them during export."
  (let ((org-inlinetask-min-level 999))
    (apply oldfun args)))

(advice-add 'org-store-link :around #'pm--inlinetask-link-patch-advice)
;;(advice-add 'org-export-resolve-id-link :around #'pm--inlinetask-link-patch-advice) ; does not help
;;(advice-remove 'org-export-id-fuzzy-link #'pm--inlinetask-link-patch-advice)
;;(advice-add 'org-export-as :around #'pm--inlinetask-link-patch-advice)
;;(advice-add 'org-html-format-headline-default-function :around #'pm--inlinetask-link-patch-advice)

;;;; Dates
(defun pm-read-date-advice (&rest _args)
  "Enforce default keybindings even when org-replace-disputed-keys is t
     To be added as advice for org-read-date"
  (define-key org-read-date-minibuffer-local-map (kbd ".")
    (lambda () (interactive)
      ;; Are we at the beginning of the prompt?
      (if (looking-back "^[^:]+: "
                        (let ((inhibit-field-text-motion t))
                          (line-beginning-position)))
          (org-eval-in-calendar '(calendar-goto-today))
        (insert "."))))
  (define-key org-read-date-minibuffer-local-map (kbd "M-.")
    (lambda () (interactive)
      (org-eval-in-calendar '(calendar-goto-today))))
  (define-key org-read-date-minibuffer-local-map (kbd "M-S-<left>")
    (lambda () (interactive)
      (org-eval-in-calendar '(calendar-backward-month 1))))
  (define-key org-read-date-minibuffer-local-map (kbd "M-S-<right>")
    (lambda () (interactive)
      (org-eval-in-calendar '(calendar-forward-month 1))))
  (define-key org-read-date-minibuffer-local-map (kbd "M-S-<up>")
    (lambda () (interactive)
      (org-eval-in-calendar '(calendar-backward-year 1))))
  (define-key org-read-date-minibuffer-local-map (kbd "M-S-<down>")
    (lambda () (interactive)
      (org-eval-in-calendar '(calendar-forward-year 1))))
  (define-key org-read-date-minibuffer-local-map (kbd "M-<up>")
    (lambda () (interactive)
      (org-eval-in-calendar '(calendar-backward-week 1))))
  (define-key org-read-date-minibuffer-local-map (kbd "M-<down>")
    (lambda () (interactive)
      (org-eval-in-calendar '(calendar-forward-week 1))))
  (define-key org-read-date-minibuffer-local-map (kbd "M-<left>")
    (lambda () (interactive)
      (org-eval-in-calendar '(calendar-backward-day 1))))
  (define-key org-read-date-minibuffer-local-map (kbd "M-<right>")
    (lambda () (interactive)
      (org-eval-in-calendar '(calendar-forward-day 1))))
  (define-key org-read-date-minibuffer-local-map (kbd "!")
    (lambda () (interactive)
      (org-eval-in-calendar '(diary-view-entries))
      (message ""))))

;;;; Capture
(defun pm-capture (&optional goto keys)
  "Wrapper for org-capture to provide originating heading and buffer to capture template."
  (interactive "P")
  (setq pm-capture-buffer (buffer-name))
  (setq pm-capture-heading (save-excursion (when (or (not (org-at-heading-p)) (org-inlinetask-at-task-p) (org-inlinetask-in-task-p))  (org-previous-visible-heading 1)) (org-entry-get nil "ITEM")))
  (org-capture goto keys))

;;;; Refile & goto

(defun pm-refile (&optional ARG)
  (interactive)
  (let* ((org-refile-targets '((buffer-file-name :maxlevel . 3) (org-agenda-files :maxlevel . 3))))
    (org-refile ARG)))

(defun pm-org-refile-get-targets-advice (targets)
  "Insert PROJECT_NAME value into target if this file property exists.
To avoid this just redefine this function as:
(defun pm-org-refile-get-targets-advice (targets) targets)"
  (mapcar (lambda (target)
            (with-current-buffer (org-get-agenda-file-buffer (cadr target))
              (save-excursion
                (org-with-wide-buffer
                 (goto-char 0)
                 (if (re-search-forward ":PROJECT_NAME:\\s-*\\(\\w.*\\)" 200 t)
                     (let ((first (car target)))
                       (setq first (replace-regexp-in-string ".*\\(\\.org\\)\\(/.*\\)?" (concat "-" (match-string 1)) first t t 1))
                       (cons first (cdr target)))
                   target)))))
           targets))

(defun pm-goto ()
  (interactive)
  (let* ((org-refile-targets '((buffer-file-name :maxlevel . 15) (org-agenda-files :maxlevel . 3)))
         (org-reverse-note-order t))
    (org-refile '(4))))

;;;; Agenda
;;;;; Agenda files
(defun pm--collect-ancestors-tags (el)
  (let ((tags (org-element-property :tags el))
        (parent (org-element-property :parent el)))
    (when parent
      (setq tags (-union tags (pm--collect-ancestors-tags parent))))
    tags))

(defun pm--load-agenda-files-from-file (file &optional referrer)
  (message "Adding agenda file %s referred in %s." file referrer)
  (unless (-contains? org-agenda-files file)
    (if (not (f-exists? file))
        (lwarn 'PM :warn "Missing agenda file %s refered from %s." file referrer)
      (with-current-buffer (find-file-noselect file)
        (org-with-wide-buffer
         (setq org-agenda-files (nconc org-agenda-files(list file)))
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
                       (pm--load-agenda-files-from-file
                        (condition-case err
                            (pm-project-file path)
                          (user-error
                           (message "Error collecting agenda files from %s.\n%s" file (error-message-string err))))
                        file))
                      ((s-suffix? ".org" path)
                       (when (-contains? tags "_notes")
                         (setq org-default-notes-file (f-expand path (f-dirname (buffer-file-name)))))
                       (pm--load-agenda-files-from-file (f-expand path (f-dirname (buffer-file-name))) file))
                      ((f-directory? path)
                       (--map (pm--load-agenda-files-from-file it) (f-files path (lambda (f) (s-suffix? ".org" f)) t)))))))))))))))
;;(pm-load-agenda-files)

(defun pm-load-agenda-files ()
  "Build org-agenda-files recursively from links to .org files starting with root file pm_agenda_file.
Links in archived branches are ignored. A link tagged with \"_notes\" is set as org-default-notes-file."
  (interactive)
  (setq org-agenda-files nil)
  (when pm-agenda-files-root
    (pm--load-agenda-files-from-file pm-agenda-files-root))
  (if (or (not org-agenda-files) (< (length org-agenda-files) 2))
      (lwarn 'PM :warning "No agenda files loaded from file %s." pm-agenda-files-root)
    (let ((inhibit-debugger t))
      (customize-save-variable 'org-agenda-files org-agenda-files))
    (message "Loaded %s agenda files." (length org-agenda-files))))

;;;;; Agenda definitions
(defun pm-entry-get-actors ()
  (--map (substring-no-properties it)
         (--filter (s-prefix? "@" it) (org-get-tags))))

(defun pm-skip-if-delegated (&optional mine)
  (let ((actors (pm-entry-get-actors)))
    (when
        (xor mine (and actors (not (-contains? actors (concat "@" user-match-code)))))
      (org-entry-end-position))))

(defun pm-skip-undue-tasks-and-not-current-periods ()
  (org-agenda-skip-entry-if 'deadline 'scheduled))

(defun pm-agenda-skip-if-later (date-name &optional working-days)
  "If this function returns nil, the current match should not be skipped.
  Otherwise, the function must return a position from where the search
  should be continued."
  (ignore-errors
    (let ((end (org-entry-end-position))
          (scheduled-seconds
           (time-to-seconds
            (org-time-string-to-time
             (org-entry-get nil date-name))))
          (now (time-to-seconds (current-time))))
      (if working-days
          (dotimes (number working-days)
            (setq now (+ now (* 24 3600
                                (let ((week-day (decoded-time-weekday (decode-time now))))
                                  (cond ((eq 5 week-day) 3) ; Fri
                                        ((eq 6 week-day) 2) ; Sat
                                        (t 1))))))))
      (and scheduled-seconds
           (> scheduled-seconds now)
           end))))

(defun pm-agenda-entry-get-category ()
  (truncate-string-to-width (org-entry-get nil "CATEGORY") 9 0 ?\s))

(defun pm-agenda-entry-get-deadline ()
  (condition-case nil
      (let* ((deadline (substring (org-entry-get nil "DEADLINE") 1 11))
             (prefix (if (<= (+ (org-time-string-to-seconds deadline)
                                (if (eq 10 (length deadline)) 86400 0))
                             (time-to-seconds (current-time))) "!" " ")))
        (concat prefix (substring deadline 5)))
    (error "      ")))

(defun my-org-agenda-format-date-aligned (date)                                                                                                                  "Format a DATE string for display in the daily/weekly agenda, or timeline.
  This function makes sure that dates are aligned for easy reading."
       (require 'cal-iso)
       (let* ((dayname (calendar-day-name date 1 nil))
              (day (cadr date))
              (day-of-week (calendar-day-of-week date))
              (month (car date))
              (monthname (calendar-month-name month 1))
              (year (nth 2 date))
              (iso-week (org-days-to-iso-week
                         (calendar-absolute-from-gregorian date)))
              (weekyear (cond ((and (= month 1) (>= iso-week 52))
                               (1- year))
                              ((and (= month 12) (<= iso-week 1))
                               (1+ year))
                              (t year)))
              (weekstring (if (= day-of-week 1)
                              (format " W%02d" iso-week)
                            "")))
         (format "%-8s %2d %s %4d%s"
                 dayname day monthname year weekstring)))

;;;; Macros
;;;;; Patch for macro expansion in links

(defun pm-macro-replace-all (templates &optional keywords)
  "Replace all macros in current buffer by their expansion.

TEMPLATES is an alist of templates used for expansion.  See
`org-macro-templates' for a buffer-local default value.

Optional argument KEYWORDS, when non-nil is a list of keywords,
as strings, where macro expansion is allowed.

Return an error if a macro in the buffer cannot be associated to
a definition in TEMPLATES."
  (org-with-wide-buffer
   (goto-char (point-min))
   (let ((properties-regexp (format "\\`EXPORT_%s\\+?\\'"
				                            (regexp-opt keywords)))
	       record)
     (while (re-search-forward "{{{[-A-Za-z0-9_]" nil t)
       (unless (save-match-data (org-in-commented-heading-p))
	       (let* ((datum (save-match-data (org-element-context)))
		            (type (org-element-type datum))
		            (macro
		             (cond
		              ((eq type 'macro) datum)
		              ;; In parsed keywords and associated node
		              ;; properties, force macro recognition.
		              ((or (and (eq type 'keyword)
			                      (member (org-element-property :key datum) keywords))
                       ;; BEGIN OF PATCH
                       (eq type 'link)
                       ;; END OF PATCH
		                   (and (eq type 'node-property)
			                      (string-match-p properties-regexp
					                                  (org-element-property :key datum))))
		               (save-excursion
		                 (goto-char (match-beginning 0))
		                 (org-element-macro-parser))))))
	         (when macro
             ;; `:parent' property might change as we modify buffer.
             ;; We do not care about it when checking for circular
             ;; dependencies.  So, setting `:parent' to nil making sure
             ;; that actual macro element (if org-element-cache is
             ;; active) is unchanged.
             (setq macro (cl-copy-list macro))
             (org-element-put-property macro :parent nil)
	           (let* ((key (org-element-property :key macro))
		                (value (org-macro-expand macro templates))
		                (begin (org-element-property :begin macro))
		                (signature (list begin
				                             macro
				                             (org-element-property :args macro))))
	             ;; Avoid circular dependencies by checking if the same
	             ;; macro with the same arguments is expanded at the
	             ;; same position twice.
	             (cond ((member signature record)
		                  (error "Circular macro expansion: %s" key))
		                 (value
		                  (push signature record)
		                  (delete-region
		                   begin
		                   ;; Preserve white spaces after the macro.
		                   (progn (goto-char (org-element-property :end macro))
			                        (skip-chars-backward " \t")
			                        (point)))
		                  ;; Leave point before replacement in case of
		                  ;; recursive expansions.
		                  (save-excursion (insert value)))
		                 ;; Special "results" macro: if it is not defined,
		                 ;; simply leave it as-is.  It will be expanded in
		                 ;; a second phase.
		                 ((equal key "results"))
		                 (t
		                  (error "Undefined Org macro: %s; aborting"
			                       (org-element-property :key macro))))))))))))

;;;;; Macro expansion of strings

(defmacro pm-with-raw-buffer-copy (&rest body)
  "Evaluate body in a temp-buffer loaded with the raw contents of the current buffer.
The point is at the same position as in the original buffer."
  `(let ((point (point))
         (contents (buffer-substring-no-properties 1 (point-max))))
     (with-temp-buffer
       (org-mode)
       (insert contents)
       (goto-char point)
       (progn ,@body))))

(defun pm-expand-string (string)
  "Expand string in context of point in current buffer."
  ;; Taking the full overhead of expanding the whole buffer. But this is consistent with macro expansion during export.
  (org-with-wide-buffer
   (pm-with-raw-buffer-copy
    (when (org-at-item-p)
      ;; Break out from lists
      (insert "\n"))
    (when (and (eq 0 (current-column))
               (org-at-heading-p))
      ;; Workaround bug of (org-insert-subheading)
      (forward-char))
    (org-insert-subheading nil)
    (insert "_dummy_heading\n")
    (setq point (point)) ; declared in (pm-with-raw-buffer-copy)
    (save-excursion
      (insert string)
      (insert "\nFdw0BJFB")
      (goto-char point)
      (org-up-heading-safe)
      (org-macro-replace-all (org-macro-initialize-templates org-export-global-macros)))
    (setq point (point))
    (when (search-forward "\nFdw0BJFB" nil t)
      (buffer-substring-no-properties point (match-beginning 0))))))

;;;;; Advanced property macro

(defun pm-get-property (key &optional local search)
  (org-with-wide-buffer
   (when search
     (org-link-search search nil t))
   (org-entry-get nil key (not local))))

;;;;; Stakeholders

(defconst pm-stakeholder-property-definition
  '("^[[:blank:]]*\\(\\[\\(.+\\)\\]\\)\\|\\(\\(@\\([[:alnum:]]+\\)@\\)?[[:blank:]]*\\(#\\([[:alnum:]]+\\)\\)?[[:blank:]]*\\(\"\\(.+\\)\"\\)?[[:blank:]]*\\([^[:blank:]<(][^<(]*[^[:blank:]<(]\\)?[[:blank:]]*\\(<\\(.+\\)>\\)?[[:blank:]]*\\((\\(.*\\))\\)?\\)[[:blank:]]*$"
    (nil nil "_TABLE" nil nil "MC" nil "User_ID" nil "Nick" "Name" "Email" nil nil "Role")
    "Name"
    pm-stakeholder-fallback-format)
  "Tuple of
- regexp pattern to parse stakeholder property into attributes.
- a list of attribute names for relevant groups.
- the default format.
- a fallback functions.
The group name _TABLE is reserved for references to stakeholder tables.
When format does not provide a result for the filtered stakeholders then the fallback function is called. It needs to accept 2 arguments: the requested format and plist of parsed attributes as arguments. The fallback function can be used for custom formats or to get data from other people registers.
@MCa #A999999 \"Nickname\" Name Parts <Email@Address> (Role)
The pattern can be tested with the function pm--parse-stakeholder.")

(defun pm--parse-stakeholder (sh)
  "Returns alist of parse stakeholder property according to pm-stakeholder-property-definition."
  (setq sh (s-trim sh))
  (when (and pm-stakeholder-property-definition (string-match (nth 0 pm-stakeholder-property-definition) sh))
    (--mapcat it
              (--map-indexed (when (and it (match-string it-index sh))
                               (list (upcase it) (match-string it-index sh)))
                             (nth 1 pm-stakeholder-property-definition)))))
;; (pm--parse-stakeholder "@xix #A123456 \"Xix\" Xa Ix <x.ix@dja-dss.com> (PM, Artist)")

(defun pm--stakeholders-from-table (table)
  (let ((headers (org-table-get-remote-range table "@<$<..@<$>"))
        (values (org-table-get-remote-range table "@2$<..@>$>")))
    (setq headers (--map
                   (s-replace-regexp "[ /]" "_" (upcase (substring-no-properties it)))
                   headers))
    (setq values (-partition (length headers) values))
    (--map
     (--mapcat it
               (--map-indexed (when (< 0 (length it))
                                (list (nth it-index headers) (substring-no-properties it)))
                              it)) values)))

(defun pm-get-stakeholders (&optional property)
  (let ((shs (org-entry-get nil (or (and property (upcase property)) "STAKEHOLDERS") t)))
    (when shs
      (-flatten-n 1
                  (--map
                   (let* ((sh (pm--parse-stakeholder it))
                          (table (lax-plist-get sh "_TABLE")))
                     (if table
                         (pm--stakeholders-from-table table)
                       (list sh)))
                   (s-split ";" shs))))))

(defun pm-expand-stakeholders (property &optional filter format separator)
  "Expand details of stakeholders e.g. from TEAM property. This is primarily intended to be used as function in a macro.
FILTER specifies which roles to include (separated by blanks), or exclude if the list starts with -.
FORMAT specifies what representation of the stakeholder shall be taken. e.g. emai.
SEPARATOR specifies what string to place between the extracted stakeholders."
  (setq filter (if (and filter (org-string-nw-p filter)) (s-trim filter) "- ")) ; exclude nothing -> include all
  (if (not (string-match-p "^\\(-\s+\\)?\\(?:[[:alnum:]_]+\s*\\)*$" filter))
      (user-error "Invalid filter argument for TEAM macro `%s'" filter))
  (setq format (or (and format (s-trim format)) (or (nth 2 pm-stakeholder-property-definition) "")))
  (setq separator (if separator (s-trim separator) " "))
  (when (and (string-prefix-p "\"" separator) (string-suffix-p "\"" separator))
    (setq separator (substring separator 1 -1)))
  (let* ((filter (s-split "\s+" (upcase filter) t))
         (result (or (pm-get-stakeholders property) (user-error "No property: %s" property)))
         ;; filter by roles
         (result (--filter
                  (let* ((roles (lax-plist-get it "ROLE"))
                         (roles (and roles (s-split "\s+" (upcase roles) t))))
                    (if (equal "-" (car filter))
                        (or (not roles)
                            (--any?
                             (not (-any? (lambda (f) (s-equals? f it)) (cdr filter)))
                             roles))
                      (--any?
                       (-any? (lambda (f) (s-equals? f it)) filter)
                       roles)))
                  result))
         ;; format
         (fallback-fun (nth 3 pm-stakeholder-property-definition))
         (result (--map
                  (or (lax-plist-get it (upcase format))
                      (and fallback-fun (funcall fallback-fun format it)))
                  result)))
    (string-join (-non-nil result) separator)))

(defun pm-stakeholder-fallback-format (format attributes)
  (when (s-equals? "@" format)
    (concat "@" (lax-plist-get attributes "MC"))))

;;(defun pm--export-save-point-in-original-buffer-advice (oldfun &rest args)
;;  (setq pm--export-point-in-original-buffer (point))
;;  (apply oldfun args)
;;  (setq pm--export-point-in-original-buffer nil))
;;(advice-add 'org-export-as :around #'pm--export-save-point-in-original-buffer-advice)
;;(advice-remove 'org-export-as #'pm--export-save-point-in-original-buffer-advice)

;;(org-export-as) (org-export-with-buffer-copy) (org-export--annotate-info)
;;    (org-macro-initialize-templates org-export-global-macros)
;;    (org-macro-replace-all org-macro-templates parsed-keywords)




;;;;; Registers (action plan, issue log, decision log)

(defun pm-action-plan (&optional up recently-done)
  (setq up (if (org-string-nw-p up) (string-to-number up) 1))
  (let ((priorities '())
        outlist
        headers)
    (org-with-wide-buffer
      (org-back-to-heading t)
      (dotimes (i up)
        (org-up-element))
      (save-restriction ; (org-map-entries ... 'tree) does not work here ???
        (org-narrow-to-subtree)
        (org-map-entries
         (lambda ()
           (let ((state (org-entry-get nil "TODO"))
                 (priority (org-entry-get nil "PRIORITY"))
                 (item (org-entry-get nil "ITEM"))
                 (tags (org-entry-get nil "TAGS"))
                 (alltags (org-entry-get nil "ALLTAGS"))
                 (dl (org-entry-get nil "DEADLINE"))
                 (closing-date (org-entry-get nil "CLOSED"))
                 id)
             (setq alltags (if alltags (s-split ":" alltags) '()))
             (when (and (or (not org-export-select-tags) (not (-intersection org-export-select-tags (-flatten (org-get-buffer-tags)))) (-intersection alltags org-export-select-tags))
                        (or org-export-exclude-tags (not (-intersection alltags org-export-exclude-tags)))
                        (or (equal state "TODO")
                            (equal state "WAITING")
                            (equal state "QUESTION")
                            (and (or (equal state "DONE") (equal state "CANCELED"))
                                 closing-date (>= (org-time-convert-to-integer (org-time-string-to-time closing-date))
                                                  (org-time-convert-to-integer (org-read-date t t (or recently-done "")))))))
               (setq id (pm-extract-task-id item))
               (add-to-list 'outlist
                            (list 
                             (cond ((equal state "TODO") '("b" "blue"))
                                   ((equal state "DONE") '("d" "steelblue"))
                                   ((equal state "CANCELED") '("e" "grey"))
                                   ((equal state "WAITING") '("c" "turquoise"))
                                   ((equal state "QUESTION") '("a" "rosybrown")))
                             state
                             priority
                             (if dl dl "_/*None*/_")
                             (or (and id (format "@QY@QYhtml:<a href=\"#%s\">#%s</a>@@ %s" id id (substring item (+ (length id) 2)))) item)
                             (if tags
                                 (mapconcat 'identity (--filter (s-starts-with? "@" it) (s-split ":" tags)) " ")
                               "_/*None*/_")))
               (setq priorities (-union priorities (list priority))))))))
      (if (not outlist)
          "No open tasks"
        (setq outlist (--sort
                       (string-lessp (concat (caar it) (nth 2 it) (nth 3 it) (nth 4 it))
                                     (concat (caar other) (nth 2 other) (nth 3 other) (nth 4 other))) outlist))
        (setq outlist (--map
                       (cons (format "@QY@QYhtml:<span style=\"color:%s\">%s</span>@@" (cadar it) (cadr it)) (cddr it))
                       outlist))
        (if (> (length priorities) 1)
            (setq headers '("State" "Pri" "Due" "Title" "Actor"))
          (setq headers '("State" "Due" "Title" "Actor"))
          (setq outlist (-select-columns '(0 2 3 4) outlist)))
        (replace-regexp-in-string "@QY@QYhtml:" "@@html:" ; work around orgtbl-to-orgtbl filtering out inline html
                                  (orgtbl-to-orgtbl (nconc (list headers 'hline) outlist) '())
                                  t t)))))

(defun pm-decisions-log (&optional up)
  (setq up (if (org-string-nw-p up) (string-to-number up) 1))
  (let (outlist)
    (org-with-wide-buffer
      (org-back-to-heading t)
      (dotimes (i up)
        (org-up-element))
      (org-map-entries
       (lambda ()
         (let ((state (org-entry-get nil "TODO"))
               (item (org-entry-get nil "ITEM"))
               (closing-date (org-entry-get nil "CLOSED")))
           (when (equal state "DECISION")
             (add-to-list 'outlist
                          (list item (and closing-date (concat (substring closing-date 0 11) ">")))))))
       nil 'tree))
    (if (not outlist)
        "No decisions yet"
      (setq outlist (sort outlist
                          #'(lambda (x y) (string-greaterp (nth 1 x) (nth 1 y)))))
      (orgtbl-to-orgtbl (nconc (list '("Title" "Date") 'hline) outlist) '()))))

(defun pm-issues-log (&optional up states)
  (setq up (if (org-string-nw-p up) (string-to-number up) 1))
  (setq states (if (org-string-nw-p states) (s-split " " states t) '("ISSUE" "RISK" "OPPORTUNITY")))
  (let (outlist)
    (org-with-wide-buffer
      (org-back-to-heading t)
      (dotimes (i up)
        (org-up-element))
      (org-map-entries
       (lambda ()
         (let ((state (org-entry-get nil "TODO"))
               (item (org-entry-get nil "ITEM"))
               (closing-date (org-entry-get nil "CLOSED")))
           (when (-contains? states state)
             (add-to-list 'outlist
                          (list item)))))
       nil 'tree))
    (if (not outlist)
        "No open issues"
      (setq outlist (sort outlist
                          (lambda (x y) (string-lessp (nth 0 x) (nth 0 y)))))
      (orgtbl-to-orgtbl (nconc (list '("Title") 'hline) outlist) '()))))

;;;; Export

(defun pm-html-filter-people-links (body backend info)
  (when (org-export-derived-backend-p backend 'html)
    (let ((base-url (or (plist-get info :people-url) "")))
      (setq body (replace-regexp-in-string "\\b@\\(\\w+\\)\\b" (format "<a href=\"%s#pm-_\\1\">@\\1</a>" base-url) body t))
      (setq body (replace-regexp-in-string "\\b@\\(\\w+\\)@\\b" "<a id=\"pm-_\\1\" />@\\1" body t))
      body)
    ))

(defun pm-html-format-headline-function (todo todo-type priority text tags info)
  "Hide control tags (beginning with underscore)"
  (org-html-format-headline-default-function todo todo-type priority text 
                                             (and tags (seq-filter (lambda (tag) (not (string-prefix-p "_" tag))) tags))
                                             info))

(defun pm--html-format-inlinetask-function (todo todo-type priority text tags contents info)
  "Format inline tasks to include deadline in heading and ignore rest of dates lines"
  (let (deadline)
    (when (and contents (string-match "<p>.*?<span class=\"timestamp-kwd\">\\(DEADLINE\\|SCHEDULED\\|CLOSED\\):</span>.+?p>$" contents))
      (setq deadline (match-string 0 contents))
      (setq contents (replace-match "" nil nil contents))
      (setq deadline (if (string-match "<span class=\"timestamp-kwd\">DEADLINE:</span> <span class=\"timestamp\">&lt;\\(.+?\\)&gt;</span>" deadline)
                         (concat "≤" (match-string 1 deadline))
                       "")))
    (format "<div class=\"inlinetask\" id=\"%s\">\n<b>%s%s</b>%s\n%s</div>"
            (or (pm-extract-task-id text) "#no-id")
            (org-html-format-headline-default-function todo todo-type priority text tags info)
            (format "<span class=\"deadline\">&#xa0;&#xa0;%s&#xa0;</span>" (or deadline ""))
            (org-html-close-tag "br" nil info)
            ;;            (if pm--feedback-string (format " <a href=\"%s\">Feedback</a>" (format pm--feedback-string text (url-hexify-string (pm--build-back-reference))) ""))
            (or contents ""))))

(defun pm--remove-action-links (backend)
  (while (re-search-forward "\\[\\[ct\\w+:.+\\].*\\]" nil t)
    (replace-match "" nil t)))

(defun pm-html--format-toc-headline (orig-fun headline info)
  "monkey patched function to tailor toc depth with tags"
  (let ((tags (org-export-get-tags headline info))
        (alltags (org-export-get-tags headline info nil t))
        (parent (org-element-property :parent headline)))
    (unless (or (seq-contains-p alltags "_notoc")
                (and (seq-contains-p alltags "_notoc1")
                     (not (seq-contains-p tags "_notoc1")))
                (and parent
                     (seq-contains-p (org-export-get-tags parent info nil t) "_notoc2")
                     (not (seq-contains-p (org-export-get-tags parent info) "_notoc2"))))
      (apply orig-fun (list headline info)))))

;;;;; Context specific settings
;; just work in specific (Windows) environments
;; TODO: generalize this

(require 'ox-html)
(defun pm-load-html-style (&optional css-file)
  ;; html style, with optional TOC and Sharepoint limitations warning.
  (setq org-html-head-include-default-style nil)
  ;; <!--/*--><![CDATA[/*><!--*/
  ;; /*]]>*/-->
  (setq org-html-head
        (format
         "<style type=\"text/css\">
<!--/*--><![CDATA[/*><!--*/
%s/*]]>*/-->
</style>
" (f-read-text (or css-file (f-join (f-dirname (symbol-file 'pm-mod)) "page.css")))))
  (setq org-html-scripts "
<script>
  window.onload = function() {
    document.getElementById('SharepointWarning').remove();
  };
</script>")
  (setq org-html-head-include-scripts t))

(defun pm-export-as-html-and-pdf (&optional async subtreep visible-only body-only ext-plist file)
  "Create pdf docs by printing html with Chrome in post-processing html export."
  (interactive)
  (setq file (or file (org-export-output-file-name ".html" subtreep) (concat (f-no-ext (or (buffer-file-name) (read-file-name "File name: "))) ".html")))
  (message "Exporting to %s ..." file)
  (org-export-to-file 'pm-html file async subtreep visible-only body-only ext-plist
                      (lambda (file)
                        (f-write-text (pm--patch-html-for-non-toc (pm-expand-string (f-read-text file))) 'utf-8 file)
                        (message "If export hangs here then make sure the pdf file is no already open.")
                        (call-process "C:/Program Files/Google/Chrome/Application/chrome.exe" nil nil t "--headless" (concat "--print-to-pdf=" (concat (f-no-ext (f-full file)) ".pdf")) "--no-margins" "--print-to-pdf-no-header" (org-export-file-uri (f-full file)))
                        file)))

(defun pm-export (link)
  "Export to html and pdf. To be used for action link."
  (pm-export-as-html-and-pdf nil t nil nil nil
                             (pm-path (pm-expand-string (string-trim link "\"" "\"")))))

(defun pm--patch-html-for-non-toc (text)
  "Remove toc from html text."
  ;; dirty hack to remove toc area for html mails
  (if (s-contains? "<div id=\"text-table-of-contents\"" text)
      text
    (s-replace "#content{\n  margin-left:300px;\n  background:#fcfcfc;" "#content{" text)))

(defun pm-html-filter-sharepoint-warning (body backend info)
  "Insert warning about limitation when viewing html docs on Sharepoint."
  (if (org-export-derived-backend-p backend 'html)
      (concat "<div id='SharepointWarning' style='color:red;'><h1>In Sharepoint 365 internal links are not working. Your experience will be better if you download this document and open it locally.</h1></div>\n"
              body)
    body))

;;;; Project IDs, paths, and links

(defvar pm-project-id-types nil
  "List of project ID type in order of preference to use in case of. Each type is a tuple (list) of
1. regexp pattern for valid project ID of that type
2. the function to get the file path from the ID for a project of that type
3. a property name, which is used to store this type of ID
Example:
  '((\"[[:alpha:]]\\{2\\}[[:digit:]]\\{6\\}\" my-sap-project-path \"SAP_ID\")
    (\"[[:digit:]]\\{10\\}\" my-salesforce-project-path \"SALESFORCE_ID\"))
")

(defun pm-project-id-type (id)
  (and id
       (--first (string-match-p (concat "\\`" (car it) "\\'") id)
                pm-project-id-types)))

(defun pm-assured-valid-project-id-type (id)
  (or (pm-project-id-type id)
      (user-error "XXX No valid project ID: %s" id)))

(defun pm-project-file (id)
  (let ((fun (nth 1 (pm-project-id-type id))))
    (if fun
        (pm-path (funcall fun id))
      "UNKNOWN_PROJECT_ID_FORMAT")))

(defun pm-project-id ()
  (or (--any (org-entry-get nil (nth 2 it) t) pm-project-id-types) (org-entry-get nil "PROJECT_ID" t)))

(defun pm-open-project (project-id)
  "Try to open a file by project number."
  (interactive (list (read-string "Enter a project code: " nil nil (thing-at-point 'symbol))))
  (condition-case nil
      (find-file (and (pm-assured-valid-project-id-type project-id) (pm-project-file project-id)))
    (error
     (user-error "Cannot open project file: %s" file)))
  nil)

;;;; Links and protocol
(defun pm--link-open-expand-advice (oldfun link &optional arg)
  (let ((link (org-element-copy link)))
    (org-element-put-property link :path (pm-path (pm-expand-string (org-element-property :path link))))
    (apply oldfun link arg)))

(defun pm-org-invoke-babel-named (name)
  (interactive)
  (save-excursion
    (org-babel-goto-named-src-block name)
    (org-babel-execute-src-block-maybe)))

(defun pm-protocol (query)
  "Jump to org file (PATH) or project (PID) and optionally position at SEARCH.
org-protocol://ct:/path=c:/path/file.org&search=*Heading"
  (let ((pars (org-protocol-convert-query-to-plist query)))
    (if (plist-get pars :pid)
        (pm-open-project (plist-get pars :pid))
      (let ((default-directory pm-home))
        (org-open-file (pm-path (url-unhex-string (plist-get pars :path))))))
    (org-link-search (url-unhex-string (plist-get pars :search))))
  (raise-frame)
  nil)

(defface pm-action-face
  '((t :inverse-video t :underline t))
  "Face for action links."
  :group 'org-faces)

(org-link-set-parameters "ctrun" :follow 'pm-org-invoke-babel-named :face 'pm-action-face)
(add-to-list 'org-protocol-protocol-alist
             '("CoolTool"
               :protocol "pm"
               :function pm-protocol))

;;;; Interfaces with other apps
;;;;; Powershell

(defconst pm--tryrun-powershell nil
  "Print code instead of executing it. Primarily for testing.")

(defun pm-run-in-powershell(code watch &rest inserts)
  "Execute PowerShell CODE replacing %s with INSERTS before execution. If WATCH then show output in buffer '*PowerShell Output*' in other window. Returns messages buffer if PM--TRYRUN-POWERSHELL, output buffer if WATCH, output otherwise."
  (unless pm--tryrun-powershell
    (setq code (s-join ";" (--filter (not (s-prefix? "#" it)) (s-split "\n" code)))))
  (setq code (apply #'format (push code inserts)))
  (if pm--tryrun-powershell
      (and (message "This code would be run in Powershell:\n%s" code) (messages-buffer))
    (if watch
        (let ((buf (get-buffer-create "*PowerShell Output*")))
          (with-current-buffer-window buf nil nil
            (call-process "powershell.exe" nil "*PowerShell Output*" t code))
          buf)
      (if (< (length code) 8000)
          (shell-command-to-string (concat "powershell.exe -Command \"" code "\""))
        (let ((file (make-temp-file "temp" nil ".ps1" code)))
          (shell-command-to-string (concat "powershell.exe -file \"" file "\""))
          (f-delete file))))))

(defun pm-run-in-powershell-as-admin(code &rest inserts)
  (setq code (concat "
$myWindowsID=[System.Security.Principal.WindowsIdentity]::GetCurrent()
$myWindowsPrincipal=new-object System.Security.Principal.WindowsPrincipal($myWindowsID)
$adminRole=[System.Security.Principal.WindowsBuiltInRole]::Administrator
if ($myWindowsPrincipal.IsInRole($adminRole)) {
'Running as admin ...'
} else {
   $newProcess = new-object System.Diagnostics.ProcessStartInfo 'PowerShell';
   $newProcess.Arguments = $myInvocation.MyCommand.Definition;
   $newProcess.Verb = 'runas';
   [System.Diagnostics.Process]::Start($newProcess);
   exit
}" code))
  (apply #'pm-run-in-powershell code nil inserts))

(defun pm--pssavpar(par)
  "Pass a string insert savely to Powershell by utf-8 base64 encoding"
  (format "([System.Text.Encoding]::UTF8.GetString([System.Convert]::FromBase64String('%s')))"
          (base64-encode-string (encode-coding-string par 'utf-8) t)))

;;;;; MsOutlook

(defun pm-create-outlook-item (link item-type)
  ;; patch old link format for backward compatibility
  (when (< (or (s-index-of "?" link) 9999) (or (s-index-of "=" link) 9999))
    (setq link (concat "to=" (s-replace "?" "&" link)))
    (setq link (s-replace "subject=" "sub=" link)))
  (let (pars)
    (--map (setq pars (lax-plist-put pars (car it) (cadr it))) (--map (s-split "=" it) (s-split "&" link)))
    (let ((body (replace-regexp-in-string
                 "<div id='SharepointWarning'.+\n" ""
                 (pm--patch-html-for-non-toc
                  (let ((org-ascii-text-width 9999))
                    (org-export-as (if (eq item-type 'mail) 'pm-html 'ascii) t nil nil)))))
          (sub (lax-plist-get pars "sub"))
          (to  (lax-plist-get pars "to"))
          (cc  (lax-plist-get pars "cc"))
          (for (lax-plist-get pars "for"))
          (deadline (lax-plist-get pars "deadline"))
          (scheduled (lax-plist-get pars "scheduled"))
          setters)
      (when for
        (setq cc (concat for ";" cc)))
      (setq setters
            (s-join
             "\n"
             (-non-nil
              (list
               (when sub
                 (format "$item.Subject = %s"
                         (pm--pssavpar sub)))
               (when body
                 (format "$item.%s = %s"
                         (plist-get '(mail "HtmlBody" appointment "Body" task "Body") item-type) (pm--pssavpar body)))
               (when to
                 (format "$item.%s = %s"
                         (plist-get '(mail "To" appointment "RequiredAttendees" task "Owner") item-type) (pm--pssavpar to)))
               (when cc
                 (format "$item.%s = %s"
                         (plist-get '(mail "Cc" appointment "OptionalAttendees") item-type) (pm--pssavpar cc)))
               (when for
                 (format "$item.%s = %s"
                         (plist-get '(mail "SentOnBehalfOfName" appointment "SentOnBehalfOfName" task "Delegator") item-type) (pm--pssavpar for)))
               (when deadline
                 (format "$item.DueDate = %s"
                         (pm--pssavpar deadline)))
               (when scheduled
                 (format "$item.ReminderTime = %s"
                         (pm--pssavpar deadline)))
               ))))
      (pm-run-in-powershell "
$ol = New-Object -comObject Outlook.Application
$item = $ol.CreateItem(%d)
%s
$inspector = $item.GetInspector
$inspector.Display()
Start-Sleep -Seconds 2
$inspector.Activate()
"
                            nil (plist-get '(mail 0 appointment 1 task 3) item-type) setters))))

(defun pm-outlook-create-tasks (&optional up)
  "Take code from (pm-outlook-create-tasks) as basis.")

(defun pm-outlook-open (link) 
  (shell-command (concat "\"" pm-outlook-cmd "\" outlook:" link)))

(defun pm-instructions-to-link-to-outlook-items ()
  (interactive)
  (with-temp-buffer
    (insert "'Export a link to the currently selected item

Private Declare PtrSafe Function ShellExecute Lib \"shell32.dll\" Alias \"ShellExecuteA\" (ByVal hwnd As Long, ByVal lpOperation As String, ByVal lpFile As String, ByVal lpParameters As String, ByVal lpDirectory As String, ByVal nShowCmd As Long) As Long

Sub ExportLink()
    Dim objItem As Object
    Dim url As String
      
    Set objItem = Application.ActiveExplorer.Selection.Item(1)
    url = \"org-protocol:/store-link?url=outlook:\" + objItem.EntryID + \"&title=\" + getItemTitle(objItem)
    ShellExecute 0, vbNullString, url, vbNullString, vbNullString, vbNormalFocus
End Sub

Sub FollowUp()
    Dim objItem As Object
    Dim url As String
      
    Set objItem = Application.ActiveExplorer.Selection.Item(1)
    url = \"org-protocol:/capture?template=t&body=\" + \"[[outlook:\" + objItem.EntryID + \"][\" + getItemTitle(objItem) + \"]]\"
    ShellExecute 0, vbNullString, url, vbNullString, vbNullString, vbNormalFocus
End Sub


Function getItemTitle(objItem As Object)
    If Application.ActiveExplorer.Selection.Count <> 1 Then
        MsgBox (\"Select one and only one item.\")
        Exit Function
    End If
    If objItem.Class = olMail Then
        itemTitle = \"Mail: \" + objItem.Subject + \" (\" + objItem.SenderName + \")\"
    ElseIf objItem.Class = olAppointment Then
        itemTitle = \"Meeting: \" + objItem.Subject + \" (\" + objItem.Organizer + \")\"
    ElseIf objItem.Class = olTask Then
        itemTitle = \"Task: \" + objItem.Subject + \" (\" + objItem.Owner + \")\"
    ElseIf objItem.Class = olContact Then
        itemTitle = \"Contact: \" + objItem.Subject + \" (\" + objItem.FullName + \")\"
    ElseIf objItem.Class = olJournal Then
        itemTitle = \"Journal: \" + objItem.Subject + \" (\" + objItem.Type + \")\"
    ElseIf objItem.Class = olNote Then
        itemTitle = \"Note: \" + objItem.Subject + \" (\" + \" \" + \")\"
    Else
        itemTitle = \"Item: \" + objItem.Subject + \" (\" + objItem.MessageClass + \")\"
    End If
    getItemTitle = URLEncode(itemTitle)
End Function

Public Function URLEncode( _
   StringVal As String, _
   Optional SpaceAsPlus As Boolean = False _
) As String

  Dim StringLen As Long: StringLen = Len(StringVal)

  If StringLen > 0 Then
    ReDim result(StringLen) As String
    Dim i As Long, CharCode As Integer
    Dim Char As String, Space As String

    If SpaceAsPlus Then Space = \"+\" Else Space = \"%20\"

    For i = 1 To StringLen
      Char = Mid$(StringVal, i, 1)
      CharCode = Asc(Char)
      Select Case CharCode
        Case 97 To 122, 65 To 90, 48 To 57, 45, 46, 95, 126
          result(i) = Char
        Case 32
          result(i) = Space
        Case 0 To 15
          result(i) = \"%0\" & Hex(CharCode)
        Case Else
          result(i) = \"%\" & Hex(CharCode)
      End Select
    Next i
    URLEncode = Join(result, \"\")
  End If
End Function
")
    (copy-region-as-kill (point-min) (point-max)))
  (message "Copied code into clipboard. Paste it into the Outlook VBA editor, and assign shortcuts to the 2 functions (best for the main window and the specific windows)."))

;;;;; MsTeams

(defun pm-setup-msteams()
  (message "Installing MsTeams integration ...")
  (let ((code "
'';'';'ATTENTION';'If you get an error PackageManagement\\Uninstall-Package : Access to the cloud file is denied then there is still a known PowerShell bug. See https://github.com/PowerShell/PowerShellGet/issues/262'
%sSet-ExecutionPolicy RemoteSigned
Install-Module -Name PowerShellGet -Scope CurrentUser -Force -AllowClobber
Install-Module -Name MicrosoftTeams -Scope CurrentUser -Force -AllowClobber
%sSet-ExecutionPolicy %s
read-host 'Press ENTER to close window...'")
        (old-policy (pm-run-in-powershell "Get-ExecutionPolicy" nil))
        (commenter "# "))
    (unless (s-contains? "RemoteSigned" old-policy)
      (setq commenter ""))
    (pm-run-in-powershell-as-admin code commenter commenter old-policy)))

(defun pm--extract-raw-email(text)
  (cadr (s-match "\\([-+_.~a-zA-Z][-+_.~:a-zA-Z0-9]*@[-.a-zA-Z0-9]+\\)>? *$" text)))

(defun pm-msteams-setup-team(id options owners members channels)
  "Create a teams with a NAME or update one identified by ID.
OPTIONS is a list of strings holding option name without prefix '-' and values."
  (setq members (--map (or (pm--extract-raw-email it)
                           (user-error "'%s' is not a valid member." it))
                       members))
  (setq owners  (--map (or (pm--extract-raw-email it)
                           (user-error "'%s' is not a valid owner." it))
                       owners))
  (setq members (-uniq (-concat members owners)))
  (let ((code (list "Import-Module MicrosoftTeams" "Connect-MicrosoftTeams"
                    (if (s-present? id)
                        (format "$group = Get-Team -GroupID %s" id)
                      (format "$group = New-Team -DisplayName %s"
                              (when-let ((name (--filter (s-equals? (upcase (car it)) "DISPLAYNAME") options)))
                                (pm--pssavpar (cdar name)))))
                    "$id = $group.GroupId"
                    (pm--update-options options nil)
                    (pm--update-users members "Member" nil)
                    (pm--update-users owners "Owner" nil)
                    (pm--update-channels channels)
                    "'DONE with creating/updating team:'"
                    "Return $id")))
    (setq code (s-join "\n" (-non-nil code)))
    (message "Be patient! MsTeam may need quite a few seconds to create a new team.")
    (cadr (s-match "\n\\([0-9a-f\\-]+\\)\n+"
                   (with-current-buffer
                       (pm-run-in-powershell code t)
                     (buffer-string))))))

(defun pm--update-options(options channel)
  (when options
    (let ((cmdsup (if channel "Channel" ""))
          (idsup (if channel (format " -CurrentDisplayName %s" (pm--pssavpar channel)) "")))
      (format "Set-Team%s -GroupId $id%s %s"
              cmdsup idsup
              (s-join " "
                      (--map (format "-%s %s" (car it)
                                     (if (stringp (cdr it))
                                         (pm--pssavpar (or (cdr it) ""))
                                       (if (cdr it) "$true" "$false")))
                             options))))))

(defun pm--update-users(users role channel)
  "Add&remove owners or members to/from channel."
  (setq users (if users (s-join ", " (--map (concat "'" it "'") users)) "@()"))
  (setq role (if (and role (s-equals? role "Owner")) (format " -Role '%s'" role) ""))
  (let ((cmdsup (if channel "Channel" ""))
        (idsup (if channel (format " -DisplayName %s" (pm--pssavpar channel)) "")))
    (format "# Update %s users (%s) for %s.
$olds = (Get-Team%sUser -GroupID $id%s %s).User
$news = @(%s)
foreach ($it in $news) { if ($olds -notcontains $it) { Add-Team%sUser -GroupID $id%s -User $it%s } }
foreach ($it in $olds) { if ($news -notcontains $it) { Remove-Team%sUser -GroupID $id%s -User $it%s } }"
            cmdsup role channel cmdsup idsup role users cmdsup idsup role cmdsup idsup role)))

(defun pm--update-channels(channels)
  (if channels
      (let ((data
             (-unzip-lists
              (--map
               (let* ((name (nth 0 it))
                      (options (nth 1 it))
                      (owners (nth 2 it))
                      (members (nth 3 it))
                      (members (-uniq (-concat members owners)))
                      (type (--filter (s-equals? (upcase (car it)) "MEMBERSHIPTYPE") options))
                      (type (upcase (or (cdar type) "STANDARD")))
                      (options (--remove (s-equals? (upcase (car it)) "MEMBERSHIPTYPE") options)))
                 (list
                  (pm--pssavpar name)
                  (format "%s = '%s'" (pm--pssavpar name) type)
                  (s-join "\n" (-non-nil (list
                                          (pm--update-options options name)
                                          (unless (s-equals? type "STANDARD")
                                            (pm--update-users members "Member" name))
                                          (unless (s-equals? type "STANDARD")
                                            (pm--update-users owners "Owner" name)))))))
               channels))))
        (format "$olds = (Get-TeamChannel -GroupID $Id).DisplayName
$news = @(%s)
$types = @{%s}
foreach ($it in $news) { if ($olds -notcontains $it) { New-TeamChannel -GroupID $id -DisplayName $it -MembershipType $types[$it]} }
%s"
                (s-join ", " (nth 0 data)) (s-join "; " (nth 1 data)) (s-join "\n" (nth 2 data))))))

(defconst pm-msteams-properties
  '("MST_T_DisplayName" nil
    "MST_T_Description" nil
    "MST_T_MailNickName" nil 
    "MST_T_Visibility" ("Private" "Public")
    "MST_T_AllowGuestCreateUpdateChannels" ("No" "Yes")
    "MST_T_AllowGuestDeleteChannels" ("No" "Yes")
    "MST_T_AllowCreateUpdateChannels" ("No" "Yes")
    "MST_T_AllowDeleteChannels" ("No" "Yes")
    "MST_T_AllowAddRemoveApps" ("No" "Yes")
    "MST_T_AllowCreateUpdateRemoveTabs" ("No" "Yes")
    "MST_T_AllowCreateUpdateRemoveConnectors" ("No" "Yes")
    "MST_T_AllowUserEditMessages" ("No" "Yes")
    "MST_T_AllowUserDeleteMessages" ("No" "Yes")
    "MST_T_AllowOwnerDeleteMessages" ("No" "Yes")
    "MST_T_AllowTeamMentions" ("No" "Yes")
    "MST_T_AllowChannelMentions" ("No" "Yes")
    "MST_T_ShowInTeamsSearchAndSuggestions" ("No" "Yes")
    "MST_C_MembershipType" ("Standard" "Private" "Shared")
    "MST_C_Description" nil
    "MST_Hook" nil
    )
  "Plist of allowed properties for MsTeams teams definitions and allow values for them.")

(defun pm-msteams-allowed-property-values (prop)
  (lax-plist-get pm-msteams-properties prop))

(defun pm-element-get-raw-text (node)
;;  (with-current-buffer parse-buf
  (buffer-substring-no-properties (org-element-property :begin node) (org-element-property :end node)))
;;)

(defun pm-element-get-children (node types)
  (-non-nil
   (org-element-map node types
     (lambda (it)
       (when (eq (org-element-property :parent it) node)
         it)))))

(defun pm-msteams-setup-team-with-structure (link)
  (org-with-wide-buffer
   (org-narrow-to-subtree)
   (goto-char (point-min))
   (let* ((groupid (org-entry-get (point) "MST_Id"))
          (text (buffer-substring-no-properties (point-min) (point-max)))
;;          (parse-buf (get-buffer-create "*Parse Buffer*"))
          (team (caddr 
;;                 (with-current-buffer parse-buf
;;                   (org-mode)
;;                   (insert text)
                 (org-element-parse-buffer 'object)))
;;          )
          (options (pm--msteams-extract-options-from-structure team ":MST_T_"))
          owners members channels)
     (--map
      (let ((text (org-element-property :raw-value it)))
        (cond
         ((s-equals? text "Owners") (setq owners (pm--msteams-extract-users-from-structure it)))
         ((s-equals? text "Members") (setq members (pm--msteams-extract-users-from-structure it)))
         ((s-equals? text "Channels") (setq channels (pm--msteams-extract-channels-from-structure it)))))
      (pm-element-get-children team 'headline))
;;     (kill-buffer parse-buf)
     (setq groupid (pm-msteams-setup-team groupid options owners members channels))
     (org-set-property "MST_Id" groupid))))

(defun pm--msteams-extract-options-from-structure (node prefix)
  (--map (cons
          (s-chop-prefix prefix (car it))
          (cond
           ((s-equals? (cdr it) "NO") nil)
           ((s-equals? (cdr it) "YES") t)
           (t (pm-expand-string (or (cdr it) "")))))
         (--filter (s-prefix? prefix (car it))
                   (--map (cons (symbol-name (car it)) (cadr it))
                          (-partition 2 (cadr node))))))

(defun pm--msteams-extract-users-from-structure (node)
  (--remove (s-equals? it "")
            (s-split ";"
                     (s-join ";"
                             (org-element-map node 'paragraph
                               (lambda (it)
                                 (s-replace "\n" ";"
                                            (pm-expand-string (pm-element-get-raw-text it)))))))))

(defun pm--msteams-extract-channels-from-structure (node)
  (--map
   (let ((name (org-element-property :raw-value it))
         (options (pm--msteams-extract-options-from-structure it ":MST_C_"))
         owners members)
     (--map
      (let ((text (s-trim (or (org-element-property :raw-value it) ""))))
        (cond
         ((s-equals? text "Owners") (setq owners (pm--msteams-extract-users-from-structure it)))
         ((s-equals? text "Members") (setq members (pm--msteams-extract-users-from-structure it)))))
      (pm-element-get-children it 'headline))
     (let ((type (--filter (s-equals? (upcase (car it)) "MEMBERSHIPTYPE") options)))
       (when (and (or (not type)
                      (s-equals? (upcase (cdar type)) "STANDARD"))
                  (or owners members))
         (message "User specifications are ignored for Team with MembershipType Standard: %s." name)
         (setq owners nil members nil)))
     (list (pm-expand-string name) options owners members))
   (pm-element-get-children node 'headline)))

(require 'request)
(defvar pm-mst-webhook-urlbase nil
  "URL prefix for MsTeams incoming webhooks")

(defun pm-post-into-channel (link)
  "Post the contents of this branch as message into the MsTeams channel referenced by a web hook ID.
The web hook ID can be specified as link, or is otherwise taken from the property/keyword MST_Hook."
  (let ((link (if (length> (s-trim link) 0)
                  (pm-expand-string link)
                (save-excursion (org-entry-get nil "MST_Hook"))))
        (msg (replace-regexp-in-string
              "<div id='SharepointWarning'.+\n" ""
              (org-export-as 'pm-html t nil t))))
    (unless link
      (user-error "No web hook specified."))
    (request
      (concat (if (or (not pm-mst-webhook-urlbase) (s-prefix? "http" link t)) "" pm-mst-webhook-urlbase) link)
      :type "POST"
      :headers '(("Content-Type" . "application/json"))
      :data (json-encode `(("text" . ,msg)))
      :parser 'json-read
      :success (cl-function (lambda (&key data &allow-other-keys) (message "message posted:\n%s" data)))
      :error (cl-function (lambda (&key error-thrown &allow-other-keys)
                            (error "Posting failed:\nError: %s" (cdr error-thrown)))))))

(org-link-set-parameters "pm_msteams_team_setup" :follow (lambda (link) (pm-msteams-setup-team-with-structure link)) :face 'pm-action-face)
(org-link-set-parameters "pm_channel_post" :follow #'pm-post-into-channel :face 'pm-action-face)
(setq org-default-properties
      (-concat org-default-properties
               (--map (car it)
                      (-partition 2 pm-msteams-properties))))

;;;;; Jira
;; Very basic implementation based on deep links

(defvar pm-jira-url nil
  "Base URL to access Jira instance.")

(defun pm-jira (action fields)
  "Create or query Jira ticket."
  (pm-open-externally
   (print
    (pm-expand-string
     (concat pm-jira-url
             (cond ((eq action 'query) "/issues/?")
                   ((eq action 'create) "/secure/CreateIssueDetails!init.jspa?")
                   (t (user-error "Incorrect action: %s." action)))
             (mapconcat (lambda (pair) (format "%s=%s" (car pair) (cadr pair)))
                        fields "&"))))))




;;;; Completion

(defmacro pm-with-minibuffer-selected-window (&rest body)
  "Execute the forms in BODY from the minibuffer in its original window.
  When used in a minibuffer window, select the window selected just before
  the minibuffer was activated, and execute the forms,
  else just execute the form."
  (declare (indent 0) (debug t))
  `(let ((window (minibuffer-selected-window)))
     (if window
         (with-selected-window window
           ,@body)
       ,@body)))

(defun pm-company-backend (command &optional arg &rest ignored)
  (interactive (list 'interactive))
  (let ((case-fold-search nil))
    (pcase command
      (`interactive (company-begin-backend 'pm-company-backend))
      (`prefix
       (when (looking-back "@\\w*")     ;; use limit parameter
         (match-string 0)))
      (`candidates
       (let ((cands (list)))
         (pm-with-minibuffer-selected-window
           (save-excursion
             (widen)
             (goto-char 1)
             (unless (< (length arg) 3)
               (goto-char 1)
               (while (re-search-forward (format "^.*?\\(\\(\\(%s\\w*\\)@\\)\\|\\(\\(@\\w\\w+\\)@.+%s\\)\\).+?$" arg (substring arg 1)) nil t)
                 (add-to-list 'cands (propertize (concat (match-string-no-properties 3) (match-string-no-properties 5)) 'annotation (match-string 0)))))))
         cands))
      (`annotation
       (concat (substring "      " (min (length arg) 5))
               (get-text-property 0 'annotation arg)))
      (`sorted t))))

(defun pm-company-settings ()
  (setq-local company-minimum-prefix-length 2)
  (setq-local ac-auto-start 6)
  ;; Show tool tip even when there is only one candidate - DOES NOT WORK
  ;;(setq company-frontends '(company-pseudo-tooltip-frontend company-echo-metadata-frontend))
  (add-hook 'completion-at-point-functions 'pcomplete-completions-at-point nil t)
  (make-local-variable 'company-backends)
  (add-to-list 'company-backends 'pm-company-backend))

(defun pm-minibuffer-mode()
  ;;(setq-local ivy-mode nil)
  (company-mode 1)
  (setq-local company-minimum-prefix-length 2)
  (setq-local ac-auto-start 6)
  (setq-local resize-mini-windows 'grow-only)
  (make-local-variable 'company-backends)
  (add-to-list 'company-backends 'pm-company-backend)
  (make-local-variable 'minibuffer-local-map))

;;;; Style

;;;;; org-modern

;; Fix against high horizontal separators in tables wtih org-modern and org-indent
;; from https://misohena.jp/blog/2022-08-27-fix-org-table-lines-using-org-modern-and-org-indent.html
(defun pm-org-indent--compute-prefixes-after ()
  (let ((prefixes org-indent--text-line-prefixes))
    (dotimes (i (length prefixes))
      (let* ((space-str (aref prefixes i))
             (space-length (length space-str)))
        (when (> space-length 0)
          (aset prefixes i
                (org-add-props
                    space-str
                    nil
                  'display (cons 'space
                                 (list :width space-length
                                       :height '(1))))))))))

;;;;; modus themes

;;Disable scale-headings while in columns-view
(defun pm--reload-modus-theme ()
  (pcase (modus-themes--current-theme)
    ('modus-operandi (modus-themes-load-operandi))
    ('modus-vivendi (modus-themes-load-vivendi))
    (_ (modus-themes--load-prompt))))
(defun pm-scale-headings-on (&rest args)
  (setq modus-themes-scale-headings t)
  (pm--reload-modus-theme))
(defun pm-scale-headings-off (&rest args)
  (setq modus-themes-scale-headings nil)
  (pm--reload-modus-theme))

(require 'org-colview)
(defun pm--adjust-scale-headings (win)
  (if org-columns-current-fmt
      (when modus-themes-scale-headings
        (setq modus-themes-scale-headings nil)
        (pm--reload-modus-theme))
    (unless modus-themes-scale-headings
      (setq modus-themes-scale-headings t)
      (pm--reload-modus-theme))))

(defun pm-set-fixed-pitch-selectively ()
  (variable-pitch-mode 1)
  (--map
   (set-face-attribute it nil :inherit 'fixed-pitch)
   '(org-column org-code org-block org-table org-property-value org-formula org-tag org-date company-tooltip org-special-keyword org-block-begin-line org-block-end-line org-meta-line org-document-info-keyword)))

(defun pm-reset-faces ()
  (set-face-attribute 'org-verbatim nil :background "#ff7f00")
  (set-face-attribute 'org-drawer nil :height 0.5))

;;;; Babel

(defun pm-eval-block (name)
  (let ((point (point)))
    (org-with-wide-buffer
     (goto-char (point-min))
    (when (re-search-forward (format "^#\\+NAME:[ \t]+%s[ \t]*$" (regexp-quote name)) nil t)
     (org-babel-eval-wipe-error-buffer)
      (org-babel-execute-src-block nil nil '((var . ("point" . point))))))))

;;;; Context sensitive key binding
;;;;; M-Left/Right

(defun pm-metaleft ()
  (interactive)
  (cond ((org-at-heading-p)
         (org-promote-subtree)
         t)))
(defun pm-metaright ()
  (interactive)
  (cond ((org-at-heading-p)
         (org-demote-subtree)
         t)))

;;;;; S-M-Left/Right

(defun pm-shiftmetaleft ()
  (interactive)
  (cond ((org-at-heading-p)
         (org-do-promote)
         t)))
(defun pm-shiftmetaright ()
  (interactive)
  (cond ((org-at-heading-p)
         (org-do-demote)
         t)))

;;;;; M-Up/Down
;; Work-around to fix moving inline tasks

(defun ch/org-metaup-inlinetask ()
  "If at inline task and not active region, drag inline task backward."
  (if (and (not (org-region-active-p)) (org-inlinetask-at-task-p))
      (org-drag-element-backward)
    nil))

(defun ch/org-metadown-inlinetask ()
  "If at inline task and not active region, drag inline task forward."
  (if (and (not (org-region-active-p)) (org-inlinetask-at-task-p))
      (org-drag-element-forward)
    nil))

;;;;; Return
;; Patched to open link even in table

(defun pm-return (&optional indent)
  "Modification of org-return to open links even in tables."
  (interactive)
  (let ((context (org-element-context)))
    (if (and org-return-follows-link
	           (or (and (eq 'link (org-element-type context))
		                  ;; Ensure point is not on the white spaces after
		                  ;; the link.
		                  (let ((origin (point)))
		                    (org-with-point-at (org-element-property :end context)
			                    (skip-chars-backward " \t\n")
			                    (> (point) origin))))
	               (org-in-regexp org-ts-regexp-both nil t)
	               (org-in-regexp org-tsr-regexp-both nil  t)
	               (org-in-regexp org-link-any-re nil t)))
        (call-interactively #'org-open-at-point)
      (call-interactively #'org-return indent))))

;;;;; C-Return

(defun pm-before-ctrl-c-ctrl-c ()
  (cond
   ;; show people directory entry if on @xxx
   ((let ((link (thing-at-point 'word t)))
      (cond
       ((and link (string-match "\\b\\(@[[:alnum:]]+\\)@\\b" link))
        (swiper-isearch (concat (match-string 1 link) "\\b"))
        t)
       ((and link (string-match-p "\\b@[[:alnum:]]+\\((.*\\)?" link))
        (let ((destination
               (save-excursion
                 (widen)
                 (goto-char 1)
                 (if (search-forward (concat link "@") nil t)
                     (point)))))
          (if destination
              (progn
                (unless (and (<= (point-min) destination)
                             (>= (point-max) destination))
                  (widen))
                (org-mark-ring-push)
                (goto-char destination)
                (when (or (org-invisible-p) (org-invisible-p2)) (org-fold-show-context 'mark-goto)))
            (user-error "Person not found." link)))
        t))))
   ;; generate task juggler reports
   ((let ((tags (org-entry-get nil "ALLTAGS")))
      (when (and tags (string-match ":taskjuggler_project:" tags))
        (pm-taskjuggle)
        t)))
   ;; edit tags when in task
   ((and (org-at-heading-p) (org-entry-get nil "TODO"))
    (pm-set-tags)
    t)
   ;; recalculate table
   ((org-at-table-p)
    (org-table-iterate)
    (org-table-shrink)
    nil)
   ;; enforce opening links with system app
   ((eq 'link (org-element-type (org-element-context)))
    (org-link-open (org-element-context) '(system)))))

;;;;; C-S-Return

(defun pm-ctrl-shift-return (&rest args)
  (interactive)
  (let ((context (org-element-context)))
        (cond
         ;; enforce opening links in other window
         ((and (eq 'link (org-element-type context))
               ;; Ensure point is not on the white spaces after
		           ;; the link.
		           (let ((origin (point)))
		             (org-with-point-at (org-element-property :end context)
			             (skip-chars-backward " \t\n")
			             (> (point) origin))))
          (let ((org-link-frame-setup (copy-alist org-link-frame-setup)))
            (setf (alist-get 'file org-link-frame-setup) 'find-file-other-window)
            (org-open-at-point args)))
      (t (org-insert-todo-heading-respect-content)))))

;;;;; M-Return
(defun mist()
  (list (org-element-type (org-element-context (org-element-at-point))) (face-at-point nil t)))
(defun pm-meta-return ()
  (let ((context (org-element-context)))
    (cond
     ;; enforce opening links in emacs
     ((and (eq 'link (org-element-type (org-element-context)))
           ;; Ensure point is not on the white spaces after
		       ;; the link.
		       (let ((origin (point)))
		         (org-with-point-at (org-element-property :end context)
			         (skip-chars-backward " \t\n")
			         (> (point) origin))))
      (org-link-open (org-element-context) '(emacs)))
     ;; toogle heading for active region
     ((org-region-active-p)
      (org-toggle-heading)
      t))))

;;;;; Tab

(defun pm-next-actionable ()
  (interactive)
  (when (eq (org-element-type (org-element-context)) 'link)
    (forward-char 3))
  (if (re-search-forward (format "\\(%s\\)\\|\\(%s\\)" org-link-any-re org-todo-line-regexp) nil t)
      (cond
       ((org-at-heading-p) (beginning-of-line))
       (t (search-backward "[[" nil t))) ; link
    (message "No more actionables found.")))

(defun pm-tab ()
  (interactive)
  (cond
   ((or (org-at-heading-p) (org-at-block-p) (org-in-src-block-p) (org-at-drawer-p) (org-at-property-p) (org-at-table-p))
    (org-cycle))
   (t (pm-next-actionable))))

;;;;; Cancel

(defun pm-keyboard-escape-quit ()
  "Like keyboard-escape-quit, but does not close other windows."
  (interactive)
  (cond ((eq last-command 'mode-exited) nil)
	((region-active-p)
	 (deactivate-mark))
	((> (minibuffer-depth) 0)
	 (abort-recursive-edit))
	(current-prefix-arg
	 nil)
	((> (recursion-depth) 0)
	 (exit-recursive-edit))
	(buffer-quit-function
	 (funcall buffer-quit-function))
	;;((not (one-window-p t))
	;; (delete-other-windows))
	((string-match "^ \\*" (buffer-name (current-buffer)))
	 (bury-buffer))))

;;;; Mouse

(defun jump-ispell-word (@click)
  (interactive "e")
  (let ((p1 (posn-point (event-start @click))))
    (goto-char p1)
    (ispell-word)))

(defun pm-scroll-up-n-lines ()
  (interactive)
  (scroll-up 5))

(defun pm-scroll-down-n-lines ()
  (interactive)
  (scroll-down 5))

;;; Provide
(provide 'pm-mod)

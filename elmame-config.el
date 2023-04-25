;;; elmame-config.el --- The elmame config panel

;; Author: Yong <luo.yong.name@gmail.com>
;; URL: https://github.com/Iacob/elmame
;; Version: 0.1a
;; Package-Requires: ((emacs "27.1"))

;;; Commentary:

;; elmame configuration panel


;;; Code:


(provide 'elmame-config)

(require 'widget)
(require 'wid-edit)

(require 'elmame)

(defvar elmame-config-user-config-text "" "User config file content.")

(defvar elmame-config-widget-text-exec nil)
(defvar elmame-config-text-exec "")
(defvar elmame-config-widget-text-working-dir nil)
(defvar elmame-config-text-working-dir "")
(defvar elmame-config-widget-text-rom-dir nil)
(defvar elmame-config-text-rom-dir "")
(defvar elmame-config-widget-extra-args nil)

;; (defvar widget-text-exec nil)
;; (defvar text-exec "")
;; (defvar widget-text-working-dir nil)
;; (defvar text-working-dir "")
;; (defvar widget-text-rom-dir nil)
;; (defvar text-rom-dir "")
;; (defvar widget-extra-args nil)

;;;###autoload
(defun elmame-config-open-config-panel ()
  "Open elmame config panel."
  (interactive)
  
  (let (fn-save-config)
    (switch-to-buffer "**elmame-mame configuration**")
    
    (make-local-variable 'elmame-config-widget-text-exec)
    (make-local-variable 'elmame-config-text-exec)
    (make-local-variable 'elmame-config-widget-text-working-dir)
    (make-local-variable 'elmame-config-text-working-dir)
    (make-local-variable 'elmame-config-widget-text-rom-dir)
    (make-local-variable 'elmame-config-text-rom-dir)
    (make-local-variable 'elmame-config-widget-extra-args)

    (setq fn-save-config
	  (lambda (&rest params)
	    (setq elmame-config-user-config-text "")
	    (when (> (length elmame-config-text-exec) 0)
	      (setq elmame-config-user-config-text
		    (concat elmame-config-user-config-text
			    "exec " (json-serialize elmame-config-text-exec) "\n")))
	    (when (> (length elmame-config-text-working-dir) 0)
	      (setq elmame-config-user-config-text
		    (concat elmame-config-user-config-text
			    "working-dir " (json-serialize elmame-config-text-working-dir) "\n")))
	    (when (> (length elmame-config-text-rom-dir) 0)
	      (setq elmame-config-user-config-text
		    (concat elmame-config-user-config-text
			    "rompath " (json-serialize elmame-config-text-rom-dir) "\n")))
	    (when (> (length (widget-value elmame-config-widget-extra-args)) 0)
	      (setq elmame-config-user-config-text
		    (concat elmame-config-user-config-text
			    "args " (json-serialize (widget-value elmame-config-widget-extra-args)) "\n")))
	    (setq elmame-config-user-config-text
		  (concat "(" elmame-config-user-config-text ")"))
	    
	    (save-window-excursion
	      (with-temp-buffer
		(insert elmame-config-user-config-text)
		(write-file "~/.elmame-mame" 't) ) ) ) )
  
    ;; (let ((inhibit-read-only 't))
    ;;   (erase-buffer))
    (insert "\n" (propertize "elmame-mame configuration" 'face 'info-title-2) "\n\n")
    (widget-create 'link
		   :notify (lambda (&rest params)
			     (setq elmame-config-text-working-dir
				   (read-directory-name
				    "Please select working directory: "))
			     (widget-value-set elmame-config-widget-text-working-dir
					       elmame-config-text-working-dir))
		   "Select working directory")
    (insert "\n")
    (setq elmame-config-widget-text-working-dir (widget-create 'const :format "➥ %v" ""))
    (insert "\n")
    (widget-create 'link
		   :notify (lambda (&rest params)
			     (setq elmame-config-text-exec
				   (read-directory-name
				    "Please input path of the mame exutable: "))
			     (widget-value-set elmame-config-widget-text-exec
					       elmame-config-text-exec))
		   "Select mame executable")
    (insert "\n")
    (setq elmame-config-widget-text-exec (widget-create 'const :format "➥ %v" ""))
    (insert "\n")
    (widget-create 'link
		   :notify (lambda (&rest params)
			     (setq elmame-config-text-rom-dir
				   (read-directory-name
				    "Please select rom directory: "))
			     (widget-value-set elmame-config-widget-text-rom-dir
					       elmame-config-text-rom-dir))
		   "Select rom directory")
    (insert "\n")
    (setq elmame-config-widget-text-rom-dir (widget-create 'const :format "➥ %v" ""))
    (insert "\nextra arguments:\n")
    (setq elmame-config-widget-extra-args (widget-create 'editable-field ""))
    (insert "\n")
    (widget-create 'link :notify fn-save-config "Save")
    (widget-create 'link
		   :notify (lambda (&rest params) (kill-buffer (buffer-name)))
		   "Close")
    (widget-setup)

    ;; Initial values
    (let ((cfg (elmame-mame-read-user-config)))
      (message "cfg: %s" cfg)
      (when cfg
	(setq elmame-config-text-exec (or (plist-get cfg 'exec) ""))
	(setq elmame-config-text-rom-dir (or (plist-get cfg 'rompath) ""))
	(setq elmame-config-text-working-dir (or (plist-get cfg 'working-dir) ""))
	(widget-value-set elmame-config-widget-text-exec elmame-config-text-exec)
	(widget-value-set elmame-config-widget-text-rom-dir elmame-config-text-rom-dir)
	(widget-value-set elmame-config-widget-text-working-dir elmame-config-text-working-dir)
	(widget-value-set elmame-config-widget-extra-args (plist-get cfg 'args)) ) ) ) )

;;; elmame-config.el ends here

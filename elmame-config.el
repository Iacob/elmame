
(defun elmame-mame-config-panel ()
  (interactive)
  
  (defvar elmame-mame-config-text "")
  
  (let (fn-save-config)
    (switch-to-buffer "**elmame-mame configuration**")
    (defvar-local widget-text-working-dir nil)
    (defvar-local text-working-dir "")
    (defvar-local widget-text-rom-dir nil)
    (defvar-local text-rom-dir "")
    (defvar-local widget-extra-args nil)

    (setq fn-save-config
	  (lambda (&rest params)
	    (setq elmame-mame-config-text "")
	    (when (> (length text-working-dir) 0)
	      (setq elmame-mame-config-text
		    (concat elmame-mame-config-text
			    "working-dir " "\"" text-working-dir "\"" "\n")))
	    (when (> (length text-rom-dir) 0)
	      (setq elmame-mame-config-text
		    (concat elmame-mame-config-text
			    "rompath " "\"" text-rom-dir "\"" "\n")))
	    (when (> (length (widget-value widget-extra-args)) 0)
	      (setq elmame-mame-config-text
		    (concat elmame-mame-config-text
			    "args " "\"" (widget-value widget-extra-args) "\"" "\n")))
	    (setq elmame-mame-config-text
		  (concat "(" elmame-mame-config-text ")"))
	    
	    (save-window-excursion
	      (with-temp-buffer		  
		(insert elmame-mame-config-text)
		(write-file "~/.elmame-mame" 't) ) ) ) )
  
    ;; (let ((inhibit-read-only 't))
    ;;   (erase-buffer))
    (insert "\n" (propertize "elmame-mame configuration" 'face 'info-title-2) "\n\n")
    (widget-create 'link
		   :notify (lambda (&rest params)
			     (setq text-working-dir
				   (read-directory-name
				    "Please select working directory: "))
			     (widget-value-set widget-text-working-dir
					       text-working-dir))
		   "Select working directory")
    (insert "\n")
    (setq widget-text-working-dir (widget-create 'const :format "➥ %v" ""))
    (insert "\n")
    (widget-create 'link
		   :notify (lambda (&rest params)
			     (setq text-rom-dir
				   (read-directory-name
				    "Please select rom directory: "))
			     (widget-value-set widget-text-rom-dir
					       text-rom-dir))
		   "Select rom directory")
    (insert "\n")
    (setq widget-text-rom-dir (widget-create 'const :format "➥ %v" ""))
    (insert "\nextra arguments:\n")
    (setq widget-extra-args (widget-create 'editable-field ""))
    (insert "\n")
    (widget-create 'link :notify fn-save-config "Save")
    (widget-create 'link
		   :notify (lambda (&rest params) (kill-buffer (buffer-name)))
		   "Close")
    (widget-setup) ) )



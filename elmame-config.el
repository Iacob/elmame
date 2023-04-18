
(defun elmame-mame-config-panel ()
  (interactive)
  (let ()
    (switch-to-buffer "**elmame-mame configuration**")
    (defvar-local widget-text-working-dir nil)
    (defvar-local text-working-dir "")
    (defvar-local widget-text-rom-dir nil)
    (defvar-local text-rom-dir "")
    (let ((inhibit-read-only 't))
      (erase-buffer))
    (insert "\nelmame-mame configuration\n\n")
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
    (widget-create 'editable-field "")
    (insert "\n")
    (widget-create 'link "Save")
    (widget-create 'link
		   :notify (lambda (&rest params) (kill-buffer (buffer-name)))
		   "Close")
    (widget-setup) )
  )



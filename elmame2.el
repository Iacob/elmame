
(load-file "mame_machine_info_loader.el")

(defun elmame-mame-get-config (name)
  (let ((default-config '(exec "mame" rompath "roms"))
	config-value)
    (if (boundp 'elmame-mame-config)
	(setq config-value (plist-get elmame-mame-config name)))
    (if (not config-value)
	(setq config-value (plist-get default-config name)))
    config-value ) )

(defun elmame-mame-save-context (name value)
  (if (boundp 'elmame-mame-context)
      (if (assoc name elmame-mame-context)
	  (setcdr (assoc name elmame-mame-context) (list value))
	(nconc elmame-mame-context (list name value)) )
    (setq elmame-mame-context (list (list name value)))
    )
  )

(defun elmame-mame-list-roms ()
  (let ((rompath (elmame-mame-get-config 'rompath))
	filelist
	machinelist
	(machinedefs (elmame-mame-load-machine-defs)))
    (when (file-directory-p rompath)
      (setq filelist (directory-files rompath nil directory-files-no-dot-files-regexp))
      (setq filelist (mapcar (lambda (x) (car (split-string x "\\."))) filelist))
      ;;(message "filelist: %s" filelist)
      (setq machinelist (mapcar (lambda (m) (seq-find (lambda (def) (string= (plist-get def 'name) m)) machinedefs)) filelist))
      (setq machinelist (seq-filter (lambda (m) m) machinelist)) )
    ;;(message "machinelist: %s" machinelist)
    machinelist ) )

(defun elmame-mame-make-shell-command (machine-name)
  ;; mame <machine-name> -rompath roms
  (let ((exec (elmame-mame-get-config 'exec))
	(rompath (elmame-mame-get-config 'rompath))
	(args (elmame-mame-get-config 'args))
	args-text)
    (if (not args)
	(setq args-text "")
      (setq args-text
	    (string-join (mapcar (lambda (arg) (format "%s" arg)) args) " ")))
    
    (format "%s %s -rompath %s %s" exec machine-name rompath args-text) ) )

(defun elmame-mame ()
  (interactive)
  (run-hooks 'elmame-mame-mode-hook)
  (let (machinelist
	column-width
	(working-dir (elmame-mame-get-config 'working-dir))
	fn-calc-width)

    (setq machinelist (elmame-mame-list-roms))
    (setq fn-calc-width (lambda (col)
			  (seq-max (mapcar (lambda (x) (length (plist-get x col))) machinelist))
			  ) )
    (setq column-width
	  (list 'name (funcall fn-calc-width 'name)
		'year (funcall fn-calc-width 'year)
		'manufacturer (funcall fn-calc-width 'manufacturer)
		'desc (funcall fn-calc-width 'desc) ) )
    (elmame-mame-save-context 'column-width column-width)


    (switch-to-buffer "**machine list**")
    (setq buffer-read-only nil)
    (erase-buffer)
    (when working-dir
      (cd working-dir) )
    (when working-dir
      (message "Swtiching to directory: %s" working-dir)
      (cd working-dir) )
    (mapcar (lambda (m)
	      (let ((machine-name (plist-get m 'name)))
		(insert-button
		 machine-name
		 'action (lambda (x)
			   ;;(message "%s" (button-get x 'machine-name))
			   (let* ((machine-name (button-get x 'machine-name))
				  (cmd-line (elmame-mame-make-shell-command machine-name)))
			     ;;(message-box cmd-line)
			     (switch-to-buffer-other-window "**mame output**")
			     
			     (insert (format "Running command: %s" cmd-line) "\n")
			     (start-process-shell-command "mame command" "**mame output**" cmd-line)
			     
			     )
			   )
		 'machine-name machine-name
		 'follow-link 't)
		)
	      
	      (insert "\n")
	      )
	    machinelist)
    (beginning-of-buffer)
    (setq buffer-read-only 't)
    
    ) )

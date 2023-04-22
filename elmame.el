
(load-library "elmame_mame_machine_info_loader")
;;(load-file "mame_machine_info_loader.el")

(defvar elmame-mame-user-config nil "elmame-mame user config from file")

(defun elmame-mame-read-user-config ()
  (let (config-text cfg)
    (condition-case err
	(when (file-readable-p "~/.elmame-mame")
	  (with-temp-buffer
	    (insert-file "~/.elmame-mame")
	    (setq config-text
		  (buffer-substring-no-properties (point-min) (point-max))))
	  (setq cfg (read config-text)))
      (error (message "Exception: %s" err) ) )
    cfg ) )

(defun elmame-mame-reload-user-config ()
  (setq elmame-mame-user-config (elmame-mame-read-user-config)) )

(defun elmame-mame-get-user-config ()
  (or elmame-mame-user-config (elmame-mame-reload-user-config)))

(defun elmame-mame-get-config (name)
  "Get config value with a name."
  (let ((default-config '(exec "mame" rompath "roms"))
	(user-config (elmame-mame-get-user-config))
	config-value)
    (if (boundp 'elmame-mame-config)
	(setq config-value (plist-get elmame-mame-config name)))
    (if (not config-value)
	(setq config-value (plist-get user-config name)))
    (if (not config-value)
	(setq config-value (plist-get default-config name)))
    config-value ) )

(defun elmame-mame-save-context (name value)
  "Save value to elmame context with a name."
  (if (boundp 'elmame-mame-context)
      (if (assoc name elmame-mame-context)
	  (setcdr (assoc name elmame-mame-context) value)
	(nconc elmame-mame-context (cons name value)) )
    (setq elmame-mame-context (list (cons name value))) ) )

(defun elmame-mame-list-roms ()
  "Filter mame roms in rompath and return the list of valid ones."
  (let ((rompath (elmame-mame-get-config 'rompath))
	filelist
	machinelist
	(machinedefs (elmame-mame-load-machine-defs)))
    (message "Current dir: %s" (pwd))
    (message "Current rompath: %s" rompath)
    (when (file-directory-p rompath)
      (setq filelist (directory-files rompath nil directory-files-no-dot-files-regexp))
      (setq filelist (mapcar (lambda (x) (car (split-string x "\\."))) filelist))
      ;;(message "filelist: %s" filelist)
      (setq machinelist (mapcar (lambda (m) (seq-find (lambda (def) (string= (plist-get def 'name) m)) machinedefs)) filelist))
      (setq machinelist (seq-filter (lambda (m) m) machinelist)) )
    ;;(message "machinelist: %s" machinelist)
    machinelist ) )

(defun elmame-mame-make-shell-command (machine-name)
  
  (let ((exec (elmame-mame-get-config 'exec))
	(rompath (elmame-mame-get-config 'rompath))
	(args (elmame-mame-get-config 'args))
	args-text)
    (if (not args)
	(setq args-text "")
      (if (stringp args)
	  (setq args-text args)
	(setq args-text
	      (string-join (mapcar (lambda (arg) (format "%s" arg)) args) " "))))
    ;; mame <machine-name> -rompath roms
    (format "%s %s -rompath %s %s" exec machine-name rompath args-text) ) )

(defun elmame-mame ()
  (interactive)
  (run-hooks 'elmame-mame-mode-hook)
  (elmame-mame-reload-user-config)
  (message "config: %s" (elmame-mame-get-user-config))
  (message "rompath: %s" (elmame-mame-get-config 'rompath))
  (let (machinelist
	column-width
	(working-dir (elmame-mame-get-config 'working-dir))
	fn-calc-width
	fn-get-width)

    (when working-dir
      (message "Swtiching to directory: %s" working-dir)
      (cd working-dir) )

    (setq machinelist (elmame-mame-list-roms))
    (setq fn-calc-width
	  (lambda (col)
	    (let (textlen-list)
	      (setq textlen-list (mapcar (lambda (x) (length (plist-get x col))) machinelist))
	      (if textlen-list (seq-max textlen-list) 0) ) ) )

    (setq fn-get-width
	  (lambda (col)
	    (message "::%s" (alist-get 'column-width elmame-mame-context))
	    (plist-get (alist-get 'column-width elmame-mame-context) col) ) )
    
    (setq column-width
	  (list 'name (funcall fn-calc-width 'name)
		'year (funcall fn-calc-width 'year)
		'manufacturer (funcall fn-calc-width 'manufacturer)
		'desc (funcall fn-calc-width 'desc) ) )
    (elmame-mame-save-context 'column-width column-width)


    (switch-to-buffer "**machine list**")
    (setq buffer-read-only nil)
    (setq truncate-lines 't)
    (erase-buffer)
    
    (mapcar (lambda (m)
	      (let ((machine-name (plist-get m 'name))
		    (year (plist-get m 'year))
		    (manufacturer (plist-get m 'manufacturer))
		    (desc (plist-get m 'desc))
		    (name-width (funcall fn-get-width 'name))
		    (year-width (funcall fn-get-width 'year))
		    (manufacturer-width (funcall fn-get-width 'manufacturer))
		    (desc-width (funcall fn-get-width 'desc))
		    )
		
		(insert-button
		 machine-name
		 'action
		 (lambda (x)
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
		
		(insert (make-string (1+ (- name-width (length machine-name))) ?\s))
		(insert year)
		(insert (make-string (1+ (- year-width (length year))) ?\s))
		(insert manufacturer)
		(insert (make-string (1+ (- manufacturer-width (length manufacturer))) ?\s))
		(insert desc)
		(insert (make-string (1+ (- desc-width (length desc))) ?\s))
		(insert "\n")
		)
	      )
	    machinelist)
    (insert "\n\n")
    (insert (propertize "Use ``M-x elmame-mame-open-config-panel'' to open config panel" 'face 'italic))
    (insert "\n" (propertize "Use ``M-x elmame-mame'' to refresh this page" 'face 'italic))
    (insert "\n")
    (beginning-of-buffer)
    (setq buffer-read-only 't) ) )


(defun elmame-mame-open-config-panel ()
  (interactive)
  (load-library "elmame-config")
  (elmame-mame-config-panel) )

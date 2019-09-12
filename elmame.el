
(let ((v-executable (if (boundp 'elmame-executable) elmame-executable "mame"))
      (v-rom-path (if (boundp 'elmame-rom-path) elmame-rom-path "./roms"))
      (v-cmd-params (if (boundp 'elmame-cmd-params) elmame-cmd-params '()))
      (v-buffer-name "*elmame*")
      v-cmd-paramhash
      v-cmd
      f-show-ui
      f-read-file-name)

  (setq elmame-context (make-hash-table))

  (setq v-cmd (format "%s" v-executable))

  ;;(message-box v-cmd)

  (setq v-cmd-paramhash (make-hash-table))

  ;; User parameters
  (mapcar #'(lambda (param) (puthash (car param) (cdr param) v-cmd-paramhash))
	  v-cmd-params)
  ;; Default parameters
  (puthash '-rompath v-rom-path v-cmd-paramhash)

  ;; Append parameters to command line
  (maphash (lambda (k v)
	     (setq v-cmd (format "%s %s" v-cmd
				 (if v (format "%s %s" k v) "")))
	     ) v-cmd-paramhash)

  (setq elmame-cmd v-cmd)


  ;; Function definition -- show user interface
  (setq f-show-ui (lambda ()
    
    ;; Close buffer
    (if (get-buffer v-buffer-name)
	(kill-buffer v-buffer-name))
    
    ;; Re-create buffer
    (get-buffer-create v-buffer-name)
    
    ;; Write file list
    (with-current-buffer v-buffer-name

      (let (f-handle-file)

	(setq f-handle-file
	  (lambda (file)
	    (if (not (member file '("." "..")))
	      (progn
		(if (not (eq (line-number-at-pos) 1)) (insert " \t\n") )
	        (insert-button file
		  'action (lambda (button1)
			    (message-box (concat elmame-cmd " file"))
			    (funcall (gethash 'f-launch elmame-context))
			  ) ) ) ) ) )

      (mapcar f-handle-file (directory-files v-rom-path)) ) )

    ;; Show the buffer just created
    (switch-to-buffer v-buffer-name)
    (setq buffer-read-only 't)
  
		    
    ) )

  ;; Function definition -- read file name from current line
  (setq f-read-file-name (lambda ()
    (let (v1-file-name)
      (save-excursion
	(setq v1-file-name
	(buffer-substring
          (progn (beginning-of-line) (point))
          (progn (search-forward ".") (left-char) (point))) ) )
      v1-file-name ) ) )
  ;; Add function definition to context
  (puthash 'f-read-file-name f-read-file-name elmame-context)

  ;; Function definition -- launch
  (setq f-launch (lambda ()
    (let (v1-cmd (v1-buffer "*elmame-output*"))
      (setq v1-cmd (concat elmame-cmd " " (funcall (gethash 'f-read-file-name elmame-context))))
      (get-buffer-create v1-buffer)
      (switch-to-buffer-other-window v1-buffer)
      (start-process-shell-command "elmame-launch" v1-buffer v1-cmd)
      ;;(message-box v1-cmd)
    ) ) )
  ;; Add function definition to context
  (puthash 'f-launch f-launch elmame-context)

  (funcall f-show-ui)
  
)

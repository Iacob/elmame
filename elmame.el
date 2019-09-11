
(let ((v-executable (if (boundp 'elmame-executable) elmame-executable "mame"))
      (v-rom-path (if (boundp 'elmame-rom-path) elmame-rom-path "./roms"))
      (v-cmd-params (if (boundp 'elmame-cmd-params) elmame-cmd-params '()))
      (v-buffer-name "*elmame*")
      v-cmd-paramhash
      v-cmd
      f-show-ui)

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
	          'action (lambda (button1) (message-box (concat elmame-cmd " " file))))
	        ) ) ) )

      (mapcar f-handle-file (directory-files v-rom-path)) ) )

    ;; Show the buffer just created
    (switch-to-buffer v-buffer-name)
  
		    
    ) )

  (funcall f-show-ui)
  
)

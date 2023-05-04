
(defun elmame-machines-extractor ()
  (let (info-data)
    (save-window-excursion
      ;;(switch-to-buffer "**mame info xml**")
      (with-temp-buffer
	(erase-buffer)
	(insert-file "../documents/local/info/mame.xml")
	(setq info-data (libxml-parse-xml-region (point-min) (point-max))) )
      
      (switch-to-buffer "**machine lines**")
      (erase-buffer)
      (insert "(defun elmame-mame-load-machine-defs () '(") ;; write function def
      (dolist (machine (seq-drop info-data 2))
	(let (manufacturer name year desc isdevice runnable isbios)
	  
	  (setq isdevice (alist-get 'isdevice (nth 1 machine)))
	  (setq runnable (alist-get 'runnable (nth 1 machine)))
	  (setq isbios (alist-get 'isbios (nth 1 machine)))

	  (when (and (not (string= isdevice "yes"))
		     ;;(not (string= runnable "yes"))
		     (not (string= runnable "no")) ;; TODO: need to test
		     (not (string= isbios "yes")))
	    
	    (setq name (alist-get 'name (nth 1 machine)))
	    
	    (setq year (assoc 'year (seq-drop machine 2)))
	    (when year
	      (setq year (nth 2 year)) )
	    
	    (setq manufacturer (assoc 'manufacturer (seq-drop machine 2)))
	    (when manufacturer
	      (setq manufacturer (nth 2 manufacturer)) )
	    
	    (setq desc (assoc 'description (seq-drop machine 2)))
	    (when desc
	      (setq desc (nth 2 desc)) )
	    (insert (format "(name %s year %s manufacturer %s desc %s)"
			    (json-encode-string name)
			    (json-encode-string year)
			    (json-encode-string manufacturer)
			    (json-encode-string desc)) "\n")
	    )
	  )
	)
      (insert ") )") ;; write function end
      )
    )
  )

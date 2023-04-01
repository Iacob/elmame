
(defun elmame-machines-extractor ()
  (let (info-data)
    (save-window-excursion
      ;;(switch-to-buffer "**mame info xml**")
      (with-temp-buffer
	(erase-buffer)
	(insert-file "../documents/info/mame.xml")
	(setq info-data (libxml-parse-xml-region (point-min) (point-max))) )
      
      (switch-to-buffer "**machine lines**")
      (erase-buffer)
      (dolist (machine (seq-drop info-data 2))
	(let (manufacturer year desc)
	  (setq year (assoc 'year (seq-drop machine 2)))
	  (when year
	    (setq year (nth 2 year)) )
	  
	  (setq manufacturer (assoc 'manufacturer (seq-drop machine 2)))
	  (when manufacturer
	    (setq manufacturer (nth 2 manufacturer)) )

	  (setq desc (assoc 'description (seq-drop machine 2)))
	  (when desc
	    (setq desc (nth 2 desc)) )
	  (insert (format "%s %s %s" year manufacturer desc) "\n")
	  )
	)
      )
    )
  )

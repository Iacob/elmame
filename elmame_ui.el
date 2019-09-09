
(let ((v-buffer-name "*elmame*"))

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
	      (insert-button
	        file
	        'action (lambda (button1) (message-box (format "%s" (line-number-at-pos)))))
	      (insert "\t\n") ) )

      (mapcar f-handle-file (directory-files "")) ) )

  ;; Show the buffer just created
  (switch-to-buffer v-buffer-name)

  
  
)

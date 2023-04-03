
(load-file "mame_machine_info_loader.el")

(defun elmame-mame-get-config (name)
  (if (boundp 'elmame-mame-config)
      (plist-get elmame-mame-config name)
    nil) )

(defun elmame-mame ()
  (interactive)
  (run-hooks 'elmame-mame-mode-hook)
  (let ((rompath (elmame-mame-get-config 'rompath))
	filelist
	machinelist
	(machinedefs (elmame-mame-load-machine-defs)))
    (when (file-directory-p rompath)
      (setq filelist (directory-files rompath nil directory-files-no-dot-files-regexp))
      (setq filelist (mapcar (lambda (x) (car (split-string x "\\."))) filelist))
      ;;(message "filelist: %s" filelist)
      (setq machinelist (mapcar (lambda (m) (seq-find (lambda (def) (string= (plist-get def 'name) m)) machinedefs)) filelist))
      (setq machinelist (seq-filter (lambda (m) m) machinelist))
      (message "machinelist: %s" machinelist)
      )
    )
  )


(add-hook 'elmame-mame-mode-hook
	  (lambda ()
	    (setq elmame-mame-config
		  '(rompath "roms2" args (-nowindow "")))
	    ))

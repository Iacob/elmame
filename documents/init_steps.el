
(add-hook 'elmame-mame-mode-hook
	  (lambda ()
	    (setq elmame-mame-config
		  '(rompath "roms2"
			    working-dir "/myhome"
			    args (-window "")))
	    ))


(let ((v-executable (if (boundp 'elmame-executable) elmame-executable "mame"))
      (v-rom-path (if (boundp 'elmame-rom-path) elmame-rom-path "./roms"))
      (v-cmd-params (if (boundp 'elmame-cmd-params) elmame-cmd-params ()))
      v-cmd)

  (setq v-cmd (format "%s -rompath %s " v-executable v-rom-path))

  (message-box v-cmd)
  
)

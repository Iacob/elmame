;;; mame.el --- A MAME front-end

;; Author: Yong <luo.yong.name@gmail.com>
;; URL: https://github.com/Iacob/elmame
;; Version: 1.0-rc1
;; Package-Requires: ((emacs "27.1"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; A MAME front-end.

;; ``M-x mame'' to start the main interface.
;; Then `mame' menu from the menu bar to open config panel or refresh the page content.

;;; Code:

(provide 'mame)

(require 'mame-machine-info-loader)
;;(load-file "mame_machine_info_loader.el")

(require 'mame-config)

(defvar mame-user-config nil "The `mame.el' user config from file.")
(defvar mame-context nil "The `mame.el' context at runtime.")

(defun mame-read-user-config ()
  "Read user config."
  (let (config-text cfg)
    (condition-case err
	(when (file-readable-p "~/.elmame-mame")
	  (with-temp-buffer
	    (insert-file-contents "~/.elmame-mame")
	    (setq config-text
		  (buffer-substring-no-properties (point-min) (point-max))))
	  (setq cfg (read config-text)))
      (error (message "Exception: %s" err) ) )
    cfg ) )

(defun mame-reload-user-config ()
  "Reload user config to variable and return it."
  (setq mame-user-config (mame-read-user-config)) )

(defun mame-get-user-config ()
  "Get user config, load it into memory if it's not loaded yet."
  (or mame-user-config (mame-reload-user-config)))

(defun mame-get-config (name)
  "Get config value with a NAME."
  (let ((default-config '(exec "mame" rompath "roms"))
	(user-config (mame-get-user-config))
	config-value)
    (if (boundp 'mame-config)
	(setq config-value (plist-get mame-config name)))
    (if (not config-value)
	(setq config-value (plist-get user-config name)))
    (if (not config-value)
	(setq config-value (plist-get default-config name)))
    config-value ) )

(defun mame-save-context (name value)
  "Save VALUE to context with key NAME."
  (if (and (boundp 'mame-context) mame-context)
      (if (assoc name mame-context)
	  (setcdr (assoc name mame-context) value)
	(nconc mame-context (cons name value)) )
    (setq mame-context (list (cons name value))) ) )

(defun mame-list-roms ()
  "Filter mame roms in rompath and return the list of valid ones."
  (let ((rompath (mame-get-config 'rompath))
	(working-dir (mame-get-config 'working-dir))
	filelist
	machinelist
	(machinedefs (mame-machine-info-loader-load)))

    (with-temp-buffer
      (when working-dir
	(message "Switching to directory: %s" working-dir)
	(cd working-dir) )
      (message "Current dir: %s" (pwd))
      (message "Current rompath: %s" rompath)
      (when (file-directory-p rompath)
	(setq filelist (directory-files rompath nil directory-files-no-dot-files-regexp))
	(setq filelist (mapcar (lambda (x) (car (split-string x "\\."))) filelist))
	;;(message "filelist: %s" filelist)
	(setq machinelist (mapcar (lambda (m) (seq-find (lambda (def) (string= (plist-get def 'name) m)) machinedefs)) filelist))
	(setq machinelist (seq-filter (lambda (m) m) machinelist)) )
      ;;(message "machinelist: %s" machinelist)
      machinelist ) ) )

(defun mame-make-shell-command (machine-name)
  "Make the shell command to start a machine with MACHINE-NAME."
  (let ((exec (mame-get-config 'exec))
	(rompath (mame-get-config 'rompath))
	(args (mame-get-config 'args))
	args-text)
    (if (not args)
	(setq args-text "")
      (if (stringp args)
	  (setq args-text args)
	(setq args-text
	      (string-join (mapcar (lambda (arg) (format "%s" arg)) args) " "))))
    ;; mame <machine-name> -rompath roms
    (format "%s %s -rompath %s %s" exec machine-name rompath args-text) ) )


;;;###autoload
(defun mame ()
  "Start MAME front-end."
  (interactive)
  (run-hooks 'mame-mode-hook)
  (mame-reload-user-config)
  (message "config: %s" (mame-get-user-config))
  (message "rompath: %s" (mame-get-config 'rompath))
  (let (machinelist
	column-width
	(working-dir (mame-get-config 'working-dir))
	fn-calc-width
	fn-get-width)

    ;; (when working-dir
    ;;   (message "Swtiching to directory: %s" working-dir)
    ;;   (cd working-dir) )

    (setq machinelist (mame-list-roms))
    (setq fn-calc-width
	  (lambda (col)
	    (let (textlen-list)
	      (setq textlen-list (mapcar (lambda (x) (length (plist-get x col))) machinelist))
	      (if textlen-list (seq-max textlen-list) 0) ) ) )

    (setq fn-get-width
	  (lambda (col)
	    (message "::%s" (alist-get 'column-width mame-context))
	    (plist-get (alist-get 'column-width mame-context) col) ) )
    
    (setq column-width
	  (list 'name (funcall fn-calc-width 'name)
		'year (funcall fn-calc-width 'year)
		'manufacturer (funcall fn-calc-width 'manufacturer)
		'desc (funcall fn-calc-width 'desc) ) )
    (mame-save-context 'column-width column-width)


    (switch-to-buffer "**machine list**")
    (setq buffer-read-only nil)
    (setq truncate-lines 't)
    (erase-buffer)

    (when working-dir
      (message "Swtiching to directory: %s" working-dir)
      (cd working-dir) )

    (if (null (current-local-map))
	(use-local-map (make-sparse-keymap "mame")) )
    (define-key (current-local-map) [menu-bar mame]
      (cons "mame" (make-sparse-keymap "mame")))
    (define-key (current-local-map) [menu-bar mame refresh]
      '("Refresh" . mame))
    (define-key (current-local-map) [menu-bar mame config]
      '("Config Panel" . mame-config-open-config-panel))

    (insert "\n" (propertize "Use mame menu from the menubar to open config panel or refresh this page." 'face 'italic) "\n\n")
    
    (mapc (lambda (m)
	      (let ((machine-name (plist-get m 'name))
		    (year (plist-get m 'year))
		    (manufacturer (plist-get m 'manufacturer))
		    (desc (plist-get m 'desc))
		    (name-width (funcall fn-get-width 'name))
		    (year-width (funcall fn-get-width 'year))
		    (manufacturer-width (funcall fn-get-width 'manufacturer))
		    (desc-width (funcall fn-get-width 'desc)) )
		
		(insert-button
		 machine-name
		 'action
		 (lambda (x)
		   (let* ((machine-name (button-get x 'machine-name))
			  (cmd-line (mame-make-shell-command machine-name)))
		     ;;(message-box cmd-line)
		     (switch-to-buffer-other-window "**mame output**")
		     
		     (insert (format "Running command: %s" cmd-line) "\n")
		     (start-process-shell-command "mame command" "**mame output**" cmd-line) ) )
		 'machine-name machine-name
		 'follow-link 't)
		
		(insert (make-string (1+ (- name-width (length machine-name))) ?\s))
		(insert year)
		(insert (make-string (1+ (- year-width (length year))) ?\s))
		(insert manufacturer)
		(insert (make-string (1+ (- manufacturer-width (length manufacturer))) ?\s))
		(insert desc)
		(insert (make-string (1+ (- desc-width (length desc))) ?\s))
		(insert "\n") ) )
	    machinelist)
    (insert "\n")
    (goto-char (point-min))
    (setq buffer-read-only 't) ) )

;;; elmame.el ends here

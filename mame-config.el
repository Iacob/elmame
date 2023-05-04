;;; elmame-config.el --- The elmame config panel

;; Author: Yong <luo.yong.name@gmail.com>
;; URL: https://github.com/Iacob/elmame
;; Version: 0.1a
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

;; elmame configuration panel


;;; Code:


(provide 'elmame-config)

(require 'widget)
(require 'wid-edit)

(require 'elmame)

(defvar elmame-config-user-config-text "" "User config file content.")
(defvar elmame-config-form '() "Form fields.")
(defvar elmame-config-formvalues (make-hash-table)  "Form values.")

;;;###autoload
(defun elmame-config-open-config-panel ()
  "Open elmame config panel."
  (interactive)
  
  (let (fn-save-config)
    (switch-to-buffer "**elmame-mame configuration**")

    (make-local-variable 'elmame-config-form)
    (make-local-variable 'elmame-config-formvalues)

    (setq fn-save-config
	  (lambda (&rest params)
	    (setq elmame-config-user-config-text "")
	    (let (config-text)
	      ;; Write values in 'value-widget' to formvalues hashmap.
	      (dolist (field elmame-config-form)
		(when (equal (plist-get field 'type) 'value-widget)
		  (puthash (plist-get field 'name)
			   (widget-value (plist-get field 'widget))
			   elmame-config-formvalues)))
	      (maphash (lambda (config-key config-value)
			 (when (> (length (or config-value "")) 0)
			   (setq elmame-config-user-config-text
			       (concat elmame-config-user-config-text
				       (format "%s " config-key)
				       (json-serialize config-value)
				       "\n"))))
		       elmame-config-formvalues)
	      (setq elmame-config-user-config-text
		    (concat "(" elmame-config-user-config-text ")"))
	      (save-window-excursion
		(with-temp-buffer
		  (message "config content to save:\n %s" elmame-config-user-config-text)
		  (insert elmame-config-user-config-text)
		  (write-file "~/.elmame-mame" 't))))))
    
    ;; (let ((inhibit-read-only 't))
    ;;   (erase-buffer))
    (insert "\n" (propertize "elmame-mame configuration" 'face 'info-title-2) "\n\n")
    (widget-create 'link
		   :notify
		   (lambda (&rest params)
		     (let (dir-name)
		       (setq dir-name
			     (read-directory-name
			      "Please select working directory: "))
		       (puthash 'working-dir dir-name
				elmame-config-formvalues)
		       (dolist (field elmame-config-form)
			 (when (equal (plist-get field 'name) 'working-dir)
			   (widget-value-set (plist-get field 'widget) dir-name)))))
		   "Select working directory")
    (insert "\n")
    (add-to-list 'elmame-config-form
		 (list 'name 'working-dir
		       'type 'widget
		       'widget (widget-create 'const :format "➥ %v" "") ) )
    (insert "\n")
    (widget-create 'link
		   :notify
		   (lambda (&rest params)
		     (let (dir-name)
		       (let (dir-name)
			 (setq dir-name
			       (read-file-name
				"Please select mame executable: "))
			 (puthash 'exec dir-name elmame-config-formvalues)
			 (dolist (field elmame-config-form)
			   (when (equal (plist-get field 'name) 'exec)
			     (widget-value-set (plist-get field 'widget)
					       dir-name) ) ) ) ) )
		   "Select mame executable")
    (insert "\n")
    (add-to-list 'elmame-config-form
		 (list 'name 'exec
		       'type 'text-widget
		       'widget (widget-create 'const :format "➥ %v" "")))
    (insert "\n")
    (widget-create 'link
		   :notify
		   (lambda (&rest params)
		     (let (dir-name)
		       (setq dir-name
			     (read-file-name
			      "Please select rom directory: "))
		       (puthash 'rompath dir-name elmame-config-formvalues)
		       (dolist (field elmame-config-form)
			 (when (equal (plist-get field 'name) 'rompath)
			   (widget-value-set (plist-get field 'widget)
					     dir-name) ) ) ) )
		   "Select rom directory")
    (insert "\n")
    (add-to-list 'elmame-config-form
		 (list 'name 'rompath
		       'type 'text-widget
		       'widget (widget-create 'const :format "➥ %v" "") ) )
    (insert "\n")
    (insert "\nextra arguments:\n")
    (add-to-list 'elmame-config-form
		 (list 'name 'args
		       'type 'value-widget
		       'widget (widget-create 'editable-field "") ) )
    
    (insert "\n")
    (widget-create 'link :notify fn-save-config "Save")
    (widget-create 'link
		   :notify (lambda (&rest params)
			     (kill-buffer (buffer-name)))
		   "Close")
    (widget-setup)

    ;; Initial values
    (let ((cfg (elmame-mame-read-user-config)))
      (message "cfg: %s" cfg)
      (when cfg
	(puthash 'working-dir (plist-get cfg 'working-dir) elmame-config-formvalues)
	(puthash 'exec (plist-get cfg 'exec) elmame-config-formvalues)
	(puthash 'rompath (plist-get cfg 'rompath) elmame-config-formvalues)
	(puthash 'args (plist-get cfg 'args) elmame-config-formvalues)

	(dolist (field elmame-config-form)
	  (let* ((widget-name (plist-get field 'name))
		 (config-value
		  (gethash widget-name elmame-config-formvalues)))
	    ;;(message "Current config: %s:%s" widget-name config-value)
	    (when config-value
	      (widget-value-set (plist-get field 'widget)
				config-value))))))))

;;; elmame-config.el ends here

;;; mame-config.el --- Config panel of mame.el

;; Author: Yong <luo.yong.name@gmail.com>
;; URL: https://github.com/Iacob/elmame
;; Version: 1.2

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

;; Configuration panel for mame.el


;;; Code:


(require 'widget)
(require 'wid-edit)

(require 'mame-base)

(defvar mame-config-user-config-text "" "User config file content.")
(defvar mame-config-form '() "Form fields.")
(defvar mame-config-formvalues (make-hash-table)  "Form values.")

;;;###autoload
(defun mame-config-open-config-panel ()
  "Open mame.el config panel."
  (interactive)
  
  (let (fn-save-config)
    (switch-to-buffer "**mame.el configuration**")

    (make-local-variable 'mame-config-form)
    (make-local-variable 'mame-config-formvalues)

    (setq fn-save-config
	  (lambda (&rest params)
	    (setq mame-config-user-config-text "")
	    (let (config-text)
	      ;; Write values in 'value-widget' to formvalues hashmap.
	      (dolist (field mame-config-form)
		(when (equal (plist-get field 'type) 'value-widget)
		  (puthash (plist-get field 'name)
			   (widget-value (plist-get field 'widget))
			   mame-config-formvalues)))
	      (maphash (lambda (config-key config-value)
			 (when (> (length (or config-value "")) 0)
			   (setq mame-config-user-config-text
			       (concat mame-config-user-config-text
				       (format "%s " config-key)
				       (json-serialize config-value)
				       "\n"))))
		       mame-config-formvalues)
	      (setq mame-config-user-config-text
		    (concat "(" mame-config-user-config-text ")"))
	      (save-window-excursion
		(with-temp-buffer
		  (message "config content to save:\n %s" mame-config-user-config-text)
		  (insert mame-config-user-config-text)
		  (write-file "~/.elmame-mame" 't))))))
    
    ;; (let ((inhibit-read-only 't))
    ;;   (erase-buffer))
    (insert "\n" (propertize "mame.el configuration" 'face 'info-title-2) "\n\n")
    (widget-create 'link
		   :notify
		   (lambda (&rest params)
		     (let (dir-name)
		       (setq dir-name
			     (read-directory-name
			      "Please select working directory: "))
		       (puthash 'working-dir dir-name
				mame-config-formvalues)
		       (dolist (field mame-config-form)
			 (when (equal (plist-get field 'name) 'working-dir)
			   (widget-value-set (plist-get field 'widget) dir-name)))))
		   "Select working directory")
    (insert "\n")
    (add-to-list 'mame-config-form
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
			 (puthash 'exec dir-name mame-config-formvalues)
			 (dolist (field mame-config-form)
			   (when (equal (plist-get field 'name) 'exec)
			     (widget-value-set (plist-get field 'widget)
					       dir-name) ) ) ) ) )
		   "Select mame executable")
    (insert "\n")
    (add-to-list 'mame-config-form
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
		       (puthash 'rompath dir-name mame-config-formvalues)
		       (dolist (field mame-config-form)
			 (when (equal (plist-get field 'name) 'rompath)
			   (widget-value-set (plist-get field 'widget)
					     dir-name) ) ) ) )
		   "Select rom directory")
    (insert "\n")
    (add-to-list 'mame-config-form
		 (list 'name 'rompath
		       'type 'text-widget
		       'widget (widget-create 'const :format "➥ %v" "") ) )
    (insert "\n")
    (insert "\nextra arguments:\n")
    (add-to-list 'mame-config-form
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
    (let ((cfg (mame-base-read-user-config)))
      (message "cfg: %s" cfg)
      (when cfg
	(puthash 'working-dir (plist-get cfg 'working-dir) mame-config-formvalues)
	(puthash 'exec (plist-get cfg 'exec) mame-config-formvalues)
	(puthash 'rompath (plist-get cfg 'rompath) mame-config-formvalues)
	(puthash 'args (plist-get cfg 'args) mame-config-formvalues)

	(dolist (field mame-config-form)
	  (let* ((widget-name (plist-get field 'name))
		 (config-value
		  (gethash widget-name mame-config-formvalues)))
	    ;;(message "Current config: %s:%s" widget-name config-value)
	    (when config-value
	      (widget-value-set (plist-get field 'widget)
				config-value))))))))



(provide 'mame-config)

;;; mame-config.el ends here

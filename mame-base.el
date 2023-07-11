;;; mame-base.el --- Base functions of mame.el

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

;; Base functions of mame.el


;;; Code:

(defun mame-base-read-user-config ()
  "Read user config."
  (let (config-text cfg)
    (condition-case err
        (when (file-readable-p "~/.elmame-mame")
          (with-temp-buffer
            (insert-file-contents "~/.elmame-mame")
            (setq config-text
                  (buffer-substring-no-properties (point-min) (point-max))))
          (setq cfg (read config-text)))
      (error (message "Exception: %s" err)))
    cfg))


(provide 'mame-base)

;;; mame-base.el ends here

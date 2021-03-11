;;; php-ui-phpactor.el --- UI support for PHP developing      -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Friends of Emacs-PHP development

;; Author: USAMI Kenta <tadsan@zonu.me>
;; Keywords: tools, files
;; URL: https://github.com/emacs-php/php-mode
;; Version: 1.24.0
;; License: GPL-3.0-or-later

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; PHP-UI implementation to integrate Phpactor (phpactor.el).

;;; Code:
(require 'phpactor)
(require 'popup nil t)

(defvar-local php-ui-phpactor-buffer nil)
(defvar-local php-ui-phpactor-hover-last-pos nil)
(defvar-local php-ui-phpactor-hover-last-msg nil)

(defvar php-ui-phpactor-timer nil
  "Timer object for execute Phpactor and display hover message.")

(defun php-ui-phpactor-hover ()
  "Show brief information about the symbol underneath the cursor."
  (interactive)
  (when php-ui-phpactor-buffer
    (if (eq (point) php-ui-phpactor-hover-last-pos)
        (when php-ui-phpactor-hover-last-msg
          (let ((msg php-ui-phpactor-hover-last-msg))
            (setq php-ui-phpactor-hover-last-msg nil)
            (popup-tip msg)))
      (setq php-ui-phpactor-hover-last-pos (point))
      (let ((main-buffer (current-buffer)))
        (phpactor--rpc-async "hover" (phpactor--command-argments :source :offset)
          (lambda (proc)
            (let* ((response (phpactor--parse-json (process-buffer proc)))
                   (msg (plist-get (plist-get response :parameters) :message)))
              (with-current-buffer main-buffer
                (setq php-ui-phpactor-hover-last-msg msg)))))))))

(defun php-ui-phpactor-activate ()
  "Activate PHP-UI using phpactor.el."
  (interactive)
  (unless php-ui-phpactor-timer
    (setq php-ui-phpactor-timer (run-with-timer 1.0 1 #'php-ui-phpactor-hover)))
  (setq php-ui-phpactor-buffer t))

(defun php-ui-phpactor-deactivate ()
  "Dectivate PHP-UI using phpactor.el."
  (interactive)
  (when php-ui-phpactor-timer
    (cancel-timer php-ui-phpactor-timer)
    (setq php-ui-phpactor-timer nil))
  (setq php-ui-phpactor-buffer nil))

(provide 'php-ui-phpactor)
;;; php-ui-phpactor.el ends here

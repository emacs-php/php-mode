;;; php-ui-phpactor.el --- UI support for PHP developing      -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Friends of Emacs-PHP development

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
(require 'phpactor nil t)
(require 'popup nil t)

(defvar-local php-ui-phpactor-buffer nil
  ".")

(defvar-local php-ui-phpactor-last-hover-pos nil
  ".")

(defvar php-ui-phpactor-timer nil
  ".")

(defun php-ui-phpactor-hover ()
  ""
  (interactive)
  (when (and php-ui-phpactor-buffer (not (eq (point) php-ui-phpactor-last-hover-pos)))
    (setq php-ui-phpactor-last-hover-pos (point))
    (popup-tip (phpactor-hover))))

;;;###autoload
(defun php-ui-phpactor-activate ()
  ""
  (interactive)
  (unless php-ui-phpactor-timer
    (setq php-ui-phpactor-timer (run-with-timer 1.0 5 #'php-ui-phpactor-hover)))
  (setq php-ui-phpactor-buffer t))

;;;###autoload
(defun php-ui-phpactor-deactivate ()
  ""
  (interactive)
  (setq php-ui-phpactor-buffer nil))

(provide 'php-ui-phpactor)
;;; php-ui-phpactor.el ends here

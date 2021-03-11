;;; php-ui.el --- UI support for PHP developing      -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Friends of Emacs-PHP development

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

;; PHP Mode integrates LSP Mode (lsp-ui), Phpactor (phpactor.el) and IDE-like tools.

;;; Code:
(eval-when-compile
  (require 'php-ui-phpactor))

(defvar php-ui-feature-alist
  '((phpactor :test (lambda () (and (require 'phpactor nil t) (featurep 'phpactor)))
              :activate php-ui-phpactor-activate
              :deactivate php-ui-phpactor-activate)
    (lsp-ui :test (lambda () (and (require 'lsp-ui nil t) (featurep 'lsp-ui)))
            :activate (lambda () (user-error "Not implemented"))
            :deactivate (lambda () (user-error "Not implemented")))))

(defvar php-ui-impl nil)

(define-minor-mode php-ui-mode
  "Minor mode for integrate IDE-like tools."
  :tag "PHP-UI"
  (let ((ui-plist (cdr-safe (assq 'phpactor php-ui-feature-alist))))
    (if php-ui-mode
        (php-ui--activate-buffer ui-plist)
      (php-ui--deactivate-buffer ui-plist))))

(defun php-ui--activate-buffer (ui-plist)
  "Activate php-ui implementation by UI-PLIST."
  (funcall (plist-get ui-plist :activate)))

(defun php-ui--deactivate-buffer (ui-plist)
  "Deactivate php-ui implementation by UI-PLIST."
  (funcall (plist-get ui-plist :deactivate)))

(provide 'php-ui)
;;; php-ui.el ends here

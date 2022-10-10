;;; php-ide-phpactor.el --- PHP-IDE feature using Phpactor RPC -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Friends of Emacs-PHP development

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

;; PHP-IDE implementation to integrate Phpactor (phpactor.el).
;; This feature depends on <https://github.com/emacs-php/phpactor.el>.

;;; Code:
(require 'phpactor nil t)
(require 'popup nil t)
(require 'smart-jump nil t)
(eval-when-compile
  (require 'cl-lib))

(defvar-local php-ide-phpactor-buffer nil)
(defvar-local php-ide-phpactor-hover-last-pos nil)
(defvar-local php-ide-phpactor-hover-last-msg nil)

(declare-function phpactor--command-argments "ext:phpactor" (&rest arg-keys))
(declare-function phpactor--parse-json "ext:phpactor" (buffer))
(declare-function phpactor--rpc-async "ext:phpactor" (action arguments callback))
(declare-function phpactor-goto-definition "ext:phpactor" ())
(declare-function popup-tip "ext:popup" (string))
(declare-function smart-jump-back "ext:smart-jump" ())
(declare-function smart-jump-go "ext:smart-jump" (&optional smart-list continue))
(declare-function smart-jump-references "ext:smart-jump" (&optional smart-list continue))

(defgroup php-ide-phpactor nil
  "UI support for PHP developing."
  :tag "PHP-IDE Phpactor"
  :prefix "php-ide-phpactor-"
  :group 'php-ide)

(defcustom php-ide-phpactor-activate-features '(all)
  "A set of Phpactor features you want to enable."
  :tag "PHP-IDE Phpactor Activate Features"
  :type '(set (const all :tag "All")
              (const hover)
              (const navigation))
  :safe (lambda (v) (and (listp v)))
  :group 'php-ide-phpactor)

(defvar php-ide-phpactor-timer nil
  "Timer object for execute Phpactor and display hover message.")

(defvar php-ide-phpactor-disable-hover-at-point-functions
  '(php-in-string-or-comment-p))

(defun php-ide-phpactor--disable-hover-at-point-p ()
  "Return non-NIL if any function return non-NIL for disable to hover at point."
  (cl-loop for f in php-ide-phpactor-disable-hover-at-point-functions
           never (not (funcall f))))

(defun php-ide-phpactor-hover ()
  "Show brief information about the symbol underneath the cursor."
  (interactive)
  (when (and php-ide-phpactor-buffer (not (php-ide-phpactor--disable-hover-at-point-p)))
    (if (eq (point) php-ide-phpactor-hover-last-pos)
        (when php-ide-phpactor-hover-last-msg
          (let ((msg php-ide-phpactor-hover-last-msg))
            (setq php-ide-phpactor-hover-last-msg nil)
            (popup-tip msg)))
      (setq php-ide-phpactor-hover-last-pos (point))
      (let ((main-buffer (current-buffer)))
        (phpactor--rpc-async "hover" (phpactor--command-argments :source :offset)
          (lambda (proc)
            (let* ((response (phpactor--parse-json (process-buffer proc)))
                   (msg (plist-get (plist-get response :parameters) :message)))
              (with-current-buffer main-buffer
                (setq php-ide-phpactor-hover-last-msg msg)))))))))

(defsubst php-ide-phpactor--feature-activated-p (feature)
  "Is FEATURE activated in `php-ide-phpactor-activate-features'."
  (or (memq 'all php-ide-phpactor-activate-features)
      (memq feature php-ide-phpactor-activate-features)))

;;;###autoload
(defun php-ide-phpactor-activate ()
  "Activate PHP-IDE using phpactor.el."
  (interactive)
  (when (php-ide-phpactor--feature-activated-p 'navigation)
    (if (not (bound-and-true-p phpactor-smart-jump-initialized))
        (local-set-key [remap xref-find-definitions] #'phpactor-goto-definition)
      (local-set-key [remap xref-find-definitions] #'smart-jump-go)
      (local-set-key [remap xref-pop-marker-stack] #'smart-jump-back)
      (local-set-key [remap xref-find-references] #'smart-jump-references)))
  (when (php-ide-phpactor--feature-activated-p 'hover)
    (unless php-ide-phpactor-timer
      (setq php-ide-phpactor-timer (run-with-timer 0.8 0.8 #'php-ide-phpactor-hover))))
  (setq php-ide-phpactor-buffer t))

;;;###autoload
(defun php-ide-phpactor-deactivate ()
  "Dectivate PHP-IDE using phpactor.el."
  (interactive)
  (local-unset-key [remap xref-find-definitions])
  (local-unset-key [remap xref-pop-marker-stack])
  (local-unset-key [remap xref-find-references])

  (when php-ide-phpactor-timer
    (cancel-timer php-ide-phpactor-timer)
    (setq php-ide-phpactor-timer nil))
  (setq php-ide-phpactor-buffer nil))

(provide 'php-ide-phpactor)
;;; php-ide-phpactor.el ends here

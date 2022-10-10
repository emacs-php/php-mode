;;; php-eglot.el --- Eglot enhancement for PHP development     -*- lexical-binding: t; -*-

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

;; PHP Mode integrates Eglot <https://github.com/joaotavora/eglot>.
;;
;; **Note**:
;; This feature is under development and experimental.
;; All of these functions, modes and terms are subject to change without notice.
;;
;; ## Configuration
;;
;; Put follows code into your .emacs (~/.emacs.d/init.el) file:
;;
;;     (add-to-list 'eglot-server-programs ((php-mode phps-mode) . php-eglot-server-program))
;;
;; ### For per project configuration
;;
;; Put follows code into .dir-locals.el in project directory:
;;
;;     ((nil (php-project-root . git)
;;           (php-eglot-executable . ("psalm-language-server"))
;;           ;; or (php-eglot-executable . ("php" "vendor/bin/path/to/server"))
;;           ))
;;
;; If you can't put .dir-locals.el in your project directory, consider the sidecar-locals package.
;;     https://melpa.org/#/sidecar-locals
;;     https://codeberg.org/ideasman42/emacs-sidecar-locals
;;

;;; Code:
(require 'cl-lib)
(require 'php-project)
(require 'eglot nil t)
(require 'phpactor nil t)

(eval-when-compile
  (defvar eglot-server-programs)
  (defvar eglot--managed-mode)
  (declare-function eglot-ensure "ext:eglot" ())
  (declare-function phpactor--find-executable "ext:phpactor" ()))

(defvar php-eglot-lsp-command-alist
  '((intelephense "intelephense" "--stdio")
    (phpactor . (lambda () (list (if (require 'phpactor nil t)
                                     (phpactor--find-executable)
                                   "phpactor"))))))

(defgroup php-eglot nil
  "Eglot PHP integration."
  :tag "PHP-Eglot"
  :prefix "php-eglot-"
  :group 'eglot
  :group 'php)

;;;###autoload
(defcustom php-eglot-executable nil
  "Command name or path to the command of Eglot LSP executable."
  :tag "Php-Eglot Executable"
  :group 'php-eglot
  :type '(choice
          (const intelephense)
          (const phpactor)
          string (repeat string))
  :safe (lambda (v) (cond
                     ((stringp v) (file-exists-p v))
                     ((listp v) (cl-every #'stringp v))
                     ((assq v php-eglot-lsp-command-alist)))))

;;;###autoload
(defun php-eglot-server-program ()
  "Return a list of command to execute LSP Server."
  (cond
   ((stringp php-eglot-executable) (list php-eglot-executable))
   ((listp php-eglot-executable) php-eglot-executable)
   ((when-let (command (assq php-eglot-executable php-eglot-lsp-command-alist))
      (cond
       ((functionp command) (funcall command))
       ((listp command) command))))))

(defun php-eglot-ensure ()
  "Start PHP-specific Eglot session for current buffer if there isn't one."
  (when-let (server-program (php-eglot-server-program))
    (setq-local eglot-server-programs (list (cons major-mode server-program))))
  (setq-local project-find-functions (list #'php-project-project-find-function))
  (add-function :after (local 'eglot--managed-mode) #'php-eglot--eglot--managed-mode-after)
  (eglot-ensure))

(defun php-eglot--eglot--managed-mode-after (&optional _arg)
  "Rollback variables when turning off `eglot--managed-mode'."
  (unless eglot--managed-mode
    (setq-local eglot-server-programs (default-value 'eglot-server-programs))))

(provide 'php-eglot)
;;; php-eglot.el ends here

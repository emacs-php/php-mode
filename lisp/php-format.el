;;; php-format.el --- Code reformatter for PHP buffer  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Friends of Emacs-PHP development

;; Author: USAMI Kenta <tadsan@zonu.me>
;; Created: 5 Mar 2023
;; Version: 0.1.0
;; Keywords: tools, php
;; URL: https://github.com/emacs-php/php-mode.el
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

;; This feature is for execute PHP code formatting tools.

;; ## Supported tools:
;;
;; - Easy Coding Standard (ecs) https://github.com/easy-coding-standard/easy-coding-standard
;; - PHP-CS-Fixer (php-cs-fixer) https://github.com/PHP-CS-Fixer/PHP-CS-Fixer
;; - PHP_CodeSniffer (phpcbf) https://github.com/squizlabs/PHP_CodeSniffer
;;
;; It supports both per-project and globally installed ones.
;;
;; ## How to use
;;
;; Add following line to setup function for php-mode.
;;
;;     (php-format-auto-mode +1)
;;
;; ## Customization
;;
;; These variables can be set either by dir-locals.el or by custom-set-variable.
;;
;; - php-format-auto-mode-hook-depth
;; - php-format-command
;; - php-format-command-dir
;; - php-format-default-idle-time
;; - php-format-result-display-method-alist
;;
;; ## Display methods
;;
;; How formatting is performed and how the results are displayed can be controlled
;; by the following keywords.
;;
;; - idle: Asynchronously apply formatting to idle time in Emacs using `run-with-idle-timer'
;; - async: Immediately execute an asynchronous process to apply formatting
;; - compile: Apply formatting using the compile command.  Doesn't lock, but results pop up
;; - silent: Apply formatting immediately and synchronously.
;;   No message is displayed, but Emacs is locked while it is being processed.
;; - nil: Apply formatting immediately and synchronously.
;;   Emacs will be locked until formatting is done and the result will pop up.
;;

;;; Code:
(require 'cl-lib)
(require 'php-project)

(defvar php-format-formatter-alist
  '((ecs :marker ("ecs.php")
         :command ("ecs" "check" "--fix" "--no-progress-bar" "--"))
    (php-cs-fixer :marker (".php-cs-fixer.dist.php" ".php-cs-fixer.php")
                  :command ("php-cs-fixer" "fix" "--show-progress=none"))
    (phpcbf :marker ("phpcs.xml.dist" "phpcs.xml")
            :command ("phpcbf"))))

(defvar php-format-lighter " phpf")
(defvar php-format-output-buffer " *PHP Format*")
(defvar php-format--exec-method nil)
(defvar php-format--idle-timer nil)

;; Customize variables
(defgroup php-format nil
  "Apply code reformat."
  :tag "PHP Format"
  :group 'php)

(defcustom php-format-auto-mode-hook-depth -50
  "A depth number in the range -100..100 for `add-hook'."
  :tag "PHP Format Auto Mode Hook Depth"
  :type 'integer
  :safe #'integerp
  :group 'php)

(defcustom php-format-command 'auto
  "A formatter symbol, or a list of command and arguments."
  :tag "PHP Format Command"
  :type '(choice (const nil :tag "Disabled reformat codes")
                 (const 'auto :tag "Auto")
                 (const 'ecs :tag "Easy Coding Standard")
                 (const 'php-cs-fixer :tag "PHP-CS-Fixer")
                 (const 'phpcbf :tag "PHP Code Beautifier and Fixer")
                 (repeat string :tag "Command and arguments"))
  :safe (lambda (v) (or (symbolp v) (listp v)))
  :group 'php-format)

(defcustom php-format-command-dir "vendor/bin"
  "A relative path to the directory where formatting tool is installed."
  :tag "PHP Format Command"
  :type 'string
  :safe #'stringp
  :group 'php-format)

(defcustom php-format-default-idle-time 3
  "Number of seconds to wait idle before formatting."
  :tag "PHP Format Auto Mode Hook Depth"
  :type 'integer
  :safe #'integerp
  :group 'php)

(defcustom php-format-result-display-method-alist '((php-format-on-after-save-hook . idle)
                                          (php-format-this-buffer-file . silent)
                                          (php-format-project . compile))
  "An alist of misplay the result method of the formatting process."
  :tag "PHP Format Result Display Method"
  :type '(alist :key-type function
                :value-type symbol)
  :group 'php-format)

;; Internal functions
(defsubst php-format--register-timer (sec command-args)
  "Register idle-timer with SEC and COMMAND-ARGS."
  (unless php-format--idle-timer
    (setq php-format--idle-timer
          (run-with-idle-timer sec nil #'php-format--execute-delayed-format
                               default-directory command-args))))

(defun php-format--execute-format (files)
  "Execute PHP formatter with FILES."
  (let* ((default-directory (php-project-get-root-dir))
         (command-args (php-format--get-command-args))
         command-line)
    (when (null command-args)
      (user-error "No available PHP formatter settings detected"))
    (setq command-args (append command-args files))
    (setq command-line (mapconcat #'shell-quote-argument command-args " "))
    (pcase php-format--exec-method
      (`(idle ,sec) (php-format--register-timer sec command-args))
      ('idle (php-format--register-timer php-format-default-idle-time command-args))
      ('async (apply #'call-process-shell-command (car command-args) nil nil nil
                     (append (cdr command-args) (list "&"))))
      ('compile (compile command-line))
      ('silent (shell-command-to-string command-line))
      ('nil (shell-command command-line))
      (_ (user-error "`%s' is unexpected php-format--exec-method" php-format--exec-method)))))

(defun php-format--get-command-args ()
  "Return a list of command and arguments."
  (if (listp php-format-command)
      php-format-command
    (let ((cmd php-format-command)
          args executable vendor)
      (when (eq 'auto cmd)
        (setq cmd (cl-loop for (sym . plist) in php-format-formatter-alist
                           for files = (plist-get plist :marker)
                           if (cl-find-if
                               (lambda (file) (file-exists-p (expand-file-name file default-directory)))
                               files)
                           return sym))
        (setq-local php-format-command cmd))
      (when-let (tup (plist-get (cdr-safe (assq cmd php-format-formatter-alist)) :command))
        (setq executable (car tup))
        (setq args (cdr tup))
        (setq vendor (expand-file-name executable (expand-file-name php-format-command-dir default-directory)))
        (cond
         ((file-exists-p vendor) (cons vendor args))
         ((executable-find executable) (cons executable args)))))))

(defun php-format--execute-delayed-format (dir command-args)
  "Asynchronously execute PHP format with COMMAND-ARGS in DIR."
  (setq php-format--idle-timer nil)
  (let ((default-directory dir))
    (apply #'call-process-shell-command (car command-args) nil nil nil
           (append (cdr command-args) (list "&")))))

;; Public functions and minor mode

;;;###autoload
(defun php-format-this-buffer-file ()
  "Apply format this buffer file."
  (interactive)
  (when php-format-command
    (when (null buffer-file-name)
      (user-error "This file has not yet been saved"))
    (when (file-remote-p buffer-file-name)
      (user-error "PHP Format feature does not yet support remote files"))
    (let ((php-format--exec-method (cdr-safe (assq 'php-format-this-buffer-file php-format-result-display-method-alist))))
      (php-format--execute-format (list buffer-file-name)))))

;;;###autoload
(defun php-format-project ()
  "Apply format this buffer file."
  (interactive)
  (unless php-format-command
    (user-error "Disabled `php-format-command' in this project"))
  (let ((php-format--exec-method (cdr-safe (assq 'php-format-project php-format-result-display-method-alist))))
    (php-format--execute-format nil)))

;;;###autoload
(defun php-format-on-after-save-hook ()
  "Apply format on after save hook."
  (when (and php-format-command buffer-file-name (not (file-remote-p buffer-file-name)))
    (let ((php-format--exec-method (cdr-safe (assq 'php-format-on-after-save-hook php-format-result-display-method-alist))))
    (php-format--execute-format nil))))

;;;###autoload
(define-minor-mode php-format-auto-mode
  "Automatically apply formatting when saving an edited file."
  :group 'php-format
  :lighter php-format-lighter
  (if php-format-auto-mode
      (add-hook 'after-save-hook 'php-format-on-after-save-hook php-format-auto-mode-hook-depth t)
    (remove-hook 'after-save-hook 'php-format-on-after-save-hook t)))

(provide 'php-format)
;;; php-format.el ends here

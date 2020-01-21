;;; php-project.el --- Project support for PHP application  -*- lexical-binding: t; -*-

;; Copyright (C) 2018-2019  Friends of Emacs-PHP development

;; Author: USAMI Kenta <tadsan@zonu.me>
;; Keywords: tools, files
;; URL: https://github.com/emacs-php/php-mode
;; Version: 1.22.2
;; Package-Requires: ((emacs "24.3"))
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

;; Define project specific functions and variables for PHP application.
;;
;; ## API
;;
;; ### `php-project-get-root-dir()'
;;
;; Return root directory of current buffer file.  The root directory is
;; determined by several marker file or directory.
;;
;; ### `php-project-get-bootstrap-scripts()'
;;
;; Return list of path to bootstrap script file.
;;
;; ### `php-project-get-php-executable()'
;;
;; Return path to PHP executable file with the project settings overriding.
;;
;; ### `php-project-get-phan-executable()'
;;
;; Return path to Phan executable file with the project settings overriding.
;; Phan is a static analyzer and LSP server implementation for PHP.
;; See https://github.com/phan/phan
;;
;; ## `.dir-locals.el' support
;;
;; - `php-project-coding-style'
;;   - Symbol value of the coding style.  (ex.  `pear', `psr2')
;; - `php-project-root'
;;   - Symbol of marker file of project root.  (ex.  `git', `composer')
;;   - Full path to project root directory.  (ex.  "/path/to/your-project")
;; - `php-project-bootstrap-scripts'
;;   - List of path to bootstrap file of project.
;;     (ex.  (((root . "vendor/autoload.php") (root . "inc/bootstrap.php")))
;; - `php-project-php-executable'
;;   - Path to project specific PHP executable file.
;;   - If you want to use a file different from the system wide `php' command.
;; - `php-project-phan-executable'
;;   - Path to project specific Phan executable file.
;;   - When not specified explicitly, it is automatically searched from
;;     Composer's dependency of the project and `exec-path'.
;;

;;; Code:
(require 'cl-lib)
(require 'projectile nil t)

;; Constants
(defconst php-project-composer-autoloader "vendor/autoload.php")

;; Custom variables
(defgroup php-project nil
  "Major mode for editing PHP code."
  :tag "PHP Project"
  :prefix "php-project-"
  :group 'php)

(defcustom php-project-auto-detect-etags-file nil
  "If `T', automatically detect etags file when file is opened."
  :tag "PHP Project Auto Detect Etags File"
  :group 'php-project
  :type 'boolean)

(defcustom php-project-use-projectile-to-detect-root nil
  "If `T' and projectile-mode is activated, use Projectile for root detection."
  :tag "PHP Project Use Projectile To Detect Root"
  :group 'php-project
  :type 'boolean)

;; Variables
(defvar php-project-available-root-files
  '((projectile ".projectile")
    (composer   "composer.json" "composer.lock")
    (git        ".git")
    (mercurial  ".hg")
    (subversion ".svn")
    ;; NOTICE: This method does not detect the top level of .editorconfig
    ;;         However, we can integrate it by adding the editorconfig.el's API.
    ;;(editorconfig . ".editorconfig")
    ))

;; Buffer local variables

;;;###autoload
(progn
  (defvar-local php-project-root 'auto
    "Method of searching for the top level directory.

`auto' (default)
      Try to search file in order of `php-project-available-root-files'.

SYMBOL
      Key of `php-project-available-root-files'.

STRING
      A file/directory name of top level marker.
      If the string is an actual directory path, it is set as the absolute path
      of the root directory, not the marker.")
  (put 'php-project-root 'safe-local-variable
       #'(lambda (v) (or (stringp v) (assq v php-project-available-root-files))))

  (defvar-local php-project-etags-file nil)
  (put 'php-project-etags-file 'safe-local-variable
       #'(lambda (v) (or (functionp v)
                         (eq v t)
                         (php-project--eval-bootstrap-scripts v))))

  (defvar-local php-project-bootstrap-scripts nil
    "List of path to bootstrap php script file.

The ideal bootstrap file is silent, it only includes dependent files,
defines constants, and sets the class loaders.")
  (put 'php-project-bootstrap-scripts 'safe-local-variable #'php-project--eval-bootstrap-scripts)

  (defvar-local php-project-php-executable nil
    "Path to php executable file.")
  (put 'php-project-php-executable 'safe-local-variable
       #'(lambda (v) (and (stringp v) (file-executable-p v))))

  (defvar-local php-project-phan-executable nil
    "Path to phan executable file.")
  (put 'php-project-phan-executable 'safe-local-variable #'php-project--eval-bootstrap-scripts)

  (defvar-local php-project-coding-style nil
    "Symbol value of the coding style of the project that PHP major mode refers to.

Typically it is `pear', `drupal', `wordpress', `symfony2' and `psr2'.")
  (put 'php-project-coding-style 'safe-local-variable #'symbolp)

  (defvar-local php-project-php-file-as-template 'auto
    "
`auto' (default)
      Automatically switch to mode for template when HTML tag detected in file.

`t'
      Switch all PHP files in that directory to mode for HTML template.

`nil'
      Any .php  in that directory is just a PHP script.

\(\(PATTERN . SYMBOL))
      Alist of file name pattern regular expressions and the above symbol pairs.
      PATTERN is regexp pattern.
")
  (put 'php-project-php-file-as-template 'safe-local-variable #'php-project--validate-php-file-as-template)

  (defvar-local php-project-repl nil
    "Function name or path to REPL (interactive shell) script.")
  (put 'php-project-repl 'safe-local-variable
       #'(lambda (v) (or (functionp v)
                         (php-project--eval-bootstrap-scripts v))))

  (defvar-local php-project-unit-test nil
    "Function name or path to unit test script.")
  (put 'php-project-unit-test 'safe-local-variable
       #'(lambda (v) (or (functionp v)
                         (php-project--eval-bootstrap-scripts v))))

  (defvar-local php-project-deploy nil
    "Function name or path to deploy script.")
  (put 'php-project-deploy 'safe-local-variable
       #'(lambda (v) (or (functionp v)
                         (php-project--eval-bootstrap-scripts v))))

  (defvar-local php-project-build nil
    "Function name or path to build script.")
  (put 'php-project-build 'safe-local-variable
       #'(lambda (v) (or (functionp v)
                         (php-project--eval-bootstrap-scripts v))))

  (defvar-local php-project-server-start nil
    "Function name or path to server-start script.")
  (put 'php-project-server-start 'safe-local-variable
       #'(lambda (v) (or (functionp v)
                         (php-project--eval-bootstrap-scripts v)))))

;; Functions
(defun php-project--validate-php-file-as-template (val)
  "Return T when `VAL' is valid list of safe ."
  (cond
   ((null val) t)
   ((memq val '(t auto)) t)
   ((listp val)
    (cl-loop for v in val
             always (and (consp v)
                         (stringp (car v))
                         (php-project--validate-php-file-as-template (cdr v)))))
   (t nil)))

(defun php-project--eval-bootstrap-scripts (val)
  "Return T when `VAL' is valid list of safe bootstrap php script."
  (cond
   ((stringp val) (and (file-exists-p val) val))
   ((eq 'composer val)
    (let ((path (expand-file-name php-project-composer-autoloader (php-project-get-root-dir))))
      (and (file-exists-p path) path)))
   ((and (consp val) (eq 'root (car val)) (stringp (cdr val)))
    (let ((path (expand-file-name (cdr val) (php-project-get-root-dir))))
      (and (file-exists-p path) path)))
   ((null val) nil)
   ((listp val)
    (cl-loop for v in val collect (php-project--eval-bootstrap-scripts v)))
   (t nil)))

(defun php-project-get-php-executable ()
  "Return path to PHP executable file."
  (cond
   ((and (stringp php-project-php-executable)
         (file-executable-p php-project-php-executable))
    php-project-php-executable)
   ((boundp 'php-executable) php-executable)
   (t (executable-find "php"))))

(defun php-project-get-phan-executable ()
  "Return path to phan executable file."
  (or (car-safe (php-project--eval-bootstrap-scripts
                 (list php-project-phan-executable
                       (cons 'root "vendor/bin/phan"))))
      (executable-find "phan")))

(defun php-project-get-file-html-template-type (filename)
  "Return symbol T, NIL or `auto' by `FILENAME'."
  (cond
   ((not php-project-php-file-as-template) nil)
   ((eq t php-project-php-file-as-template) t)
   ((eq 'auto php-project-php-file-as-template) 'auto)
   ((listp php-project-php-file-as-template)
    (assoc-default filename php-project-php-file-as-template #'string-match-p))
   (t (prog1 nil
        (warn "php-project-php-file-as-template is unexpected format")))))

(defun php-project-apply-local-variables ()
  "Apply php-project variables to local variables."
  (when (null tags-file-name)
    (when (or (and php-project-auto-detect-etags-file
                   (null php-project-etags-file))
              (eq php-project-etags-file t))
      (let ((tags-file (expand-file-name "TAGS" (php-project-get-root-dir))))
        (when (file-exists-p tags-file)
          (setq-local php-project-etags-file tags-file))))
    (when php-project-etags-file
      (setq-local tags-file-name (php-project--eval-bootstrap-scripts php-project-etags-file)))))
;;;###autoload
(defun php-project-get-bootstrap-scripts ()
  "Return list of bootstrap script."
  (let ((scripts (php-project--eval-bootstrap-scripts php-project-bootstrap-scripts)))
    (if (stringp scripts) (list scripts) scripts)))

;;;###autoload
(defun php-project-get-root-dir ()
  "Return path to current PHP project."
  (if (and (stringp php-project-root) (file-directory-p php-project-root))
      php-project-root
    (php-project--detect-root-dir)))

(defun php-project--detect-root-dir ()
  "Return detected project root."
  (if (and php-project-use-projectile-to-detect-root
           (bound-and-true-p projectile-mode)
           (fboundp 'projectile-project-root))
      (projectile-project-root default-directory)
    (let ((detect-method
           (cond
            ((stringp php-project-root) (list php-project-root))
            ((eq php-project-root 'auto)
             (cl-loop for m in php-project-available-root-files
                      append (cdr m)))
            (t (cdr-safe (assq php-project-root php-project-available-root-files))))))
      (cl-loop for m in detect-method
               thereis (locate-dominating-file default-directory m)))))

(provide 'php-project)
;;; php-project.el ends here

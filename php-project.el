;;; php-project.el --- Project support for PHP application  -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Friends of Emacs-PHP development

;; Author: USAMI Kenta <tadsan@zonu.me>
;; Keywords: tools, files
;; URL: https://github.com/ejmr/php-mode
;; Version: 1.19.0
;; Package-Requires: ((emacs "24") (cl-lib "0.5"))
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
;;

;;; Code:
(require 'cl-lib)

;; Constants
(defconst php-project-composer-autoloader "vendor/autoload.php")

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
  (defvar php-project-root 'auto
    "Method of searching for the top level directory.

`auto' (default)
      Try to search file in order of `php-project-available-root-files'.

SYMBOL
      Key of `php-project-available-root-files'.")
  (make-variable-buffer-local 'php-project-root)
  (put 'php-project-root 'safe-local-variable
       #'(lambda (v) (assq v php-project-available-root-files))))

;;;###autoload
(progn
  (defvar php-project-bootstrap-scripts nil
    "List of path to bootstrap php script file.

The ideal bootstrap file is silent, it only includes dependent files,
defines constants, and sets the class loaders.")
  (make-variable-buffer-local 'php-project-bootstrap-scripts)
  (put 'php-project-bootstrap-scripts 'safe-local-variable #'php-project--eval-bootstrap-scripts))

;;;###autoload
(progn
  (defvar php-project-coding-style nil
    "Symbol value of the coding style of the project that PHP major mode refers to.

Typically it is `pear', `drupal', `wordpress', `symfony2' and `psr2'.")
  (make-variable-buffer-local 'php-project-coding-style)
  (put 'php-project-coding-style 'safe-local-variable #'symbolp))


;; Functions

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

;;;###autoload
(defun php-project-get-bootstrap-scripts ()
  "Return list of bootstrap script."
  (let ((scripts (php-project--eval-bootstrap-scripts php-project-bootstrap-scripts)))
    (if (stringp scripts) (list scripts) scripts)))

;;;###autoload
(defun php-project-get-root-dir ()
  "Return path to current PHP project."
  (let ((detect-method
         (cond
          ((stringp php-project-root) (list php-project-root))
          ((eq php-project-root 'auto)
           (cl-loop for m in php-project-available-root-files
                    append (cdr m)))
          (t (cdr-safe (assq php-project-root php-project-available-root-files))))))
    (cl-loop for m in detect-method
             thereis (locate-dominating-file default-directory m))))

(provide 'php-project)
;;; php-project.el ends here

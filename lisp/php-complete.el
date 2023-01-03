;;; php-complete.el --- PHP auto-compiletion functions         -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Friends of Emacs-PHP development
;; Copyright (C) 2021, 2022  Free Software Foundation, Inc.

;; Author: USAMI Kenta <tadsan@zonu.me>

;; Created: 18 Sep 2022
;; Version: 1.24.2
;; Keywords: languages, php

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

;; Provide auto-compiletion functions.

;; These functions are copied function from GNU ELPA.
;;
;; - cape--table-with-properties (cape.el)
;; - cape--bounds (cape.el)
;; - cape--interactive (cape.el)

;;; Code:
(eval-when-compile
  (require 'cl-lib))
(require 'php)
(require 'php-defs)

;;;###autoload
(defgroup php-complete nil
  "Auto completion for PHP edition."
  :tag "PHP Completion"
  :group 'php-mode)

;;;###autoload
(defcustom php-complete-function-modules '(bcmath core gmp libxml intl mbstring pcntl posix sodium xml xmlwriter)
  "Module names for function names completion."
  :tag "PHP Complete Function Modules"
  :type (eval-when-compile `(set ,@(mapcar (lambda (elm) (list 'const (car elm)))
                                           php-defs-functions-alist)))
  :safe (lambda (value) (and (listp value) (cl-loop for v in values
                                                    always (assq v php-defs-functions-alist))))
  :group 'php-complete)

;;; Cape functions:

;; These functions are copied from cape.el package.  https://github.com/minad/cape
;; Thanks to original author Daniel Mendler (@minad)

(cl-defun php-complete--cape-table-with-properties (table &key category (sort t) &allow-other-keys)
  "Create completion TABLE with properties.
CATEGORY is the optional completion category.
SORT should be nil to disable sorting."
  (if (or (not table) (and (not category) sort))
      table
    (let ((metadata `(metadata
                      ,@(and category `((category . ,category)))
                      ,@(and (not sort) '((display-sort-function . identity)
                                          (cycle-sort-function . identity))))))
      (lambda (str pred action)
        (if (eq action 'metadata)
            metadata
          (complete-with-action action table str pred))))))

(defun php-complete--cape-bounds (thing)
  "Return bounds of THING."
  (or (bounds-of-thing-at-point thing) (cons (point) (point))))

(defun php-complete--cape-interactive (capf)
  "Complete with CAPF."
  (let ((completion-at-point-functions (list capf)))
    (or (completion-at-point) (user-error "%s: No completions" capf))))

;;; Variables:
(defvar php-complete--functions-cache (make-hash-table :test #'equal))

;;; Data source functions:
(defun php-complete--functions ()
  "Return PHP function names."
  (let* ((modules (sort php-complete-function-modules #'string<))
         (functions (gethash modules php-complete--functions-cache)))
    (unless functions
      (setq functions (sort (cl-loop for module in modules
                                     append (assq module php-defs-functions-alist))
                            #'string<))
      (puthash modules functions php-complete--functions-cache))
    functions))

;;; Compiletion function:

;;;###autoload
(defun php-complete-complete-function (&optional interactive)
  "Complete PHP keyword at point.

If INTERACTIVE is nil the function acts like a capf."
  (interactive (list t))
  (if interactive
      (php-complete--cape-interactive #'php-complete-complete-function)
    (let ((bounds (php-complete--cape-bounds 'symbol))
          (tokens (nreverse (php-leading-tokens 2))))
      `(,(car bounds) ,(cdr bounds)
        ,(php-complete--cape-table-with-properties
          (unless (or (member (nth 0 tokens) '("->" "::"))
                      (string-prefix-p "$" (nth 1 tokens)))
            (php-complete--functions))
          :category 'cape-keyword)
        :annotation-function (lambda (_) " PHP functions")
        :company-kind (lambda (_) 'keyword)
        :exclusive 'no))))

(provide 'php-complete)
;;; php-complete.el ends here

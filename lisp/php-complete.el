;;; php-complete.el --- PHP auto-compiletion functions         -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Friends of Emacs-PHP development
;; Copyright (C) 2021, 2022  Free Software Foundation, Inc.

;; Author: USAMI Kenta <tadsan@zonu.me>

;; Created: 18 Sep 2022
;; Version: 1.26.1
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

;; php-complete.el provides a small collection of dependency-light,
;; offline completion-at-point functions (capfs) for PHP.  It is not an
;; LSP replacement; it targets the cases an LSP server cannot or will not
;; handle well.  Each capf is a small, independent unit usable both as an
;; `M-x' command and as a building block composed with `cape-capf-super':
;;
;; - `php-complete-complete-function' -- built-in function name source.
;; - `php-complete-complete-path'     -- filesystem path inside the
;;                                       `__DIR__ . '/...'' idiom.
;;
;; Key-driven insertion (e.g. a context-sensitive "." key via smartchr)
;; is intentionally kept out of this file; it belongs to the orthogonal
;; primitive `php-dot-context'.  Both share the same notion of "string"
;; and "magic constant", so insertion and completion stay consistent.

;; The following helpers are copied from cape.el on GNU ELPA; thanks to
;; the original author Daniel Mendler (@minad).
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
                                                    always (assq v php-defs-functions-alist)))))

;;;###autoload
(defcustom php-complete-path-dir-constants '("__DIR__")
  "Magic constants treated as the current file directory for path completion.

This is the directory-valued subset of `php-magical-constants' used by
`php-complete-complete-path'.  Only constants that resolve to a directory
belong here: \"__DIR__\" qualifies, whereas \"__FILE__\" (a file) does not."
  :tag "PHP Complete Path Dir Constants"
  :type '(repeat string)
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
  "Complete a PHP built-in function name at point.

This is the offline built-in function-name source: it offers names from
the modules listed in `php-complete-function-modules', and is meant for
environments without an LSP server.  It does not fire after `->' or `::',
nor after a variable, so it only suggests where a bare function call makes
sense.

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

;;; Path completion rooted at `__DIR__':

(defun php-complete--path-directory ()
  "Return the directory that `__DIR__' resolves to for the current buffer."
  (or (and buffer-file-name (file-name-directory buffer-file-name))
      default-directory))

(defun php-complete--path-string-bounds ()
  "Return (CONTENT-BEG . STR-END) when point is inside a `__DIR__ . STRING'.

CONTENT-BEG is placed after the opening quote and a single leading slash,
so the path is completed relative to the directory of the current file.
Return nil when point is not inside such a string.

The recognized directory-valued constants are held in
`php-complete-path-dir-constants', a subset of `php-magical-constants',
so this shares its notion of \"magic constant\" with `php-dot-context'."
  (when (php-in-string-p)
    ;; Take the string start from `syntax-ppss' (nth 8): it is reliable even
    ;; for the unterminated string that is normal while typing
    ;; ("__DIR__ . '/" before the closing quote exists).  Match the preceding
    ;; "CONST ." with `looking-back' rather than the token scanner, which is
    ;; not meant to be entered from the opening-quote position.
    (let ((str-beg (nth 8 (syntax-ppss))))
      (when (save-excursion
              (goto-char str-beg)
              (looking-back
               (concat (regexp-opt php-complete-path-dir-constants 'symbols)
                       "[ \t\r\n]*\\.[ \t\r\n]*")
               (max (point-min) (- str-beg 120))))
        (let ((content-beg (1+ str-beg)))
          ;; Keep a single leading "/" fixed so it stays a separator and the
          ;; path resolves relative to `__DIR__' instead of the filesystem root.
          (when (eq (char-after content-beg) ?/)
            (setq content-beg (1+ content-beg)))
          (cons content-beg
                (or (ignore-errors
                      (save-excursion (goto-char str-beg) (forward-sexp) (point)))
                    (point))))))))

(defun php-complete--path-table ()
  "Return a file-name completion table rooted at the current file directory.
The directory is what `__DIR__' resolves to at runtime, bound when the
table is called rather than captured from the buffer `default-directory'."
  (let ((dir (php-complete--path-directory)))
    (lambda (string pred action)
      (let ((default-directory dir)
            (non-essential t))
        (read-file-name-internal string pred action)))))

;;;###autoload
(defun php-complete-complete-path (&optional interactive)
  "Complete a filesystem path written as `__DIR__ . \\='/...\\='.'

Inside the string of `__DIR__ . \\='/PATH\\='', complete PATH from the
directory of the current file, one path component at a time.  This is the
completion half of the `__DIR__' path idiom; inserting the leading
`. \\='/\\='' is left to the editor (see `php-dot-context' and the smartchr
recipe in the README), keeping key-driven insertion and
completion-at-point orthogonal but consistent.

If INTERACTIVE is nil the function acts like a capf."
  (interactive (list t))
  (if interactive
      (php-complete--cape-interactive #'php-complete-complete-path)
    (when-let* ((bounds (php-complete--path-string-bounds)))
      `(,(min (car bounds) (point)) ,(point)
        ,(php-complete--path-table)
        :annotation-function ,(lambda (_) " __DIR__ path")
        :company-kind ,(lambda (_) 'file)
        :exclusive no))))

(provide 'php-complete)
;;; php-complete.el ends here

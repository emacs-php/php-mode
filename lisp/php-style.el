;;; php-style.el --- Coding style management for PHP Mode  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Friends of Emacs-PHP development

;; Author: USAMI Kenta <tadsan@zonu.me>
;; Keywords: languages, php
;; Homepage: https://github.com/emacs-php/php-mode
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

;; This file provides a self-contained, variable-based coding style
;; mechanism for the cc-mode independent `php-mode'.  Unlike the legacy
;; `php-cc-mode', styles here are plain alists of (VARIABLE . VALUE) pairs
;; applied as buffer-local variables; there is no dependency on CC Mode's
;; `c-add-style' / `c-set-style' machinery.
;;
;; The indentation variables referenced by the styles below
;; (`php-indent-offset' and `php-indent-switch-case-offset') are
;; defined in php-indent.el, which is
;; implemented in a parallel task.  They are forward-declared with
;; `defvar' here so that this file byte-compiles cleanly whether or not
;; php-indent.el has been loaded yet.

;;; Code:

(require 'php)

;; Forward declarations for variables owned by php-indent.el (implemented
;; separately).  These `defvar' forms exist only to keep the byte
;; compiler quiet; php-indent.el is the canonical definition site.
(defvar php-indent-offset)
(defvar php-indent-switch-case-offset)

;; Forward declaration for the shared coding-style option.  In the target
;; design this lives in php.el; while php-style.el is dormant scaffolding
;; it is only referenced, never defined, here.
(defvar php-mode-coding-style)

;;; Style definitions

(defvar php-style-alist
  '(("php" . ((php-indent-offset . 4)
              (indent-tabs-mode . nil)
              (tab-width . 4)
              (php-indent-switch-case-offset . 4)))
    ("per" . ((php-style-parent . "php")
              (fill-column . 120)
              (show-trailing-whitespace . t)
              (php-style-delete-trailing-whitespace . t)))
    ("psr2" . ((php-style-parent . "per")
               (fill-column . 78)))
    ("pear" . ((php-style-parent . "php")
               (php-indent-switch-case-offset . 0)
               (show-trailing-whitespace . nil)
               (php-style-delete-trailing-whitespace . nil)))
    ("drupal" . ((php-style-parent . "php")
                 (php-indent-offset . 2)
                 (tab-width . 2)
                 (fill-column . 78)
                 (show-trailing-whitespace . t)
                 (php-style-delete-trailing-whitespace . t)))
    ("wordpress" . ((php-style-parent . "php")
                    (indent-tabs-mode . t)
                    (fill-column . 78)
                    (show-trailing-whitespace . nil)
                    (php-style-delete-trailing-whitespace . nil))))
  "Alist of PHP Mode coding styles.

Each element is (STYLE-NAME . VARIABLE-ALIST) where STYLE-NAME is a
string and VARIABLE-ALIST is an alist of (VARIABLE . VALUE) pairs to be
set buffer-locally by `php-set-style'.

A style may inherit from another style with a single level of
inheritance by including a (php-style-parent . PARENT-NAME) entry;
`php-style--resolve' merges the parent's variables underneath the
child's own, with the child's values taking precedence.")

(defun php-style--resolve (stylename)
  "Resolve STYLENAME in `php-style-alist' into a flat variable alist.

Follows a single `php-style-parent' inheritance link (if present),
merging the parent's resolved alist underneath STYLENAME's own
entries so that the child's values win.  The pseudo-variable
`php-style-parent' itself is stripped from the result.

Signal an error if STYLENAME is not present in `php-style-alist'."
  (let* ((entry (or (cdr (assoc stylename php-style-alist))
                     (error "Undefined PHP coding style: %s" stylename)))
         (parent (cdr (assq 'php-style-parent entry)))
         (own (assq-delete-all 'php-style-parent (copy-alist entry)))
         (parent-alist (if (stringp parent) (php-style--resolve parent) nil)))
    (append own
            (seq-remove (lambda (pair) (assq (car pair) own)) parent-alist))))

;;; php-style-delete-trailing-whitespace

(defcustom php-style-delete-trailing-whitespace nil
  "When non-nil, delete trailing whitespace in this buffer before saving.

This is set buffer-locally by `php-set-style' according to the current
coding style, and controls whether `delete-trailing-whitespace' is
added to `before-save-hook' buffer-locally."
  :group 'php
  :tag "PHP Style Delete Trailing Whitespace"
  :type 'boolean
  :safe #'booleanp)
(make-variable-buffer-local 'php-style-delete-trailing-whitespace)

;;; php-set-style

(defvar php-style-set-style-history nil
  "History of style names passed to `php-set-style'.")

(defvar-local php-style--current-style nil
  "Name of the coding style last applied by `php-set-style' in this buffer.")

(defun php-set-style (stylename &optional dont-override)
  "Set the current `php-mode' buffer to use the coding style STYLENAME.

STYLENAME is one of the names in `php-style-alist' (customarily one
of the values accepted by `php-mode-coding-style').

If DONT-OVERRIDE is non-nil, variables that already have a buffer-local
value in this buffer are left untouched; only variables without an
existing buffer-local value are set.

After applying the style's variables, this function manages
`before-save-hook' according to `php-style-delete-trailing-whitespace'
and finally runs the hook `php-mode-STYLENAME-hook' (e.g.
`php-mode-per-hook' for STYLENAME \"per\")."
  (interactive
   (list (completing-read "Which PHP coding style? "
                          (mapcar #'car php-style-alist)
                          nil t nil
                          'php-style-set-style-history)))
  (let ((resolved (php-style--resolve stylename)))
    (dolist (pair resolved)
      (let ((var (car pair))
            (val (cdr pair)))
        (unless (and dont-override (local-variable-p var))
          (set (make-local-variable var) val)))))
  (if (eq php-style-delete-trailing-whitespace t)
      (add-hook 'before-save-hook #'delete-trailing-whitespace nil t)
    (remove-hook 'before-save-hook #'delete-trailing-whitespace t))
  (setq php-style--current-style stylename)
  (run-hooks (intern (format "php-mode-%s-hook" stylename)))
  stylename)

;;; Style wrapper commands

(defun php-enable-per-coding-style ()
  "Set PHP Mode to use the PHP-FIG PER coding style."
  (interactive)
  (php-set-style "per"))

(defun php-enable-default-coding-style ()
  "Set PHP Mode to use reasonable default formatting (the PER style)."
  (interactive)
  (php-set-style "per"))

(defun php-enable-pear-coding-style ()
  "Set up `php-mode' to use the coding styles preferred for PEAR code and modules."
  (interactive)
  (php-set-style "pear"))

(defun php-enable-drupal-coding-style ()
  "Make `php-mode' use coding styles that are preferable for working with Drupal."
  (interactive)
  (php-set-style "drupal"))

(defun php-enable-wordpress-coding-style ()
  "Make `php-mode' use coding styles preferable for working with WordPress."
  (interactive)
  (php-set-style "wordpress"))

(defun php-enable-psr2-coding-style ()
  "Make `php-mode' comply with the PSR-2 / PSR-12 coding style."
  (interactive)
  (php-set-style "psr2"))

(define-obsolete-function-alias 'php-enable-symfony2-coding-style
  #'php-enable-per-coding-style "1.27.0"
  "The Symfony2 coding style has been removed; use the PER coding style
instead, which supersedes it.")

;;; c-basic-offset migration layer

(defcustom php-mode-enable-project-coding-style nil
  "When non-nil, override `php-mode-coding-style' with `php-project-coding-style'.

If you want to suppress styles from being overwritten by directory /
file local variables, set this to nil."
  :group 'php
  :tag "PHP Mode Enable Project Coding Style"
  :type 'boolean)

(defun php-style--honor-legacy-c-basic-offset ()
  "Honor a legacy buffer-local `c-basic-offset' by mapping it to
`php-indent-offset'.

Some projects still set `c-basic-offset' (a CC Mode variable) via
file-local or directory-local variables to control indentation.  The
cc-mode independent `php-mode' does not consult `c-basic-offset' for
indentation; this function bridges the gap for backward compatibility
by copying a buffer-local integer value of `c-basic-offset' into
`php-indent-offset' and warning that `c-basic-offset' is obsolete in
this context."
  (when (and (local-variable-p 'c-basic-offset)
             (boundp 'c-basic-offset)
             (integerp (symbol-value 'c-basic-offset)))
    (set (make-local-variable 'php-indent-offset) (symbol-value 'c-basic-offset))
    (display-warning 'php-mode
                      "`c-basic-offset' is obsolete here; use `php-indent-offset'"
                      :warning)))

(defun php-style--apply-project-or-default-style ()
  "Apply the project's coding style, or fall back to `php-mode-coding-style'.

Intended to run from `hack-local-variables-hook' once directory/file
local variables (including `php-project-coding-style') are known.
Removes itself from the buffer-local hook after running."
  (let ((style (or (and (bound-and-true-p php-project-coding-style)
                        (symbol-name php-project-coding-style))
                    (and php-mode-coding-style (symbol-name php-mode-coding-style)))))
    (when style
      (php-set-style style)))
  (remove-hook 'hack-local-variables-hook #'php-style--apply-project-or-default-style t))

;;;###autoload
(defun php-style-setup-buffer ()
  "Set up buffer-local coding style state for `php-mode'.

This is the entry point the new cc-mode independent `php-mode' calls
during major-mode initialization.  It performs three things:

1. Applies the coding style named by `php-mode-coding-style' (or, when
   `php-mode-enable-project-coding-style' is non-nil, delays this until
   directory/file local variables --- in particular
   `php-project-coding-style' --- have been processed, so that the
   project's style can take priority).
2. Buffer-locally hooks `php-style--honor-legacy-c-basic-offset' onto
   `hack-local-variables-hook' so that a `c-basic-offset' set via
   file/directory local variables is still honored.
3. Runs `php-style--honor-legacy-c-basic-offset' once immediately, in
   case `c-basic-offset' was already made buffer-local before this
   function was called."
  (if php-mode-enable-project-coding-style
      (add-hook 'hack-local-variables-hook
               #'php-style--apply-project-or-default-style t t)
    (when php-mode-coding-style
      (php-set-style (symbol-name php-mode-coding-style))))
  (add-hook 'hack-local-variables-hook
           #'php-style--honor-legacy-c-basic-offset t t)
  (php-style--honor-legacy-c-basic-offset))

(provide 'php-style)
;;; php-style.el ends here

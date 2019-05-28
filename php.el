;;; php.el --- PHP support for friends               -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Friends of Emacs-PHP development

;; Author: USAMI Kenta <tadsan@zonu.me>
;; Created: 5 Dec 2018
;; Version: 1.21.4
;; Keywords: languages, php
;; Homepage: https://github.com/emacs-php/php-mode
;; Package-Requires: ((emacs "24.3") (cl-lib "0.5"))
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

;; This file provides common variable and functions for PHP packages.

;;; Code:
(require 'flymake)
(require 'php-project)

;;;###autoload
(defgroup php nil
  "Language support for PHP."
  :tag "PHP"
  :group 'languages
  :link '(url-link :tag "Official Site" "https://github.com/emacs-php/php-mode")
  :link '(url-link :tag "PHP Mode Wiki" "https://github.com/emacs-php/php-mode/wiki"))

(defcustom php-executable (or (executable-find "php") "/usr/bin/php")
  "The location of the PHP executable."
  :group 'php
  :tag "PHP Executable"
  :type 'string)

(defcustom php-site-url "https://php.net/"
  "Default PHP.net site URL.

The URL to use open PHP manual and search word."
  :group 'php
  :tag "PHP Site URL"
  :type 'string)

(defcustom php-manual-url 'en
  "URL at which to find PHP manual.
You can replace \"en\" with your ISO language code."
  :group 'php
  :tag "PHP Manual URL"
  :type '(choice (const  :tag "English" 'en)
                 (const  :tag "Brazilian Portuguese" 'pt_BR)
                 (const  :tag "Chinese (Simplified)" 'zh)
                 (const  :tag "French" 'fr)
                 (const  :tag "German" 'de)
                 (const  :tag "Japanese" 'ja)
                 (const  :tag "Romanian" 'ro)
                 (const  :tag "Russian" 'ru)
                 (const  :tag "Spanish" 'es)
                 (const  :tag "Turkish" 'tr)
                 (string :tag "PHP manual URL")))

(defcustom php-search-url nil
  "URL at which to search for documentation on a word."
  :group 'php
  :tag "PHP Search URL"
  :type '(choice (string :tag "URL to search PHP documentation")
                 (const  :tag "Use `php-site-url' variable" nil)))

(defcustom php-class-suffix-when-insert "::"
  "Suffix for inserted class."
  :group 'php
  :type 'string)

(defcustom php-namespace-suffix-when-insert "\\"
  "Suffix for inserted namespace."
  :group 'php
  :type 'string)

(defcustom php-default-major-mode 'php-mode
  "Major mode for editing PHP script."
  :group 'php
  :tag "PHP Default Major Mode"
  :type 'function)

(defcustom php-html-template-major-mode 'web-mode
  "Major mode for editing PHP-HTML template."
  :group 'php
  :tag "PHP-HTML Template Major Mode"
  :type 'function)

(defcustom php-blade-template-major-mode 'web-mode
  "Major mode for editing Blade template."
  :group 'php
  :tag "PHP Blade Template Major Mode"
  :type 'function)

(defcustom php-template-mode-alist
  `(("\\.blade" . ,php-blade-template-major-mode)
    ("\\.phpt\\'" . ,(if (fboundp 'phpt-mode) 'phpt-mode php-html-template-major-mode))
    ("\\.phtml\\'" . ,php-html-template-major-mode))
  "Automatically use another MAJOR-MODE when open template file."
  :group 'php
  :tag "PHP Template Mode Alist"
  :type '(alist :key-type regexp :value-type function)
  :link '(url-link :tag "web-mode" "http://web-mode.org/")
  :link '(url-link :tag "phpt-mode" "https://github.com/emacs-php/phpt-mode"))

(defcustom php-mode-maybe-hook nil
  "List of functions to be executed on entry to `php-mode-maybe'."
  :group 'php
  :tag "PHP Mode Maybe Hook"
  :type 'hook)

;;; PHP Keywords
(defconst php-magical-constants
  (list "__LINE__" "__FILE__" "__FUNCTION__" "__CLASS__" "__TRAIT__" "__METHOD__" "__NAMESPACE__")
  "Magical keyword that is expanded at compile time.

These are different from \"constants\" in strict terms.
see https://www.php.net/manual/language.constants.predefined.php")

;;; Utillity for locate language construction
(defsubst php-in-string-p ()
  "Return non-nil if inside a string.
it is the character that will terminate the string, or t if the string should be terminated by a generic string delimiter."
  (nth 3 (syntax-ppss)))

(defsubst php-in-comment-p ()
  "Return nil if outside a comment, t if inside a non-nestable comment, else an integer (the current comment nesting)."
  (nth 4 (syntax-ppss)))

(defsubst php-in-string-or-comment-p ()
  "Return character address of start of comment or string; nil if not in one."
  (nth 8 (syntax-ppss)))

(defsubst php-in-poly-php-html-mode ()
  "Return T if current buffer is in `poly-html-mode'."
  (and (boundp 'poly-php-html-mode)
       (symbol-value 'poly-php-html-mode)))

(defun php-create-regexp-for-method (visibility)
  "Make a regular expression for methods with the given VISIBILITY.

VISIBILITY must be a string that names the visibility for a PHP
method, e.g. 'public'.  The parameter VISIBILITY can itself also
be a regular expression.

The regular expression this function returns will check for other
keywords that can appear in method signatures, e.g. 'final' and
'static'.  The regular expression will have one capture group
which will be the name of the method."
  (concat
   ;; Initial space with possible 'abstract' or 'final' keywords
   "^\\s-*\\(?:\\(?:abstract\\|final\\)\\s-+\\)?"
   ;; 'static' keyword may come either before or after visibility
   "\\(?:" visibility "\\(?:\\s-+static\\)?\\|\\(?:static\\s-+\\)?" visibility "\\)\\s-+"
   ;; Make sure 'function' comes next with some space after
   "function\\s-+"
   ;; Capture the name as the first group and the regexp and make sure
   ;; by the end we see the opening parenthesis for the parameters.
   "\\(\\(?:\\sw\\|\\s_\\)+\\)\\s-*("))

(defun php-create-regexp-for-classlike (type)
  "Accepts a `TYPE' of a 'classlike' object as a string, such as
'class' or 'interface', and returns a regexp as a string which
can be used to match against definitions for that classlike."
  (concat
   ;; First see if 'abstract' or 'final' appear, although really these
   ;; are not valid for all values of `type' that the function
   ;; accepts.
   "^\\s-*\\(?:\\(?:abstract\\|final\\)\\s-+\\)?"
   ;; The classlike type
   type
   ;; Its name, which is the first captured group in the regexp.  We
   ;; allow backslashes in the name to handle namespaces, but again
   ;; this is not necessarily correct for all values of `type'.
   "\\s-+\\(\\(?:\\sw\\|\\\\\\|\\s_\\)+\\)"))

(defvar php-imenu-generic-expression
  `(("Namespaces"
    ,(php-create-regexp-for-classlike "namespace") 1)
   ("Classes"
    ,(php-create-regexp-for-classlike "class") 1)
   ("Interfaces"
    ,(php-create-regexp-for-classlike "interface") 1)
   ("Traits"
    ,(php-create-regexp-for-classlike "trait") 1)
   ("All Methods"
    ,(php-create-regexp-for-method "\\(?:\\sw\\|\\s_\\)+") 1)
   ("Private Methods"
    ,(php-create-regexp-for-method "private") 1)
   ("Protected Methods"
    ,(php-create-regexp-for-method "protected")  1)
   ("Public Methods"
    ,(php-create-regexp-for-method "public") 1)
   ("Anonymous Functions"
    "\\<\\(\\(?:\\sw\\|\\s_\\)+\\)\\s-*=\\s-*function\\s-*(" 1)
   ("Named Functions"
    "^\\s-*function\\s-+\\(\\(?:\\sw\\|\\s_\\)+\\)\\s-*(" 1))
  "Imenu generic expression for PHP Mode.  See `imenu-generic-expression'.")

(defvar php--re-namespace-pattern
  (php-create-regexp-for-classlike "namespace"))

(defvar php--re-classlike-pattern
  (php-create-regexp-for-classlike (regexp-opt '("class" "interface" "trait"))))

(defun php-get-current-element (re-pattern)
  "Return backward matched element by RE-PATTERN."
  (save-excursion
    (when (re-search-backward re-pattern nil t)
      (match-string-no-properties 1))))

;;; Provide support for Flymake so that users can see warnings and
;;; errors in real-time as they write code.
(defun php-flymake-php-init ()
  "PHP specific init-cleanup routines.

This is an alternative function of `flymake-php-init'.
Look at the `php-executable' variable instead of the constant \"php\" command."
  (let* ((init (funcall (eval-when-compile
                          (if (fboundp 'flymake-proc-php-init)
                              'flymake-proc-php-init
                            'flymake-php-init)))))
    (list php-executable (cdr init))))

(defconst php-re-detect-html-tag
  (eval-when-compile
    (rx (or (: string-start (* (in space))
               "<!"
               (or "DOCTYPE" "doctype")
               (+ (in space))
               (or "HTML" "html"))
            (: (or line-start
                   (: "<" (? "/")
                      (* (in space)) (+ (in alpha "-")) (* (in space)) ">"))
               (: "<" (* (in space)) (+ (in alpha "-")) (* (in space)) ">"))))))

(defun php-buffer-has-html-tag ()
  "Return position of HTML tag or NIL in current buffer."
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (re-search-forward php-re-detect-html-tag nil t))))

(defun php-derivation-major-mode ()
  "Return major mode for PHP file by file-name and its content."
  (let ((mode (assoc-default buffer-file-name
                             php-template-mode-alist
                             #'string-match-p))
        type)
    (when (and (null mode) buffer-file-name
               php-project-php-file-as-template)
      (setq type (php-project-get-file-html-template-type buffer-file-name))
      (cond
       ((eq t type) (setq mode php-html-template-major-mode))
       ((eq 'auto type)
        (when (php-buffer-has-html-tag)
          (setq mode php-html-template-major-mode)))))
    (when (and mode (not (fboundp mode)))
      (if (string-match-p "\\.blade\\." buffer-file-name)
          (warn "php-mode is NOT support blade template. %s"
                "Please install `web-mode' package")
        (setq mode nil)))
    (or mode php-default-major-mode)))

;;;###autoload
(defun php-mode-maybe ()
  "Select PHP mode or other major mode."
  (interactive)
  (run-hooks php-mode-maybe-hook)
  (funcall (php-derivation-major-mode)))

;;;###autoload
(defun php-current-class ()
  "Insert current class name if cursor in class context."
  (interactive)
  (let ((matched (php-get-current-element php--re-classlike-pattern)))
    (when matched
      (insert (concat matched php-class-suffix-when-insert)))))

;;;###autoload
(defun php-current-namespace ()
  "Insert current namespace if cursor in namespace context."
  (interactive)
  (let ((matched (php-get-current-element php--re-namespace-pattern)))
    (when matched
      (insert (concat matched php-namespace-suffix-when-insert)))))

(provide 'php)
;;; php.el ends here

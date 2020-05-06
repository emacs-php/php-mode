;;; php.el --- PHP support for friends               -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Friends of Emacs-PHP development

;; Author: USAMI Kenta <tadsan@zonu.me>
;; Created: 5 Dec 2018
;; Version: 1.23.0
;; Keywords: languages, php
;; Homepage: https://github.com/emacs-php/php-mode
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

;; This file provides common variable and functions for PHP packages.

;;; Code:
(require 'cl-lib)
(require 'flymake)
(require 'php-project)
(require 'rx)

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
    ("\\.phpt\\'" . ,(if (fboundp 'phpt-mode) 'phpt-mode php-default-major-mode))
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

(defcustom php-default-builtin-web-server-port 3939
  "Port number of PHP Built-in HTTP server (php -S)."
  :group 'php
  :tag "PHP Default Built-in Web Server Port"
  :type 'integer
  :link '(url-link :tag "Built-in web server"
                   "https://www.php.net/manual/features.commandline.webserver.php"))

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

(defconst php-beginning-of-defun-regexp
  "^\\s-*\\(?:\\(?:abstract\\|final\\|private\\|protected\\|public\\|static\\)\\s-+\\)*function\\s-+&?\\(\\(\\sw\\|\\s_\\)+\\)\\s-*("
  "Regular expression for a PHP function.")

(eval-when-compile
  (defun php-create-regexp-for-method (&optional visibility)
    "Make a regular expression for methods with the given VISIBILITY.

VISIBILITY must be a string that names the visibility for a PHP
method, e.g. 'public'.  The parameter VISIBILITY can itself also
be a regular expression.

The regular expression this function returns will check for other
keywords that can appear in method signatures, e.g. 'final' and
'static'.  The regular expression will have one capture group
which will be the name of the method."
    (when (stringp visibility)
      (setq visibility (list visibility)))
    (rx-to-string `(: line-start
                      (* (syntax whitespace))
                      ,@(if visibility
                            `((* (or "abstract" "final" "static")
                                 (+ (syntax whitespace)))
                              (or ,@visibility)
                              (+ (syntax whitespace))
                              (* (or "abstract" "final" "static")
                                 (+ (syntax whitespace))))
                          '((* (* (or "abstract" "final" "static"
                                      "private" "protected" "public")
                                  (+ (syntax whitespace))))))
                      "function"
                      (+ (syntax whitespace))
                      (? "&" (* (syntax whitespace)))
                      (group (+ (or (syntax word) (syntax symbol))))
                      (* (syntax whitespace))
                      "(")))

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
     "\\s-+\\(\\(?:\\sw\\|\\\\\\|\\s_\\)+\\)")))

(defconst php-imenu-generic-expression
  (eval-when-compile
    `(("Namespaces"
       ,(php-create-regexp-for-classlike "namespace") 1)
      ("Classes"
       ,(php-create-regexp-for-classlike "class") 1)
      ("Interfaces"
       ,(php-create-regexp-for-classlike "interface") 1)
      ("Traits"
       ,(php-create-regexp-for-classlike "trait") 1)
      ("All Methods"
       ,(php-create-regexp-for-method) 1)
      ("Private Methods"
       ,(php-create-regexp-for-method '("private")) 1)
      ("Protected Methods"
       ,(php-create-regexp-for-method '("protected"))  1)
      ("Public Methods"
       ,(php-create-regexp-for-method '("public")) 1)
      ("Anonymous Functions"
       "\\<\\(\\(?:\\sw\\|\\s_\\)+\\)\\s-*=\\s-*f\\(unctio\\)?n\\s-*(" 1)
      ("Named Functions"
       "^\\s-*function\\s-+\\(\\(?:\\sw\\|\\s_\\)+\\)\\s-*(" 1)))
  "Imenu generic expression for PHP Mode.  See `imenu-generic-expression'.")

(defconst php--re-namespace-pattern
  (eval-when-compile
    (php-create-regexp-for-classlike "namespace")))

(defconst php--re-classlike-pattern
  (eval-when-compile
    (php-create-regexp-for-classlike (regexp-opt '("class" "interface" "trait")))))

(defun php-get-current-element (re-pattern)
  "Return backward matched element by RE-PATTERN."
  (save-excursion
    (save-match-data
      (when (re-search-backward re-pattern nil t)
        (match-string-no-properties 1)))))

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

(defconst php-re-detect-html-tag-aggressive
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

(defconst php-re-detect-html-tag-default
  (eval-when-compile
    (rx (or (: string-start (* (in space))
               "<!"
               (or "DOCTYPE" "doctype")
               (+ (in space))
               (or "HTML" "html"))
            (: line-start
               (: "<" (* (in space)) (+ (in alpha "-")) (* (in space)) ">"))))))

(defcustom php-re-detect-html-tag 'php-re-detect-html-tag-default
  "Regexp pattern variable-name of HTML detection."
  :group 'php
  :tag "PHP Re Detect HTML Tag"
  :type '(choice (const :tag "Default pattern" 'php-re-detect-html-tag-default)
                 (const :tag "Aggressive pattern" 'php-re-detect-html-tag-aggressive)
                 (variable :tag "Variable name of RegExp pattern")))

(defsubst php-re-detect-html-tag ()
  "Return RegExp pattern for HTML detection."
  (if (symbolp php-re-detect-html-tag)
      (symbol-value php-re-detect-html-tag)
    php-re-detect-html-tag))

(defun php-buffer-has-html-tag ()
  "Return position of HTML tag or NIL in current buffer."
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (save-match-data
        (re-search-forward (php-re-detect-html-tag) nil t)))))

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

;;;###autoload
(defun php-copyit-fqsen ()
  "Copy/kill class/method FQSEN."
  (interactive)
  (let ((namespace (or (php-get-current-element php--re-namespace-pattern) ""))
        (class     (or (php-get-current-element php--re-classlike-pattern) ""))
        (namedfunc (php-get-current-element php-beginning-of-defun-regexp)))
    (kill-new (concat (if (string= namespace "") "" namespace)
                      (if (string= class "") "" (concat "\\" class "::"))
                      (if (string= namedfunc "") "" (concat namedfunc "()"))))))

;;;###autoload
(defun php-run-builtin-web-server (router-or-dir hostname port &optional document-root)
  "Run PHP Built-in web server.

`ROUTER-OR-DIR': Path to router PHP script or Document root.
`HOSTNAME': Hostname or IP address of Built-in web server.
`PORT': Port number of Built-in web server.
`DOCUMENT-ROOT': Path to Document root.

When `DOCUMENT-ROOT' is NIL, the document root is obtained from `ROUTER-OR-DIR'."
  (interactive
   (let ((insert-default-directory t)
         (d-o-r (read-file-name "Document root or Script: " default-directory)))
     (list
      (expand-file-name d-o-r)
      (read-string "Hostname: " "0.0.0.0")
      (read-number "Port: " php-default-builtin-web-server-port)
      (if (file-directory-p d-o-r)
          nil
        (let ((root-input (read-file-name "Document root: " (directory-file-name d-o-r))))
          (file-name-directory
           (if (file-directory-p root-input)
               root-input
             (directory-file-name root-input))))))))
  (let* ((default-directory
           (or document-root
               (if (file-directory-p router-or-dir)
                   router-or-dir
                 (directory-file-name router-or-dir))))
         (short-dirname (abbreviate-file-name default-directory))
         (short-filename (abbreviate-file-name router-or-dir))
         (buf-name (format "php -S %s:%s -t %s %s"
                           hostname
                           port
                           short-dirname
                           (if document-root short-filename "")))
         (args (cl-remove-if
                #'null
                (list "-S"
                      (format "%s:%d" hostname port)
                      "-t"
                      default-directory
                      (when document-root router-or-dir)))))
    (message "Run PHP built-in server: %s" buf-name)
    (apply #'make-comint buf-name php-executable nil args)
    (funcall
     (if (called-interactively-p 'interactive) #'display-buffer #'get-buffer)
     (format "*%s*" buf-name))))

(defun php-ini ()
  "Get `php --ini' output buffer."
  (interactive)
  (let ((buffer (get-buffer-create " *php --ini*")))
    (with-current-buffer buffer
      (view-mode -1)
      (read-only-mode -1)
      (erase-buffer)
      (shell-command (concat php-executable " --ini") buffer)
      (view-mode +1))
    (if (called-interactively-p 'interactive)
        (pop-to-buffer buffer)
      buffer)))

;;;###autoload
(defun php-find-system-php-ini-file (&optional file)
  "Find php.ini FILE by `php --ini'."
  (interactive
   (list
    (let* ((default-directory (expand-file-name "~"))
           (buffer (php-ini))
           (path (with-current-buffer buffer
                   (goto-char (point-min))
                   (save-match-data
                     (when (re-search-forward ": \\(.+?\\)$" nil nil)
                       (match-string 1))))))
      (when (or (null path) (not (file-directory-p path)))
        (when (called-interactively-p 'interactive)
          (pop-to-buffer buffer))
        (user-error "Failed get path to PHP ini files directory"))
      (read-file-name "Find php.ini file: "
                      (concat (expand-file-name path) "/")
                      nil nil nil
                      #'file-exists-p))))
  (find-file file))

(provide 'php)
;;; php.el ends here

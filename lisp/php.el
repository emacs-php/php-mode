;;; php.el --- PHP support for friends               -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Friends of Emacs-PHP development
;; Copyright (C) 1985, 1987, 1992-2022 Free Software Foundation, Inc.

;; Author: USAMI Kenta <tadsan@zonu.me>
;; Created: 5 Dec 2018
;; Version: 1.25.0
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

;; This file provides common variable and functions for PHP packages.

;; These functions are copied function from GNU Emacs.
;;
;; - c-end-of-token (cc-engine.el)
;;

;;; Code:
(eval-when-compile
  (require 'cc-mode)
  (require 'cl-lib))
(require 'cc-engine)
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

(defcustom php-site-url "https://www.php.net/"
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
  :type '(choice (const  :tag "English" en)
                 (const  :tag "Brazilian Portuguese" pt_BR)
                 (const  :tag "Chinese (Simplified)" zh)
                 (const  :tag "French" fr)
                 (const  :tag "German" de)
                 (const  :tag "Japanese" ja)
                 (const  :tag "Romanian" ro)
                 (const  :tag "Russian" ru)
                 (const  :tag "Spanish" es)
                 (const  :tag "Turkish" tr)
                 (string :tag "PHP manual URL")))

(defcustom php-search-url nil
  "URL at which to search for documentation on a word."
  :group 'php
  :tag "PHP Search URL"
  :type '(choice (string :tag "URL to search PHP documentation")
                 (const  :tag "Use `php-site-url' variable" nil)))

(defcustom php-completion-file ""
  "Path to the file which contains the function names known to PHP."
  :type 'string)

(defcustom php-manual-path ""
  "Path to the directory which contains the PHP manual."
  :type 'string)

(defcustom php-search-documentation-function #'php-search-web-documentation
  "Function to search PHP Manual at cursor position."
  :group 'php
  :tag "PHP Search Documentation Function"
  :type '(choice (const :tag "Use online documentation" #'php-search-web-documentation)
                 (const :tag "Use local documentation" #'php-local-manual-search)
                 (function :tag "Use other function")))

(defcustom php-search-documentation-browser-function nil
  "Function to display PHP documentation in a WWW browser.

If non-nil, this shadows the value of `browse-url-browser-function' when
calling `php-search-documentation' or `php-search-local-documentation'."
  :group 'php
  :tag "PHP Search Documentation Browser Function"
  :type '(choice (const :tag "default" nil) function)
  :link '(variable-link browse-url-browser-function))

;; Define function for browsing manual
(defun php-browse-documentation-url (url)
  "Browse a documentation URL using the configured browser function.

See `php-search-documentation-browser-function'."
  (let ((browse-url-browser-function
         (or php-search-documentation-browser-function
             browse-url-browser-function)))
    (browse-url url)))

(defun php-browse-manual ()
  "Bring up manual for PHP."
  (interactive)
  (browse-url (if (stringp php-manual-url)
                  php-manual-url
                (format "%smanual/%s/" php-site-url php-manual-url))))

(defun php-search-web-documentation (word)
  "Return URL to search PHP manual search by `WORD'."
  (interactive (list (current-word)))
  (php-browse-documentation-url (concat (or php-search-url php-site-url) word)))

(defun php-search-documentation (&optional word)
  "Search PHP documentation for the `WORD' at point.

If `php-manual-path' has a non-empty string value then the command
will first try searching the local documentation.  If the requested
documentation does not exist it will fallback to searching the PHP
website.

With a prefix argument, prompt for a documentation word to search
for.  If the local documentation is available, it is used to build
a completion list."
  (interactive)
  (if (called-interactively-p 'interactive)
      (call-interactively php-search-documentation-function)
    (funcall php-search-documentation-function word)))

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

(defconst php-re-token-symbols
  (eval-when-compile
    (regexp-opt (list "&" "&=" "array(" "(array)" "&&" "||" "(bool)" "(boolean)" "break;" "?>" "%>"
                      "??" "??=" ".=" "--" "/=" "=>" "(real)" "(double)" "(float)" "::" "..."
                      "__halt_compiler()" "++" "(int)" "(integer)" "==" ">=" "===" "!=" "<>" "!=="
                      "<=" "-=" "%=" "*=" "\\" "(object)" "->" "?->" "<?php" "<?" "<?=" "|=" "+="
                      "**" "**=" "<<" "<<=" "<=>" ">>" ">>=" "<<<" "(string)" "^=" "yield from"
                      "[" "]" "(" ")" "{" "}" ";")
                t)))

;;; Utillity for locate language construction
(defsubst php-in-string-p ()
  "Return non-nil if inside a string.
It is the character that will terminate the string, or t if the string should
be terminated by a generic string delimiter."
  (nth 3 (syntax-ppss)))

(defsubst php-in-comment-p ()
  "Return NIL if outside a comment, T if inside a non-nestable comment, else
an integer (the current comment nesting)."
  (nth 4 (syntax-ppss)))

(defsubst php-in-string-or-comment-p ()
  "Return character address of start of comment or string; nil if not in one."
  (nth 8 (syntax-ppss)))

(defsubst php-in-poly-php-html-mode ()
  "Return T if current buffer is in `poly-html-mode'."
  (bound-and-true-p poly-php-html-mode))

(defconst php-beginning-of-defun-regexp
  (eval-when-compile
    (rx bol
        (* (syntax whitespace))
        (* (or "abstract" "final" "private" "protected" "public" "static")
           (+ (syntax whitespace)))
        "function"
        (+ (syntax whitespace))
        (? "&" (* (syntax whitespace)))
        (group (+ (or (syntax word) (syntax symbol))))
        (* (syntax whitespace))
        "("))
  "Regular expression for a PHP function.")

(eval-when-compile
  (cl-defun php-create-regexp-for-method (&optional visibility &key include-args)
    "Make a regular expression for methods with the given VISIBILITY.

VISIBILITY must be a string that names the visibility for a PHP
method, e.g. `public'.  The parameter VISIBILITY can itself also
be a regular expression.

The regular expression this function returns will check for other
keywords that can appear in method signatures, e.g. `final' and
`static'.  The regular expression will have one capture group
which will be the name of the method."
    (when (stringp visibility)
      (setq visibility (list visibility)))
    (rx-to-string `(: line-start
                      (* (syntax whitespace))
                      (group
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
                       "("
                       ,@(when include-args
                           '((* any) line-end))))))

  (defun php-create-regexp-for-classlike (type)
    "Accepts a `TYPE' of a `classlike' object as a string, such as
`class' or `interface', and returns a regexp as a string which
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

(defconst php-imenu-generic-expression-default
  (eval-when-compile
    `(("Methods"
       ,(php-create-regexp-for-method nil :include-args t) 1)
      ("Properties"
       ,(rx line-start
            (* (syntax whitespace))
            (group
             (+ (or "public" "protected" "private" "static" "var")
                (+ (syntax whitespace)))
             (* (? (? (or "|" "?"))
                   (or "\\" (syntax word) (syntax symbol))
                   (+ (syntax whitespace))))
             "$" (+ (or (syntax word) (syntax symbol)))
             word-boundary))
       1)
      ("Constants"
       ,(rx line-start
            (* (syntax whitespace))
            (group
             (* (or "public" "protected" "private")
                (+ (syntax whitespace)))
             "const"
             (+ (syntax whitespace))
             (+ (or (syntax word) (syntax symbol)))
             (* (syntax whitespace))
             (? "=" (* (syntax whitespace))
                (repeat 0 40 any))))
       1)
      ("Functions"
       ,(rx line-start
            (* (syntax whitespace))
            (group
             "function"
             (+ (syntax whitespace))
             (+ (or (syntax word) (syntax symbol)))
             (* (syntax whitespace))
             "("
             (repeat 0 100 any)))
       1)
      ("Import"
       ,(rx line-start
            ;; (* (syntax whitespace))
            (group
             "use"
             (+ (syntax whitespace))
             (repeat 0 100 any)))
       1)
      ("Classes"
       ,(php-create-regexp-for-classlike "\\(?:class\\|interface\\|trait\\|enum\\)") 0)
      ("Namespace"
       ,(php-create-regexp-for-classlike "namespace") 1)))
  "Imenu generic expression for PHP Mode.  See `imenu-generic-expression'.")

(defconst php-imenu-generic-expression-simple
  (eval-when-compile
    `(("Methods"
       ,(php-create-regexp-for-method nil) 2)
      ("Properties"
       ,(rx line-start
            (* (syntax whitespace))
            (+ (or "public" "protected" "private" "static" "var")
               (+ (syntax whitespace)))
            (* (? (? (or "|" "?"))
                  (or "\\" (syntax word) (syntax symbol))
                  (+ (syntax whitespace))))
            (group
             "$" (+ (or (syntax word) (syntax symbol))))
            word-boundary)
       1)
      ("Constants"
       ,(rx line-start
            (* (syntax whitespace))
            (group
             (* (or "public" "protected" "private")
                (+ (syntax whitespace)))
             "const"
             (+ (syntax whitespace))
             (+ (or (syntax word) (syntax symbol)))))
       1)
      ("Functions"
       ,(rx line-start
            (* (syntax whitespace))
            "function"
            (+ (syntax whitespace))
            (group
             (+ (or (syntax word) (syntax symbol)))))
       1)
      ("Classes"
       ,(php-create-regexp-for-classlike "\\(?:class\\|interface\\|trait\\|enum\\)") 1)
      ("Namespace"
       ,(php-create-regexp-for-classlike "namespace") 1)))
  "Imenu generic expression for PHP Mode.  See `imenu-generic-expression'.")

(defconst php-imenu-generic-expression-legacy
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
       ,(php-create-regexp-for-method '("private")) 2)
      ("Protected Methods"
       ,(php-create-regexp-for-method '("protected"))  2)
      ("Public Methods"
       ,(php-create-regexp-for-method '("public")) 2)
      ("Anonymous Functions"
       "\\<\\(\\(?:\\sw\\|\\s_\\)+\\)\\s-*=\\s-*f\\(unctio\\)?n\\s-*(" 1)
      ("Named Functions"
       "^\\s-*function\\s-+\\(\\(?:\\sw\\|\\s_\\)+\\)\\s-*(" 1)))
  "Imenu generic expression for PHP Mode.  See `imenu-generic-expression'.")

(defcustom php-imenu-generic-expression 'php-imenu-generic-expression-default
  "Default Imenu generic expression for PHP Mode.  See `imenu-generic-expression'."
  :type '(choice (alist :key-type string :value-type list)
                 (const php-imenu-generic-expression-legacy)
                 (const php-imenu-generic-expression-simple)
                 variable)
  :group 'php)

(defconst php--re-namespace-pattern
  (eval-when-compile
    (php-create-regexp-for-classlike "namespace")))

(defconst php--re-classlike-pattern
  (eval-when-compile
    (php-create-regexp-for-classlike (regexp-opt '("class" "interface" "trait")))))

(defvar php--analysis-syntax-table
  (eval-when-compile
    (let ((table (make-syntax-table)))
      (c-populate-syntax-table table)
      (modify-syntax-entry ?_ "w" table)
      (modify-syntax-entry ?`  "\""  table)
      (modify-syntax-entry ?\" "\""  table)
      (modify-syntax-entry ?#  "< b" table)
      (modify-syntax-entry ?\n "> b" table)
      table)))

(defun php-get-current-element (re-pattern)
  "Return backward matched element by RE-PATTERN."
  (save-excursion
    (save-match-data
      (when (re-search-backward re-pattern nil t)
        (match-string-no-properties 1)))))

(eval-and-compile
  (if (eval-when-compile (fboundp 'thing-at-point-bounds-of-string-at-point))
      (defalias 'php--thing-at-point-bounds-of-string-at-point #'thing-at-point-bounds-of-string-at-point)
    ;; Copyright (C) 1991-1998, 2000-2022 Free Software Foundation, Inc.
    ;; Follows function is copied from Emacs 28's thingatpt.el.
    ;; https://github.com/emacs-mirror/emacs/commit/2abf143f8185fced544c4f8d144ea710142d7a59
    (defun php--thing-at-point-bounds-of-string-at-point ()
      "Return the bounds of the string at point.
Prefer the enclosing string with fallback on sexp at point.
\[Internal function used by `bounds-of-thing-at-point'.]"
      (save-excursion
        (let ((ppss (syntax-ppss)))
          (if (nth 3 ppss)
              ;; Inside the string
              (ignore-errors
                (goto-char (nth 8 ppss))
                (cons (point) (progn (forward-sexp) (point))))
            ;; At the beginning of the string
            (if (eq (char-syntax (char-after)) ?\")
                (let ((bound (bounds-of-thing-at-point 'sexp)))
	          (and bound
	               (<= (car bound) (point)) (< (point) (cdr bound))
	               bound))))))))
  (if (eval-when-compile (fboundp 'c-end-of-token))
      (defalias 'php--c-end-of-token #'c-end-of-token)
    ;; Copyright (C) 1985, 1987, 1992-2022 Free Software Foundation, Inc.
    ;; Follows function is copied from Emacs 27's cc-engine.el.
    ;; https://emba.gnu.org/emacs/emacs/-/commit/95fb826dc58965eac287c0826831352edf2e56f7
    (defun php--c-end-of-token (&optional back-limit)
      ;; Move to the end of the token we're just before or in the middle of.
      ;; BACK-LIMIT may be used to bound the backward search; if given it's
      ;; assumed to be at the boundary between two tokens.  Return non-nil if the
      ;; point is moved, nil otherwise.
      ;;
      ;; This function might do hidden buffer changes.
      (let ((start (point)))
        (cond ;; ((< (skip-syntax-backward "w_" (1- start)) 0)
         ;;  (skip-syntax-forward "w_"))
         ((> (skip-syntax-forward "w_") 0))
         ((< (skip-syntax-backward ".()" back-limit) 0)
          (while (< (point) start)
	    (if (looking-at c-nonsymbol-token-regexp)
	        (goto-char (match-end 0))
	      ;; `c-nonsymbol-token-regexp' should always match since
	      ;; we've skipped backward over punctuation or paren
	      ;; syntax, but move forward in case it doesn't so that
	      ;; we don't leave point earlier than we started with.
	      (forward-char))))
         (t (if (looking-at c-nonsymbol-token-regexp)
	        (goto-char (match-end 0)))))
        (> (point) start)))))

(defun php-leading-tokens (length)
  "Return a list of leading LENGTH tokens from cursor point.

The token list is lined up in the opposite side of the visual arrangement.
The order is reversed by calling as follows:
     \(nreverse \(php-leading-tokens 3\)\)"
  (save-excursion
    (save-match-data
      (with-syntax-table php--analysis-syntax-table
        (cl-loop
         repeat length
         do (progn
              (forward-comment (- (point)))
              (c-backward-token-2 1 nil))
         collect
         (cond
          ((when-let (bounds (php--thing-at-point-bounds-of-string-at-point))
             (prog1 (buffer-substring-no-properties (car bounds) (cdr bounds))
               (goto-char (car bounds)))))
          ((looking-at php-re-token-symbols)
           (prog1 (match-string-no-properties 0)
             (goto-char (match-beginning 0))))
          (t
             (buffer-substring-no-properties (point)
                                             (save-excursion (php--c-end-of-token) (point))))))))))

(defun php-get-pattern ()
  "Find the pattern we want to complete.
`find-tag-default' from GNU Emacs etags.el."
  (car (php-leading-tokens 1)))

;;; Provide support for Flymake so that users can see warnings and
;;; errors in real-time as they write code.
(defun php-flymake-php-init ()
  "PHP specific init-cleanup routines.

This is an alternative function of `flymake-php-init'.
Look at the `php-executable' variable instead of the constant \"php\" command."
  (let ((init (with-no-warnings (flymake-php-init))))
    (setf (car init) php-executable)
    init))

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
  :type '(choice (const :tag "Default pattern" php-re-detect-html-tag-default)
                 (const :tag "Aggressive pattern" php-re-detect-html-tag-aggressive)
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
        (let ((root-input (expand-file-name
                           (read-file-name "Document root: " (directory-file-name d-o-r)))))
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
         (args (delq
                nil
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

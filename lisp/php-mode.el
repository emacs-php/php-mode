;;; php-mode.el --- Major mode for editing PHP code  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Friends of Emacs-PHP development
;; Copyright (C) 1999-2026 Free Software Foundation, Inc.
;; Copyright (C) 1999, 2000, 2001, 2003, 2004 Turadg Aleahmad
;;               2008 Aaron S. Hawley
;;               2011-2017 Eric James Michael Ritz

;; Author: USAMI Kenta <tadsan@zonu.me>
;; Maintainer: USAMI Kenta <tadsan@zonu.me>
;; URL: https://github.com/emacs-php/php-mode
;; Keywords: languages php
;; Version: 1.26.1
;; Package-Requires: ((emacs "28.1"))
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

;; `php-mode' is a major mode for editing PHP script.  Unlike the legacy
;; `php-cc-mode' (kept for backward compatibility in lisp/php-cc-mode.el),
;; this implementation does NOT depend on CC Mode.  Indentation is handled
;; by the `syntax-ppss'-based engine in php-indent.el, coding styles by
;; php-style.el, and the PHP vocabulary comes from php-keywords.el.
;;
;; This mode is designed for PHP scripts consisting of a single <?php
;; block.  We recommend Web Mode for HTML and Blade templates mixed with
;; PHP.  http://web-mode.org/

;;; Code:

(require 'php)
(require 'php-face)
(require 'php-project)
(require 'php-indent)
(require 'php-style)
(require 'php-keywords)
(require 'font-lock)
(require 'rx)
(require 'imenu)

(eval-when-compile
  (require 'cl-lib)
  (require 'regexp-opt)
  (defvar add-log-current-defun-header-regexp)
  (defvar add-log-current-defun-function))

(autoload 'php-mode-debug "php-mode-debug"
  "Display informations useful for debugging PHP Mode." t)

;;;###autoload
(defgroup php-mode nil
  "Major mode for editing PHP code."
  :tag "PHP Mode"
  :prefix "php-mode-"
  :group 'languages
  :group 'php
  :link '(url-link :tag "Official Site" "https://github.com/emacs-php/php-mode")
  :link '(url-link :tag "PHP Mode Wiki" "https://github.com/emacs-php/php-mode/wiki"))

;;; Mode-specific customization

(defcustom php-mode-hook nil
  "List of functions to be executed on entry to `php-mode'."
  :tag "PHP Mode Hook"
  :type 'hook)

(defcustom php-mode-page-delimiter
  (eval-when-compile
    (rx symbol-start
        (or "namespace" "function" "class" "trait" "interface")
        symbol-end))
  "Regexp describing line-beginnings that PHP declaration statements."
  :tag "PHP Mode Page Delimiter"
  :type 'regexp)

(defcustom php-mode-warn-if-html-template t
  "Warn and prompt to switch to an HTML template major mode when indenting HTML."
  :tag "PHP Mode Warn If HTML Template"
  :safe #'booleanp
  :type '(choice (const :tag "Warn" t) (const :tag "Don't warn" nil)))

(defcustom php-mode-replace-flymake-diag-function
  (eval-when-compile (when (boundp 'flymake-diagnostic-functions)
                       #'php-flymake))
  "Flymake function to replace, if NIL do not replace."
  :tag "PHP Mode Replace Flymake Diag Function"
  :type '(choice function
                 (const :tag "Disable to replace" nil)))

(defcustom php-mode-force-pear nil
  "Normally PEAR coding rules are enforced only when the filename contains \"PEAR\".
Turning this on will force PEAR rules on all PHP files."
  :tag "PHP Mode Force Pear"
  :type 'boolean)

;;; Obsolete style variables (superseded by php-indent.el customization).
(define-obsolete-variable-alias
  'php-mode-enable-backup-style-variables 'php-style-delete-trailing-whitespace "1.27.0")
(define-obsolete-variable-alias
  'php-mode-lineup-cascaded-calls 'php-indent-chain-indent "1.27.0")

;;; Syntax table

(defvar php-mode-syntax-table
  (copy-syntax-table php--base-syntax-table)
  "Syntax table used in `php-mode' buffers.")

;;; Syntax propertize
;;
;; The heredoc/nowdoc, PHP 8 attribute and comment-quote propertize
;; helpers below are cc-mode independent.  They are duplicated verbatim
;; from lisp/php-cc-mode.el (rather than shared through a common file) so
;; that both major modes can be loaded at the same time: the definitions
;; are identical, so a later redefinition by the other file is harmless.

(eval-and-compile
  (defconst php-heredoc-start-re
    (rx "<<<"
        (* (syntax whitespace))
        (or (group (+ (or (syntax word) (syntax symbol))))
            (: "\"" (group (+ (or (syntax word) (syntax symbol)))) "\"")
            (: "'" (group (+ (or (syntax word) (syntax symbol)))) "'"))
        line-end)
    "Regular expression for the start of a PHP heredoc."))

(defun php-heredoc-end-re (heredoc-start)
  "Build a regular expression for the end of a heredoc started by the string
HEREDOC-START."
  ;; Extract just the identifier without <<< and quotes.
  (string-match "\\_<.+?\\_>" heredoc-start)
  (concat "^\\s-*\\(" (match-string 0 heredoc-start) "\\)\\W"))

(eval-and-compile
  (defconst php-syntax-propertize-rules
    (syntax-propertize-precompile-rules
     (php-heredoc-start-re
      (0 (ignore (php--syntax-propertize-heredoc
                  (match-beginning 0)
                  (or (match-string 1) (match-string 2) (match-string 3))
                  (null (match-string 3))))))
     ((rx "#[")
      (0 (ignore (php--syntax-propertize-attributes (match-beginning 0)))))
     ((rx (or "'" "\""))
      (0 (ignore (php--syntax-propertize-quotes-in-comment (match-beginning 0))))))))

(defalias 'php-syntax-propertize-function
  (syntax-propertize-rules php-syntax-propertize-rules))

(defun php--syntax-propertize-heredoc (start id _is-heredoc)
  "Apply propertize Heredoc and Nowdoc from START, with ID and IS-HEREDOC."
  (let ((terminator (rx-to-string `(: line-start (* (syntax whitespace)) ,id word-boundary))))
    (put-text-property start (1+ start) 'syntax-table (string-to-syntax "|"))
    (re-search-forward terminator nil t)
    (when (match-string 0)
      (put-text-property (1- (point)) (point) 'syntax-table (string-to-syntax "|")))))

(defun php--syntax-propertize-quotes-in-comment (pos)
  "Apply propertize quotes (' and \") from POS."
  (when (php-in-comment-p)
    (put-text-property pos (1+ pos) 'syntax-table (string-to-syntax "_"))))

(defun php--syntax-propertize-attributes (start)
  "Apply propertize PHP8 #[Attributes] (without # comment) from START."
  (unless (php-in-string-p)
    (put-text-property start (1+ start) 'syntax-table (string-to-syntax "."))))

(defvar-local php-mode--propertize-extend-region-current nil
  "Prevent undesirable recursion in PHP-SYNTAX-PROPERTIZE-EXTEND-REGION.")

(defun php-syntax-propertize-extend-region (start end)
  "Extend the propertize region if START or END falls inside a PHP heredoc."
  (let ((pair (cons start end)))
    (when (not (member pair php-mode--propertize-extend-region-current))
      ;; re-search functions may trigger
      ;; syntax-propertize-extend-region-functions to be called again, which in
      ;; turn call this to be called again.
      (push pair php-mode--propertize-extend-region-current)
      (unwind-protect
          (let (new-start new-end)
            (goto-char start)
            (when (re-search-backward php-heredoc-start-re nil t)
              (let ((maybe (point)))
                (when (and (re-search-forward (php-heredoc-end-re (match-string 0)) nil t)
                           (> (point) start))
                  (setq new-start maybe)
                  (when (> (point) end)
                    (setq new-end (point))))))
            (unless new-end
              (goto-char end)
              (when (re-search-backward php-heredoc-start-re start t)
                (if (re-search-forward (php-heredoc-end-re (match-string 0)) nil t)
                    (when (> (point) end)
                      (setq new-end (point)))
                  (setq new-end (point-max)))))
            (when (or new-start new-end)
              (cons (or new-start start) (or new-end end))))
        ;; Cleanup
        (setq php-mode--propertize-extend-region-current
              (delete pair php-mode--propertize-extend-region-current))))))

;;; PHPDoc font-lock

(defconst php-phpdoc-type-names
  '(;; PHPStan and Psalm types
    "__stringandstringable" "array" "array-key" "associative-array" "bool" "boolean"
    "callable" "callable-array" "callable-object" "callable-string" "class-string"
    "closed-resource" "double" "empty" "empty-scalar" "enum-string" "false" "float"
    "int" "integer" "interface-string" "iterable" "list" "literal-string" "lowercase-string"
    "mixed" "negative-int" "never" "never-return" "never-returns" "no-return" "non-empty-array"
    "non-empty-list" "non-empty-literal-string" "non-empty-lowercase-string" "non-empty-mixed"
    "non-empty-scalar" "non-empty-string" "non-empty-uppercase-string" "non-falsy-string"
    "non-negative-int" "non-positive-int" "non-zero-int" "noreturn" "null" "number" "numeric"
    "numeric-string" "object" "open-resource" "parent" "positive-int" "pure-callable"
    "pure-closure" "resource" "scalar" "self" "static" "string" "trait-string" "true"
    "truthy-string" "uppercase-string" "void"
    ;; PHPStan Generic Types
    "key-of" "value-of" "int-mask-of" "int-mask" "__benevolent" "template-type" "new")
  "A list of type and pseudotype names that can be used in PHPDoc.")

(defconst php-phpdoc-type-tags
  (list "package" "param" "property" "property-read" "property-write"
        "return" "throws" "var" "self-out" "this-out" "param-out"
        "type" "extends" "require-extends" "implemtents" "require-implements"
        "template" "template-covariant" "template-extends" "template-implements"
        "require-extends" "require-implements"
        "assert" "assert-if-true" "assert-if-false" "if-this-is")
  "A list of tags specifying type names.")

(defconst php-phpdoc-font-lock-doc-comments
  `(("{@[-[:alpha:]]+\\s-*\\([^}]*\\)}" ; "{@foo ...}" markup.
     (0 'php-doc-annotation-tag prepend nil)
     (1 'php-string prepend nil))
    (,(rx (group "$") (group (in "A-Za-z_") (* (in "0-9A-Za-z_"))))
     (1 'php-doc-variable-sigil prepend nil)
     (2 'php-variable-name prepend nil))
    ("\\(\\$\\)\\(this\\)\\>" (1 'php-doc-$this-sigil prepend nil) (2 'php-doc-$this prepend nil))
    (,(concat "\\s-@" (rx (? (or "phan" "phpstan" "psalm") "-")) (regexp-opt php-phpdoc-type-tags) "\\s-+"
              "\\(" (rx (+ (? "?") (? "\\") (+ (in "0-9A-Z_a-z")) (? "[]") (? "|"))) "\\)+")
     1 'php-string prepend nil)
    (,(concat "\\(?:|\\|\\?\\|\\s-\\)\\("
              (regexp-opt php-phpdoc-type-names 'words)
              "\\)")
     1 font-lock-type-face prepend nil)
    ("^\\(?:/\\*\\)?\\(?:\\s \\|\\*\\)*\\(@[[:alpha:]][-[:alpha:]\\]*\\)" ; "@foo ..." markup.
     1 'php-doc-annotation-tag prepend nil)))

(defun php--fontlock-doc-apply-highlight (highlight)
  "Apply a single font-lock HIGHLIGHT form using the current match data.
HIGHLIGHT is (GROUP FACE OVERRIDE LAXMATCH) as in `font-lock-keywords'."
  (let* ((group (nth 0 highlight))
         (face (eval (nth 1 highlight) t))
         (override (nth 2 highlight))
         (beg (match-beginning group))
         (end (match-end group)))
    (when (and beg end)
      (pcase override
        ('prepend (font-lock-prepend-text-property beg end 'face face))
        ('append (font-lock-append-text-property beg end 'face face))
        ('t (put-text-property beg end 'face face))
        (_ (unless (get-text-property beg 'face)
             (put-text-property beg end 'face face)))))))

(defun php--fontlock-doc-comments (start-re limit keywords)
  "Fontify PHPDoc comments up to LIMIT, replacing `c-font-lock-doc-comments'.
START-RE matches the beginning of a doc comment (e.g. \"/\\*\\*\").
KEYWORDS is a list of font-lock matcher entries applied within the body
of each doc comment."
  (let ((case-fold-search nil))
    (while (re-search-forward start-re limit t)
      (let ((beg (match-beginning 0))
            (ppss (save-excursion (syntax-ppss (match-beginning 0))))
            end)
        (when (nth 4 ppss)
          (let ((cstart (or (nth 8 ppss) beg)))
            (setq end (save-excursion
                        (goto-char cstart)
                        (if (re-search-forward "\\*/" limit t) (point) limit)))
            (save-excursion
              (save-restriction
                (narrow-to-region cstart end)
                (dolist (kw keywords)
                  (let ((matcher (car kw))
                        (rest (cdr kw)))
                    (goto-char (point-min))
                    (while (re-search-forward matcher nil t)
                      (if (numberp (car rest))
                          (php--fontlock-doc-apply-highlight rest)
                        (dolist (h rest)
                          (php--fontlock-doc-apply-highlight h))))))))
            (goto-char end)))))
    nil))

(defvar php-phpdoc-font-lock-keywords
  `((,(lambda (limit)
        (php--fontlock-doc-comments "/\\*\\*" limit
                                    php-phpdoc-font-lock-doc-comments)))))

;;; Font-lock custom matchers

(defconst php-string-interpolated-variable-regexp
  "{\\$[^}\n\\\\]*\\(?:\\\\.[^}\n\\\\]*\\)*}\\|\\${\\sw+}\\|\\$\\sw+")

(defun php-mode--error-control-op-font-lock-find (limit)
  "Font-lock matcher for the error-control operator `@' up to LIMIT.
Match a single `@' used as the error-control operator, skipping
occurrences inside strings or comments."
  (let (found)
    (while (and (not found)
                (re-search-forward "@" limit t))
      (unless (save-match-data (php-in-string-or-comment-p))
        (setq found t)))
    found))

(defun php-mode--string-interpolated-variable-font-lock-find (limit)
  "Apply text-property to LIMIT for string interpolation by font-lock."
  (let (quoted-stuff)
    (while (re-search-forward php-string-interpolated-variable-regexp limit t)
      (setq quoted-stuff (php-in-string-p))
      (when (or (eq ?\" quoted-stuff) (eq ?` quoted-stuff))
        (put-text-property (match-beginning 0) (match-end 0) 'face 'php-variable-name))))
  nil)

;;; Font-lock keywords

(defconst php-font-lock-keywords
  (append
   ;; PHPDoc /** ... */ comments.
   php-phpdoc-font-lock-keywords

   ;; Patterns that must win over the generic keyword matchers below.
   `(
     ;; Class declaration specification keywords (implements, extends)
     ("\\_<\\(?:implements\\|extends\\)\\_>" . 'php-class-declaration-spec)
     ;; Namespace declaration
     ("\\_<namespace\\_>" . 'php-namespace-declaration)
     ;; import statement
     ("\\_<use\\_>" . 'php-import-declaration)
     ;; Class modifiers (abstract, final)
     ("\\_<\\(abstract\\|final\\)\\_>\\s-+\\_<class\\>" 1 'php-class-modifier)

     ;; Highlight variables, e.g. 'var' in '$var' and '$obj->var', but
     ;; not in $obj->var()
     ("\\(->\\)\\(\\sw+\\)\\s-*(" (1 'php-object-op) (2 php-method-call))
     ("\\<\\(const\\)\\s-+\\(\\_<.+?\\_>\\)" (1 'php-keyword) (2 'php-constant-assign))

     ;; Logical operator (!)
     ("\\(!\\)[^=]" 1 'php-logical-op)

     ;; Highlight special variables
     ("\\(\\$\\)\\(this\\)\\>" (1 'php-this-sigil) (2 'php-this))
     ("\\(\\$+\\)\\(\\sw+\\)" (1 'php-variable-sigil) (2 'php-variable-name))
     ("\\(->\\)\\([a-zA-Z0-9_]+\\)" (1 'php-object-op) (2 'php-property-name))

     ;; Highlight function/method names
     ("\\<function\\s-+&?\\(\\(?:\\sw\\|\\s_\\)+\\)\\s-*(" 1 'php-function-name)

     ;; 'array' and 'callable' are keywords, except in the following situations:
     ;; - when used as a type hint
     ;; - when used as a return type
     ("\\b\\(array\\|callable\\)\\s-+&?\\$" 1 font-lock-type-face)
     (")\\s-*:\\s-*\\??\\(array\\|callable\\)\\b" 1 font-lock-type-face)
     ;; For 'array', there is an additional situation:
     ;; - when used as cast, so that (int) and (array) look the same
     ("(\\(array\\))" 1 font-lock-type-face)

     (,(regexp-opt php-magical-constants 'symbols) (1 'php-magical-constant))
     ;; namespaces
     ("\\(\\([a-zA-Z0-9_]+\\\\\\)+[a-zA-Z0-9_]+\\|\\(\\\\[a-zA-Z0-9_]+\\)+\\)[^:a-zA-Z0-9_\\\\]" 1 'font-lock-type-face)
     ("\\(\\([a-zA-Z0-9_]+\\\\\\)+[a-zA-Z0-9_]+\\|\\(\\\\[a-zA-Z0-9_]+\\)+\\)::" 1 'php-constant)
     (,(eval-when-compile
         (rx bol (* (syntax whitespace))
             (or "private" "protected" "public")
             (+ (syntax whitespace))
             (group (? "?") (+ (or "\\" (syntax word) (syntax symbol))))
             (+ (syntax whitespace))
             (: "$" (+ (or (syntax word) (syntax symbol))))))
      1 'php-class)
     ;; Support the ::class constant in PHP5.6
     ("\\sw+\\(::\\)\\(class\\)\\b" (1 'php-paamayim-nekudotayim) (2 'php-magical-constant))
     ;; Class declaration keywords (class, trait, interface)
     ("\\_<\\(class\\|trait\\|interface\\)\\_>" . 'php-class-declaration)

     ;; Highlight static method calls as such. This is necessary for method
     ;; names which are identical to keywords to be highlighted correctly.
     ("\\sw+::\\(\\sw+\\)(" 1 php-static-method-call)
     ;; Multiple catch (FooException | BarException $e)
     (,(rx symbol-start "catch" symbol-end
           (* (syntax whitespace)) "(" (* (syntax whitespace))
           (group (+ (or (syntax word) (syntax symbol)))))
      (1 font-lock-type-face)
      (,(rx (* (syntax whitespace)) "|" (* (syntax whitespace))
            (group (+ (or (syntax word) (syntax symbol))) symbol-end))
       nil nil (1 font-lock-type-face)))
     ;; PHP open/close tags.
     (,(regexp-opt '("<?php" "<?=" "?>"
                     "<?"      ;; obsolete short open tag
                     "<%" "%>" ;; obsolete ASP tag
                     ))
      0 'php-php-tag))

   ;; Keyword matchers replacing what CC Mode used to fontify.
   `(
     ;; `__halt_compiler' is a language construct written with a call-like
     ;; `()' suffix; fontify it before the generic function-call matcher.
     ("\\_<__halt_compiler\\_>" 0 'php-keyword)
     ;; true / false / null literals.
     (,php-keywords--constants-re 1 'php-constant)
     ;; Control structures, declarations and statements => php-keyword.
     (,php-keywords--control-structures-re 1 'php-keyword)
     (,php-keywords--declarations-re 1 'php-keyword)
     (,php-keywords--statements-re 1 'php-keyword)
     ;; Primitive / pseudo type names.
     (,php-keywords--types-re 1 font-lock-type-face))

   ;; Patterns applied last: only fill faces not already fontified.
   `(
     (php-mode--error-control-op-font-lock-find 0 'php-errorcontrol-op t)
     ;; import function statement
     (,(rx symbol-start (group "use" (+ (syntax whitespace)) "function")
           (+ (syntax whitespace)))
      (1 'php-import-declaration)
      (,(rx (group (+ (or (syntax word) (syntax symbol) "\\" "{" "}")))) nil nil (1 'php-function-name t)))
     ;; import constant statement
     (,(rx symbol-start (group "use" (+ (syntax whitespace)) "const")
           (+ (syntax whitespace)))
      (1 'php-import-declaration)
      (,(rx (group (+ (or (syntax word) (syntax symbol) "\\" "{" "}")))) nil nil (1 'php-constant-assign t)))
     ;; Highlight function calls
     ("\\(\\_<\\(?:\\sw\\|\\s_\\)+?\\_>\\)\\s-*(" 1 php-function-call)
     ;; Highlight all upper-cased symbols as constant
     ("\\<\\([A-Z_][A-Z0-9_]+\\)\\>" 1 'php-constant)

     ;; Highlight all statically accessed class names as constant.
     ("\\(\\sw+\\)\\(::\\)" (1 'php-constant) (2 'php-paamayim-nekudotayim))

     ;; Highlight class name after "use .. as"
     ("\\<as\\s-+\\(\\sw+\\)" 1 font-lock-type-face)

     ;; Class names in declarations (class, trait, interface, enum).
     (,(concat (regexp-opt '("class" "trait" "interface" "enum"))
               " \\(\\sw+\\)")
      1 font-lock-type-face)

     ;; Highlight the ? character for nullable return types.
     ("function.+:\\s-*\\(\\?\\)\\(?:\\sw\\|\\s_\\|\\\\\\)+" 1 font-lock-type-face)
     (")\\s-*:\\s-*\\(\\?\\)\\(?:\\sw\\|\\s_\\|\\\\\\)+\\s-*\\(?:\{\\|;\\)" 1 font-lock-type-face)

     ;; Highlight the ? character for nullable type hints.
     ("\\(\\?\\)\\(:?\\sw\\|\\s_\\|\\\\\\)+\\s-+\\$" 1 font-lock-type-face)

     ;; Class names without a namespace used as nullable type hints/returns.
     ("\\?\\(\\(:?\\sw\\|\\s_\\)+\\)\\s-+\\$" 1 font-lock-type-face)
     ("function.+:\\s-*\\??\\(\\(?:\\sw\\|\\s_\\)+\\)" 1 font-lock-type-face)
     (")\\s-*:\\s-*\\??\\(\\(?:\\sw\\|\\s_\\)+\\)\\s-*\\(?:\{\\|;\\)" 1 font-lock-type-face)

     ;; Assignment operators (=, +=, ...)
     ("\\([^=<!>]+?\\([\-+./%]?=\\)[^=<!]+?\\)" 2 'php-assignment-op)

     ;; Comparison operators (==, ===, >=, ...)
     ("\\([!=]=\\{1,2\\}[>]?\\|[<>]=?\\)" 1 'php-comparison-op)

     ;; Arithmetic operators (+, -, *, **, /, %)
     ("\\(?:[A-Za-z0-9[:blank:]]\\)\\([\-+*/%]\\*?\\)\\(?:[A-Za-z0-9[:blank:]]\\)" 1 'php-arithmetic-op)

     ;; Increment and Decrement operators (++, --)
     ("\\(\-\-\\|\+\+\\)\$\\w+" 1 'php-inc-dec-op) ;; pre inc/dec
     ("\$\\w+\\(\-\-\\|\+\+\\)" 1 'php-inc-dec-op) ;; post inc/dec

     ;; Logical operators (&&, ||)
     ("\\(&&\\|||\\)" 1 'php-logical-op)
     ;; string interpolation ("$var, ${var}, {$var}")
     (php-mode--string-interpolated-variable-font-lock-find 0 nil)
     (,(rx symbol-start (group (or "get" "set")) (+ (syntax whitespace)) (or "{" "=>"))
      1 'php-builtin)))
  "Font-lock keywords for `php-mode'.")

;;; Navigation

(defun php-beginning-of-defun (&optional arg)
  "Move to the beginning of the ARGth PHP function from point.
Implements PHP version of `beginning-of-defun-function'."
  (interactive "p")
  (let (found-p (arg (or arg 1)))
    (while (> arg 0)
      (setq found-p (re-search-backward php-beginning-of-defun-regexp
                                        nil 'noerror))
      (setq arg (1- arg)))
    (while (< arg 0)
      (end-of-line 1)
      (let ((opoint (point)))
        (beginning-of-defun 1)
        (forward-list 2)
        (forward-line 1)
        (if (eq opoint (point))
            (setq found-p (re-search-forward php-beginning-of-defun-regexp
                                             nil 'noerror)))
        (setq arg (1+ arg))))
    (not (null found-p))))

(defun php-end-of-defun (&optional arg)
  "Move the end of the ARGth PHP function from point.
Implements PHP version of `end-of-defun-function'.

See `php-beginning-of-defun'."
  (interactive "p")
  (php-beginning-of-defun (- (or arg 1))))

;;; HTML template warning

(defvar-local php-mode--warned-bad-indent nil
  "Non-nil once the user has been warned about indenting this buffer.")

(defun php-mode--check-html-for-indentation ()
  "Return non-nil when the current buffer may be indented as PHP.
Warn and offer to switch to `php-html-template-major-mode' when the
buffer looks like an HTML template edited in plain `php-mode'."
  (cond
   ((not php-mode-warn-if-html-template) t)
   ((php-in-poly-php-html-mode) t)
   ((not (php-buffer-has-html-tag)) t)
   (php-mode--warned-bad-indent nil)
   ((fboundp php-html-template-major-mode)
    (if (y-or-n-p (format "This file seems to contain an HTML tag.  Switch to %s? "
                          php-html-template-major-mode))
        (prog1 nil (funcall php-html-template-major-mode))
      (prog1 nil
        (setq-local php-mode--warned-bad-indent t))))
   (t
    (setq-local php-mode--warned-bad-indent t)
    (lwarn 'php-mode
           :warning "Indentation fails badly with mixed HTML/PHP in the HTML part in plain `php-mode'.
It is highly recommended to install a major mode that supports PHP and HTML templates, such as Web Mode.

Set `php-html-template-major-mode' variable to use a mode other than `web-mode'.
Set `php-mode-warn-if-html-template' variable to nil to suppress the warning.
")
    nil)))

(defun php-mode--indent-line ()
  "Indent the current line, warning about HTML templates first."
  (when (php-mode--check-html-for-indentation)
    (php-indent-line)))

(defun php-mode--indent-region (start end)
  "Indent region START to END, warning about HTML templates first."
  (when (php-mode--check-html-for-indentation)
    (php-indent-region start end)))

;;; Keymap

(defvar php-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-M-h") #'mark-defun)
    (define-key map (kbd "C-c C-f") #'php-search-documentation)
    (define-key map (kbd "C-c C-m") #'php-browse-manual)
    (define-key map (kbd "C-c C-r") #'php-set-style)
    map)
  "Keymap for `php-mode'.")

;;; Major mode

;;;###autoload
(define-derived-mode php-mode php-base-mode "PHP"
  "Major mode for editing PHP code, without a dependency on CC Mode.

\\{php-mode-map}"
  :syntax-table php-mode-syntax-table
  ;; PHP keywords are case-insensitive.
  (setq case-fold-search t)

  ;; Comments (newcomment.el).
  (setq-local comment-start "// ")
  (setq-local comment-end "")
  (setq-local comment-start-skip
              (eval-when-compile
                (rx (group (or (: "#" (not (any "[")))
                               (: "/" (+ "/"))
                               (: "/*")))
                    (* (syntax whitespace)))))

  ;; Paragraphs / page delimiter.
  (setq-local page-delimiter php-mode-page-delimiter)
  (setq-local parse-sexp-ignore-comments t)

  ;; Syntax propertize (heredoc/nowdoc, attributes, comment quotes).
  (setq-local syntax-propertize-function #'php-syntax-propertize-function)
  (add-hook 'syntax-propertize-extend-region-functions
            #'php-syntax-propertize-extend-region t t)

  ;; Font-lock.
  (with-suppressed-warnings ((obsolete font-lock-string-face
                                       font-lock-keyword-face
                                       font-lock-builtin-face
                                       font-lock-function-name-face
                                       font-lock-variable-name-face
                                       font-lock-constant-face))
    (setq-local font-lock-string-face 'php-string)
    (setq-local font-lock-keyword-face 'php-keyword)
    (setq-local font-lock-builtin-face 'php-builtin)
    (setq-local font-lock-function-name-face 'php-function-name)
    (setq-local font-lock-variable-name-face 'php-variable-name)
    (setq-local font-lock-constant-face 'php-constant))
  (setq font-lock-defaults '(php-font-lock-keywords nil t (("_$" . "w")) nil))

  ;; Indentation.
  (setq-local indent-line-function #'php-mode--indent-line)
  (setq-local indent-region-function #'php-mode--indent-region)
  (setq-local open-paren-in-column-0-is-defun-start nil)
  (setq-local defun-prompt-regexp
              "^\\s-*function\\s-+&?\\s-*\\(\\(\\sw\\|\\s_\\)+\\)\\s-*")

  ;; Navigation.
  (setq-local beginning-of-defun-function #'php-beginning-of-defun)
  (setq-local end-of-defun-function #'php-end-of-defun)
  (setq-local add-log-current-defun-function nil)
  (setq-local add-log-current-defun-header-regexp php-beginning-of-defun-regexp)

  ;; Imenu.
  (setq imenu-generic-expression (if (symbolp php-imenu-generic-expression)
                                     (symbol-value php-imenu-generic-expression)
                                   php-imenu-generic-expression))
  (setq-local imenu-create-index-function #'imenu-default-create-index-function)

  ;; Coding style (php-style.el): applies the style variables, honors a
  ;; legacy buffer-local `c-basic-offset', and supports project styles.
  (php-style-setup-buffer)

  (when (or php-mode-force-pear
            (and (stringp buffer-file-name)
                 (string-match "PEAR\\|pear" buffer-file-name)
                 (string-match "\\.php\\'" buffer-file-name)))
    (php-set-style "pear"))

  ;; Flymake.
  (when (and (eval-when-compile (boundp 'flymake-diagnostic-functions))
             php-mode-replace-flymake-diag-function)
    (add-hook 'flymake-diagnostic-functions php-mode-replace-flymake-diag-function nil t)))

;;; Autoload registration

;;;###autoload
(add-to-list 'interpreter-mode-alist
             ;; Match php, php-3, php5, php7, php5.5, php-7.0.1, etc.
             (cons "php\\(?:-?[34578]\\(?:\\.[0-9]+\\)*\\)?" 'php-mode))

;;;###autoload
(progn
  (add-to-list 'auto-mode-alist '("/\\.php_cs\\(?:\\.dist\\)?\\'" . php-mode))
  (add-to-list 'auto-mode-alist '("\\.\\(?:php\\.inc\\|stub\\)\\'" . php-mode))
  (add-to-list 'auto-mode-alist '("\\.\\(?:php[s345]?\\|phtml\\)\\'" . php-mode-maybe)))

(provide 'php-mode)
;;; php-mode.el ends here

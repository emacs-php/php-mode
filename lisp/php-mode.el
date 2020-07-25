;;; php-mode.el --- Major mode for editing PHP code

;; Copyright (C) 2020  Friends of Emacs-PHP development
;; Copyright (C) 1999, 2000, 2001, 2003, 2004 Turadg Aleahmad
;;               2008 Aaron S. Hawley
;;               2011, 2012, 2013, 2014, 2015, 2016, 2017 Eric James Michael Ritz

;; Author: Eric James Michael Ritz
;; Maintainer: USAMI Kenta <tadsan@zonu.me>
;; URL: https://github.com/emacs-php/php-mode
;; Keywords: languages php
;; Version: 1.23.0
;; Package-Requires: ((emacs "24.3"))
;; License: GPL-3.0-or-later

(defconst php-mode-version-number "1.23.0"
  "PHP Mode version number.")

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

;; PHP Mode is a major mode for editing PHP script.  It's an extension
;; of CC mode; thus it inherits all C mode's navigation functionality.
;; But it colors according to the PHP syntax and indents according to the
;; PSR-2 coding guidelines.  It also includes a couple handy IDE-type
;; features such as documentation search and a source and class browser.

;; Please read the manual for setting items compatible with CC Mode.
;; https://www.gnu.org/software/emacs/manual/html_mono/ccmode.html

;; This mode is designed for PHP scripts consisting of a single <?php block.
;; We recommend the introduction of Web Mode for HTML and Blade templates combined with PHP.
;; http://web-mode.org/

;; Modern PHP Mode can be set on a project basis by .dir-locals.el.
;; Please read php-project.el for details of directory local variables.

;; If you are using a package manager, you do not need (require 'php-mode) in
;; your ~/.emacs.d/init.el.  Read the README for installation instructions.
;; https://github.com/emacs-php/php-mode

;;; Code:

(require 'php)
(require 'php-face)
(require 'cc-mode)
(require 'cc-langs)

(eval-when-compile
  (require 'cc-fonts))

;; Boilerplate from other `cc-mode' derived modes. See
;; http://cc-mode.sourceforge.net/derived-mode-ex.el for details on how this all
;; fits together.
(eval-and-compile
  (c-add-language 'php-mode 'java-mode))

(require 'font-lock)
(require 'custom)
(require 'etags)
(require 'speedbar)
(require 'imenu)
(require 'nadvice nil t)

(require 'cl-lib)
(require 'mode-local)
(require 'php-project)

(eval-when-compile
  (require 'regexp-opt)
  (defvar add-log-current-defun-header-regexp)
  (defvar add-log-current-defun-function)
  (defvar c-vsemi-status-unknown-p)
  (defvar syntax-propertize-via-font-lock))

;; Work around emacs bug#18845, cc-mode expects cl to be loaded
;; while php-mode only uses cl-lib (without compatibility aliases)
(eval-and-compile
  (when (and (= emacs-major-version 24) (>= emacs-minor-version 4))
    (require 'cl)))

;; Work around https://github.com/emacs-php/php-mode/issues/310.
;;
;; In emacs 24.4 and 24.5, lines after functions with a return type
;; are incorrectly analyzed as member-init-cont.
;;
;; Before emacs 24.4, c member initializers are not supported this
;; way. Starting from emacs 25.1, cc-mode only detects member
;; initializers when the major mode is c++-mode.
(eval-and-compile
  (if (and (= emacs-major-version 24) (or (= emacs-minor-version 4)
                                          (= emacs-minor-version 5)))
      (defun c-back-over-member-initializers ()
        ;; Override of cc-engine.el, cc-mode in emacs 24.4 and 24.5 are too
        ;; optimistic in recognizing c member initializers. Since we don't
        ;; need it in php-mode, just return nil.
        nil)))

(autoload 'php-mode-debug "php-mode-debug"
  "Display informations useful for debugging PHP Mode." t)

;; Local variables

;;;###autoload
(defgroup php-mode nil
  "Major mode for editing PHP code."
  :tag "PHP Mode"
  :prefix "php-mode-"
  :group 'languages
  :group 'php
  :link '(url-link :tag "Official Site" "https://github.com/emacs-php/php-mode")
  :link '(url-link :tag "PHP Mode Wiki" "https://github.com/emacs-php/php-mode/wiki"))

(define-obsolete-variable-alias 'php-default-face 'php-mode-default-face "1.20.0")
(defcustom php-mode-default-face 'default
  "Default face in `php-mode' buffers."
  :group 'php-mode
  :tag "PHP Mode Default Face"
  :type 'face)

(define-obsolete-variable-alias 'php-speedbar-config 'php-mode-speedbar-config "1.20.0")
(defcustom php-mode-speedbar-config t
  "When set to true automatically configures Speedbar to observe PHP files.
Ignores php-file patterns option; fixed to expression \"\\.\\(inc\\|php[s345]?\\)\""
  :group 'php-mode
  :tag "PHP Mode Speedbar Config"
  :type 'boolean
  :set (lambda (sym val)
         (set-default sym val)
         (when val
             (speedbar-add-supported-extension
              "\\.\\(inc\\|php[s345]?\\|phtml\\)"))))

(defcustom php-mode-speedbar-open nil
  "Normally `php-mode' starts with the speedbar closed.
Turning this on will open it whenever `php-mode' is loaded."
  :group 'php-mode
  :tag "PHP Mode Speedbar Open"
  :type 'boolean
  :set (lambda (sym val)
         (set-default sym val)
         (when val
           (speedbar 1))))

(define-obsolete-variable-alias 'php-template-compatibility 'php-mode-template-compatibility "1.20.0")
(defcustom php-mode-template-compatibility t
  "Should detect presence of html tags."
  :group 'php-mode
  :tag "PHP Mode Template Compatibility"
  :type 'boolean)

(define-obsolete-variable-alias 'php-lineup-cascaded-calls 'php-mode-lineup-cascaded-calls "1.20.0")
(defcustom php-mode-lineup-cascaded-calls nil
  "Indent chained method calls to the previous line."
  :group 'php-mode
  :tag "PHP Mode Lineup Cascaded Calls"
  :type 'boolean)

(defcustom php-mode-page-delimiter
  (eval-when-compile
    (rx symbol-start
        (or "namespace" "function" "class" "trait" "interface")
        symbol-end))
  "Regexp describing line-beginnings that PHP declaration statements."
  :group 'php-mode
  :tag "PHP Mode Page Delimiter"
  :type 'regexp)

(define-obsolete-variable-alias 'php-do-not-use-semantic-imenu 'php-mode-do-not-use-semantic-imenu "1.20.0")
(defcustom php-mode-do-not-use-semantic-imenu t
  "Customize `imenu-create-index-function' for `php-mode'.

If using function `semantic-mode' `imenu-create-index-function' will be
set to `semantic-create-imenu-index' due to `c-mode' being its
parent.  Set this variable to t if you want to use
`imenu-default-create-index-function' even with `semantic-mode'
enabled."
  :group 'php-mode
  :tag "PHP Mode Do Not Use Semantic Imenu"
  :type 'boolean)

(defcustom php-completion-file ""
  "Path to the file which contains the function names known to PHP."
  :type 'string)

(defcustom php-manual-path ""
  "Path to the directory which contains the PHP manual."
  :type 'string)

;;;###autoload
(if (version< emacs-version "24.4")
    (dolist (i '("php" "php5" "php7"))
      (add-to-list 'interpreter-mode-alist (cons i 'php-mode)))
  (add-to-list 'interpreter-mode-alist
               ;; Match php, php-3, php5, php7, php5.5, php-7.0.1, etc.
               (cons "php\\(?:-?[3457]\\(?:\\.[0-9]+\\)*\\)?" 'php-mode)))

(defcustom php-mode-hook nil
  "List of functions to be executed on entry to `php-mode'."
  :group 'php-mode
  :tag "PHP Mode Hook"
  :type 'hook)

(defcustom php-mode-pear-hook nil
  "Hook called when a PHP PEAR file is opened with `php-mode'."
  :group 'php-mode
  :tag "PHP Mode Pear Hook"
  :type 'hook)

(defcustom php-mode-drupal-hook nil
  "Hook called when a Drupal file is opened with `php-mode'."
  :group 'php-mode
  :tag "PHP Mode Drupal Hook"
  :type 'hook)

(defcustom php-mode-wordpress-hook nil
  "Hook called when a WordPress file is opened with `php-mode'."
  :group 'php-mode
  :tag "PHP Mode WordPress Hook"
  :type 'hook)

(defcustom php-mode-symfony2-hook nil
  "Hook called when a Symfony2 file is opened with `php-mode'."
  :group 'php-mode
  :tag "PHP Mode Symfony2 Hook"
  :type 'hook)

(defcustom php-mode-psr2-hook nil
  "Hook called when a PSR-2 file is opened with `php-mode'."
  :group 'php-mode
  :tag "PHP Mode PSR-2 Hook"
  :type 'hook)

(defcustom php-mode-force-pear nil
  "Normally PEAR coding rules are enforced only when the filename contains \"PEAR.\"
Turning this on will force PEAR rules on all PHP files."
  :group 'php-mode
  :tag "PHP Mode Force Pear"
  :type 'boolean)

(defcustom php-mode-warn-if-mumamo-off t
  "Warn once per buffer if you try to indent a buffer without
mumamo-mode turned on.  Detects if there are any HTML tags in the
buffer before warning, but this is is not very smart; e.g. if you
have any tags inside a PHP string, it will be fooled."
  :group 'php-mode
  :tag "PHP Mode Warn If MuMaMo Off"
  :type '(choice (const :tag "Warn" t) (const "Don't warn" nil)))

(defcustom php-mode-coding-style 'pear
  "Select default coding style to use with php-mode.
This variable can take one of the following symbol values:

`Default' - use a reasonable default style for PHP.
`PSR-2' - use PSR standards (PSR-2, PSR-12).
`PEAR' - use coding styles preferred for PEAR code and modules.
`Drupal' - use coding styles preferred for working with Drupal projects.
`WordPress' - use coding styles preferred for working with WordPress projects.
`Symfony2' - use coding styles preferred for working with Symfony2 projects."
  :group 'php-mode
  :tag "PHP Mode Coding Style"
  :type '(choice (const :tag "Default" php)
                 (const :tag "PEAR" pear)
                 (const :tag "Drupal" drupal)
                 (const :tag "WordPress" wordpress)
                 (const :tag "Symfony2" symfony2)
                 (const :tag "PSR-2" psr2))
  :initialize 'custom-initialize-default)

;; Since this function has a bad influence on the environment of many users,
;; temporarily disable it
(defcustom php-mode-enable-project-coding-style nil
  "When set to true override php-mode-coding-style by php-project-coding-style.

If you want to suppress styles from being overwritten by directory / file
local variables, set NIL."
  :group 'php-mode
  :tag "PHP Mode Enable Project Coding Style"
  :type 'boolean)

(defcustom php-mode-enable-backup-style-variables t
  "When set to `T', back up values set by hook and buffer local variables.

This function may interfere with other hooks and other behaviors.
In that case set to `NIL'."
  :group 'php-mode
  :tag "PHP Mode Enable Backup Style Variables"
  :type 'boolean)

(defcustom php-mode-disable-c-auto-align-backslashes t
  "When set to non-NIL, override `c-auto-align-backslashes' to NIL."
  :group 'php-mode
  :tag "PHP Mode Disable c-auto-align-backslashes"
  :type 'boolean)

(define-obsolete-variable-alias 'php-mode-disable-parent-mode-hooks 'php-mode-disable-c-mode-hook "1.21.0")
(defcustom php-mode-disable-c-mode-hook t
  "When set to `T', do not run hooks of parent modes (`java-mode', `c-mode')."
  :group 'php-mode
  :tag "PHP Mode Disable C Mode Hook"
  :type 'boolean)

(defcustom php-mode-enable-project-local-variable t
  "When set to `T', apply project local variable to buffer local variable."
  :group 'php-mode
  :tag "PHP Mode Enable Project Local Variable"
  :type 'boolean)

(defconst php-mode-cc-vertion
  (eval-when-compile c-version))

(defun php-mode-version ()
  "Display string describing the version of PHP Mode."
  (interactive)
  (let ((fmt
         (eval-when-compile
           (let ((id "$Id$"))
             (concat "PHP Mode %s"
                     (if (string= id (concat [?$ ?I ?d ?$]))
                         ""
                       (concat " " id)))))))
    (funcall
     (if (called-interactively-p 'interactive) #'message #'format)
     fmt php-mode-version-number)))

;;;###autoload
(define-obsolete-variable-alias 'php-available-project-root-files 'php-project-available-root-files "1.19.0")

(defvar php-mode-map
  (let ((map (make-sparse-keymap "PHP Mode")))
    ;; Remove menu item for c-mode
    (define-key map [menu-bar C] nil)

    ;; By default PHP Mode binds C-M-h to c-mark-function, which it
    ;; inherits from cc-mode.  But there are situations where
    ;; c-mark-function fails to properly mark a function.  For
    ;; example, if we use c-mark-function within a method definition
    ;; then the region will expand beyond the method and into the
    ;; class definition itself.
    ;;
    ;; Changing the default to mark-defun provides behavior that users
    ;; are more likely to expect.
    (define-key map (kbd "C-M-h") 'mark-defun)

    ;; Many packages based on cc-mode provide the 'C-c C-w' binding
    ;; to toggle Subword Mode.  See the page
    ;;
    ;;     https://www.gnu.org/software/emacs/manual/html_node/ccmode/Subword-Movement.html
    ;;
    ;; for more information about Subword mode.
    (define-key map (kbd "C-c C-w") 'subword-mode)

    ;; We inherit c-beginning-of-defun and c-end-of-defun from CC Mode
    ;; but we have two replacement functions specifically for PHP.  We
    ;; remap the commands themselves and not their default
    ;; key-bindings so that our PHP-specific versions will work even
    ;; if the user has reconfigured their keys, e.g. if they rebind
    ;; c-end-of-defun to something other than C-M-e.
    (define-key map [remap c-beginning-of-defun] 'php-beginning-of-defun)
    (define-key map [remap c-end-of-defun] 'php-end-of-defun)
    (define-key map [remap c-set-style] 'php-set-style)

    (define-key map [(control c) (control f)] 'php-search-documentation)
    (define-key map [(meta tab)] 'php-complete-function)
    (define-key map [(control c) (control m)] 'php-browse-manual)
    (define-key map [(control .)] 'php-show-arglist)
    (define-key map [(control c) (control r)] 'php-send-region)
    ;; Use the Emacs standard indentation binding. This may upset c-mode
    ;; which does not follow this at the moment, but I see no better
    ;; choice.
    (define-key map [tab] 'indent-for-tab-command)
    map)
  "Keymap for `php-mode'.")

(c-lang-defconst c-mode-menu
  php (append '(["Complete function name" php-complete-function t]
                ["Browse manual" php-browse-manual t]
                ["Search documentation" php-search-documentation t]
                ["----" t])
              (c-lang-const c-mode-menu)))

(c-lang-defconst c-at-vsemi-p-fn
  php 'php-c-at-vsemi-p)

(c-lang-defconst c-vsemi-status-unknown-p-fn
  php 'php-c-vsemi-status-unknown-p)

(c-lang-defconst c-get-state-before-change-functions
  php nil)

(c-lang-defconst c-before-font-lock-functions
  php (c-get-lang-constant 'c-before-font-lock-functions nil t))

;; Make php-mode recognize opening tags as preprocessor macro's.
;;
;; This is a workaround, the tags must be recognized as something
;; in order for the syntactic guesses of code below the tag
;; to be correct and as a result not break indentation.
;;
;; Note that submatches or \\| here are not expected by cc-mode.
(c-lang-defconst c-opt-cpp-prefix
  php "\\s-*<\\?")

(c-lang-defconst c-anchored-cpp-prefix
  php "\\s-*\\(<\\?(=\\|\\sw+)\\)")

(c-lang-defconst c-identifier-ops
  php '(
        (left-assoc "\\" "::" "->")
        (prefix "\\" "::")))

(c-lang-defconst c-operators
  php `((prefix "new" "clone")
        ,@(c-lang-const c-identifier-ops)
        (postfix "->")
        (postfix "++" "--" "[" "]" "(" ")")
        (right-assoc "**")
        (prefix "++" "--" "+" "-" "~" "(" ")" "@")
        (prefix "instanceof")
        (prefix "!")
        (left-assoc "*" "/" "%")
        (left-assoc "+" "-" ".")
        (left-assoc "<<" ">>")
        (left-assoc "<" ">" "<=" ">=")
        (left-assoc "==" "!=" "===" "!==" "<>" "<=>")
        (left-assoc "&")
        (left-assoc "^")
        (left-assoc "|")
        (left-assoc "&&")
        (left-assoc "||")
        (right-assoc "??")
        (left-assoc "?:")
        (right-assoc-sequence "?" ":")
        (right-assoc ,@(c-lang-const c-assignment-operators))
        (left-assoc "and")
        (left-assoc "xor")
        (left-assoc "or")
        (left-assoc ",")))

;; Allow '\' when scanning from open brace back to defining
;; construct like class
(c-lang-defconst c-block-prefix-disallowed-chars
  php (cl-set-difference (c-lang-const c-block-prefix-disallowed-chars)
                      '(?\\)))

;; Allow $ so variables are recognized in cc-mode and remove @. This
;; makes cc-mode highlight variables and their type hints in arglists.
(c-lang-defconst c-symbol-start
  php (concat "[" c-alpha "_$]"))

;; All string literals can possibly span multiple lines
(c-lang-defconst c-multiline-string-start-char
  php t)

(c-lang-defconst c-assignment-operators
  php '("=" "*=" "/=" "%=" "+=" "-=" ">>=" "<<=" "&=" "^=" "|=" ".=" "??="))

(c-lang-defconst beginning-of-defun-function
  php 'php-beginning-of-defun)

(c-lang-defconst end-of-defun-function
  php 'php-end-of-defun)

(c-lang-defconst c-primitive-type-kwds
  php '("int" "integer" "bool" "boolean" "float" "double" "real"
        "string" "object" "void" "mixed"))

(c-lang-defconst c-class-decl-kwds
  "Keywords introducing declarations where the following block (if any)
contains another declaration level that should be considered a class."
  php '("class" "trait" "interface"))

(c-lang-defconst c-brace-list-decl-kwds
  "Keywords introducing declarations where the following block (if
any) is a brace list.

PHP does not have an \"enum\"-like keyword."
  php nil)

(c-lang-defconst c-typeless-decl-kwds
  php (append (c-lang-const c-class-decl-kwds) '("function")))

(c-lang-defconst c-modifier-kwds
  php '("abstract" "const" "final" "static"))

(c-lang-defconst c-protection-kwds
  "Access protection label keywords in classes."
  php '("private" "protected" "public"))

(c-lang-defconst c-postfix-decl-spec-kwds
  php '("implements" "extends"))

(c-lang-defconst c-type-list-kwds
  php '("@new" ;; @new is *NOT* language construct, it's workaround for coloring.
        "new" "use" "implements" "extends" "namespace" "instanceof" "insteadof"))

(c-lang-defconst c-ref-list-kwds
  php nil)

(c-lang-defconst c-block-stmt-2-kwds
  php '("catch" "declare" "elseif" "for" "foreach" "if" "switch" "while"))

(c-lang-defconst c-simple-stmt-kwds
  php '("break" "continue" "die" "echo" "exit" "goto" "return" "throw"
        "include" "include_once" "print" "require" "require_once"))

(c-lang-defconst c-constant-kwds
  php '("true" "false" "null"))

(c-lang-defconst c-lambda-kwds
  php '("function" "use"))

(c-lang-defconst c-inexpr-block-kwds
  php '("match"))

(c-lang-defconst c-other-block-decl-kwds
  php '("namespace"))

(c-lang-defconst c-other-kwds
  "Keywords not accounted for by any other `*-kwds' language constant."
  php
  '(
    "__halt_compiler"
    "and"
    "array"
    "as"
    "break"
    "catch"
    "clone"
    "default"
    "empty"
    "enddeclare"
    "endfor"
    "endforeach"
    "endif"
    "endswitch"
    "endwhile"
    "eval"
    "fn" ;; NOT c-lambda-kwds
    "global"
    "isset"
    "list"
    "or"
    "parent"
    "static"
    "unset"
    "var"
    "xor"
    "yield"
    "yield from"

    ;; Below keywords are technically not reserved keywords, but
    ;; threated no differently by php-mode from actual reserved
    ;; keywords
    ;;
    ;;; declare directives:
    "encoding"
    "ticks"
    "strict_types"

    ;;; self for static references:
    "self"))

;; PHP does not have <> templates/generics
(c-lang-defconst c-recognize-<>-arglists
  php nil)

(c-lang-defconst c-<>-type-kwds
  php nil)

(c-lang-defconst c-inside-<>-type-kwds
  php nil)

(c-lang-defconst c-enums-contain-decls
  php nil)

(c-lang-defconst c-nonlabel-token-key
  "Regexp matching things that can't occur in generic colon labels.

This overrides cc-mode `c-nonlabel-token-key' to support switching on
double quoted strings and true/false/null.

Note: this regexp is also applied to goto-labels, a future improvement
might be to handle switch and goto labels differently."
  php (concat
     ;; All keywords except `c-label-kwds' and `c-constant-kwds'.
     (c-make-keywords-re t
       (cl-set-difference (c-lang-const c-keywords)
                       (append (c-lang-const c-label-kwds)
                               (c-lang-const c-constant-kwds))
                       :test 'string-equal))))

(c-lang-defconst c-basic-matchers-before
  php (cl-remove-if (lambda (elm) (and (listp elm) (equal (car elm) "\\s|")))
                    (c-lang-const c-basic-matchers-before php)))

(c-lang-defconst c-basic-matchers-after
  php (cl-remove-if (lambda (elm) (and (listp elm) (memq 'c-annotation-face elm)))
                    (c-lang-const c-basic-matchers-after php)))

(c-lang-defconst c-opt-<>-sexp-key
  php nil)

(defconst php-mode--re-return-typed-closure
  (eval-when-compile
    (rx symbol-start "function" symbol-end
               (* (syntax whitespace))
               "(" (* (not (any "("))) ")"
               (* (syntax whitespace))
               (? symbol-start "use" symbol-end
                  (* (syntax whitespace))
                  "(" (* (not (any "("))) ")"
                  (* (syntax whitespace)))
               ":" (+ (not (any "{}")))
               (group "{"))))

(defun php-c-lineup-arglist (langelem)
  "Line up the current argument line under the first argument using `c-lineup-arglist' LANGELEM."
  (let (in-return-typed-closure)
    (when (and (consp langelem)
               (eq 'arglist-cont-nonempty (car langelem)))
      (save-excursion
        (save-match-data
          (when (re-search-backward php-mode--re-return-typed-closure (cdr langelem) t)
            (goto-char (match-beginning 1))
            (when (not (php-in-string-or-comment-p))
              (setq in-return-typed-closure t))))))
    (unless in-return-typed-closure
      (c-lineup-arglist langelem))))

(defun php-lineup-cascaded-calls (langelem)
  "Line up chained methods using `c-lineup-cascaded-calls',
but only if the setting is enabled"
  (if php-mode-lineup-cascaded-calls
      (c-lineup-cascaded-calls langelem)
    (save-excursion
      (beginning-of-line)
      (if (looking-at-p "\\s-*->") '+ nil))))

(defun php-c-looking-at-or-maybe-in-bracelist (&optional containing-sexp lim)
  "Replace `c-looking-at-or-maybe-in-bracelist'.

CONTAINING-SEXP is the position of the brace/paren/bracket enclosing
POINT, or nil if there is no such position, or we do not know it.  LIM is
a backward search limit."
  (cond
   ((looking-at-p "{")
    (save-excursion
      (c-backward-token-2 2 t lim)
      ;; PHP 8.0 match expression
      ;; echo match ($var) |{
      ;;     ↑ matches   ↑ initial position
      (when (looking-at-p (eval-when-compile (rx symbol-start "match" symbol-end)))
        (cons (point) t))))
   (t nil)))

(c-add-style
 "php"
 `((c-basic-offset . 4)
   (c-offsets-alist . ((arglist-close . php-lineup-arglist-close)
                       (arglist-cont . (first php-lineup-cascaded-calls 0))
                       (arglist-cont-nonempty . (first php-lineup-cascaded-calls php-c-lineup-arglist))
                       (arglist-intro . php-lineup-arglist-intro)
                       (case-label . +)
                       (class-open . 0)
                       (comment-intro . 0)
                       (inexpr-class . 0)
                       (inlambda . 0)
                       (inline-open . 0)
                       (namespace-open . 0)
                       (lambda-intro-cont . +)
                       (label . +)
                       (statement-cont . (first php-lineup-cascaded-calls php-lineup-string-cont +))
                       (substatement-open . 0)
                       (topmost-intro-cont . (first php-lineup-cascaded-calls +))))
   (indent-tabs-mode . nil)
   (tab-width . ,(default-value 'tab-width))
   (fill-column . ,(default-value 'fill-column))
   (show-trailing-whitespace . ,(default-value 'show-trailing-whitespace))
   (php-mode-lineup-cascaded-calls . t)
   (php-style-delete-trailing-whitespace . nil)))

(defun php-enable-default-coding-style ()
  "Set PHP Mode to use reasonable default formatting."
  (interactive)
  (php-set-style "php"))

(c-add-style
 "pear"
 '("php"
   (c-basic-offset . 4)
   (c-offsets-alist . ((case-label . 0)))
   (tab-width . 4)))

(defun php-enable-pear-coding-style ()
  "Set up php-mode to use the coding styles preferred for PEAR code and modules."
  (interactive)
  (php-set-style "pear"))

(c-add-style
 "drupal"
 '("php"
   (c-basic-offset . 2)
   (tab-width . 2)
   (fill-column . 78)
   (show-trailing-whitespace . t)
   (php-mode-lineup-cascaded-calls . nil)
   (php-style-delete-trailing-whitespace . t)))

(defun php-enable-drupal-coding-style ()
  "Make php-mode use coding styles that are preferable for working with Drupal."
  (interactive)
  (php-set-style "drupal"))

(c-add-style
  "wordpress"
  '("php"
    (c-basic-offset . 4)
    (c-indent-comments-syntactically-p t)
    (indent-tabs-mode . t)
    (tab-width . 4)
    (fill-column . 78)))

(defun php-enable-wordpress-coding-style ()
  "Make php-mode use coding styles that are preferable for working with Wordpress."
  (interactive)
  (php-set-style "wordpress"))

(c-add-style
  "symfony2"
  '("php"
    (c-offsets-alist . ((statement-cont . php-lineup-hanging-semicolon)))
    (c-indent-comments-syntactically-p . t)
    (php-mode-lineup-cascaded-calls . nil)
    (fill-column . 78)))

(defun php-enable-symfony2-coding-style ()
  "Make php-mode use coding styles that are preferable for working with Symfony2."
  (interactive)
  (php-set-style "symfony2"))

(c-add-style
 "psr2" ; PSR-2 / PSR-12
  '("php"
    (c-offsets-alist . ((statement-cont . +)))
    (c-indent-comments-syntactically-p . t)
    (fill-column . 78)
    (show-trailing-whitespace . t)
    (php-mode-lineup-cascaded-calls . nil)
    (php-style-delete-trailing-whitespace . t)))

(defun php-enable-psr2-coding-style ()
  "Make php-mode comply to the PSR-2 coding style."
  (interactive)
  (php-set-style "psr2"))

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
Implements PHP version of `end-of-defun-function'

See `php-beginning-of-defun'."
  (interactive "p")
  (php-beginning-of-defun (- (or arg 1))))


(defvar php-warned-bad-indent nil)

;; Do it but tell it is not good if html tags in buffer.
(defun php-check-html-for-indentation ()
  (let ((html-tag-re "^\\s-*</?\\sw+.*?>")
        (here (point)))
    (goto-char (line-beginning-position))
    (if (or (when (boundp 'mumamo-multi-major-mode) mumamo-multi-major-mode)
            ;; Fix-me: no idea how to check for mmm or multi-mode
            (save-match-data
              (not (or (re-search-forward html-tag-re (line-end-position) t)
                       (re-search-backward html-tag-re (line-beginning-position) t)))))
        (progn
          (goto-char here)
          t)
      (goto-char here)
      (setq php-warned-bad-indent t)
      (let* ((known-multi-libs '(("mumamo" mumamo (lambda () (nxhtml-mumamo)))
                                 ("mmm-mode" mmm-mode (lambda () (mmm-mode 1)))
                                 ("multi-mode" multi-mode (lambda () (multi-mode 1)))
                                 ("web-mode" web-mode (lambda () (web-mode)))))
             (known-names (mapcar (lambda (lib) (car lib)) known-multi-libs))
             (available-multi-libs (delq nil
                                         (mapcar
                                          (lambda (lib)
                                            (when (locate-library (car lib)) lib))
                                          known-multi-libs)))
             (available-names (mapcar (lambda (lib) (car lib)) available-multi-libs))
             (base-msg
              (concat
               "Indentation fails badly with mixed HTML/PHP in the HTML part in
plain `php-mode'.  To get indentation to work you must use an
Emacs library that supports 'multiple major modes' in a buffer.
Parts of the buffer will then be in `php-mode' and parts in for
example `html-mode'.  Known such libraries are:\n\t"
               (mapconcat 'identity known-names ", ")
               "\n"
               (if available-multi-libs
                   (concat
                    "You have these available in your `load-path':\n\t"
                    (mapconcat 'identity available-names ", ")
                    "\n\n"
                    "Do you want to turn any of those on? ")
                 "You do not have any of those in your `load-path'.")))
             (is-using-multi
              (catch 'is-using
                (dolist (lib available-multi-libs)
                  (when (and (boundp (cadr lib))
                             (symbol-value (cadr lib)))
                    (throw 'is-using t))))))
        (unless is-using-multi
          (if available-multi-libs
              (if (not (y-or-n-p base-msg))
                  (message "Did not do indentation, but you can try again now if you want")
                (let* ((name
                        (if (= 1 (length available-multi-libs))
                            (car available-names)
                          ;; Minibuffer window is more than one line, fix that first:
                          (message "")
                          (completing-read "Choose multiple major mode support library: "
                                           available-names nil t
                                           (car available-names)
                                           '(available-names . 1)
                                           )))
                       (mode (when name
                               (cl-caddr (assoc name available-multi-libs)))))
                  (when mode
                    ;; Minibuffer window is more than one line, fix that first:
                    (message "")
                    (load name)
                    (funcall mode))))
            (lwarn 'php-indent :warning base-msg)))
        nil))))

(defun php-cautious-indent-region (start end &optional quiet)
  "Carefully indent region `START' `END' in contexts other than HTML templates.

If the optional argument `QUIET' is non-nil then no syntactic errors are
reported, even if `c-report-syntactic-errors' is non-nil."
  (if (or (not php-mode-warn-if-mumamo-off)
          (not (php-in-poly-php-html-mode))
          php-warned-bad-indent
          (php-check-html-for-indentation))
      (funcall 'c-indent-region start end quiet)))

(defun php-cautious-indent-line ()
  "Carefully indent lines in contexts other than HTML templates."
  (if (or (not php-mode-warn-if-mumamo-off)
          (not (php-in-poly-php-html-mode))
          php-warned-bad-indent
          (php-check-html-for-indentation))
      (let ((here (point))
            (c-auto-align-backslashes
             (unless php-mode-disable-c-auto-align-backslashes
               c-auto-align-backslashes))
            doit)
        (move-beginning-of-line nil)
        ;; Don't indent heredoc end mark
        (save-match-data
          (unless (and (looking-at "[a-zA-Z0-9_]+;\n")
                       (php-in-string-p))
            (setq doit t)))
        (goto-char here)
        (when doit
          (funcall 'c-indent-line)))))

(defun php-c-at-vsemi-p (&optional pos)
  "Return T on HTML lines (including php tag) or PHP8 Attribute, otherwise NIL.
POS is a position on the line in question.

This is was done due to the problem reported here:

  URL `https://answers.launchpad.net/nxhtml/+question/43320'"
  ;; If this function could call c-beginning-of-statement-1, change php-c-vsemi-status-unknown-p.
  (save-excursion
    (if pos
        (goto-char pos)
      (setq pos (point)))
    (unless (php-in-string-or-comment-p)
      (or
       ;; Detect PHP8 attribute: <<Attribute()>>
       (when (and (< 2 pos) (< 2 (- pos (c-point 'bol))))
         (backward-char 2)
         (looking-at-p ">>\\s-*\\(?:<<\\|$\\)"))
       ;; Detect HTML/XML tag and PHP tag (<?php, <?=, ?>)
       (when php-mode-template-compatibility
         (beginning-of-line)
         (looking-at-p
          (eval-when-compile
            (rx (or (: bol (0+ space) "<" (in "a-z\\?"))
                    (: (0+ not-newline) (in "a-z\\?") ">" (0+ space) eol))))))))))

(defun php-c-vsemi-status-unknown-p ()
  "Always return NIL.  See `c-vsemi-status-unknown-p'."
  ;; Current implementation of php-c-at-vsemi-p never calls c-beginning-of-statement-1
  nil)

(defun php-lineup-string-cont (langelem)
  "Line up string toward equal sign or dot.
e.g.
$str = 'some'
     . 'string';
this ^ lineup"
  (save-excursion
    (goto-char (cdr langelem))
    (let (ret finish)
      (while (and (not finish) (re-search-forward "[=.]" (line-end-position) t))
        (unless (php-in-string-or-comment-p)
          (setq finish t
                ret (vector (1- (current-column))))))
      ret)))

(defun php-lineup-arglist-intro (langelem)
  (save-excursion
    (goto-char (cdr langelem))
    (vector (+ (current-column) c-basic-offset))))

(defun php-lineup-arglist-close (langelem)
  (save-excursion
    (goto-char (cdr langelem))
    (vector (current-column))))

(defun php-lineup-arglist (_langelem)
  (save-excursion
    (beginning-of-line)
    (if (looking-at-p "\\s-*->") '+ 0)))

(defun php-lineup-hanging-semicolon (_langelem)
  (save-excursion
    (beginning-of-line)
    (if (looking-at-p "\\s-*;\\s-*$") 0 '+)))

(eval-and-compile
  (defconst php-heredoc-start-re
    "<<<\\(?:\\_<.+?\\_>\\|'\\_<.+?\\_>'\\|\"\\_<.+?\\_>\"\\)$"
    "Regular expression for the start of a PHP heredoc."))

(defun php-heredoc-end-re (heredoc-start)
  "Build a regular expression for the end of a heredoc started by the string HEREDOC-START."
  ;; Extract just the identifier without <<< and quotes.
  (string-match "\\_<.+?\\_>" heredoc-start)
  (concat "^\\s-*\\(" (match-string 0 heredoc-start) "\\)\\W"))

(defun php-syntax-propertize-function (start end)
  "Apply propertize rules from START to END."
  (goto-char start)
  (while (and (< (point) end)
              (re-search-forward php-heredoc-start-re end t))
    (php-heredoc-syntax))
  (goto-char start)
  (while (re-search-forward "['\"]" end t)
    (when (php-in-comment-p)
      (c-put-char-property (match-beginning 0)
                           'syntax-table (string-to-syntax "_")))))

(defun php-heredoc-syntax ()
  "Mark the boundaries of searched heredoc."
  (goto-char (match-beginning 0))
  (c-put-char-property (point) 'syntax-table (string-to-syntax "|"))
  (if (re-search-forward (php-heredoc-end-re (match-string 0)) nil t)
      (goto-char (match-end 1))
    ;; Did not find the delimiter so go to the end of the buffer.
    (goto-char (point-max)))
  (c-put-char-property (1- (point)) 'syntax-table (string-to-syntax "|")))

(defvar-local php-mode--propertize-extend-region-current nil
  "Prevent undesirable recursion in PHP-SYNTAX-PROPERTIZE-EXTEND-REGION")

(defun php-syntax-propertize-extend-region (start end)
  "Extend the propertize region if START or END falls inside a PHP heredoc."
  (let ((pair (cons start end)))
    (when (not (member pair php-mode--propertize-extend-region-current))
      ;; re-search functions may trigger
      ;; syntax-propertize-extend-region-functions to be called again, which in
      ;; turn call this to be called again.
      (push pair php-mode--propertize-extend-region-current)
      (unwind-protect
          (let ((new-start)
                (new-end))
            (goto-char start)
            (when (re-search-backward php-heredoc-start-re nil t)
              (let ((maybe (point)))
                (when (and (re-search-forward (php-heredoc-end-re (match-string 0)) nil t)
                           (> (point) start))
                  (setq new-start maybe))))
            (goto-char end)
            (when (re-search-backward php-heredoc-start-re nil t)
              (if (re-search-forward (php-heredoc-end-re (match-string 0)) nil t)
                  (when (> (point) end)
                    (setq new-end (point)))
                (setq new-end (point-max))))
            (when (or new-start new-end)
              (cons (or new-start start) (or new-end end))))
        ;; Cleanup
        (setq php-mode--propertize-extend-region-current
              (delete pair php-mode--propertize-extend-region-current))))))

(easy-menu-define php-mode-menu php-mode-map "PHP Mode Commands"
  (cons "PHP" (c-lang-const c-mode-menu php)))

(defun php-mode-get-style-alist ()
  "Return an alist consisting of `php' style and styles that inherit it."
  (cl-loop for l in c-style-alist
           if (or (string= (car l) "php")
                  (equal (cadr l) "php"))
           collect l))

(defvar php-mode-set-style-history nil)
(defvar-local php-mode--delayed-set-style nil)
(defvar-local php-style-delete-trailing-whitespace nil)

(defun php-set-style (stylename &optional dont-override)
  "Set the current `php-mode' buffer to use the style STYLENAME.
STYLENAME is one of the names selectable in `php-mode-coding-style'.

Borrow the `interactive-form' from `c-set-style' and use the original
`c-set-style' function to set all declared stylevars.
For compatibility with `c-set-style' pass DONT-OVERRIDE to it.

After setting the stylevars run hooks according to STYLENAME

  \"pear\"      `php-mode-pear-hook'
  \"drupal\"    `php-mode-drupal-hook'
  \"wordpress\" `php-mode-wordpress-hook'
  \"symfony2\"  `php-mode-symfony2-hook'
  \"psr2\"      `php-mode-psr2-hook'"
  (interactive
   (list (let ((completion-ignore-case t)
	       (prompt (format "Which %s indentation style? "
			       mode-name)))
	   (completing-read prompt
                            (php-mode-get-style-alist)
                            nil t nil
			    'php-mode-set-style-history
			    c-indentation-style))))
  (php-mode--disable-delay-set-style)

  ;; Back up manually set variables
  (let* (value
         (backup-vars
          (and php-mode-enable-backup-style-variables
               (cl-loop for name in c-style-variables
                        do (setq value (symbol-value name))
                        if (and value (not (eq 'set-from-style value)))
                        collect (cons name value)))))
    (c-set-style stylename dont-override)
    ;; Restore variables
    (cl-loop for (name . value) in backup-vars
             do (set (make-local-variable name) value)))

  (if (eq (symbol-value 'php-style-delete-trailing-whitespace) t)
      (add-hook 'before-save-hook 'delete-trailing-whitespace nil t)
    (remove-hook 'before-save-hook 'delete-trailing-whitespace t))
  (cond ((equal stylename "pear")      (run-hooks 'php-mode-pear-hook))
        ((equal stylename "drupal")    (run-hooks 'php-mode-drupal-hook))
        ((equal stylename "wordpress") (run-hooks 'php-mode-wordpress-hook))
        ((equal stylename "symfony2")  (run-hooks 'php-mode-symfony2-hook))
        ((equal stylename "psr2")      (run-hooks 'php-mode-psr2-hook))))

(defun php-mode--disable-delay-set-style (&rest args)
  "Disable php-mode-set-style-delay on after hook.  `ARGS' be ignore."
  (setq php-mode--delayed-set-style nil)
  (when (fboundp 'advice-remove)
    (advice-remove #'php-mode--disable-delay-set-style #'c-set-style)))

(defun php-mode-set-style-delay ()
  "Set the current `php-mode' buffer to use the style by custom or local variables."
  (when php-mode--delayed-set-style
    (let ((coding-style (or (and (boundp 'php-project-coding-style) php-project-coding-style)
                            php-mode-coding-style)))
      (prog1 (when coding-style
               (php-set-style (symbol-name coding-style)))
        (remove-hook 'hack-local-variables-hook #'php-mode-set-style-delay)))))

(defun php-mode-set-local-variable-delay ()
  "Set local variable from php-project."
  (php-project-apply-local-variables)
  (remove-hook 'hack-local-variables-hook #'php-mode-set-local-variable-delay))

(defvar php-mode-syntax-table
  (let ((table (make-syntax-table)))
    (c-populate-syntax-table table)
    (modify-syntax-entry ?_  "_"   table)
    (modify-syntax-entry ?`  "\""  table)
    (modify-syntax-entry ?\" "\""  table)
    (modify-syntax-entry ?#  "< b" table)
    (modify-syntax-entry ?\n "> b" table)
    (modify-syntax-entry ?$  "_"   table)
    table))

;;;###autoload
(define-derived-mode php-mode c-mode "PHP"
  "Major mode for editing PHP code.

\\{php-mode-map}"
  :syntax-table php-mode-syntax-table
  ;; :after-hook (c-update-modeline)
  ;; (setq abbrev-mode t)

  (unless (string= php-mode-cc-vertion c-version)
    (user-error "CC Mode has been updated.  %s"
                (if (package-installed-p 'php-mode)
                    "Please run `M-x package-reinstall php-mode' command."
                  "Please byte recompile PHP Mode files.")))

  (when php-mode-disable-c-mode-hook
    (setq-local c-mode-hook nil)
    (setq-local java-mode-hook nil))
  (c-initialize-cc-mode t)
  (c-init-language-vars php-mode)
  (c-common-init 'php-mode)

  (setq-local comment-start "// ")
  (setq-local comment-start-skip
              (eval-when-compile
                (rx (group (or (: "#")
                               (: "/" (+ "/"))
                               (: "/*")))
                    (* (syntax whitespace)))))
  (setq-local comment-end "")
  (setq-local page-delimiter php-mode-page-delimiter)

  (setq-local font-lock-string-face 'php-string)
  (setq-local font-lock-keyword-face 'php-keyword)
  (setq-local font-lock-builtin-face 'php-builtin)
  (setq-local c-preprocessor-face-name 'php-php-tag)
  (setq-local font-lock-function-name-face 'php-function-name)
  (setq-local font-lock-variable-name-face 'php-variable-name)
  (setq-local font-lock-constant-face 'php-constant)

  (setq-local syntax-propertize-function #'php-syntax-propertize-function)
  (add-hook 'syntax-propertize-extend-region-functions
            #'php-syntax-propertize-extend-region t t)

  (setq imenu-generic-expression php-imenu-generic-expression)

  ;; PHP vars are case-sensitive
  (setq case-fold-search t)

  (when php-mode-enable-project-local-variable
    (add-hook 'hack-local-variables-hook #'php-mode-set-local-variable-delay t t))

  ;; When php-mode-enable-project-coding-style is set, it is delayed by hook.
  ;; Since it depends on the timing at which the file local variable is set.
  ;; File local variables are set after initialization of major mode except `run-hook' is complete.
  (if php-mode-enable-project-coding-style
      (progn
        (add-hook 'hack-local-variables-hook #'php-mode-set-style-delay t t)
        (setq php-mode--delayed-set-style t)
        (when (fboundp 'advice-add)
          (advice-add #'c-set-style :after #'php-mode--disable-delay-set-style '(local))))
    (let ((php-mode-enable-backup-style-variables nil))
      (php-set-style (symbol-name php-mode-coding-style))))

  (when (or php-mode-force-pear
            (and (stringp buffer-file-name)
                 (string-match "PEAR\\|pear" buffer-file-name)
                 (string-match "\\.php\\'" buffer-file-name)))
      (php-set-style "pear"))

  (setq indent-line-function 'php-cautious-indent-line)
  (setq indent-region-function 'php-cautious-indent-region)
  (setq c-at-vsemi-p-fn #'php-c-at-vsemi-p)
  (setq c-vsemi-status-unknown-p-fn #'php-c-vsemi-status-unknown-p)

  ;; We map the php-{beginning,end}-of-defun functions so that they
  ;; replace the similar commands that we inherit from CC Mode.
  ;; Because of our remapping we may not actually need to keep the
  ;; following two local variables, but we keep them for now until we
  ;; are completely sure their removal will not break any current
  ;; behavior or backwards compatibility.
  (setq-local beginning-of-defun-function 'php-beginning-of-defun)
  (setq-local end-of-defun-function 'php-end-of-defun)

  (setq-local open-paren-in-column-0-is-defun-start nil)
  (setq-local defun-prompt-regexp
              "^\\s-*function\\s-+&?\\s-*\\(\\(\\sw\\|\\s_\\)+\\)\\s-*")
  (setq-local add-log-current-defun-function nil)
  (setq-local add-log-current-defun-header-regexp php-beginning-of-defun-regexp)

  (when (fboundp 'c-looking-at-or-maybe-in-bracelist)
    (advice-add #'c-looking-at-or-maybe-in-bracelist
                :override 'php-c-looking-at-or-maybe-in-bracelist))

  (when (>= emacs-major-version 25)
    (with-silent-modifications
      (save-excursion
        (let* ((start (point-min))
               (end (min (point-max)
                         (+ start syntax-propertize-chunk-size))))
          (php-syntax-propertize-function start end))))))

(declare-function semantic-create-imenu-index "semantic/imenu" (&optional stream))

(defvar-mode-local php-mode imenu-create-index-function
  (if php-do-not-use-semantic-imenu
      #'imenu-default-create-index-function
    (require 'semantic/imenu)
    #'semantic-create-imenu-index)
  "Imenu index function for PHP.")


;; Define function name completion function
(defvar php-completion-table nil
  "Obarray of tag names defined in current tags table and functions known to PHP.")

(defun php-complete-function ()
  "Perform function completion on the text around point.
Completes to the set of names listed in the current tags table
and the standard php functions.
The string to complete is chosen in the same way as the default
for \\[find-tag] (which see)."
  (interactive)
  (let ((pattern (php-get-pattern))
        beg
        completion
        (php-functions (php-completion-table)))
    (if (not pattern) (message "Nothing to complete")
        (if (not (search-backward pattern nil t))
            (message "Can't complete here")
          (setq beg (point))
          (forward-char (length pattern))
          (setq completion (try-completion pattern php-functions nil))
          (cond ((eq completion t))
                ((null completion)
                 (message "Can't find completion for \"%s\"" pattern)
                 (ding))
                ((not (string= pattern completion))
                 (delete-region beg (point))
                 (insert completion))
                (t
                 (let ((selected (completing-read
                                  "Select completion: "
                                  (all-completions pattern php-functions)
                                  nil t pattern)))
                   (delete-region beg (point))
                   (insert selected))))))))

(defun php-completion-table ()
  "Build variable `php-completion-table' on demand.
The table includes the PHP functions and the tags from the
current `tags-file-name'."
  (or (and tags-file-name
           (save-excursion (tags-verify-table tags-file-name))
           php-completion-table)
      (let ((tags-table
             (when tags-file-name
               (with-current-buffer (get-file-buffer tags-file-name)
                 (etags-tags-completion-table))))
            (php-table
             (cond ((and (not (string= "" php-completion-file))
                         (file-readable-p php-completion-file))
                    (php-build-table-from-file php-completion-file))
                   ((and (not (string= "" php-manual-path))
                         (file-directory-p php-manual-path))
                    (php-build-table-from-path php-manual-path))
                   (t nil))))
        (unless (or php-table tags-table)
          (error
           (concat "No TAGS file active nor are "
                   "`php-completion-file' or `php-manual-path' set")))
        (when tags-table
          ;; Combine the tables.
          (if (obarrayp tags-table)
              (mapatoms (lambda (sym) (intern (symbol-name sym) php-table))
                        tags-table)
            (setq php-table (append tags-table php-table))))
        (setq php-completion-table php-table))))

(defun php-build-table-from-file (filename)
  (let ((table (make-vector 1022 0))
        (buf (find-file-noselect filename)))
    (with-current-buffer buf
      (goto-char (point-min))
      (while (re-search-forward
              "^\\([-a-zA-Z0-9_.]+\\)\n"
              nil t)
        (intern (buffer-substring (match-beginning 1) (match-end 1))
                table)))
    (kill-buffer buf)
    table))

(defun php-build-table-from-path (path)
  "Return list of PHP function name from `PATH' directory."
  (cl-loop for file in (directory-files path nil "^function\\..+\\.html$")
           if (string-match "\\.\\([-a-zA-Z_0-9]+\\)\\.html$" file)
           collect (replace-regexp-in-string
                    "-" "_" (substring file (match-beginning 1) (match-end 1)) t)))

;; Find the pattern we want to complete
;; find-tag-default from GNU Emacs etags.el
(defun php-get-pattern ()
  (save-excursion
    (while (looking-at "\\sw\\|\\s_")
      (forward-char 1))
    (if (or (re-search-backward "\\sw\\|\\s_"
                                (save-excursion (beginning-of-line) (point))
                                t)
            (re-search-forward "\\(\\sw\\|\\s_\\)+"
                               (save-excursion (end-of-line) (point))
                               t))
        (progn (goto-char (match-end 0))
               (buffer-substring-no-properties
                (point)
                (progn (forward-sexp -1)
                       (while (looking-at "\\s'")
                         (forward-char 1))
                       (point))))
      nil)))

(defun php-show-arglist ()
  "Show function arguments at cursor position."
  (interactive)
  (let* ((tagname (php-get-pattern))
         (buf (find-tag-noselect tagname nil nil))
         arglist)
    (with-current-buffer buf
      (save-excursion
        (goto-char (point-min))
        (when (re-search-forward
               (format "function\\s-+%s\\s-*(\\([^{]*\\))" tagname)
               nil t)
          (setq arglist (buffer-substring-no-properties
                         (match-beginning 1) (match-end 1))))))
    (if arglist
        (message "Arglist for %s: %s" tagname arglist)
      (message "Unknown function: %s" tagname))))

(defcustom php-search-documentation-browser-function nil
  "Function to display PHP documentation in a WWW browser.

If non-nil, this shadows the value of `browse-url-browser-function' when
calling `php-search-documentation' or `php-search-local-documentation'."
  :group 'php
  :tag "PHP Search Documentation Browser Function"
  :type '(choice (const :tag "default" nil) function)
  :link '(variable-link browse-url-browser-function))

(defun php-browse-documentation-url (url)
  "Browse a documentation URL using the configured browser function.

See `php-search-documentation-browser-function'."
  (let ((browse-url-browser-function
         (or php-search-documentation-browser-function
             browse-url-browser-function)))
    (browse-url url)))

(defvar php-search-local-documentation-types
  (list "function" "control-structures" "class" "book")
  ;; "intro" and "ref" also look interesting, but for all practical purposes
  ;; their terms are sub-sets of the "book" terms (with the few exceptions
  ;; being very unlikely search terms).
  "The set (and priority sequence) of documentation file prefixes
under which to search for files in the local documentation directory.")

(defvar php-search-local-documentation-words-cache nil)

(defun php--search-documentation-read-arg ()
  "Obtain interactive argument for searching documentation."
  ;; Cache the list of documentation words available for completion,
  ;; based on the defined types-of-interest.
  (let ((types-list php-search-local-documentation-types)
        (words-cache php-search-local-documentation-words-cache)
        (local-manual (and (stringp php-manual-path)
                           (not (string= php-manual-path "")))))
    (when (and local-manual
               (not (assq types-list words-cache)))
      ;; Generate the cache on the first run, or if the types changed.
      ;; We read the filenames matching our types list in the local
      ;; documentation directory, and extract the 'middle' component
      ;; of each. e.g. "function.array-map.html" => "array_map".
      (let* ((types-opt (regexp-opt types-list))
             (pattern (concat "\\`" types-opt "\\.\\(.+\\)\\.html\\'"))
             (collection
              (mapcar (lambda (filename) (subst-char-in-string
                                          ?- ?_ (replace-regexp-in-string
                                                 pattern "\\1" filename)))
                      (directory-files php-manual-path nil pattern))))
        ;; Replace the entire cache. If the types changed, we don't need
        ;; to retain the collection for the previous value.
        (setq words-cache (list (cons types-list collection)))
        (setq php-search-local-documentation-words-cache words-cache)))
    ;; By default we search for (current-word) immediately, without prompting.
    ;; With a prefix argument, or if there is no (current-word), we perform a
    ;; completing read for a word from the cached collection.
    (let* ((default (current-word))
           (prompt (if default
                       (format "Search PHP docs (%s): " default)
                     "Search PHP docs: "))
           (collection (and local-manual
                            (cdr (assq types-list words-cache))))
           (word (if (or current-prefix-arg (not default))
                     (completing-read prompt collection nil nil nil nil default)
                   default)))
      ;; Return interactive argument list.
      (list word))))

(defun php-search-local-documentation (word)
  "Search the local PHP documentation (i.e. in `php-manual-path') for
the word at point.  The function returns t if the requested documentation
exists, and nil otherwise.

With a prefix argument, prompt (with completion) for a word to search for."
  (interactive (php--search-documentation-read-arg))
  (let ((file (catch 'found
                (cl-loop for type in php-search-local-documentation-types do
                         (let* ((doc-html (format "%s.%s.html"
                                                  type
                                                  (replace-regexp-in-string
                                                   "_" "-" (downcase word))))
                                (file (expand-file-name doc-html  php-manual-path)))
                           (when (file-exists-p file)
                             (throw 'found file)))))))
    (when file
      (let ((file-url (if (string-prefix-p "file://" file)
                          file
                        (concat "file://" file))))
        (php-browse-documentation-url file-url))
      t)))

(defsubst php-search-web-documentation (word)
  "Return URL to search PHP manual search by `WORD'."
  (php-browse-documentation-url (concat (or php-search-url php-site-url) word)))

;; Define function documentation function
(defun php-search-documentation (word)
  "Search PHP documentation for the `WORD' at point.

If `php-manual-path' has a non-empty string value then the command
will first try searching the local documentation.  If the requested
documentation does not exist it will fallback to searching the PHP
website.

With a prefix argument, prompt for a documentation word to search
for.  If the local documentation is available, it is used to build
a completion list."
  (interactive (php--search-documentation-read-arg))
  (if (and (stringp php-manual-path)
           (not (string= php-manual-path "")))
      (or (php-search-local-documentation word)
          (php-search-web-documentation word))
    (php-search-web-documentation word)))

;; Define function for browsing manual
(defun php-browse-manual ()
  "Bring up manual for PHP."
  (interactive)
  (browse-url (if (stringp php-manual-url)
                  php-manual-url
                (format "%smanual/%s/" php-site-url php-manual-url))))


;; Font Lock
(defconst php-phpdoc-type-keywords
  (list "string" "integer" "int" "boolean" "bool" "float"
        "double" "object" "mixed" "array" "resource"
        "void" "null" "false" "true" "self" "static"
        "callable" "iterable" "number"))

(defconst php-phpdoc-type-tags
  (list "package" "param" "property" "property-read" "property-write"
        "return" "throws" "var"))

(defconst php-phpdoc-font-lock-doc-comments
  `(("{@[-[:alpha:]]+\\s-*\\([^}]*\\)}" ; "{@foo ...}" markup.
     (0 'php-doc-annotation-tag prepend nil)
     (1 'php-string prepend nil))
    (,(rx (group "$") (group (in "A-Za-z_") (* (in "0-9A-Za-z_"))))
     (1 'php-doc-variable-sigil prepend nil)
     (2 'php-variable-name prepend nil))
    ("\\(\\$\\)\\(this\\)\\>" (1 'php-doc-$this-sigil prepend nil) (2 'php-doc-$this prepend nil))
    (,(concat "\\s-@" (regexp-opt php-phpdoc-type-tags) "\\s-+"
              "\\(" (rx (+ (? "?") (? "\\") (+ (in "0-9A-Z_a-z")) (? "[]") (? "|"))) "\\)+")
     1 'php-string prepend nil)
    (,(concat "\\(?:|\\|\\?\\|\\s-\\)\\("
              (regexp-opt php-phpdoc-type-keywords 'words)
              "\\)")
     1 font-lock-type-face prepend nil)
    ("https?://[^\n\t ]+"
     0 'link prepend nil)
    ("^\\(?:/\\*\\)?\\(?:\\s \\|\\*\\)*\\(@[[:alpha:]][-[:alpha:]\\]*\\)" ; "@foo ..." markup.
     1 'php-doc-annotation-tag prepend nil)))

(defvar php-phpdoc-font-lock-keywords
  `((,(lambda (limit)
        (c-font-lock-doc-comments "/\\*\\*" limit
          php-phpdoc-font-lock-doc-comments)))))

(defconst php-font-lock-keywords-1 (c-lang-const c-matchers-1 php)
  "Basic highlighting for PHP Mode.")

(defconst php-font-lock-keywords-2 (c-lang-const c-matchers-2 php)
  "Medium level highlighting for PHP Mode.")

(defconst php-font-lock-keywords-3
  (append
   php-phpdoc-font-lock-keywords
   ;; php-mode patterns *before* cc-mode:
   ;;  only add patterns here if you want to prevent cc-mode from applying
   ;;  a different face.
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
     ("\\(->\\)\\(\\sw+\\)\\s-*(" (1 'php-object-op) (2 'php-method-call))
     ("\\<\\(const\\)\\s-+\\(\\_<.+?\\_>\\)" (1 'php-keyword) (2 'php-constant-assign))

     ;; Logical operator (!)
     ("\\(!\\)[^=]" 1 'php-logical-op)

     ;; Highlight special variables
     ("\\(\\$\\)\\(this\\)\\>" (1 'php-$this-sigil) (2 'php-$this))
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
     ("\\sw+::\\(\\sw+\\)(" 1 'php-static-method-call)
     ;; Multiple catch (FooException | BarException $e)
     (,(rx symbol-start "catch" symbol-end
           (* (syntax whitespace)) "(" (* (syntax whitespace))
           (group (+ (or (syntax word) (syntax symbol)))))
      (1 font-lock-type-face)
      (,(rx (* (syntax whitespace)) "|" (* (syntax whitespace))
            (group (+ (or (syntax word) (syntax symbol))) symbol-end))
       nil nil (1 font-lock-type-face)))
     ;; While c-opt-cpp-* highlights the <?php opening tags, it is not
     ;; possible to make it highlight short open tags and closing tags
     ;; as well. So we force the correct face on all cases that
     ;; c-opt-cpp-* lacks for this purpose.
     ;;
     ;; Note that starting a file with <% breaks indentation, a
     ;; limitation we can/should live with.
     (,(regexp-opt '("<?php" "<?=" "?>"
                     "<?"      ;; obsolete short open tag
                     "<%" "%>" ;; obsolete ASP tag
                     ;; Obsoleted tags were deleted in PHP 7.
                     ;; @see http://php.net/manual/language.basic-syntax.phptags.php
                     ))
      0 'php-php-tag))

   ;; cc-mode patterns
   (c-lang-const c-matchers-3 php)

   ;; php-mode patterns *after* cc-mode:
   ;;   most patterns should go here, faces will only be applied if not
   ;;   already fontified by another pattern. Note that using OVERRIDE
   ;;   is usually overkill.
   `(
     ("\\<\\(@\\)" 1 'php-errorcontrol-op)
     ;; Highlight function calls
     ("\\(\\_<\\(?:\\sw\\|\\s_\\)+?\\_>\\)\\s-*(" 1 'php-function-call)
     ;; Highlight all upper-cased symbols as constant
     ("\\<\\([A-Z_][A-Z0-9_]+\\)\\>" 1 'php-constant)

     ;; Highlight all statically accessed class names as constant,
     ;; another valid option would be using type-face, but using
     ;; constant-face because this is how it works in c++-mode.
     ("\\(\\sw+\\)\\(::\\)" (1 'php-constant) (2 'php-paamayim-nekudotayim))

     ;; Highlight class name after "use .. as"
     ("\\<as\\s-+\\(\\sw+\\)" 1 font-lock-type-face)

     ;; Class names are highlighted by cc-mode as defined in
     ;; c-class-decl-kwds, below regexp is a workaround for a bug
     ;; where the class names are not highlighted right after opening
     ;; a buffer (editing a file corrects it).
     ;;
     ;; This behaviour is caused by the preceding '<?php', which
     ;; cc-mode cannot handle easily. Registering it as a cpp
     ;; preprocessor works well (i.e. the next line is not a
     ;; statement-cont) but the highlighting glitch remains.
     (,(concat (regexp-opt (c-lang-const c-class-decl-kwds php))
               " \\(\\sw+\\)")
      1 font-lock-type-face)

     ;; Highlight the ? character for nullable return types.
     ("function.+:\\s-*\\(\\?\\)\\(?:\\sw\\|\\s_\\|\\\\\\)+" 1 font-lock-type-face)
     (")\\s-*:\\s-*\\(\\?\\)\\(?:\\sw\\|\\s_\\|\\\\\\)+\\s-*\\(?:\{\\|;\\)" 1 font-lock-type-face)

     ;; Highlight the ? character for nullable type hints.
     ("\\(\\?\\)\\(:?\\sw\\|\\s_\\|\\\\\\)+\\s-+\\$" 1 font-lock-type-face)

     ;; Class names without a namespace are not highlighted at all when they
     ;; are used as nullable type hints or return types (both nullable and
     ;; non-nullable). We have to use separate regular expressions, because
     ;; we want to capture the class name as well, not just the ? character
     ;; like the regexps above.
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

     ;; Logical operators (and, or, &&, ...)
     ;; Not operator (!) is defined in "before cc-mode" section above.
     ("\\(&&\\|||\\)" 1 'php-logical-op)))
  "Detailed highlighting for PHP Mode.")

(defvar php-font-lock-keywords php-font-lock-keywords-3
  "Default expressions to highlight in PHP Mode.")

(add-to-list
 (eval-when-compile
   (if (boundp 'flymake-proc-allowed-file-name-masks)
       'flymake-proc-allowed-file-name-masks
     'flymake-allowed-file-name-masks))
 '("\\.php[345s]?\\'" php-flymake-php-init))


(defun php-send-region (start end)
  "Send the region between `START' and `END' to PHP for execution.
The output will appear in the buffer *PHP*."
  (interactive "r")
  (let ((php-buffer (get-buffer-create "*PHP*"))
        (code (buffer-substring start end)))
    ;; Calling 'php -r' will fail if we send it code that starts with
    ;; '<?php', which is likely.  So we run the code through this
    ;; function to check for that prefix and remove it.
    (let ((cleaned-php-code (if (string-prefix-p "<?php" code t)
                                (substring code 5)
                              code)))
      (call-process php-executable nil php-buffer nil "-r" cleaned-php-code))))


(defconst php-string-interpolated-variable-regexp
  "{\\$[^}\n\\\\]*\\(?:\\\\.[^}\n\\\\]*\\)*}\\|\\${\\sw+}\\|\\$\\sw+")

(defun php-string-intepolated-variable-font-lock-find (limit)
  (while (re-search-forward php-string-interpolated-variable-regexp limit t)
    (let ((quoted-stuff (nth 3 (syntax-ppss))))
      (when (and quoted-stuff (member quoted-stuff '(?\" ?`)))
        (put-text-property (match-beginning 0) (match-end 0)
                           'face 'php-variable-name))))
  nil)

(eval-after-load 'php-mode
  '(progn
     (font-lock-add-keywords
      'php-mode
      `((php-string-intepolated-variable-font-lock-find))
      'append)))



;;; Correct the behavior of `delete-indentation' by modifying the
;;; logic of `fixup-whitespace'.
(defadvice fixup-whitespace (after php-mode-fixup-whitespace)
  "Remove whitespace before certain characters in PHP Mode."
  (let* ((no-behind-space ";\\|,\\|->\\|::")
         (no-front-space "->\\|::"))
    (when (and (eq major-mode 'php-mode)
               (or (looking-at-p (concat " \\(" no-behind-space "\\)"))
                   (save-excursion
                     (forward-char -2)
                     (looking-at-p no-front-space))))
      (delete-char 1))))

(ad-activate 'fixup-whitespace)

;;;###autoload
(progn
  (add-to-list 'auto-mode-alist '("/\\.php_cs\\(?:\\.dist\\)?\\'" . php-mode))
  (add-to-list 'auto-mode-alist '("\\.\\(?:php\\.inc\\|stub\\)\\'" . php-mode))
  (add-to-list 'auto-mode-alist '("\\.\\(?:php[s345]?\\|phtml\\)\\'" . php-mode-maybe)))

(provide 'php-mode)
;;; php-mode.el ends here

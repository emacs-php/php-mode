;;; php-cc-mode.el --- CC Mode based major mode for editing PHP code  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Friends of Emacs-PHP development
;; Copyright (C) 1999, 2000, 2001, 2003, 2004 Turadg Aleahmad
;;               2008 Aaron S. Hawley
;;               2011, 2012, 2013, 2014, 2015, 2016, 2017 Eric James Michael Ritz

;; Author: Eric James Michael Ritz
;; Maintainer: USAMI Kenta <tadsan@zonu.me>
;; URL: https://github.com/emacs-php/php-mode
;; Keywords: languages php
;; Version: 1.28.0
;; Package-Requires: ((emacs "28.1"))
;; License: GPL-3.0-or-later

(eval-and-compile
  (make-obsolete-variable
   (defconst php-mode-version-number "1.28.0" "PHP Mode version number.")
   "Please call (php-mode-version :as-number t) for compatibility." "1.24.2"))

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

;; `php-cc-mode' is the legacy CC Mode based implementation of PHP Mode.
;; It is kept for backward compatibility and is in frozen maintenance:
;; only regression fixes are accepted here, new development happens in the
;; cc-mode independent `php-mode' (lisp/php-mode.el).
;;
;; PHP Mode is a major mode for editing PHP script.  It's an extension
;; of CC mode; thus it inherits all C mode's navigation functionality.
;; But it colors according to the PHP syntax and indents according to the
;; PSR-2 coding guidelines.  It also includes a couple handy IDE-type
;; features such as documentation search and a source and class browser.
;;
;; For backward compatibility this mode runs `php-mode-hook' in addition
;; to `php-cc-mode-hook' at the end of its initialization.

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

(eval-when-compile
  (require 'cc-langs)
  (require 'cc-fonts))

;; Boilerplate from other `cc-mode' derived modes. See
;; http://cc-mode.sourceforge.net/derived-mode-ex.el for details on how this all
;; fits together.
(eval-and-compile
  (c-add-language 'php-cc-mode 'java-mode))

(require 'font-lock)
(require 'custom)
(require 'speedbar)
(require 'imenu)
(require 'consult-imenu nil t)
(require 'package)
(require 'nadvice)
(require 'mode-local)
(require 'php-project)

(eval-when-compile
  (require 'rx)
  (require 'cl-lib)
  (require 'flymake)
  (require 'flymake-proc)
  (require 'php-flymake)
  (require 'regexp-opt)
  (declare-function acm-backend-tabnine-candidate-expand "ext:acm-backend-tabnine"
                    (candidate-info bound-start))
  (defvar add-log-current-defun-header-regexp)
  (defvar add-log-current-defun-function)
  (defvar c-syntactic-context)
  (defvar c-vsemi-status-unknown-p)
  (defvar syntax-propertize-via-font-lock))

(defconst php-mode-version-id
  (eval-when-compile
    (let ((fallback-version (format "%s-non-vcs" (with-no-warnings php-mode-version-number))))
      (if (locate-dominating-file default-directory ".git")
          (save-match-data
            (let ((tag (replace-regexp-in-string
                        (rx bos "v") ""
                        (shell-command-to-string "git describe --tags")))
                  (pattern (rx (group (+ nonl)) eol)))
              (if (string-match pattern tag)
                  (match-string 0 tag)
                (error "Faild to obtain git tag"))))
        fallback-version)))
  "PHP Mode build ID.

The format is follows:

\"1.23.4\": Tagged revision, compiled under Git VCS.
\"1.23.4-56-xxxxxx\": 56 commits after the last tag release, compiled under Git.
\"1.23.4-non-vcs\": Compiled in an environment not managed by Git VCS.")

(autoload 'php-mode-debug "php-mode-debug"
  "Display informations useful for debugging PHP Mode." t)

(autoload 'php-mode-debug-reinstall "php-mode-debug"
  "Reinstall PHP Mode to solve Cc Mode version mismatch.

When FORCE, try to reinstall without interactively asking.
When CALLED-INTERACTIVE then message the result." t)


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
  :tag "PHP Mode Default Face"
  :type 'face)

(define-obsolete-variable-alias 'php-speedbar-config 'php-mode-speedbar-config "1.20.0")
(defcustom php-mode-speedbar-config t
  "When set to true automatically configures Speedbar to observe PHP files.
Ignores php-file patterns option; fixed to expression \"\\.\\(inc\\|php[s345]?\\)\""
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
  :tag "PHP Mode Speedbar Open"
  :type 'boolean
  :set (lambda (sym val)
         (set-default sym val)
         (when val
           (speedbar 1))))

;; `php-mode-template-compatibility' is defined in php.el (shared).

(define-obsolete-variable-alias 'php-lineup-cascaded-calls 'php-cc-mode-lineup-cascaded-calls "1.20.0")
(defcustom php-cc-mode-lineup-cascaded-calls nil
  "Indent chained method calls to the previous line."
  :tag "PHP Mode Lineup Cascaded Calls"
  :type 'boolean)

(defcustom php-mode-page-delimiter
  (eval-when-compile
    (rx symbol-start
        (or "namespace" "function" "class" "trait" "interface")
        symbol-end))
  "Regexp describing line-beginnings that PHP declaration statements."
  :tag "PHP Mode Page Delimiter"
  :type 'regexp)

(defcustom php-mode-replace-flymake-diag-function
  (eval-when-compile (when (boundp 'flymake-diagnostic-functions)
                       #'php-flymake))
  "Flymake function to replace, if NIL do not replace."
  :tag "PHP Mode Replace Flymake Diag Function"
  :type '(choice function
                 (const :tag "Disable to replace" nil)))

(define-obsolete-variable-alias 'php-do-not-use-semantic-imenu 'php-mode-do-not-use-semantic-imenu "1.20.0")
(defcustom php-mode-do-not-use-semantic-imenu t
  "Customize `imenu-create-index-function' for `php-mode'.

If using function `semantic-mode' `imenu-create-index-function' will be
set to `semantic-create-imenu-index' due to `c-mode' being its
parent.  Set this variable to t if you want to use
`imenu-default-create-index-function' even with `semantic-mode'
enabled."
  :tag "PHP Mode Do Not Use Semantic Imenu"
  :type 'boolean)

;; NOTE: `interpreter-mode-alist' / `auto-mode-alist' registration is
;; intentionally not done here.  The cc-mode independent `php-mode' owns
;; the file/interpreter associations; `php-cc-mode' is opted into manually.

(defcustom php-mode-hook nil
  "List of functions to be executed on entry to `php-mode'."
  :tag "PHP Mode Hook"
  :type 'hook)

;; `php-mode-pear-hook', `php-mode-drupal-hook', `php-mode-wordpress-hook'
;; and `php-mode-psr2-hook' are defined in php.el (shared).

(defcustom php-mode-symfony2-hook nil
  "Hook called when a Symfony2 file is opened with `php-mode'.
The Symfony2 style is only supported by the legacy `php-cc-mode', so
this hook is defined here rather than in the shared php.el."
  :tag "PHP Mode Symfony2 Hook"
  :type 'hook)

(defcustom php-mode-force-pear nil
  "Normally PEAR coding rules are enforced only when the filename contains \"PEAR\".
Turning this on will force PEAR rules on all PHP files."
  :tag "PHP Mode Force Pear"
  :type 'boolean)

(defcustom php-mode-warn-if-html-template t
  "Warn and prompt to switch to an HTML template major mode when indenting HTML."
  :tag "PHP Mode Warn If HTML Template"
  :safe #'booleanp
  :type '(choice (const :tag "Warn" t) (const :tag "Don't warn" nil)))
(make-obsolete-variable 'php-mode-warn-if-mumamo-off 'php-mode-warn-if-html-template "2.0.0")

;; `php-mode-coding-style' is defined in php.el (shared).  Note that its
;; default value is `per'; the "per" cc-style is registered below so that
;; `php-cc-mode' initializes correctly with it.

;; Since this function has a bad influence on the environment of many users,
;; temporarily disable it
(defcustom php-mode-enable-project-coding-style nil
  "When set to true override `php-mode-coding-style' by `php-project-coding-style'.

If you want to suppress styles from being overwritten by directory / file
local variables, set NIL."
  :tag "PHP Mode Enable Project Coding Style"
  :type 'boolean)

(defcustom php-cc-mode-enable-backup-style-variables t
  "When set to `T', back up values set by hook and buffer local variables.

This function may interfere with other hooks and other behaviors.
In that case set to `NIL'."
  :tag "PHP Mode Enable Backup Style Variables"
  :type 'boolean)

(defcustom php-mode-enable-project-local-variable t
  "When set to `T', apply project local variable to buffer local variable."
  :tag "PHP Mode Enable Project Local Variable"
  :type 'boolean)

(defconst php-mode-cc-version
  (eval-when-compile c-version))

(cl-defun php-mode-version (&key as-number)
  "Display string describing the version of PHP Mode.

Although this is an interactive command, it returns a string when called
as a function.  Call with AS-NUMBER keyword to compare by `version<'.

\(version<= \"1.24.1\" (php-mode-version :as-number t))"
  (interactive (list :as-number nil))
  (if as-number
      (save-match-data (and (string-match (rx (group (+ (in ".0-9")))) php-mode-version-id)
                            (match-string 0 php-mode-version-id)))
    (funcall
     (if (called-interactively-p 'interactive) #'message #'format)
     "PHP Mode v%s" php-mode-version-id)))

;;;###autoload
(define-obsolete-variable-alias 'php-available-project-root-files 'php-project-available-root-files "1.19.0")

(defvar php-cc-mode-map
  (let ((map (make-sparse-keymap "PHP Mode")))
    (set-keymap-parent map c-mode-base-map)
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
    (define-key map (kbd "C-M-h") #'mark-defun)

    ;; Many packages based on cc-mode provide the 'C-c C-w' binding
    ;; to toggle Subword Mode.  See the page
    ;;
    ;;     https://www.gnu.org/software/emacs/manual/html_node/ccmode/Subword-Movement.html
    ;;
    ;; for more information about Subword mode.
    (define-key map (kbd "C-c C-w") #'subword-mode)

    ;; We inherit c-beginning-of-defun and c-end-of-defun from CC Mode
    ;; but we have two replacement functions specifically for PHP.  We
    ;; remap the commands themselves and not their default
    ;; key-bindings so that our PHP-specific versions will work even
    ;; if the user has reconfigured their keys, e.g. if they rebind
    ;; c-end-of-defun to something other than C-M-e.
    (define-key map [remap c-beginning-of-defun] #'php-cc-beginning-of-defun)
    (define-key map [remap c-end-of-defun] #'php-cc-end-of-defun)
    (define-key map [remap c-set-style] #'php-cc-set-style)

    (define-key map [(control c) (control f)] #'php-search-documentation)
    (define-key map [(meta tab)] #'php-complete-function)
    (define-key map [(control c) (control m)] #'php-browse-manual)
    (define-key map [(control .)] #'php-show-arglist)
    (define-key map [(control c) (control r)] #'php-send-region)
    ;; Use the Emacs standard indentation binding. This may upset c-mode
    ;; which does not follow this at the moment, but I see no better
    ;; choice.
    (define-key map "\t" nil)          ;Hide CC-mode's `TAB' binding.
    map)
  "Keymap for `php-mode'.")

(c-lang-defconst c-mode-menu
  php-cc (append '(["Complete function name" php-complete-function t]
                ["Browse manual" php-browse-manual t]
                ["Search documentation" php-search-documentation t]
                ["----" t])
              (c-lang-const c-mode-menu)))

(c-lang-defconst c-at-vsemi-p-fn
  php-cc 'php-c-at-vsemi-p)

(c-lang-defconst c-vsemi-status-unknown-p-fn
  php-cc 'php-c-vsemi-status-unknown-p)

(c-lang-defconst c-get-state-before-change-functions
  php-cc nil)

(c-lang-defconst c-before-font-lock-functions
  php-cc (c-get-lang-constant 'c-before-font-lock-functions nil t))

;; Make php-mode recognize opening tags as preprocessor macro's.
;;
;; This is a workaround, the tags must be recognized as something
;; in order for the syntactic guesses of code below the tag
;; to be correct and as a result not break indentation.
;;
;; Note that submatches or \\| here are not expected by cc-mode.
(c-lang-defconst c-opt-cpp-prefix
  php-cc "\\s-*<\\?")

(c-lang-defconst c-anchored-cpp-prefix
  php-cc "\\s-*\\(<\\?(=\\|\\sw+)\\)")

(c-lang-defconst c-identifier-ops
  php-cc '((left-assoc "\\" "::" "->")
        (prefix "\\" "::")))

(c-lang-defconst c-operators
  php-cc `((prefix "new" "clone")
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
        (left-assoc ",")
        (left-assoc "=>")))

;; Allow '\' when scanning from open brace back to defining
;; construct like class
(c-lang-defconst c-block-prefix-disallowed-chars
  php-cc (cl-set-difference (c-lang-const c-block-prefix-disallowed-chars)
                      '(?\\)))

;; Allow $ so variables are recognized in cc-mode and remove @. This
;; makes cc-mode highlight variables and their type hints in arglists.
(c-lang-defconst c-symbol-start
  php-cc (concat "[" c-alpha "_$]"))

;; All string literals can possibly span multiple lines
(c-lang-defconst c-multiline-string-start-char
  php-cc t)

(c-lang-defconst c-assignment-operators
  php-cc '("=" "*=" "/=" "%=" "+=" "-=" ">>=" "<<=" "&=" "^=" "|=" ".=" "??="))

(c-lang-defconst beginning-of-defun-function
  php-cc 'php-cc-beginning-of-defun)

(c-lang-defconst end-of-defun-function
  php-cc 'php-cc-end-of-defun)

(c-lang-defconst c-primitive-type-kwds
  php-cc '("int" "integer" "bool" "boolean" "float" "double" "real"
        "string" "object" "void" "mixed" "never"))

(c-lang-defconst c-class-decl-kwds
  "Keywords introducing declarations where the following block (if any)
contains another declaration level that should be considered a class."
  php-cc '("class" "trait" "interface" "enum"))

(c-lang-defconst c-brace-list-decl-kwds
  "Keywords introducing declarations where the following block (if
any) is a brace list.

PHP does not have an C-like \"enum\" keyword."
  php-cc nil)

;; cc-mode renamed `c-after-brace-list-decl-kwds' to `c-after-enum-list-kwds';
;; keep the old name defined so cc-langs' transitional lookup stays quiet where
;; the rename landed (see `c-after-enum-list-key' in cc-langs.el).
(c-lang-defconst c-after-brace-list-decl-kwds
  php-cc nil)

(c-lang-defconst c-typeless-decl-kwds
  php-cc (append (c-lang-const c-class-decl-kwds php-cc) '("function" "const")))

(c-lang-defconst c-modifier-kwds
  php-cc '("abstract" "final" "static" "case" "readonly"))

(c-lang-defconst c-protection-kwds
  "Access protection label keywords in classes."
  php-cc '("private" "protected" "public"))

(c-lang-defconst c-postfix-decl-spec-kwds
  php-cc '("implements" "extends"))

(c-lang-defconst c-type-list-kwds
  php-cc '("@new" ;; @new is *NOT* language construct, it's workaround for coloring.
        "new" "use" "implements" "extends" "namespace" "instanceof" "insteadof"))

(c-lang-defconst c-ref-list-kwds
  php-cc nil)

(c-lang-defconst c-block-stmt-2-kwds
  php-cc '("catch" "declare" "elseif" "for" "foreach" "if" "switch" "while"))

(c-lang-defconst c-simple-stmt-kwds
  php-cc '("break" "continue" "die" "echo" "exit" "goto" "return" "throw"
        "include" "include_once" "print" "require" "require_once"))

(c-lang-defconst c-constant-kwds
  php-cc '("true" "false" "null"))

(c-lang-defconst c-lambda-kwds
  php-cc '("function" "use"))

(c-lang-defconst c-inexpr-block-kwds
  php-cc '("match"))

(c-lang-defconst c-other-block-decl-kwds
  php-cc '("namespace"))

(c-lang-defconst c-other-kwds
  "Keywords not accounted for by any other `*-kwds' language constant."
  php-cc
  '("__halt_compiler"
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
  php-cc nil)

(c-lang-defconst c-<>-type-kwds
  php-cc nil)

(c-lang-defconst c-inside-<>-type-kwds
  php-cc nil)

(c-lang-defconst c-enums-contain-decls
  php-cc nil)

(c-lang-defconst c-nonlabel-token-key
  "Regexp matching things that can't occur in generic colon labels.

This overrides cc-mode `c-nonlabel-token-key' to support switching on
double quoted strings and true/false/null.

Note: this regexp is also applied to goto-labels, a future improvement
might be to handle switch and goto labels differently."
  php-cc (concat
     ;; All keywords except `c-label-kwds' and `c-constant-kwds'.
     (c-make-keywords-re t
       (cl-set-difference (c-lang-const c-keywords)
                       (append (c-lang-const c-label-kwds)
                               (c-lang-const c-constant-kwds))
                       :test 'string-equal))))

(c-lang-defconst c-basic-matchers-before
  php-cc (cl-remove-if (lambda (elm) (and (listp elm) (equal (car elm) "\\s|")))
                    (c-lang-const c-basic-matchers-before php-cc)))

(c-lang-defconst c-basic-matchers-after
  php-cc (cl-remove-if (lambda (elm) (and (listp elm) (memq 'c-annotation-face elm)))
                    (c-lang-const c-basic-matchers-after php-cc)))

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
  "Line up the current argument line under the first argument using
`c-lineup-arglist' LANGELEM."
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
but only if the setting is enabled."
  (cond
   (php-cc-mode-lineup-cascaded-calls (c-lineup-cascaded-calls langelem))
   ((assq 'arglist-cont-nonempty c-syntactic-context) nil)
   ((assq 'defun-block-intro c-syntactic-context) nil)
   ((assq 'defun-close c-syntactic-context) nil)
   ((assq 'statement-cont c-syntactic-context) nil)
   ((save-excursion
      (beginning-of-line)
      (let ((beginning-of-langelem (cdr langelem))
            (beginning-of-current-line (point))
            start)
        (skip-chars-forward " 	")
        (cond
         ((looking-at-p "->") '+)
         ((looking-at-p "[:?]") '+)
         ((looking-at-p "[,;]") nil)
         ((looking-at-p "//") nil)
         ;; Is the previous line terminated with `,' ?
         ((progn
            (forward-line -1)
            (end-of-line)
            (skip-chars-backward " 	")
            (backward-char 1)
            (while (and (< beginning-of-langelem (point))
                        (setq start (php-in-string-or-comment-p)))
              (goto-char start)
              (skip-chars-backward " 	\r\n")
              (backward-char 1))
            (and (not (eq (point) beginning-of-current-line))
                 (not (php-in-string-or-comment-p))
                 (not (looking-at-p ","))
                 (save-excursion
                   (backward-char) (not (looking-at-p ",")))))
          '+)
         (t nil)))))))

(defun php-c-looking-at-or-maybe-in-bracelist (orig-fun &optional containing-sexp lim &rest args)
  "Replace `c-looking-at-or-maybe-in-bracelist'.

CONTAINING-SEXP is the position of the brace/paren/bracket enclosing
POINT, or nil if there is no such position, or we do not know it.  LIM is
a backward search limit."
  (cond
   ((not (derived-mode-p 'php-cc-mode)) (apply orig-fun containing-sexp lim args))
   ((looking-at-p "{")
    (save-excursion
      (c-backward-token-2 2 t lim)
      ;; PHP 8.0 match expression
      ;; echo match ($var) |{
      ;;     ↑ matches   ↑ initial position
      (when (looking-at-p (eval-when-compile (rx symbol-start "match" symbol-end)))
        (cons (point) t))))
   (t nil)))

(defun php-mode--anonymous-function-brace-p (pos)
  "Return non-nil when the opening brace at POS starts a closure body.

POS must be the position of a `{'.  A closure is an anonymous
`function (...) [use (...)] [: type] {' expression; named function
declarations and other constructs (e.g. `match', `if') return nil."
  (save-excursion
    (goto-char pos)
    (c-backward-syntactic-ws)
    ;; Step back over an optional return type declaration `): Type {' so
    ;; that point lands just after the parameter (or `use') list's `)'.
    (unless (eq (char-before) ?\))
      (let ((colon (save-excursion
                     (skip-chars-backward "^):;{}(" (point-min))
                     (and (eq (char-before) ?:) (1- (point))))))
        (when colon
          (goto-char colon)
          (c-backward-syntactic-ws))))
    ;; Walk backwards over the `)...(' list(s): the parameter list, plus an
    ;; optional `use (...)' clause, until we reach `function' or give up.
    (let ((result nil) (continue t))
      (while (and continue (eq (char-before) ?\)))
        (condition-case nil
            (backward-list)
          (error (setq continue nil)))
        (when continue
          (c-backward-syntactic-ws)
          (if (zerop (c-backward-token-2))
              (cond
               ((looking-at-p "\\_<function\\_>") (setq result t continue nil))
               ((looking-at-p "\\_<use\\_>") (c-backward-syntactic-ws))
               (t (setq continue nil)))
            (setq continue nil))))
      result)))

(defun php-c-looking-at-statement-block (orig-fun &rest args)
  "Treat a closure body as a statement block around `c-looking-at-statement-block'.

Emacs' CC Mode >= 31 (the change for bug#19867) fails to recognize an
anonymous function body whose first line is a comment as a statement
block, and reports it as a brace list, mis-indenting both the body and
its closing brace.  When point is at the opening brace of such a closure,
return non-nil; otherwise fall back to ORIG-FUN with ARGS.

A closure body is always a statement block, so this is also correct on
Emacs versions unaffected by the bug."
  (or (and (derived-mode-p 'php-cc-mode)
           (eq (char-after) ?\{)
           (php-mode--anonymous-function-brace-p (point)))
      (apply orig-fun args)))

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
   (php-cc-mode-lineup-cascaded-calls . t)
   (php-style-delete-trailing-whitespace . nil)))

(defun php-cc-enable-default-coding-style ()
  "Set PHP Mode to use reasonable default formatting."
  (interactive)
  (php-cc-set-style "php"))

;; The shared `php-mode-coding-style' now defaults to `per'.  Register a
;; "per" cc-style (an alias of the base "php" style) so that `php-cc-mode'
;; keeps initializing when that default is in effect.
(c-add-style "per" '("php"))

(defun php-cc-enable-per-coding-style ()
  "Set PHP Mode to use the PHP-FIG PER coding style."
  (interactive)
  (php-cc-set-style "per"))

(c-add-style
 "pear"
 '("php"
   (c-basic-offset . 4)
   (c-offsets-alist . ((case-label . 0) (statement-cont . +)))
   (tab-width . 4)
   (php-cc-mode-lineup-cascaded-calls . nil)))

(defun php-cc-enable-pear-coding-style ()
  "Set up `php-mode' to use the coding styles preferred for PEAR code and modules."
  (interactive)
  (php-cc-set-style "pear"))

(c-add-style
 "drupal"
 '("php"
   (c-basic-offset . 2)
   (tab-width . 2)
   (fill-column . 78)
   (show-trailing-whitespace . t)
   (php-cc-mode-lineup-cascaded-calls . nil)
   (php-style-delete-trailing-whitespace . t)))

(defun php-cc-enable-drupal-coding-style ()
  "Make `php-mode' use coding styles that are preferable for working with Drupal."
  (interactive)
  (php-cc-set-style "drupal"))

(c-add-style
  "wordpress"
  '("php"
    (c-basic-offset . 4)
    (c-indent-comments-syntactically-p t)
    (indent-tabs-mode . t)
    (tab-width . 4)
    (fill-column . 78)))

(defun php-cc-enable-wordpress-coding-style ()
  "Make `php-mode' use coding styles that are preferable for working with
Wordpress."
  (interactive)
  (php-cc-set-style "wordpress"))

(c-add-style
  "symfony2"
  '("php"
    (c-offsets-alist . ((statement-cont . php-lineup-hanging-semicolon)))
    (c-indent-comments-syntactically-p . t)
    (php-cc-mode-lineup-cascaded-calls . nil)
    (fill-column . 78)))

(defun php-cc-enable-symfony2-coding-style ()
  "Make `php-mode' use coding styles that are preferable for working with Symfony2."
  (interactive)
  (php-cc-set-style "symfony2"))

(c-add-style
 "psr2" ; PSR-2 / PSR-12
  '("php"
    (c-offsets-alist . ((statement-cont . +)))
    (c-indent-comments-syntactically-p . t)
    (fill-column . 78)
    (show-trailing-whitespace . t)
    (php-cc-mode-lineup-cascaded-calls . nil)
    (php-style-delete-trailing-whitespace . t)))

(defun php-cc-enable-psr2-coding-style ()
  "Make `php-mode' comply to the PSR-2 coding style."
  (interactive)
  (php-cc-set-style "psr2"))

(defun php-cc-beginning-of-defun (&optional arg)
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

(defun php-cc-end-of-defun (&optional arg)
  "Move the end of the ARGth PHP function from point.
Implements PHP version of `end-of-defun-function'

See `php-cc-beginning-of-defun'."
  (interactive "p")
  (php-cc-beginning-of-defun (- (or arg 1))))


(defvar-local php-warned-bad-indent nil
  "Non-nil once the user has been warned about indenting this buffer.
Buffer-local so a warning in one buffer does not suppress it in others.")

(defun php-check-html-for-indentation ()
  "Return non-nil when the current buffer may be indented as PHP.
Warn and offer to switch to `php-html-template-major-mode' when the
buffer looks like an HTML template edited in plain `php-mode'."
  (cond
   ((not php-mode-warn-if-html-template) t)
   ;; In a polymode buffer php-mode only ever indents its own PHP chunks,
   ;; so indent normally and never warn about the surrounding HTML.
   ((php-in-poly-php-html-mode) t)
   ((not (php-buffer-has-html-tag)) t)
   (php-warned-bad-indent nil)
   ((fboundp php-html-template-major-mode)
    (if (y-or-n-p (format "This file seems to contain an HTML tag.  Switch to %s? "
                          php-html-template-major-mode))
        (funcall php-html-template-major-mode)
      (prog1 nil
        (setq-local php-warned-bad-indent t))))
   (t ;; Suppress warnings in Emacs session
    (setq-local php-warned-bad-indent t)
    (lwarn 'php-mode
           :warning "Indentation fails badly with mixed HTML/PHP in the HTML part in plain `php-mode'.
It is highly recommended to install a major mode that supports PHP and HTML templates, such as Web Mode.

Set `php-html-template-major-mode' variable to use a mode other than `web-mode'.
Set `php-mode-warn-if-html-template' variable to nil to suppress the warning.
")
    nil)))

(defun php-cautious-indent-region (start end &optional quiet)
  "Carefully indent region START to END in contexts other than HTML templates.

If the optional argument QUIET is non-nil then no syntactic errors are
reported, even if `c-report-syntactic-errors' is non-nil."
  (when (php-check-html-for-indentation)
    (c-indent-region start end quiet)))

(defun php-cautious-indent-line ()
  "Carefully indent lines in contexts other than HTML templates."
  (when (and (php-check-html-for-indentation)
             (save-excursion
               (beginning-of-line)
               ;; Don't indent heredoc end mark
               (not (and (looking-at-p "[a-zA-Z0-9_]+;\n") (php-in-string-p)))))
    (c-indent-line)))

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
    (cond
     ;; Detect PHP8 attribute: #[Attribute()]
     ((and (< 1 pos) (< 1 (- pos (c-point 'bol))))
      (backward-char 1)
      (looking-at-p (eval-when-compile (rx "]" (* (syntax whitespace)) (or "#[" line-end)))))
     ;; Detect HTML/XML tag and PHP tag (<?php, <?=, ?>)
     (php-mode-template-compatibility
      (beginning-of-line)
      (looking-at-p
       (eval-when-compile
         (rx (or (: bol (0+ space) "<" (in "?a-z"))
                 (: (0+ not-newline) (in "?a-z") ">" (0+ space) eol)))))))))

(defun php-c-vsemi-status-unknown-p ()
  "Always return NIL.  See `c-vsemi-status-unknown-p'."
  ;; Current implementation of php-c-at-vsemi-p never calls c-beginning-of-statement-1
  nil)

(defun php-lineup-string-cont (langelem)
  "Line up string toward equal sign or dot.
e.g.
$str = \\='some'
     . \\='string';
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
  (defconst php-cc-heredoc-start-re
    (rx "<<<"
        (* (syntax whitespace))
        (or (group (+ (or (syntax word) (syntax symbol))))
            (: "\"" (group (+ (or (syntax word) (syntax symbol)))) "\"")
            (: "'" (group (+ (or (syntax word) (syntax symbol)))) "'"))
        line-end)
    "Regular expression for the start of a PHP heredoc."))

(defun php-cc-heredoc-end-re (heredoc-start)
  "Build a regular expression for the end of a heredoc started by the string
HEREDOC-START."
  ;; Extract just the identifier without <<< and quotes.
  (string-match "\\_<.+?\\_>" heredoc-start)
  (concat "^\\s-*\\(" (match-string 0 heredoc-start) "\\)\\W"))

(eval-and-compile
  (defconst php-cc-syntax-propertize-rules
    (syntax-propertize-precompile-rules
     (php-cc-heredoc-start-re
      (0 (ignore (php-cc--syntax-propertize-heredoc
                  (match-beginning 0)
                  (or (match-string 1) (match-string 2) (match-string 3))
                  (null (match-string 3))))))
     ((rx "#[")
      (0 (ignore (php-cc--syntax-propertize-attributes (match-beginning 0)))))
     ((rx (or "'" "\""))
      (0 (ignore (php-cc--syntax-propertize-quotes-in-comment (match-beginning 0))))))))

(defalias 'php-cc-syntax-propertize-function
  (syntax-propertize-rules php-cc-syntax-propertize-rules))

(defun php-cc--syntax-propertize-heredoc (start id _is-heredoc)
  "Apply propertize Heredoc and Nowdoc from START, with ID and IS-HEREDOC."
  (let ((terminator (rx-to-string `(: line-start (* (syntax whitespace)) ,id word-boundary))))
    (put-text-property start (1+ start) 'syntax-table (string-to-syntax "|"))
    (re-search-forward terminator nil t)
    (when (match-string 0)
      (put-text-property (1- (point)) (point) 'syntax-table (string-to-syntax "|")))))

(defun php-cc--syntax-propertize-quotes-in-comment (pos)
  "Apply propertize quotes (' and \") from POS."
  (when (php-in-comment-p)
    (put-text-property pos (1+ pos) 'syntax-table (string-to-syntax "_"))))

(defun php-cc--syntax-propertize-attributes (start)
  "Apply propertize PHP8 #[Attributes] (without # comment) from START.
The string check parses only up to START: probing at point (after the
match) would run `syntax-ppss' across the `#' while it still has its
comment-start syntax, caching a bogus in-comment state that breaks the
propertize of everything after the attribute.  The `save-excursion'
matters too: `syntax-ppss' moves point to START, and leaving point
before the match would make `syntax-propertize-rules' rescan it
forever."
  (unless (save-excursion (nth 3 (syntax-ppss start)))
    (put-text-property start (1+ start) 'syntax-table (string-to-syntax "."))))

(defvar-local php-cc-mode--propertize-extend-region-current nil
  "Prevent undesirable recursion in PHP-SYNTAX-PROPERTIZE-EXTEND-REGION.")

(defun php-cc-syntax-propertize-extend-region (start end)
  "Extend the propertize region if START or END falls inside a PHP heredoc."
  (let ((pair (cons start end)))
    (when (not (member pair php-cc-mode--propertize-extend-region-current))
      ;; re-search functions may trigger
      ;; syntax-propertize-extend-region-functions to be called again, which in
      ;; turn call this to be called again.
      (push pair php-cc-mode--propertize-extend-region-current)
      (unwind-protect
          (let (new-start new-end)
            (goto-char start)
            ;; Consider bounding this backwards search by `beginning-of-defun'.
            ;; (Benchmarking for a wide range of cases may be needed to decide
            ;; whether that's an improvement, as `php-cc-beginning-of-defun' also
            ;; uses `re-search-backward'.)
            (when (re-search-backward php-cc-heredoc-start-re nil t)
              (let ((maybe (point)))
                (when (and (re-search-forward (php-cc-heredoc-end-re (match-string 0)) nil t)
                           (> (point) start))
                  (setq new-start maybe)
                  (when (> (point) end)
                    (setq new-end (point))))))
            (unless new-end
              (goto-char end)
              (when (re-search-backward php-cc-heredoc-start-re start t)
                (if (re-search-forward (php-cc-heredoc-end-re (match-string 0)) nil t)
                    (when (> (point) end)
                      (setq new-end (point)))
                  (setq new-end (point-max)))))
            (when (or new-start new-end)
              (cons (or new-start start) (or new-end end))))
        ;; Cleanup
        (setq php-cc-mode--propertize-extend-region-current
              (delete pair php-cc-mode--propertize-extend-region-current))))))

(easy-menu-define php-mode-menu php-cc-mode-map "PHP Mode Commands."
  (cons "PHP" (c-lang-const c-mode-menu php-cc)))

(defun php-mode-get-style-alist ()
  "Return an alist consisting of `php' style and styles that inherit it."
  (cl-loop for l in c-style-alist
           if (or (string= (car l) "php")
                  (equal (cadr l) "php"))
           collect l))

(defvar php-mode-set-style-history nil)
(defvar-local php-mode--delayed-set-style nil)
(defvar-local php-style-delete-trailing-whitespace nil)

(defun php-cc-set-style (stylename &optional dont-override)
  "Set the current `php-mode' buffer to use the style STYLENAME.
STYLENAME is one of the names selectable in `php-mode-coding-style'.

Borrow the `interactive-form' from `c-set-style' and use the original
`c-set-style' function to set all declared stylevars.
For compatibility with `c-set-style' pass DONT-OVERRIDE to it.

After setting the stylevars run hook `php-mode-STYLENAME-hook'."
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
          (and php-cc-mode-enable-backup-style-variables
               (cl-loop for name in c-style-variables
                        do (setq value (symbol-value name))
                        if (and value (not (eq 'set-from-style value)))
                        collect (cons name value)))))
    (c-set-style stylename dont-override)
    ;; Restore variables
    (cl-loop for (name . value) in backup-vars
             do (set (make-local-variable name) value)))
  (if (eq php-style-delete-trailing-whitespace t)
      (add-hook 'before-save-hook #'delete-trailing-whitespace nil t)
    (remove-hook 'before-save-hook #'delete-trailing-whitespace t))
  (run-hooks (intern (format "php-mode-%s-hook" stylename))))

(defun php-mode--disable-delay-set-style (&rest _args)
  "Disable `php-mode-set-style-delay' on after hook.  ARGS be ignore."
  (setq php-mode--delayed-set-style nil)
  (advice-remove #'php-mode--disable-delay-set-style #'c-set-style))

(defun php-mode-set-style-delay ()
  "Set the current `php-mode' buffer to use the style by custom or local variables."
  (when php-mode--delayed-set-style
    (let ((coding-style (or (and (boundp 'php-project-coding-style) php-project-coding-style)
                            php-mode-coding-style)))
      (prog1 (when coding-style
               (php-cc-set-style (symbol-name coding-style)))
        (remove-hook 'hack-local-variables-hook #'php-mode-set-style-delay)))))

(defun php-mode-set-local-variable-delay ()
  "Set local variable from php-project."
  (php-project-apply-local-variables)
  (remove-hook 'hack-local-variables-hook #'php-mode-set-local-variable-delay))

(defvar php-cc-mode-syntax-table
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
(define-derived-mode php-cc-mode php-base-mode "PHP/cc"
  "CC Mode based major mode for editing PHP code.

This is the legacy implementation kept for backward compatibility.

\\{php-cc-mode-map}"
  :syntax-table php-cc-mode-syntax-table
  :after-hook (progn (c-make-noise-macro-regexps)
                     (c-make-macro-with-semi-re)
                     (c-update-modeline))
  (unless (string= php-mode-cc-version c-version)
    (php-mode-debug-reinstall nil))

  (c-initialize-cc-mode t)
  (setq abbrev-mode t)

  ;; Must be called once as c-mode to enable font-lock for Heredoc.
  ;; TODO: This call may be removed in the future.
  (c-common-init 'c-mode)

  (c-init-language-vars php-cc-mode)
  (c-common-init 'php-cc-mode)
  (cc-imenu-init cc-imenu-c-generic-expression)

  (setq-local c-auto-align-backslashes nil)

  (setq-local comment-start "// ")
  (setq-local comment-start-skip
              (eval-when-compile
                (rx (group (or (: "#" (not (any "[")))
                               (: "/" (+ "/"))
                               (: "/*")))
                    (* (syntax whitespace)))))
  (setq-local comment-end "")
  (setq-local page-delimiter php-mode-page-delimiter)

  (with-suppressed-warnings ((obsolete font-lock-string-face
                                       font-lock-keyword-face
                                       font-lock-builtin-face
                                       font-lock-function-name-face
                                       font-lock-variable-name-face
                                       font-lock-constant-face))
    (setq-local font-lock-string-face 'php-string)
    (setq-local font-lock-keyword-face 'php-keyword)
    (setq-local font-lock-builtin-face 'php-builtin)
    (setq-local c-preprocessor-face-name 'php-php-tag)
    (setq-local font-lock-function-name-face 'php-function-name)
    (setq-local font-lock-variable-name-face 'php-variable-name)
    (setq-local font-lock-constant-face 'php-constant))

  (setq-local syntax-propertize-function #'php-cc-syntax-propertize-function)
  (add-hook 'syntax-propertize-extend-region-functions
            #'php-cc-syntax-propertize-extend-region t t)

  (setq imenu-generic-expression (if (symbolp php-imenu-generic-expression)
                                     (symbol-value php-imenu-generic-expression)
                                   php-imenu-generic-expression))

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
        (advice-add 'c-set-style :after #'php-mode--disable-delay-set-style))
    (let ((php-cc-mode-enable-backup-style-variables nil))
      (php-cc-set-style (symbol-name php-mode-coding-style))))

  (when (or php-mode-force-pear
            (and (stringp buffer-file-name)
                 (string-match "PEAR\\|pear" buffer-file-name)
                 (string-match "\\.php\\'" buffer-file-name)))
      (php-cc-set-style "pear"))

  (setq indent-line-function #'php-cautious-indent-line)
  (setq indent-region-function #'php-cautious-indent-region)
  (setq c-at-vsemi-p-fn #'php-c-at-vsemi-p)
  (setq c-vsemi-status-unknown-p-fn #'php-c-vsemi-status-unknown-p)

  ;; We map the php-{beginning,end}-of-defun functions so that they
  ;; replace the similar commands that we inherit from CC Mode.
  ;; Because of our remapping we may not actually need to keep the
  ;; following two local variables, but we keep them for now until we
  ;; are completely sure their removal will not break any current
  ;; behavior or backwards compatibility.
  (setq-local beginning-of-defun-function #'php-cc-beginning-of-defun)
  (setq-local end-of-defun-function #'php-cc-end-of-defun)

  (setq-local open-paren-in-column-0-is-defun-start nil)
  (setq-local defun-prompt-regexp
              "^\\s-*function\\s-+&?\\s-*\\(\\(\\sw\\|\\s_\\)+\\)\\s-*")
  (setq-local add-log-current-defun-function nil)
  (setq-local add-log-current-defun-header-regexp php-beginning-of-defun-regexp)

  (when (and (eval-when-compile (boundp 'flymake-diagnostic-functions))
             php-mode-replace-flymake-diag-function)
    (add-hook 'flymake-diagnostic-functions php-mode-replace-flymake-diag-function nil t))

  (advice-add 'c-looking-at-or-maybe-in-bracelist
              :around 'php-c-looking-at-or-maybe-in-bracelist)
  (when (fboundp 'c-looking-at-statement-block)
    (advice-add 'c-looking-at-statement-block
                :around 'php-c-looking-at-statement-block))
  (advice-add 'fixup-whitespace :after #'php-mode--fixup-whitespace-after)

  (advice-add 'acm-backend-tabnine-candidate-expand
              :filter-args #'php-acm-backend-tabnine-candidate-expand-filter-args)

  (when (eval-when-compile (>= emacs-major-version 25))
    (syntax-ppss-flush-cache (point-min)))

  ;; Backward compatibility: historically this mode was named `php-mode'
  ;; and users hung their configuration on `php-mode-hook'.  Run it here so
  ;; existing setups keep working under the renamed `php-cc-mode'.
  (run-hooks 'php-mode-hook))

(declare-function semantic-create-imenu-index "semantic/imenu" (&optional stream))

(defvar-mode-local php-cc-mode imenu-create-index-function
  (if php-do-not-use-semantic-imenu
      #'imenu-default-create-index-function
    (require 'semantic/imenu)
    #'semantic-create-imenu-index)
  "Imenu index function for PHP.")

(when (bound-and-true-p consult-imenu-config)
  (add-to-list 'consult-imenu-config '(php-cc-mode :toplevel "Namespace"
                                                :types ((?n "Namespace" font-lock-function-name-face)
                                                        (?p "Properties" font-lock-type-face)
                                                        (?o "Constants" font-lock-type-face)
                                                        (?c "Classes"    font-lock-type-face)
                                                        (?f "Functions" font-lock-function-name-face)
                                                        (?i "Imports" font-lock-type-face)
                                                        (?m "Methods"  font-lock-function-name-face)))))

(autoload 'php-local-manual-complete-function "php-local-manual")

(defun php-complete-function ()
  "Perform function completion on the text around point.
Completes to the set of names listed in the current tags table
and the standard php functions.
The string to complete is chosen in the same way as the default
for \\[find-tag] (which see)."
  (interactive)
  (php-local-manual-complete-function))

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

;; Font Lock
(defconst php-cc-phpdoc-type-names
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

(make-obsolete-variable 'php-phpdoc-type-keywords 'php-cc-phpdoc-type-names "1.24.2")

(defconst php-cc-phpdoc-type-tags
  (list "package" "param" "property" "property-read" "property-write"
        "return" "throws" "var" "self-out" "this-out" "param-out"
        "type" "extends" "require-extends" "implemtents" "require-implements"
        "template" "template-covariant" "template-extends" "template-implements"
        "require-extends" "require-implements"
        "assert" "assert-if-true" "assert-if-false" "if-this-is")
  "A list of tags specifying type names.")

(defconst php-cc-phpdoc-font-lock-doc-comments
  `(("{@[-[:alpha:]]+\\s-*\\([^}]*\\)}" ; "{@foo ...}" markup.
     (0 'php-doc-annotation-tag prepend nil)
     (1 'php-string prepend nil))
    (,(rx (group "$") (group (in "A-Za-z_") (* (in "0-9A-Za-z_"))))
     (1 'php-doc-variable-sigil prepend nil)
     (2 'php-variable-name prepend nil))
    ("\\(\\$\\)\\(this\\)\\>" (1 'php-doc-$this-sigil prepend nil) (2 'php-doc-$this prepend nil))
    (,(concat "\\s-@" (rx (? (or "phan" "phpstan" "psalm") "-")) (regexp-opt php-cc-phpdoc-type-tags) "\\s-+"
              "\\(" (rx (+ (? "?") (? "\\") (+ (in "0-9A-Z_a-z")) (? "[]") (? "|"))) "\\)+")
     1 'php-string prepend nil)
    (,(concat "\\(?:|\\|\\?\\|\\s-\\)\\("
              (regexp-opt php-cc-phpdoc-type-names 'words)
              "\\)")
     1 font-lock-type-face prepend nil)
    ("^\\(?:/\\*\\)?\\(?:\\s \\|\\*\\)*\\(@[[:alpha:]][-[:alpha:]\\]*\\)" ; "@foo ..." markup.
     1 'php-doc-annotation-tag prepend nil)))

(defvar php-cc-phpdoc-font-lock-keywords
  `((,(lambda (limit)
        (c-font-lock-doc-comments "/\\*\\*" limit
          php-cc-phpdoc-font-lock-doc-comments)))))

(defconst php-cc-font-lock-keywords-1 (c-lang-const c-matchers-1 php-cc)
  "Basic highlighting for PHP Mode.")

(defconst php-cc-font-lock-keywords-2 (c-lang-const c-matchers-2 php-cc)
  "Medium level highlighting for PHP Mode.")

(defconst php-cc-font-lock-keywords-3
  (append
   php-cc-phpdoc-font-lock-keywords
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
   (c-lang-const c-matchers-3 php-cc)

   ;; php-mode patterns *after* cc-mode:
   ;;   most patterns should go here, faces will only be applied if not
   ;;   already fontified by another pattern. Note that using OVERRIDE
   ;;   is usually overkill.
   `(
     (php-cc-mode--error-control-op-font-lock-find 0 'php-errorcontrol-op t)
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
     (,(concat (regexp-opt (c-lang-const c-class-decl-kwds php-cc))
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

     ;; Pipe operator (|>) --- PHP 8.5.  Must precede the comparison
     ;; operators, whose `[<>]=?' alternative would otherwise claim the
     ;; `>' and leave the `|' unfontified.
     ("\\(|>\\)" 1 'php-pipe-op)

     ;; Comparison operators (==, ===, >=, ...)
     ("\\([!=]=\\{1,2\\}[>]?\\|[<>]=?\\)" 1 'php-comparison-op)

     ;; Arithmetic operators (+, -, *, **, /, %)
     ("\\(?:[A-Za-z0-9[:blank:]]\\)\\([\-+*/%]\\*?\\)\\(?:[A-Za-z0-9[:blank:]]\\)" 1 'php-arithmetic-op)

     ;; Increment and Decrement operators (++, --)
     ("\\(\-\-\\|\+\+\\)\$\\w+" 1 'php-inc-dec-op) ;; pre inc/dec
     ("\$\\w+\\(\-\-\\|\+\+\\)" 1 'php-inc-dec-op) ;; post inc/dec

     ;; Logical operators (and, or, &&, ...)
     ;; Not operator (!) is defined in "before cc-mode" section above.
     ("\\(&&\\|||\\)" 1 'php-logical-op)
     ;; string interpolation ("$var, ${var}, {$var}")
     (php-cc-mode--string-interpolated-variable-font-lock-find 0 nil)
     (,(rx symbol-start (group (or "get" "set")) (+ (syntax whitespace)) (or "{" "=>"))
      1 'php-builtin)))
  "Detailed highlighting for PHP Mode.")

(defvar php-cc-font-lock-keywords php-cc-font-lock-keywords-3
  "Default expressions to highlight in PHP Mode.")

(eval-when-compile
   (unless (boundp 'flymake-proc-allowed-file-name-masks)
     (add-to-list 'flymake-allowed-file-name-masks
                  '("\\.php[345s]?\\'" php-flymake-php-init))))


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


(defconst php-cc-string-interpolated-variable-regexp
  "{\\$[^}\n\\\\]*\\(?:\\\\.[^}\n\\\\]*\\)*}\\|\\${\\sw+}\\|\\$\\sw+")

(defun php-cc-mode--error-control-op-font-lock-find (limit)
  "Font-lock matcher for the error-control operator `@' up to LIMIT.

Match a single `@' that is used as the error-control operator, skipping
occurrences inside strings or comments and the `@new' keyword (which CC
Mode fontifies as a keyword).  CC Mode >= 31 fontifies a bare `@' as a
keyword because `@new' is registered in `c-type-list-kwds', so this
matcher is set to override that face."
  (let (found)
    (while (and (not found)
                (re-search-forward "@" limit t))
      ;; `php-in-string-or-comment-p' calls `syntax-ppss', which may run
      ;; `syntax-propertize' and clobber the match data, so guard it to
      ;; keep the `@' match for both the `@new' check below and font-lock.
      (unless (or (save-match-data (php-in-string-or-comment-p))
                  (save-excursion
                    (goto-char (match-beginning 0))
                    (looking-at-p "@new\\_>")))
        (setq found t)))
    found))

(defun php-cc-mode--string-interpolated-variable-font-lock-find (limit)
  "Apply text-property to LIMIT for string interpolation by font-lock."
  (let (quoted-stuff)
    (while (re-search-forward php-cc-string-interpolated-variable-regexp limit t)
      (setq quoted-stuff (php-in-string-p))
      (when (or (eq ?\" quoted-stuff) (eq ?` quoted-stuff))
        (put-text-property (match-beginning 0) (match-end 0) 'face 'php-variable-name))))
  nil)

;;; Correct the behavior of `delete-indentation' by modifying the
;;; logic of `fixup-whitespace'.
(defun php-mode--fixup-whitespace-after ()
  "Remove whitespace before certain characters in PHP Mode."
  (when (and (derived-mode-p 'php-cc-mode)
             (or (looking-at-p " \\(?:;\\|,\\|->\\|::\\)")
                 (save-excursion
                   (forward-char -2)
                   (looking-at-p "->\\|::"))))
    (delete-char 1)))

;; Advice for lsp-bridge' acm-backend-tabnine
;; see https://github.com/manateelazycat/lsp-bridge/issues/402#issuecomment-1305653058
(defun php-acm-backend-tabnine-candidate-expand-filter-args (args)
  "Adjust to replace bound-start ARGS for Tabnine in PHP."
  (if (not (derived-mode-p 'php-cc-mode))
      args
    (cl-multiple-value-bind (candidate-info bound-start) args
      (save-excursion
        (goto-char bound-start)
        (when (looking-at-p (eval-when-compile (regexp-quote "$")))
          (setq bound-start (1+ bound-start))))
      (list candidate-info bound-start))))

;; NOTE: `auto-mode-alist' registration is intentionally omitted; the
;; cc-mode independent `php-mode' owns the file associations.

(provide 'php-cc-mode)
;;; php-cc-mode.el ends here

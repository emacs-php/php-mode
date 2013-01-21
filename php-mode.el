;;; php-mode.el --- Major mode for editing PHP code

;; Copyright (C) 1999, 2000, 2001, 2003, 2004 Turadg Aleahmad
;;               2008 Aaron S. Hawley
;;               2011, 2012, 2013 Eric James Michael Ritz

;;; Author: Eric James Michael Ritz
;;; URL: https://github.com/ejmr/php-mode
;;; Version: 1.9

(defconst php-mode-version-number "1.9"
  "PHP Mode version number.")

(defconst php-mode-modified "2013-01-21"
  "PHP Mode build date.")

;;; License

;; This file is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this file; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
;; 02110-1301, USA.

;;; Usage

;; Put this file in your Emacs lisp path (eg. site-lisp) and add to
;; your .emacs file:
;;
;;   (require 'php-mode)

;; To use abbrev-mode, add lines like this:
;;   (add-hook 'php-mode-hook
;;     '(lambda () (define-abbrev php-mode-abbrev-table "ex" "extends")))

;; To make php-mode compatible with html-mode, see http://php-mode.sf.net

;; Many options available under Help:Customize
;; Options specific to php-mode are in
;;  Programming/Languages/Php
;; Since it inherits much functionality from c-mode, look there too
;;  Programming/Languages/C

;;; Commentary:

;; PHP mode is a major mode for editing PHP source code.  It's an
;; extension of C mode; thus it inherits all C mode's navigation
;; functionality.  But it colors according to the PHP grammar and
;; indents according to the PEAR coding guidelines.  It also includes
;; a couple handy IDE-type features such as documentation search and a
;; source and class browser.

;;; Code:

(require 'font-lock)
(require 'cc-mode)
(require 'cc-langs)
(require 'custom)
(require 'flymake)
(eval-when-compile
  (unless (require 'cl-lib nil t)
    (require 'cl))
  (require 'regexp-opt)
  (defvar c-vsemi-status-unknown-p)
  (defvar syntax-propertize-via-font-lock))

;;; Emacs 24.3 obsoletes flet in favor of cl-flet.  So if we are not
;;; using that version then we revert to using flet.
(unless (fboundp 'cl-flet)
  (defalias 'cl-flet 'flet))

;; Local variables
;;;###autoload
(defgroup php nil
  "Major mode `php-mode' for editing PHP code."
  :prefix "php-"
  :group 'languages)

(defcustom php-executable "/usr/bin/php"
  "The location of the PHP executable."
  :type 'string
  :group 'php)

(defcustom php-default-face 'default
  "Default face in `php-mode' buffers."
  :type 'face
  :group 'php)

(defcustom php-function-call-face 'default
  "Default face for function calls in `php-mode' buffers."
  :type 'face
  :group 'php)

(defcustom php-speedbar-config t
  "When set to true automatically configures Speedbar to observe PHP files.
Ignores php-file patterns option; fixed to expression \"\\.\\(inc\\|php[s345]?\\)\""
  :type 'boolean
  :set (lambda (sym val)
         (set-default sym val)
         (if (and val (boundp 'speedbar))
             (speedbar-add-supported-extension
              "\\.\\(inc\\|php[s345]?\\|phtml\\)")))
  :group 'php)

(defcustom php-mode-speedbar-open nil
  "Normally `php-mode' starts with the speedbar closed.
Turning this on will open it whenever `php-mode' is loaded."
  :type 'boolean
  :set (lambda (sym val)
         (set-default sym val)
         (when val
             (speedbar 1)))
  :group 'php)

(defun php-create-regexp-for-method (type)
  "Accepts a `type' of function as a string, e.g. 'public' or 'private',
and returns a regexp that will match that type of function."
  (concat
   ;; Initial space with possible 'abstract' or 'final' keywords
   "^\\s-*\\(?:\\(?:abstract\\|final\\)\\s-+\\)?"
   ;; The function type
   type
   ;; Is it static?
   "\\s-+\\(?:static\\s-+\\)?"
   ;; Make sure 'function' comes next with some space after
   "function\\s-+"
   ;; Capture the name as the first group and the regexp and make sure
   ;; by the end we see the opening parenthesis for the parameters.
   "\\(\\(?:\\sw\\|\\s_\\)+\\)\\s-*("))

(defun php-create-regexp-for-classlike (type)
  "Accepts a `type' of a 'classlike' object as a string, such as
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
 "Imenu generic expression for PHP Mode. See `imenu-generic-expression'.")

(defcustom php-manual-url "http://www.php.net/manual/en/"
  "URL at which to find PHP manual.
You can replace \"en\" with your ISO language code."
  :type 'string
  :group 'php)

(defcustom php-search-url "http://www.php.net/"
  "URL at which to search for documentation on a word."
  :type 'string
  :group 'php)

(defcustom php-completion-file ""
  "Path to the file which contains the function names known to PHP."
  :type 'string
  :group 'php)

(defcustom php-manual-path ""
  "Path to the directory which contains the PHP manual."
  :type 'string
  :group 'php)

;;;###autoload
(add-to-list 'interpreter-mode-alist (cons "php" 'php-mode))

;;;###autoload
(defcustom php-file-patterns '("\\.php[s345t]?\\'" "\\.phtml\\'" "\\.inc\\'")
  "List of file patterns for which to automatically invoke `php-mode'."
  :type '(repeat (regexp :tag "Pattern"))
  :set (lambda (sym val)
         (set-default sym val)
         (let ((php-file-patterns-temp val))
           (while php-file-patterns-temp
             (add-to-list 'auto-mode-alist
                          (cons (car php-file-patterns-temp) 'php-mode))
             (setq php-file-patterns-temp (cdr php-file-patterns-temp)))))
  :group 'php)

(defcustom php-mode-hook nil
  "List of functions to be executed on entry to `php-mode'."
  :type 'hook
  :group 'php)

(defcustom php-mode-pear-hook nil
  "Hook called when a PHP PEAR file is opened with `php-mode'."
  :type 'hook
  :group 'php)

(defcustom php-mode-drupal-hook nil
  "Hook called when a Drupal file is opened with `php-mode'."
  :type 'hook
  :group 'php)

(defcustom php-mode-wordpress-hook nil
  "Hook called when a WordPress file is opened with `php-mode'."
  :type 'hook
  :group 'php)

(defcustom php-mode-force-pear nil
  "Normally PEAR coding rules are enforced only when the filename contains \"PEAR.\"
Turning this on will force PEAR rules on all PHP files."
  :type 'boolean
  :group 'php)

(defcustom php-mode-warn-if-mumamo-off t
  "Warn once per buffer if you try to indent a buffer without
mumamo-mode turned on. Detects if there are any HTML tags in the
buffer before warning, but this is is not very smart; e.g. if you
have any tags inside a PHP string, it will be fooled."
  :type '(choice (const :tag "Warg" t) (const "Don't warn" nil))
  :group 'php)

(defcustom php-mode-coding-style 'pear
  "Select default coding style to use with php-mode.
This variable can take one of the following symbol values:

`PEAR' - use coding styles preferred for PEAR code and modules.

`Drupal' - use coding styles preferred for working with Drupal projects.

`WordPress' - use coding styles preferred for working with WordPress projects."
  :type '(choice (const :tag "PEAR" pear)
                                 (const :tag "Drupal" drupal)
                                 (const :tag "WordPress" wordpress))
  :group 'php
  :set 'php-mode-custom-coding-style-set
  :initialize 'custom-initialize-default)

(defun php-mode-custom-coding-style-set (sym value)
  (set         sym value)
  (set-default sym value)
  (cond ((eq value 'pear)
                 (php-enable-pear-coding-style))
                ((eq value 'drupal)
                 (php-enable-drupal-coding-style))
                ((eq value 'wordpress)
                 (php-enable-wordpress-coding-style))))


(defun php-enable-pear-coding-style ()
  "Sets up php-mode to use the coding styles preferred for PEAR
code and modules."
  (interactive)
  (set (make-local-variable 'tab-width) 4)
  (set (make-local-variable 'c-basic-offset) 4)
  (set (make-local-variable 'indent-tabs-mode) nil)
  (c-set-offset 'block-open '-)
  (c-set-offset 'block-close 0)
  (c-set-offset 'statement-cont '+))

(defun php-enable-drupal-coding-style ()
  "Makes php-mode use coding styles that are preferable for
working with Drupal."
  (interactive)
  (setq tab-width 2)
  (setq c-basic-offset 2)
  (setq indent-tabs-mode nil)
  (setq fill-column 78)
  (setq show-trailing-whitespace t)
  (add-hook 'before-save-hook 'delete-trailing-whitespace)
  (c-set-offset 'case-label '+)
  (c-set-offset 'arglist-close 0)
  (c-set-offset 'arglist-intro '+)
  (c-set-offset 'arglist-cont-nonempty 'c-lineup-math)
  (c-set-offset 'statement-cont '+))

(defun php-enable-wordpress-coding-style ()
  "Makes php-mode use coding styles that are preferable for
working with Wordpress."
  (interactive)
  (setq indent-tabs-mode t)
  (setq fill-column 78)
  (setq tab-width 4)
  (setq c-basic-offset tab-width)
  (setq c-indent-comments-syntactically-p t)
  (c-set-offset 'arglist-cont 0)
  (c-set-offset 'arglist-intro '+)
  (c-set-offset 'case-label 2)
  (c-set-offset 'arglist-close 0)
  (c-set-offset 'defun-close 0)
  (c-set-offset 'defun-block-intro tab-width)
  (c-set-offset 'statement-cont '+))


(defun php-mode-version ()
  "Display string describing the version of PHP mode."
  (interactive)
  (message "PHP mode %s of %s"
           php-mode-version-number php-mode-modified))

(defconst php-beginning-of-defun-regexp
  "^\\s-*\\(?:\\(?:abstract\\|final\\|private\\|protected\\|public\\|static\\)\\s-+\\)*function\\s-+&?\\(\\(?:\\sw\\|\\s_\\)+\\)\\s-*("
  "Regular expression for a PHP function.")

(defun php-beginning-of-defun (&optional arg)
  "Move to the beginning of the ARGth PHP function from point.
Implements PHP version of `beginning-of-defun-function'."
  (interactive "p")
  (let ((arg (or arg 1)))
    (while (> arg 0)
      (re-search-backward php-beginning-of-defun-regexp
                          nil 'noerror)
      (setq arg (1- arg)))
    (while (< arg 0)
      (end-of-line 1)
      (let ((opoint (point)))
        (beginning-of-defun 1)
        (forward-list 2)
        (forward-line 1)
        (if (eq opoint (point))
            (re-search-forward php-beginning-of-defun-regexp
                               nil 'noerror))
        (setq arg (1+ arg))))))

(defun php-end-of-defun (&optional arg)
  "Move the end of the ARGth PHP function from point.
Implements PHP befsion of `end-of-defun-function'

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
                                 ("multi-mode" multi-mode (lambda () (multi-mode 1)))))
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
                               (caddr (assoc name available-multi-libs)))))
                  (when mode
                    ;; Minibuffer window is more than one line, fix that first:
                    (message "")
                    (load name)
                    (funcall mode))))
            (lwarn 'php-indent :warning base-msg)))
        nil))))

(defun php-cautious-indent-region (start end &optional quiet)
  (if (or (not php-mode-warn-if-mumamo-off)
          php-warned-bad-indent
          (php-check-html-for-indentation))
      (funcall 'c-indent-region start end quiet)))

(defun php-cautious-indent-line ()
  (if (or (not php-mode-warn-if-mumamo-off)
          php-warned-bad-indent
          (php-check-html-for-indentation))
      (let ((here (point))
            doit)
        (move-beginning-of-line nil)
        ;; Don't indent heredoc end mark
        (save-match-data
          (unless (looking-at "[a-zA-Z0-9_]+;\n")
            (setq doit t)))
        (goto-char here)
        (when doit
          (funcall 'c-indent-line)))))

(defconst php-tags '("<?php" "?>" "<?" "<?="))
(defconst php-tags-key (regexp-opt php-tags))

(defconst php-block-stmt-1-kwds '("do" "else" "finally" "try"))
(defconst php-block-stmt-2-kwds
  '("for" "if" "while" "switch" "foreach" "elseif"  "catch all"))

(defconst php-block-stmt-1-key
  (regexp-opt php-block-stmt-1-kwds))
(defconst php-block-stmt-2-key
  (regexp-opt php-block-stmt-2-kwds))

(defconst php-class-decl-kwds '("class" "interface" "trait" "namespace"))

(defconst php-class-key
  (concat
   "\\(" (regexp-opt php-class-decl-kwds) "\\)\\s-+"
   (c-lang-const c-symbol-key c)                ;; Class name.
   "\\(\\s-+extends\\s-+" (c-lang-const c-symbol-key c) "\\)?" ;; Name of superclass.
   "\\(\\s-+implements\\s-+[^{]+{\\)?")) ;; List of any adopted protocols.


(defun php-c-at-vsemi-p (&optional pos)
  "Return t on html lines (including php region border), otherwise nil.
POS is a position on the line in question.

This is was done due to the problem reported here:

  URL `https://answers.launchpad.net/nxhtml/+question/43320'"
  (setq pos (or pos (point)))
  (let ((here (point))
        ret)
  (save-match-data
    (goto-char pos)
    (beginning-of-line)
    (setq ret (looking-at
               (rx
                (or (seq
                     bol
                     (0+ space)
                     "<"
                     (in "a-z\\?"))
                    (seq
                     (0+ not-newline)
                     (in "a-z\\?")
                     ">"
                     (0+ space)
                     eol))))))
  (goto-char here)
  ret))

(defun php-c-vsemi-status-unknown-p ()
  "See `php-c-at-vsemi-p'."
  )

(defun php-lineup-arglist-intro (langelem)
  (save-excursion
    (goto-char (cdr langelem))
    (vector (+ (current-column) c-basic-offset))))

(defun php-lineup-arglist-close (langelem)
  (save-excursion
    (goto-char (cdr langelem))
    (vector (current-column))))

(c-set-offset 'arglist-intro 'php-lineup-arglist-intro)
(c-set-offset 'arglist-close 'php-lineup-arglist-close)

(defun php-unindent-closure ()
  (let ((syntax (mapcar 'car c-syntactic-context)))
    (if (and (member 'arglist-cont-nonempty syntax)
             (or
              (member 'statement-block-intro syntax)
              (member 'brace-list-intro syntax)
              (member 'brace-list-close syntax)
              (member 'block-close syntax)))
        (save-excursion
          (let ((count-func (if (fboundp 'cl-count) #'cl-count #'count)))
            (beginning-of-line)
            (delete-char (* (funcall count-func 'arglist-cont-nonempty syntax)
                            c-basic-offset)))))))

;;;###autoload
(define-derived-mode php-mode c-mode "PHP"
  "Major mode for editing PHP code.\n\n\\{php-mode-map}"
  (c-add-language 'php-mode 'c-mode)

  ;; PHP doesn't have C-style macros.
  ;; HACK: Overwrite this syntax with rules to match <?php and others.
  (set (make-local-variable 'c-opt-cpp-start) php-tags-key)
  (set (make-local-variable 'c-opt-cpp-prefix) php-tags-key)

  ;; These settings ensure that chained method calls line up correctly
  ;; over multiple lines.
  (c-set-offset 'topmost-intro-cont 'c-lineup-cascaded-calls)
  (c-set-offset 'brace-list-entry 'c-lineup-cascaded-calls)

  (set (make-local-variable 'c-block-stmt-1-key) php-block-stmt-1-key)
  (set (make-local-variable 'c-block-stmt-2-key) php-block-stmt-2-key)

  ;; Specify that cc-mode recognize Javadoc comment style
  (set (make-local-variable 'c-doc-comment-style)
    '((php-mode . javadoc)))

  (set (make-local-variable 'c-class-key) php-class-key)

  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults
        '((php-font-lock-keywords-1
           php-font-lock-keywords-2
           ;; Comment-out the next line if the font-coloring is too
           ;; extreme/ugly for you.
           php-font-lock-keywords-3)
          nil                ; KEYWORDS-ONLY
          t                  ; CASE-FOLD
          (("_" . "w"))      ; SYNTAX-ALIST
          nil))              ; SYNTAX-BEGIN

  (modify-syntax-entry ?_    "_" php-mode-syntax-table)
  (modify-syntax-entry ?`    "\"" php-mode-syntax-table)
  (modify-syntax-entry ?\"   "\"" php-mode-syntax-table)
  (modify-syntax-entry ?#    "< b" php-mode-syntax-table)
  (modify-syntax-entry ?\n   "> b" php-mode-syntax-table)

  (set (make-local-variable 'syntax-propertize-via-font-lock)
       '(("\\(\"\\)\\(\\\\.\\|[^\"\n\\]\\)*\\(\"\\)" (1 "\"") (3 "\""))
         ("\\(\'\\)\\(\\\\.\\|[^\'\n\\]\\)*\\(\'\\)" (1 "\"") (3 "\""))))

  (setq font-lock-maximum-decoration t
        imenu-generic-expression php-imenu-generic-expression)

  ;; PHP vars are case-sensitive
  (setq case-fold-search t)

  ;; Do not force newline at end of file.  Such newlines can cause
  ;; trouble if the PHP file is included in another file before calls
  ;; to header() or cookie().
  (set (make-local-variable 'require-final-newline) nil)
  (set (make-local-variable 'next-line-add-newlines) nil)

  ;; PEAR coding standards
  (add-hook 'php-mode-pear-hook 'php-enable-pear-coding-style
             nil t)

  ;; ;; Drupal coding standards
  (add-hook 'php-mode-drupal-hook 'php-enable-drupal-coding-style
             nil t)

  ;; ;; WordPress coding standards
  (add-hook 'php-mode-wordpress-hook 'php-enable-wordpress-coding-style
             nil t)

  (cond ((eq php-mode-coding-style 'pear)
                 (run-hooks 'php-mode-pear-hook))
                ((eq php-mode-coding-style 'drupal)
                 (run-hooks 'php-mode-drupal-hook))
                ((eq php-mode-coding-style 'wordpress)
                 (run-hooks 'php-mode-wordpress-hook)))

  (if (or php-mode-force-pear
          (and (stringp buffer-file-name)
               (string-match "PEAR\\|pear"
                             (buffer-file-name))
               (string-match "\\.php$" (buffer-file-name))))
      (run-hooks 'php-mode-pear-hook))

  (setq indent-line-function 'php-cautious-indent-line)
  (setq indent-region-function 'php-cautious-indent-region)
  (add-hook 'c-special-indent-hook 'php-unindent-closure)
  (setq c-at-vsemi-p-fn 'php-c-at-vsemi-p)
  (setq c-vsemi-status-unknown-p 'php-c-vsemi-status-unknown-p)

  (set (make-local-variable 'syntax-begin-function)
       'c-beginning-of-syntax)
  (set (make-local-variable 'beginning-of-defun-function)
       'php-beginning-of-defun)
  (set (make-local-variable 'end-of-defun-function)
       'php-end-of-defun)
  (set (make-local-variable 'open-paren-in-column-0-is-defun-start)
       nil)
  (set (make-local-variable 'defun-prompt-regexp)
       "^\\s-*function\\s-+&?\\s-*\\(\\(\\sw\\|\\s_\\)+\\)\\s-*")
  (set (make-local-variable 'add-log-current-defun-header-regexp)
       php-beginning-of-defun-regexp))

;; Make a menu keymap (with a prompt string)
;; and make it the menu bar item's definition.
(define-key php-mode-map [menu-bar] (make-sparse-keymap))
(define-key php-mode-map [menu-bar php]
  (cons "PHP" (make-sparse-keymap "PHP")))

;; Define specific subcommands in this menu.
(define-key php-mode-map [menu-bar php complete-function]
  '("Complete function name" . php-complete-function))
(define-key php-mode-map
  [menu-bar php browse-manual]
  '("Browse manual" . php-browse-manual))
(define-key php-mode-map
  [menu-bar php search-documentation]
  '("Search documentation" . php-search-documentation))

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
                 (message "Making completion list...")
                 (with-output-to-temp-buffer "*Completions*"
                   (display-completion-list
                    (all-completions pattern php-functions)))
                 (message "Making completion list...%s" "done")))))))

(defun php-completion-table ()
  "Build variable `php-completion-table' on demand.
The table includes the PHP functions and the tags from the
current `tags-file-name'."
  (or (and tags-file-name
           (save-excursion (tags-verify-table tags-file-name))
           php-completion-table)
      (let ((tags-table
             (if (and tags-file-name
                      (functionp 'etags-tags-completion-table))
                 (with-current-buffer (get-file-buffer tags-file-name)
                   (etags-tags-completion-table))
               nil))
            (php-table
             (cond ((and (not (string= "" php-completion-file))
                         (file-readable-p php-completion-file))
                    (php-build-table-from-file php-completion-file))
                   (php-manual-path
                    (php-build-table-from-path php-manual-path))
                   (t nil))))
        (unless (or php-table tags-table)
          (error
           (concat "No TAGS file active nor are "
                   "`php-completion-file' or `php-manual-path' set")))
        (when tags-table
          ;; Combine the tables.
          (mapatoms (lambda (sym) (intern (symbol-name sym) php-table))
                    tags-table))
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
  (let ((table (make-vector 1022 0))
        (files (directory-files
                path
                nil
                "^function\\..+\\.html$")))
    (mapc (lambda (file)
            (string-match "\\.\\([-a-zA-Z_0-9]+\\)\\.html$" file)
            (intern
             (replace-regexp-in-string
              "-" "_" (substring file (match-beginning 1) (match-end 1)) t)
             table))
          files)
    table))

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
  (interactive)
  (let* ((tagname (php-get-pattern))
         (buf (find-tag-noselect tagname nil nil))
         arglist)
    (with-current-buffer buf
      (goto-char (point-min))
      (when (re-search-forward
             (format "function\\s-+%s\\s-*(\\([^{]*\\))" tagname)
             nil t)
        (setq arglist (buffer-substring-no-properties
                       (match-beginning 1) (match-end 1)))))
    (if arglist
        (message "Arglist for %s: %s" tagname arglist)
        (message "Unknown function: %s" tagname))))

(defun php-search-local-documentation ()
  "Search the local PHP documentation (i.e. in `php-manual-path')
for the word at point.  The function returns t if the requested
documentation exists, and nil otherwise."
  (interactive)
  (cl-flet ((php-function-file-for (name)
                                (expand-file-name
                                 (format "function.%s.html"
                                         (replace-regexp-in-string "_" "-" name))
                                 php-manual-path)))
    (let ((doc-file (php-function-file-for (current-word))))
      (and (file-exists-p doc-file)
           (browse-url doc-file)
           t))))

;; Define function documentation function
(defun php-search-documentation ()
  "Search PHP documentation for the word at point.  If
`php-manual-path' has a non-empty string value then the command
will first try searching the local documentation.  If the
requested documentation does not exist it will fallback to
searching the PHP website."
  (interactive)
  (cl-flet ((php-search-web-documentation ()
                                       (browse-url (concat php-search-url (current-word)))))
    (if (and (stringp php-manual-path)
             (not (string= php-manual-path "")))
        (or (php-search-local-documentation)
            (php-search-web-documentation))
      (php-search-web-documentation))))

;; Define function for browsing manual
(defun php-browse-manual ()
  "Bring up manual for PHP."
  (interactive)
  (browse-url php-manual-url))

;; Define shortcut
(define-key php-mode-map
  "\C-c\C-f"
  'php-search-documentation)

;; Define shortcut
(define-key php-mode-map
  [(meta tab)]
  'php-complete-function)

;; Define shortcut
(define-key php-mode-map
  "\C-c\C-m"
  'php-browse-manual)

;; Define shortcut
(define-key php-mode-map
  '[(control .)]
  'php-show-arglist)

;; Use the Emacs standard indentation binding. This may upset c-mode
;; which does not follow this at the moment, but I see no better
;; choice.
(define-key php-mode-map [?\t] 'indent-for-tab-command)

(defconst php-constants
  (eval-when-compile
    (regexp-opt
     '(;; core constants
       "__LINE__" "__FILE__" "__DIR__"
       "__FUNCTION__" "__CLASS__" "__TRAIT__" "__METHOD__"
       "__NAMESPACE__"
       "__COMPILER_HALT_OFFSET__"
       "PHP_OS" "PHP_VERSION"
       "PHP_MINOR_VERSION" "PHP_MAJOR_VERSION" "PHP_RELEASE_VERSION"
       "PHP_VERSION_ID" "PHP_EXTRA_VERSION"
       "TRUE" "FALSE" "NULL"
       "E_ERROR" "E_NOTICE" "E_PARSE" "E_WARNING" "E_ALL" "E_STRICT"
       "E_USER_ERROR" "E_USER_WARNING" "E_USER_NOTICE"
       "E_CORE_ERROR" "E_CORE_WARNING"
       "E_COMPILE_ERROR" "E_COMPILE_WARNING"
       "E_DEPRECATED" "E_USER_DEPRECATED"
       "DEFAULT_INCLUDE_PATH" "PEAR_INSTALL_DIR" "PEAR_EXTENSION_DIR"
       "PHP_BINDIR" "PHP_LIBDIR" "PHP_DATADIR" "PHP_SYSCONFDIR"
       "PHP_LOCALSTATEDIR" "PHP_CONFIG_FILE_PATH"
       "PHP_EOL"
       "PHP_ZTS"
       "PHP_DEBUG"
       "PHP_MAXPATHLEN"
       "PHP_SAPI"
       "PHP_INT_MAX" "PHP_INT_SIZE"
       "PHP_EXTENSION_DIR"
       "PHP_PREFIX"
       "PHP_MANDIR"
       "PHP_CONFIG_FILE_SCAN_DIR"

       "PHP_WINDOWS_VERSION_MAJOR"
       "PHP_WINDOWS_VERSION_MINOR"
       "PHP_WINDOWS_VERSION_BUILD"
       "PHP_WINDOWS_VERSION_PLATFORM"
       "PHP_WINDOWS_VERSION_SP_MAJOR"
       "PHP_WINDOWS_VERSION_SP_MINOR"
       "PHP_WINDOWS_VERSION_SUITEMASK"
       "PHP_WINDOWS_VERSION_PRODUCTTYPE"
       "PHP_WINDOWS_NT_DOMAIN_CONTROLLER"
       "PHP_WINDOWS_NT_SERVER"
       "PHP_WINDOWS_NT_WORKSTATION"

       ;; date and time constants
       "DATE_ATOM" "DATE_COOKIE" "DATE_ISO8601"
       "DATE_RFC822" "DATE_RFC850" "DATE_RFC1036" "DATE_RFC1123"
       "DATE_RFC2822" "DATE_RFC3339"
       "DATE_RSS" "DATE_W3C"

       ;; upload error message constants
       "UPLOAD_ERR_CANT_WRITE" "UPLOAD_ERR_EXTENSION"
       "UPLOAD_ERR_FORM_SIZE" "UPLOAD_ERR_INI_SIZE"
       "UPLOAD_ERR_NO_FILE" "UPLOAD_ERR_NO_TMP_DIR"
       "UPLOAD_ERR_OK" "UPLOAD_ERR_PARTIAL"

       ;; from ext/standard:
       "EXTR_OVERWRITE"
       "EXTR_PREFIX_SAME"
       "EXTR_PREFIX_INVALID"
       "EXTR_IF_EXISTS"
       "SORT_DESC"
       "SORT_NUMERIC"
       "CASE_LOWER"
       "COUNT_NORMAL"
       "ASSERT_ACTIVE"
       "ASSERT_BAIL"
       "ASSERT_QUIET_EVAL"
       "CONNECTION_NORMAL"
       "INI_USER"
       "INI_SYSTEM"
       "M_E"
       "M_LOG10E"
       "M_LN10"
       "M_PI_2"
       "M_1_PI"
       "M_2_SQRTPI"
       "M_SQRT1_2"
       "CRYPT_STD_DES"
       "CRYPT_MD5"
       "DIRECTORY_SEPARATOR"
       "SEEK_CUR"
       "LOCK_SH"
       "LOCK_UN"
       "HTML_SPECIALCHARS"
       "ENT_COMPAT"
       "ENT_QUOTES"
       "ENT_NOQUOTES"
       "ENT_IGNORE"
       "ENT_SUBSTITUTE"
       "ENT_DISALLOWED"
       "ENT_HTML401"
       "ENT_XML1"
       "ENT_XHTML"
       "ENT_HTML5"
       "INFO_CREDITS"
       "INFO_MODULES"
       "INFO_VARIABLES"
       "INFO_ALL"
       "CREDITS_GENERAL"
       "CREDITS_MODULES"
       "CREDITS_FULLPAGE"
       "CREDITS_ALL"
       "STR_PAD_RIGHT"
       "PATHINFO_DIRNAME"
       "PATHINFO_EXTENSION"
       "CHAR_MAX"
       "LC_NUMERIC"
       "LC_COLLATE"
       "LC_ALL"
       "ABDAY_1"
       "ABDAY_3"
       "ABDAY_5"
       "ABDAY_7"
       "DAY_2"
       "DAY_4"
       "DAY_6"
       "ABMON_1"
       "ABMON_3"
       "ABMON_5"
       "ABMON_7"
       "ABMON_9"
       "ABMON_11"
       "MON_1"
       "MON_3"
       "MON_5"
       "MON_7"
       "MON_9"
       "MON_11"
       "AM_STR"
       "D_T_FMT"
       "T_FMT"
       "ERA"
       "ERA_D_T_FMT"
       "ERA_T_FMT"
       "INT_CURR_SYMBOL"
       "CRNCYSTR"
       "MON_THOUSANDS_SEP"
       "POSITIVE_SIGN"
       "INT_FRAC_DIGITS"
       "P_CS_PRECEDES"
       "N_CS_PRECEDES"
       "P_SIGN_POSN"
       "DECIMAL_POINT"
       "THOUSANDS_SEP"
       "GROUPING"
       "NOEXPR"
       "NOSTR"
       "LOG_EMERG"
       "LOG_CRIT"
       "LOG_WARNING"
       "LOG_INFO"
       "LOG_KERN"
       "LOG_MAIL"
       "LOG_AUTH"
       "LOG_LPR"
       "LOG_UUCP"
       "LOG_AUTHPRIV"
       "LOG_LOCAL1"
       "LOG_LOCAL3"
       "LOG_LOCAL5"
       "LOG_LOCAL7"
       "LOG_CONS"
       "LOG_NDELAY"
       "LOG_PERROR"

       ; Filter constants
       "INPUT_POST"
       "INPUT_GET"
       "INPUT_COOKIE"
       "INPUT_ENV"
       "INPUT_SERVER"
       "INPUT_SESSION"
       "INPUT_REQUEST"
       "FILTER_FLAG_NONE"
       "FILTER_REQUIRE_SCALAR"
       "FILTER_REQUIRE_ARRAY"
       "FILTER_FORCE_ARRAY"
       "FILTER_NULL_ON_FAILURE"
       "FILTER_VALIDATE_INT"
       "FILTER_VALIDATE_BOOLEAN"
       "FILTER_VALIDATE_FLOAT"
       "FILTER_VALIDATE_REGEXP"
       "FILTER_VALIDATE_URL"
       "FILTER_VALIDATE_EMAIL"
       "FILTER_VALIDATE_IP"
       "FILTER_DEFAULT"
       "FILTER_UNSAFE_RAW"
       "FILTER_SANITIZE_STRING"
       "FILTER_SANITIZE_STRIPPED"
       "FILTER_SANITIZE_ENCODED"
       "FILTER_SANITIZE_SPECIAL_CHARS"
       "FILTER_SANITIZE_EMAIL"
       "FILTER_SANITIZE_URL"
       "FILTER_SANITIZE_NUMBER_INT"
       "FILTER_SANITIZE_NUMBER_FLOAT"
       "FILTER_SANITIZE_MAGIC_QUOTES"
       "FILTER_CALLBACK"
       "FILTER_FLAG_ALLOW_OCTAL"
       "FILTER_FLAG_ALLOW_HEX"
       "FILTER_FLAG_STRIP_LOW"
       "FILTER_FLAG_STRIP_HIGH"
       "FILTER_FLAG_ENCODE_LOW"
       "FILTER_FLAG_ENCODE_HIGH"
       "FILTER_FLAG_ENCODE_AMP"
       "FILTER_FLAG_NO_ENCODE_QUOTES"
       "FILTER_FLAG_EMPTY_STRING_NULL"
       "FILTER_FLAG_ALLOW_FRACTION"
       "FILTER_FLAG_ALLOW_THOUSAND"
       "FILTER_FLAG_ALLOW_SCIENTIFIC"
       "FILTER_FLAG_PATH_REQUIRED"
       "FILTER_FLAG_QUERY_REQUIRED"
       "FILTER_FLAG_IPV4"
       "FILTER_FLAG_IPV6"
       "FILTER_FLAG_NO_RES_RANGE"
       "FILTER_FLAG_NO_PRIV_RANGE"

       ;; Password constants
       "PASSWORD_DEFAULT"
       "PASSWORD_BCRYPT"

       ;; cURL constants
       "CURLOPT_AUTOREFERER"
       "CURLOPT_COOKIESESSION"
       "CURLOPT_DNS_USE_GLOBAL_CACHE"
       "CURLOPT_DNS_CACHE_TIMEOUT"
       "CURLOPT_FTP_SSL"
       "CURLFTPSSL_TRY"
       "CURLFTPSSL_ALL"
       "CURLFTPSSL_CONTROL"
       "CURLFTPSSL_NONE"
       "CURLOPT_PRIVATE"
       "CURLOPT_FTPSSLAUTH"
       "CURLOPT_PORT"
       "CURLOPT_FILE"
       "CURLOPT_INFILE"
       "CURLOPT_INFILESIZE"
       "CURLOPT_URL"
       "CURLOPT_PROXY"
       "CURLOPT_VERBOSE"
       "CURLOPT_HEADER"
       "CURLOPT_HTTPHEADER"
       "CURLOPT_NOPROGRESS"
       "CURLOPT_NOBODY"
       "CURLOPT_FAILONERROR"
       "CURLOPT_UPLOAD"
       "CURLOPT_POST"
       "CURLOPT_FTPLISTONLY"
       "CURLOPT_FTPAPPEND"
       "CURLOPT_FTP_CREATE_MISSING_DIRS"
       "CURLOPT_NETRC"
       "CURLOPT_FOLLOWLOCATION"
       "CURLOPT_FTPASCII"
       "CURLOPT_PUT"
       "CURLOPT_MUTE"
       "CURLOPT_USERPWD"
       "CURLOPT_PROXYUSERPWD"
       "CURLOPT_RANGE"
       "CURLOPT_TIMEOUT"
       "CURLOPT_TIMEOUT_MS"
       "CURLOPT_TCP_NODELAY"
       "CURLOPT_POSTFIELDS"
       "CURLOPT_PROGRESSFUNCTION"
       "CURLOPT_REFERER"
       "CURLOPT_USERAGENT"
       "CURLOPT_FTPPORT"
       "CURLOPT_FTP_USE_EPSV"
       "CURLOPT_LOW_SPEED_LIMIT"
       "CURLOPT_LOW_SPEED_TIME"
       "CURLOPT_RESUME_FROM"
       "CURLOPT_COOKIE"
       "CURLOPT_SSLCERT"
       "CURLOPT_SSLCERTPASSWD"
       "CURLOPT_WRITEHEADER"
       "CURLOPT_SSL_VERIFYHOST"
       "CURLOPT_COOKIEFILE"
       "CURLOPT_SSLVERSION"
       "CURLOPT_TIMECONDITION"
       "CURLOPT_TIMEVALUE"
       "CURLOPT_CUSTOMREQUEST"
       "CURLOPT_STDERR"
       "CURLOPT_TRANSFERTEXT"
       "CURLOPT_RETURNTRANSFER"
       "CURLOPT_QUOTE"
       "CURLOPT_POSTQUOTE"
       "CURLOPT_INTERFACE"
       "CURLOPT_KRB4LEVEL"
       "CURLOPT_HTTPPROXYTUNNEL"
       "CURLOPT_FILETIME"
       "CURLOPT_WRITEFUNCTION"
       "CURLOPT_READFUNCTION"
       "CURLOPT_PASSWDFUNCTION"
       "CURLOPT_HEADERFUNCTION"
       "CURLOPT_MAXREDIRS"
       "CURLOPT_MAXCONNECTS"
       "CURLOPT_CLOSEPOLICY"
       "CURLOPT_FRESH_CONNECT"
       "CURLOPT_FORBID_REUSE"
       "CURLOPT_RANDOM_FILE"
       "CURLOPT_EGDSOCKET"
       "CURLOPT_CONNECTTIMEOUT"
       "CURLOPT_CONNECTTIMEOUT_MS"
       "CURLOPT_SSL_VERIFYPEER"
       "CURLOPT_CAINFO"
       "CURLOPT_CAPATH"
       "CURLOPT_COOKIEJAR"
       "CURLOPT_SSL_CIPHER_LIST"
       "CURLOPT_BINARYTRANSFER"
       "CURLOPT_NOSIGNAL"
       "CURLOPT_PROXYTYPE"
       "CURLOPT_BUFFERSIZE"
       "CURLOPT_HTTPGET"
       "CURLOPT_HTTP_VERSION"
       "CURLOPT_SSLKEY"
       "CURLOPT_SSLKEYTYPE"
       "CURLOPT_SSLKEYPASSWD"
       "CURLOPT_SSLENGINE"
       "CURLOPT_SSLENGINE_DEFAULT"
       "CURLOPT_SSLCERTTYPE"
       "CURLOPT_CRLF"
       "CURLOPT_ENCODING"
       "CURLOPT_PROXYPORT"
       "CURLOPT_UNRESTRICTED_AUTH"
       "CURLOPT_FTP_USE_EPRT"
       "CURLOPT_HTTP200ALIASES"
       "CURLOPT_HTTPAUTH"
       "CURLAUTH_BASIC"
       "CURLAUTH_DIGEST"
       "CURLAUTH_GSSNEGOTIATE"
       "CURLAUTH_NTLM"
       "CURLAUTH_ANY"
       "CURLAUTH_ANYSAFE"
       "CURLOPT_PROXYAUTH"
       "CURLOPT_MAX_RECV_SPEED_LARGE"
       "CURLOPT_MAX_SEND_SPEED_LARGE"
       "CURLCLOSEPOLICY_LEAST_RECENTLY_USED"
       "CURLCLOSEPOLICY_LEAST_TRAFFIC"
       "CURLCLOSEPOLICY_SLOWEST"
       "CURLCLOSEPOLICY_CALLBACK"
       "CURLCLOSEPOLICY_OLDEST"
       "CURLINFO_PRIVATE"
       "CURLINFO_EFFECTIVE_URL"
       "CURLINFO_HTTP_CODE"
       "CURLINFO_HEADER_OUT"
       "CURLINFO_HEADER_SIZE"
       "CURLINFO_REQUEST_SIZE"
       "CURLINFO_TOTAL_TIME"
       "CURLINFO_NAMELOOKUP_TIME"
       "CURLINFO_CONNECT_TIME"
       "CURLINFO_PRETRANSFER_TIME"
       "CURLINFO_SIZE_UPLOAD"
       "CURLINFO_SIZE_DOWNLOAD"
       "CURLINFO_SPEED_DOWNLOAD"
       "CURLINFO_SPEED_UPLOAD"
       "CURLINFO_FILETIME"
       "CURLINFO_SSL_VERIFYRESULT"
       "CURLINFO_CONTENT_LENGTH_DOWNLOAD"
       "CURLINFO_CONTENT_LENGTH_UPLOAD"
       "CURLINFO_STARTTRANSFER_TIME"
       "CURLINFO_CONTENT_TYPE"
       "CURLINFO_REDIRECT_TIME"
       "CURLINFO_REDIRECT_COUNT"
       "CURL_TIMECOND_IFMODSINCE"
       "CURL_TIMECOND_IFUNMODSINCE"
       "CURL_TIMECOND_LASTMOD"
       "CURL_VERSION_IPV6"
       "CURL_VERSION_KERBEROS4"
       "CURL_VERSION_SSL"
       "CURL_VERSION_LIBZ"
       "CURLVERSION_NOW"
       "CURLE_OK"
       "CURLE_UNSUPPORTED_PROTOCOL"
       "CURLE_FAILED_INIT"
       "CURLE_URL_MALFORMAT"
       "CURLE_URL_MALFORMAT_USER"
       "CURLE_COULDNT_RESOLVE_PROXY"
       "CURLE_COULDNT_RESOLVE_HOST"
       "CURLE_COULDNT_CONNECT"
       "CURLE_FTP_WEIRD_SERVER_REPLY"
       "CURLE_FTP_ACCESS_DENIED"
       "CURLE_FTP_USER_PASSWORD_INCORRECT"
       "CURLE_FTP_WEIRD_PASS_REPLY"
       "CURLE_FTP_WEIRD_USER_REPLY"
       "CURLE_FTP_WEIRD_PASV_REPLY"
       "CURLE_FTP_WEIRD_227_FORMAT"
       "CURLE_FTP_CANT_GET_HOST"
       "CURLE_FTP_CANT_RECONNECT"
       "CURLE_FTP_COULDNT_SET_BINARY"
       "CURLE_PARTIAL_FILE"
       "CURLE_FTP_COULDNT_RETR_FILE"
       "CURLE_FTP_WRITE_ERROR"
       "CURLE_FTP_QUOTE_ERROR"
       "CURLE_HTTP_NOT_FOUND"
       "CURLE_WRITE_ERROR"
       "CURLE_MALFORMAT_USER"
       "CURLE_FTP_COULDNT_STOR_FILE"
       "CURLE_READ_ERROR"
       "CURLE_OUT_OF_MEMORY"
       "CURLE_OPERATION_TIMEOUTED"
       "CURLE_FTP_COULDNT_SET_ASCII"
       "CURLE_FTP_PORT_FAILED"
       "CURLE_FTP_COULDNT_USE_REST"
       "CURLE_FTP_COULDNT_GET_SIZE"
       "CURLE_HTTP_RANGE_ERROR"
       "CURLE_HTTP_POST_ERROR"
       "CURLE_SSL_CONNECT_ERROR"
       "CURLE_FTP_BAD_DOWNLOAD_RESUME"
       "CURLE_FILE_COULDNT_READ_FILE"
       "CURLE_LDAP_CANNOT_BIND"
       "CURLE_LDAP_SEARCH_FAILED"
       "CURLE_LIBRARY_NOT_FOUND"
       "CURLE_FUNCTION_NOT_FOUND"
       "CURLE_ABORTED_BY_CALLBACK"
       "CURLE_BAD_FUNCTION_ARGUMENT"
       "CURLE_BAD_CALLING_ORDER"
       "CURLE_HTTP_PORT_FAILED"
       "CURLE_BAD_PASSWORD_ENTERED"
       "CURLE_TOO_MANY_REDIRECTS"
       "CURLE_UNKNOWN_TELNET_OPTION"
       "CURLE_TELNET_OPTION_SYNTAX"
       "CURLE_OBSOLETE"
       "CURLE_SSL_PEER_CERTIFICATE"
       "CURLE_GOT_NOTHING"
       "CURLE_SSL_ENGINE_NOTFOUND"
       "CURLE_SSL_ENGINE_SETFAILED"
       "CURLE_SEND_ERROR"
       "CURLE_RECV_ERROR"
       "CURLE_SHARE_IN_USE"
       "CURLE_SSL_CERTPROBLEM"
       "CURLE_SSL_CIPHER"
       "CURLE_SSL_CACERT"
       "CURLE_BAD_CONTENT_ENCODING"
       "CURLE_LDAP_INVALID_URL"
       "CURLE_FILESIZE_EXCEEDED"
       "CURLE_FTP_SSL_FAILED"
       "CURLFTPAUTH_DEFAULT"
       "CURLFTPAUTH_SSL"
       "CURLFTPAUTH_TLS"
       "CURLPROXY_HTTP"
       "CURLPROXY_SOCKS5"
       "CURL_NETRC_OPTIONAL"
       "CURL_NETRC_IGNORED"
       "CURL_NETRC_REQUIRED"
       "CURL_HTTP_VERSION_NONE"
       "CURL_HTTP_VERSION_1_0"
       "CURL_HTTP_VERSION_1_1"
       "CURLM_CALL_MULTI_PERFORM"
       "CURLM_OK"
       "CURLM_BAD_HANDLE"
       "CURLM_BAD_EASY_HANDLE"
       "CURLM_OUT_OF_MEMORY"
       "CURLM_INTERNAL_ERROR"
       "CURLMSG_DONE"
       "CURLOPT_KEYPASSWD"
       "CURLOPT_SSH_AUTH_TYPES"
       "CURLOPT_SSH_HOST_PUBLIC_KEY_MD5"
       "CURLOPT_SSH_PRIVATE_KEYFILE"
       "CURLOPT_SSH_PUBLIC_KEYFILE"
       "CURLSSH_AUTH_ANY"
       "CURLSSH_AUTH_DEFAULT"
       "CURLSSH_AUTH_HOST"
       "CURLSSH_AUTH_KEYBOARD"
       "CURLSSH_AUTH_NONE"
       "CURLSSH_AUTH_PASSWORD"
       "CURLSSH_AUTH_PUBLICKEY"

       ;; IMAP constants
       "NIL"
       "OP_DEBUG"
       "OP_READONLY"
       "OP_ANONYMOUS"
       "OP_SHORTCACHE"
       "OP_SILENT"
       "OP_PROTOTYPE"
       "OP_HALFOPEN"
       "OP_EXPUNGE"
       "OP_SECURE"
       "CL_EXPUNGE"
       "FT_UID"
       "FT_PEEK"
       "FT_NOT"
       "FT_INTERNAL"
       "FT_PREFETCHTEXT"
       "ST_UID"
       "ST_SILENT"
       "ST_SET"
       "CP_UID"
       "CP_MOVE"
       "SE_UID"
       "SE_FREE"
       "SE_NOPREFETCH"
       "SO_FREE"
       "SO_NOSERVER"
       "SA_MESSAGES"
       "SA_RECENT"
       "SA_UNSEEN"
       "SA_UIDNEXT"
       "SA_UIDVALIDITY"
       "SA_ALL"
       "LATT_NOINFERIORS"
       "LATT_NOSELECT"
       "LATT_MARKED"
       "LATT_UNMARKED"
       "SORTDATE"
       "SORTARRIVAL"
       "SORTFROM"
       "SORTSUBJECT"
       "SORTTO"
       "SORTCC"
       "SORTSIZE"
       "TYPETEXT"
       "TYPEMULTIPART"
       "TYPEMESSAGE"
       "TYPEAPPLCATION"
       "TYPEAUDIO"
       "TYPEIMAGE"
       "TYPEVIDEO"
       "TYPEOTHER"
       "ENC7BIT"
       "ENC8BIT"
       "ENCBINARY"
       "ENCBASE64"
       "ENCQUOTEDPRINTABLE"
       "ENCOTHER"
       "IMAP_OPENTIMEOUT"
       "IMAP_READTIMEOUT"
       "IMAP_WRITETIMEOUT"
       "IMAP_CLOSETIMEOUT"
       "LATT_REFERRAL"
       "LATT_HASCHILDREN"
       "LATT_HASNOCHILDREN"
       "TYPEMODEL"
       "IMAP_GC_ELT"
       "IMAP_GC_ENV"
       "IMAP_GC_TEXTS")))
  "PHP constants.")

(defconst php-keywords
  (eval-when-compile
    (regexp-opt
     ;; "class", "new" and "extends" get special treatment
     ;; "case" gets special treatment elsewhere
     '("and"
       "array"
       "as"
       "break"
       "catch all"
       "catch"
       "clone"
       "continue"
       "declare"
       "default"
       "die"
       "do"
       "echo"
       "else"
       "elseif"
       "empty"
       "endfor"
       "endforeach"
       "endif"
       "endswitch"
       "endwhile"
       "exit"
       "extends"
       "finally"
       "for"
       "foreach"
       "global"
       "if"
       "include"
       "include_once"
       "instanceof"
       "insteadof"
       "isset"
       "list"
       "or"
       "require"
       "require_once"
       "return"
       "static"
       "switch"
       "then"
       "throw"
       "try"
       "unset"
       "use"
       "var"
       "while"
       "xor"
       "yield")))
  "PHP keywords.")

(defconst php-identifier
  (eval-when-compile
    '"[a-zA-Z\_\x7f-\xff][a-zA-Z0-9\_\x7f-\xff]*")
  "Characters in a PHP identifier.")

(defconst php-types
  (eval-when-compile
    (regexp-opt '("array" "bool" "boolean" "callable" "char" "const" "double" "float"
                  "int" "integer" "long" "mixed" "object" "real"
                  "string")))
  "PHP types.")

(defconst php-superglobals
  (eval-when-compile
    (regexp-opt '("_GET" "_POST" "_COOKIE" "_SESSION" "_ENV" "GLOBALS"
                  "_SERVER" "_FILES" "_REQUEST")))
  "PHP superglobal variables.")

;; Set up font locking
(defconst php-font-lock-keywords-1
  (list

   ;; Fontify constants
   (cons
    (concat "[^_$]?\\<\\(" php-constants "\\)\\>[^_]?")
    '(1 font-lock-constant-face))

   ;; Fontify keywords
   (cons
    (concat "[^_$]?\\<\\(" php-keywords "\\)\\>[^_]?")
    '(1 font-lock-keyword-face))

   ;; Fontify keywords and targets, and case default tags.
   (list "\\<\\(break\\|case\\|continue\\)\\>\\s-+\\(-?\\sw+\\)?"
         '(1 font-lock-keyword-face) '(2 font-lock-constant-face t t))
   ;; This must come after the one for keywords and targets.
   '(":" ("^\\s-+\\(\\sw+\\)\\s-+\\s-+$"
          (beginning-of-line) (end-of-line)
          (1 font-lock-constant-face)))

   ;; treat 'print' as keyword only when not used like a function name
   '("\\<print\\s-*(" . php-function-call-face)
   '("\\<print\\>" . font-lock-keyword-face)

   ;; Fontify PHP tag
   (cons php-tags-key font-lock-preprocessor-face)

   ;; Fontify ASP-style tag
   '("<\\%\\(=\\)?" . font-lock-preprocessor-face)
   '("\\%>" . font-lock-preprocessor-face)

   )
  "Subdued level highlighting for PHP mode.")

(defconst php-font-lock-keywords-2
  (append
   php-font-lock-keywords-1
   (list

    ;; namespace, class, interface, and trait declarations
    '("\\<\\(namespace\\|class\\|interface\\|trait\\)\\s-+\\(\\(?:\\sw\\|\\\\\\)+\\)?"
      (1 font-lock-keyword-face) (2 font-lock-type-face nil t))

    ;; handle several words specially, to include following word,
    ;; thereby excluding it from unknown-symbol checks later
    ;; FIX to handle implementing multiple
    ;; currently breaks on "class Foo implements Bar, Baz"
    '("\\<\\(new\\|extends\\|implements\\)\\s-+\\$?\\(\\(:?\\sw\\|\\\\\\)+\\)"
      (1 font-lock-keyword-face) (2 font-lock-type-face nil t))

    ;; instanceof operator
    '("\\<instanceof\\s-+\\([^$]\\(:?\\sw\\|\\\\\\)+\\)"
      (1 font-lock-type-face nil t))

    ;; namespace imports
    '("\\<\\(use\\)\\s-+\\(\\(?:\\sw\\|\\(?:,\s-?\\)\\|\\\\\\)+\\)"
      (1 font-lock-keyword-face)
      (2 font-lock-type-face))

    ;; namespace imports with aliases
    '("\\<\\(use\\)\\s-+\\(\\(?:\\sw\\|\\\\\\)+\\)\\s-+\\(as\\)\\s-+\\(\\(?:\\sw\\|\\\\\\)+\\)"
      (1 font-lock-keyword-face)
      (2 font-lock-type-face)
      (3 font-lock-keyword-face)
      (4 font-lock-type-face))

    ;; constants
    '("\\<\\(const\\)\\s-+\\(\\sw+\\)"
      (1 font-lock-keyword-face)
      (2 font-lock-type-face))

    ;; function declaration
    '("\\<\\(function\\)\\s-+&?\\(\\sw+\\)\\s-*("
      (1 font-lock-keyword-face)
      (2 font-lock-function-name-face nil t))

    ;; class hierarchy
    '("\\<\\(self\\|parent\\)\\>" (1 font-lock-constant-face nil nil))

    ;; method and variable features
    '("\\<\\(private\\|protected\\|public\\)\\s-+\\$?\\sw+"
      (1 font-lock-keyword-face))

    ;; method features
    '("^\\s-*\\(abstract\\|static\\|final\\)\\s-+\\$?\\sw+"
      (1 font-lock-keyword-face))

    ;; variable features
    '("^\\s-*\\(static\\|const\\)\\s-+\\$?\\sw+"
      (1 font-lock-keyword-face))
    ))
  "Medium level highlighting for PHP mode.")

(defconst php-font-lock-keywords-3
  (append
   php-font-lock-keywords-2
   (list
    '("</?[a-z!:]+" . font-lock-constant-face)

    ;; HTML >
    '("<[^>]*\\(>\\)" (1 font-lock-constant-face))

    ;; HTML tags
    '("\\(<[a-z]+\\)[[:space:]]+\\([a-z:]+=\\)[^>]*?" (1 font-lock-constant-face) (2 font-lock-constant-face) )
    '("\"[[:space:]]+\\([a-z:]+=\\)" (1 font-lock-constant-face))

    ;; warn about $word.word -- it could be a valid concatenation,
    ;; but without any spaces we'll assume $word->word was meant.
    '("\\$\\sw+\\(\\.\\)\\sw"
      1 font-lock-warning-face)

    ;; Warn about ==> instead of =>
    '("==+>" . font-lock-warning-face)

    ;; exclude casts from bare-word treatment (may contain spaces)
    `(,(concat "(\\s-*\\(" php-types "\\)\\s-*)")
      1 font-lock-type-face)

    ;; PHP5: function declarations may contain classes as parameters type
    `("[(,]\\(?:\\s-\\|\n\\)*\\(\\(?:\\sw\\|\\\\\\)+\\)\\s-+&?\\$\\sw+\\>"
      1 font-lock-type-face)

    ;; Function calls qualified by namespaces
    '("\\(?:\\(\\sw+\\)\\\\\\)+\\sw+("
      (1 font-lock-type-face))

    ;; Fontify variables and function calls
    '("\\$\\(this\\|that\\)\\W" (1 font-lock-constant-face nil nil))

    ;; $_GET & co
    `(,(concat "\\$\\(" php-superglobals "\\)\\W")
      (1 font-lock-constant-face nil nil))

    ;; $variable
    '("\\$\\(\\sw+\\)" (1 font-lock-variable-name-face))

    ;; ->function_call
    '("->\\(\\sw+\\)\\s-*(" (1 php-function-call-face t t))

    ;; ->variable
    '("->\\(\\sw+\\)" (1 font-lock-variable-name-face t t))

    ;; class::member
    '("\\(\\(\\sw\\|\\\\\\)+\\)::\\sw+\\s-*(?" . (1 font-lock-type-face))

    ;; class::constant
    '("::\\(\\(?:\\sw\\|\\s_\\)+\\>\\)[^(]" . (1 font-lock-constant-face))

    ;; using a trait in a class
    '("\\<use\\s-+\\(\\sw+\\)\\s-*;" . (1 font-lock-type-face))

    ;; word(
    '("\\<\\(\\sw+\\s-*\\)(" . (1 php-function-call-face))

    ;; word[
    '("\\<\\(\\sw+\\s-*\\)\\[" . (1 php-default-face))

    ;; number (also matches word)
    '("\\<[0-9]+" . php-default-face)

    ;; Warn on any words not already fontified
    '("\\<\\sw+\\>" . font-lock-warning-face)))

  "Gauchy level highlighting for PHP mode.")



;;; Provide support for Flymake so that users can see warnings and
;;; errors in real-time as they write code.

(defun flymake-php-init ()
  (let* ((temp-file (flymake-init-create-temp-buffer-copy
                     'flymake-create-temp-inplace))
         (local-file (file-relative-name
                      temp-file
                      (file-name-directory buffer-file-name))))
    (list php-executable (list "-f" local-file "-l"))))

(add-to-list 'flymake-allowed-file-name-masks
             '("\\.php[345s]?$"
               flymake-php-init
               flymake-simple-cleanup
               flymake-get-real-file-name))

(add-to-list 'flymake-err-line-patterns
             '("\\(Parse\\|Fatal\\) error: \\(.*?\\) in \\(.*?\\) on line \\([0-9]+\\)" 3 4 nil 2))


(defun php-send-region (start end)
  "Send the region between `start' and `end' to PHP for execution.
The output will appear in the buffer *PHP*."
  (interactive "r")
  (let ((php-buffer (get-buffer-create "*PHP*"))
        (code (buffer-substring start end)))
    ;; Calling 'php -r' will fail if we send it code that starts with
    ;; '<?php', which is likely.  So we run the code through this
    ;; function to check for that prefix and remove it.
    (cl-flet ((clean-php-code (code)
                           (if (string-prefix-p "<?php" code t)
                               (substring code 5)
                             code)))
      (call-process "php" nil php-buffer nil "-r" (clean-php-code code)))))

(define-key php-mode-map "\C-c\C-r" 'php-send-region)


(defface php-annotations-annotation-face '((t . (:inherit 'font-lock-constant-face)))
  "Face used to highlight annotations.")

(defconst php-annotations-re "\\(\\s-\\|{\\)\\(@[[:alpha:]]+\\)")

(defmacro php-annotations-inside-comment-p (pos)
  "Return non-nil if POS is inside a comment."
  `(eq (get-char-property ,pos 'face) 'font-lock-comment-face))

(defun php-annotations-font-lock-find-annotation (limit)
  (let ((match
         (catch 'match
           (save-match-data
             (while (re-search-forward php-annotations-re limit t)
               (when (php-annotations-inside-comment-p (match-beginning 0))
                 (goto-char (match-end 0))
                 (throw 'match (match-data))))))))
    (when match
      (set-match-data match)
      t)))

(eval-after-load 'php-mode
  '(font-lock-add-keywords 'php-mode '((php-annotations-font-lock-find-annotation (2 'php-annotations-annotation-face t)))))



(provide 'php-mode)

;;; php-mode.el ends here

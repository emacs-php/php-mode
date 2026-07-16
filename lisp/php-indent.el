;;; php-indent.el --- Indentation engine for PHP Mode  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Friends of Emacs-PHP development
;; Copyright (C) 1999-2026 Free Software Foundation, Inc.

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

;; This file implements the indentation engine used by PHP Mode.
;;
;; The engine is ported from the `syntax-ppss'-based indenter of js.el
;; in GNU Emacs (originally derived from js.el in GNU Emacs).  It does
;; NOT depend on CC Mode.  The JavaScript-specific handling of js.el
;; (JSX, C preprocessor macros, array comprehensions and regular
;; expression literals) has been removed, and PHP-specific behaviour has
;; been added: `->', `?->' and `::' method/property chains, the `.'
;; string concatenation operator, PHP `switch'/`case' indentation, a
;; simplified alternative-syntax (`endif' etc.) alignment, and leaving
;; heredoc/nowdoc and string bodies untouched.
;;
;; The public entry points are `php-indent-line' (suitable as
;; `indent-line-function') and `php-indent-region'.

;;; Code:

(require 'php-core)

;;; Customization

(defcustom php-indent-offset 4
  "Number of columns for one level of PHP indentation."
  :group 'php
  :tag "PHP Indent Offset"
  :type 'integer
  :safe #'integerp)

(defcustom php-indent-switch-case-offset 4
  "Indentation of `case'/`default' labels relative to their `switch'.
Statements inside a `case' are indented by a further `php-indent-offset'."
  :group 'php
  :tag "PHP Indent Switch Case Offset"
  :type 'integer
  :safe #'integerp)

(defcustom php-indent-chain-indent t
  "If non-nil, align successive method/property chain lines.
A chained line begins with `->', `?->' or `::'."
  :group 'php
  :tag "PHP Indent Chain Indent"
  :type 'boolean
  :safe #'booleanp)

(defcustom php-indent-paren-offset 0
  "Additional indentation for lines inside a `(...)' group."
  :group 'php
  :tag "PHP Indent Paren Offset"
  :type 'integer
  :safe #'integerp)

(defcustom php-indent-square-offset 0
  "Additional indentation for lines inside a `[...]' group."
  :group 'php
  :tag "PHP Indent Square Offset"
  :type 'integer
  :safe #'integerp)

(defcustom php-indent-curly-offset 0
  "Additional indentation for lines inside a `{...}' group."
  :group 'php
  :tag "PHP Indent Curly Offset"
  :type 'integer
  :safe #'integerp)

(defcustom php-indent-switch-offset 0
  "Additional indentation applied to the whole body of a `switch'."
  :group 'php
  :tag "PHP Indent Switch Offset"
  :type 'integer
  :safe #'integerp)

;;; Constants

(defconst php-indent--name-start-re "[[:alpha:]_$]"
  "Regexp matching the start of a PHP identifier, without grouping.")

(defconst php-indent--name-re
  (concat php-indent--name-start-re "\\(?:\\s_\\|\\sw\\)*")
  "Regexp matching a PHP identifier, without grouping.")

(defun php-indent--regexp-opt-symbol (list)
  "Like `regexp-opt', but surround the result with symbol boundaries.
LIST is a list of strings to match as whole symbols."
  (concat "\\_<" (regexp-opt list t) "\\_>"))

(defconst php-indent--possibly-braceless-keyword-re
  (php-indent--regexp-opt-symbol
   '("if" "else" "elseif" "for" "foreach" "while" "do" "switch" "declare"))
  "Regexp matching keywords optionally followed by an opening brace.")

(defconst php-indent--declaration-keyword-re
  (regexp-opt '("const") 'words)
  "Regexp matching declaration keywords for multi-line alignment.")

(defconst php-indent--indent-operator-re
  (concat "[-+*/%<>&^|?:.]\\([^-+*/.]\\|$\\)\\|!?=\\([^>]\\|$\\)\\|"
          (php-indent--regexp-opt-symbol '("instanceof")))
  "Regexp matching operators that affect indentation of continued lines.
Note that `=>' (the array/arrow key operator) is intentionally excluded.")

(defconst php-indent--line-terminating-arrow-re "=>\\s-*\\(/[/*]\\|$\\)"
  "Regexp matching a `=>' token that terminates a line.
Whitespace and comments around the arrow are ignored.")

(defconst php-indent--altsyntax-re
  "\\(end\\(?:if\\|foreach\\|while\\|for\\|switch\\|declare\\)\\|else\\(?:if\\)?\\)\\_>"
  "Regexp matching alternative-syntax closing/continuation keywords.")

;;; Low-level scanning helpers (ported from js.el)

(defun php-indent--re-search-forward-inner (regexp &optional bound count)
  "Helper for `php-indent--re-search-forward'.
Search for REGEXP up to BOUND, skipping COUNT matches inside strings,
comments and heredocs."
  (let (parse str-terminator)
    (while (> count 0)
      (re-search-forward regexp bound)
      (setq parse (syntax-ppss))
      (cond ((setq str-terminator (nth 3 parse))
             (when (eq str-terminator t)
               (setq str-terminator ?/))
             (re-search-forward
              (concat "\\([^\\]\\|^\\)" (string str-terminator))
              (line-end-position) t))
            ((nth 7 parse)
             (forward-line))
            ((or (nth 4 parse)
                 (and (eq (char-before) ?/) (eq (char-after) ?*)))
             (re-search-forward "\\*/"))
            (t
             (setq count (1- count))))))
  (point))

(defun php-indent--re-search-forward (regexp &optional bound noerror count)
  "Search forward, ignoring strings and comments.
This invokes `re-search-forward' but treats the buffer as if strings and
comments have been removed.  REGEXP, BOUND, NOERROR and COUNT are as for
`re-search-forward'."
  (unless count (setq count 1))
  (let ((saved-point (point))
        (search-fun
         (cond ((< count 0) (setq count (- count))
                #'php-indent--re-search-backward-inner)
               ((> count 0) #'php-indent--re-search-forward-inner)
               (t #'ignore))))
    (condition-case err
        (funcall search-fun regexp bound count)
      (search-failed
       (goto-char saved-point)
       (unless noerror
         (signal (car err) (cdr err)))))))

(defun php-indent--re-search-backward-inner (regexp &optional bound count)
  "Helper for `php-indent--re-search-backward'.
Search backward for REGEXP up to BOUND, skipping COUNT matches inside
strings and comments."
  (let (parse)
    (while (> count 0)
      (re-search-backward regexp bound)
      (when (and (> (point) (point-min))
                 (save-excursion (backward-char) (looking-at "/[/*]")))
        (forward-char))
      (setq parse (syntax-ppss))
      (cond ((nth 8 parse)
             (goto-char (nth 8 parse)))
            ((or (nth 4 parse)
                 (and (eq (char-before) ?/) (eq (char-after) ?*)))
             (re-search-backward "/\\*"))
            (t
             (setq count (1- count))))))
  (point))

(defun php-indent--re-search-backward (regexp &optional bound noerror count)
  "Search backward, ignoring strings and comments.
REGEXP, BOUND, NOERROR and COUNT are as for `re-search-backward'."
  (php-indent--re-search-forward regexp bound noerror (if count (- count) -1)))

(defun php-indent--backward-syntactic-ws (&optional lim)
  "Move backward over whitespace and comments, not before LIM."
  (save-restriction
    (when lim (narrow-to-region lim (point-max)))
    (let ((pos (point)))
      (while (progn
               (forward-comment most-negative-fixnum)
               (/= (point) (prog1 pos (setq pos (point)))))))))

(defun php-indent--forward-syntactic-ws (&optional lim)
  "Move forward over whitespace and comments, not past LIM."
  (save-restriction
    (when lim (narrow-to-region (point-min) lim))
    (let ((pos (point)))
      (while (progn
               (forward-comment most-positive-fixnum)
               (/= (point) (prog1 pos (setq pos (point)))))))))

(defun php-indent--same-line (pos)
  "Return non-nil if POS is on the current line."
  (and (>= pos (line-beginning-position))
       (<= pos (line-end-position))))

(defun php-indent--find-newline-backward ()
  "Move backward to the nearest newline that is not in a block comment.
Return non-nil on success."
  (let ((continue t)
        (result t))
    (while continue
      (setq continue nil)
      (if (search-backward "\n" nil t)
          (let ((parse (syntax-ppss)))
            (when (nth 4 parse)
              (goto-char (nth 8 parse))
              (unless (nth 7 parse)
                (setq continue t))))
        (setq result nil)))
    result))

;;; Operator / expression predicates

(defun php-indent--looking-at-operator-p ()
  "Return non-nil if point is on a PHP operator, other than a comma."
  (save-match-data
    (and (looking-at php-indent--indent-operator-re)
         (or (not (eq (char-after) ?:))
             (save-excursion
               (php-indent--backward-syntactic-ws)
               (when (eq (char-before) ?\)) (backward-list))
               (and (php-indent--re-search-backward "[?:{]\\|\\_<case\\_>" nil t)
                    (eq (char-after) ??)))))))

(defun php-indent--continued-expression-p ()
  "Return non-nil if the current line continues an expression."
  (save-excursion
    (back-to-indentation)
    ;; Skip a leading block comment so that a chain line whose operator is
    ;; preceded by `/* ... */' is still recognized (e.g. `/* c */ ->foo()').
    (while (looking-at "/\\*")
      (forward-comment 1)
      (skip-syntax-forward " "))
    (cond
     ;; Lines starting with a chain operator continue the previous line.
     ((looking-at "->\\|\\?->\\|::") t)
     ((php-indent--looking-at-operator-p)
      (or
       (not (memq (char-after) '(?- ?+)))
       (progn
         (forward-comment (- (point)))
         (not (memq (char-before) '(?, ?\[ ?\())))))
     (t
      (and (php-indent--find-newline-backward)
           (progn
             (skip-chars-backward " \t")
             (progn
               (or (bobp) (backward-char))
               (and (> (point) (point-min))
                    (save-excursion
                      (backward-char)
                      (not (looking-at "[/*]/\\|=>")))
                    (php-indent--looking-at-operator-p)
                    (progn (backward-char)
                           (not (looking-at "\\+\\+\\|--\\|/[/*]")))))))))))

(defun php-indent--skip-term-backward ()
  "Skip a term before point.
Return non-nil if the skipped term is preceded by a `->', `?->' or `::'
chain operator, leaving point at the start of that operator.  Whitespace
and comments (including whole comment lines between chain links) are
skipped."
  (let ((term-skipped nil))
    ;; Skip backward over balanced parens.
    (let ((progress t))
      (while progress
        (setq progress nil)
        (php-indent--backward-syntactic-ws)
        (when (memq (char-before) '(?\] ?\) ?\}))
          (setq progress t term-skipped t)
          (backward-list))))
    ;; Maybe skip over a symbol.
    (let ((save-point (point)))
      (if (and (< (skip-syntax-backward "w_") 0)
               (looking-at php-indent--name-re))
          (setq term-skipped t)
        (goto-char save-point)))
    (when (and term-skipped (> (point) (point-min)))
      (php-indent--backward-syntactic-ws)
      (let ((end (point)))
        (cond
         ((and (>= (- end 3) (point-min))
               (save-excursion (goto-char (- end 3)) (looking-at "\\?->")))
          (goto-char (- end 3)) t)
         ((and (>= (- end 2) (point-min))
               (save-excursion (goto-char (- end 2)) (looking-at "->\\|::")))
          (goto-char (- end 2)) t)
         (t nil))))))

(defun php-indent--skip-terms-backward ()
  "Skip any number of chained terms backward.
Move point to the earliest chain operator without changing paren levels.
Return non-nil if at least one term was skipped."
  (when (php-indent--skip-term-backward)
    (let ((last-point (point)))
      (while (php-indent--skip-term-backward)
        (setq last-point (point)))
      (goto-char last-point)
      t)))

(defun php-indent--chained-expression-p ()
  "Return the indentation column for a method/property chain line.
Return nil when the current line is not a continued chain line, or when
the chain is rooted in a `::' static access (CC Mode indented those as
plain statement continuations rather than aligning to the operator).
Point is expected to be at `back-to-indentation'."
  (when php-indent-chain-indent
    (save-excursion
      (when (and (looking-at "->\\|\\?->\\|::")
                 (php-indent--continued-expression-p))
        (php-indent--backward-syntactic-ws)
        (when (and (php-indent--skip-terms-backward)
                   (not (looking-at "::")))
          (current-column))))))

(defun php-indent--static-chain-in-list-p (parse-status)
  "Return non-nil for a `::'-rooted chain line inside a paren/bracket group.
PARSE-STATUS is the result of `syntax-ppss'.  CC Mode did not recognize
these as statement continuations inside argument lists, so they are
indented as plain list elements without the continued-expression bonus."
  (let ((open (nth 1 parse-status)))
    (and php-indent-chain-indent
         open
         (memq (char-after open) '(?\( ?\[))
         (save-excursion
           (back-to-indentation)
           (when (looking-at "->\\|\\?->\\|::")
             (php-indent--backward-syntactic-ws)
             (and (php-indent--skip-terms-backward)
                  (looking-at "::")))))))

(defun php-indent--end-of-do-while-loop-p ()
  "Return non-nil if point is on the \"while\" of a do-while statement."
  (save-excursion
    (save-match-data
      (when (looking-at "\\s-*\\_<while\\_>")
        (if (save-excursion
              (skip-chars-backward " \t\n}")
              (looking-at "[ \t\n]*}"))
            (save-excursion
              (backward-list) (forward-symbol -1) (looking-at "\\_<do\\_>"))
          (php-indent--re-search-backward "\\_<do\\_>" (line-beginning-position) t)
          (or (looking-at "\\_<do\\_>")
              (let ((saved-indent (current-indentation)))
                (while (and (php-indent--re-search-backward "^\\s-*\\_<" nil t)
                            (/= (current-indentation) saved-indent)))
                (and (looking-at "\\s-*\\_<do\\_>")
                     (not (php-indent--re-search-forward
                           "\\_<while\\_>" (line-end-position) t))
                     (= (current-indentation) saved-indent)))))))))

(defun php-indent--ctrl-statement-indentation ()
  "Return the indentation for a braceless control-statement body, or nil."
  (save-excursion
    (back-to-indentation)
    (when (save-excursion
            (and (not (eq (line-beginning-position) (point-min)))
                 (not (looking-at "[{]"))
                 (php-indent--re-search-backward "[[:graph:]]" nil t)
                 (progn
                   (or (eobp) (forward-char))
                   (when (= (char-before) ?\)) (backward-list))
                   (skip-syntax-backward " ")
                   (skip-syntax-backward "w_")
                   (looking-at php-indent--possibly-braceless-keyword-re))
                 (memq (char-before) '(?\s ?\t ?\n ?\}))
                 (not (php-indent--end-of-do-while-loop-p))))
      (save-excursion
        (goto-char (match-beginning 0))
        (+ (current-indentation) php-indent-offset)))))

(defun php-indent--multi-line-declaration-indentation ()
  "Return the indentation for a continued `const' declaration, or nil."
  (let (forward-sexp-function ; Use Lisp version.
        at-opening-bracket)
    (save-excursion
      (back-to-indentation)
      (when (not (looking-at php-indent--declaration-keyword-re))
        (when (looking-at php-indent--indent-operator-re)
          (goto-char (match-end 0)))
        (while (and (not at-opening-bracket)
                    (not (bobp))
                    (let ((pos (point)))
                      (save-excursion
                        (php-indent--backward-syntactic-ws)
                        (or (eq (char-before) ?,)
                            (and (not (eq (char-before) ?\;))
                                 (prog2
                                     (skip-syntax-backward ".")
                                     (looking-at php-indent--indent-operator-re)
                                   (php-indent--backward-syntactic-ws))
                                 (not (eq (char-before) ?\;)))
                            (php-indent--same-line pos)))))
          (condition-case nil
              (backward-sexp)
            (scan-error (setq at-opening-bracket t))))
        (when (looking-at php-indent--declaration-keyword-re)
          (goto-char (match-end 0))
          (1+ (current-column)))))))

(defun php-indent--open-tag-before-p ()
  "Return non-nil when point is immediately after a PHP open tag."
  (looking-back "<\\?\\(?:php\\|=\\)?" (max (point-min) (- (point) 5))))

(defun php-indent--case-label-colon-p ()
  "Return non-nil when the `:' before point ends a case/default label."
  (save-excursion
    (beginning-of-line)
    (looking-at "[ \t]*\\(?:case\\_>\\|default[ \t]*:\\)")))

(defun php-indent--goto-statement-start ()
  "Move point to the first token of the statement around point.
The statement start is found by scanning balanced expressions backward
until a statement terminator (`;', `{', `}', an opening `(' or `[', a
`,', a case label `:' or a PHP open tag) is reached."
  (let (forward-sexp-function done)
    (while (not done)
      (let ((last (point)))
        (php-indent--backward-syntactic-ws)
        (if (or (bobp)
                (memq (char-before) '(?\; ?\{ ?\} ?\( ?\[ ?,))
                (php-indent--open-tag-before-p)
                (and (eq (char-before) ?:)
                     (php-indent--case-label-colon-p)))
            (progn (goto-char last) (setq done t))
          (skip-syntax-backward ".")
          (condition-case nil
              (backward-sexp)
            (scan-error (goto-char last) (setq done t))))))))

(defun php-indent--statement-continuation-indentation ()
  "Return the indentation for a continuation line of a broken statement.
A continuation line is one whose previous code token leaves the
statement unterminated: an identifier or keyword (as in stacked member
modifiers, `extends' on its own line or a broken `return'), a closing
parenthesis, a return-type `):', or a trailing `=>'.  Such lines are
indented one `php-indent-offset' beyond the first line of the
statement.  Return nil when the current line does not continue a
statement this way."
  (save-excursion
    (back-to-indentation)
    (unless (or (memq (char-after) '(?\{ ?\} ?\) ?\]))
                (looking-at "/[/*]\\|#"))
      (let ((pos (point)))
        (php-indent--backward-syntactic-ws)
        (when (and (not (bobp))
                   (not (php-indent--open-tag-before-p))
                   (or (memq (char-syntax (char-before)) '(?w ?_))
                       (eq (char-before) ?\))
                       ;; A `):' return-type (or alternative-syntax `):')
                       ;; also leaves the statement open.
                       (and (eq (char-before) ?:)
                            (eq (char-before (1- (point))) ?\)))
                       ;; So does a `=>' left dangling at the end of a
                       ;; line: an arrow function's body, an array value
                       ;; or a property hook's expression follows it.
                       ;; `=>' is excluded from
                       ;; `php-indent--indent-operator-re', so nothing
                       ;; else picks these up.
                       (and (eq (char-before) ?>)
                            (eq (char-before (1- (point))) ?=))))
          (goto-char pos)
          (php-indent--goto-statement-start)
          (unless (php-indent--same-line pos)
            (+ (current-indentation) php-indent-offset)))))))

(defun php-indent--concat-continuation-indentation ()
  "Return the alignment column for a leading-`.' concatenation line.
CC Mode aligned a continuation line beginning with the `.' string
concatenation operator to the `=' of the assignment it continues.
Return nil when the line does not start with `.' or the statement's
first line contains no plain assignment operator."
  (save-excursion
    (back-to-indentation)
    (when (looking-at "\\.[^.=0-9]")
      (php-indent--goto-statement-start)
      (let ((eol (line-end-position)) col)
        (while (and (not col) (re-search-forward "=" eol t))
          (let ((pos (match-beginning 0)))
            (when (and (not (nth 8 (save-excursion (syntax-ppss pos))))
                       (not (memq (char-before pos)
                                  '(?= ?! ?< ?> ?+ ?- ?* ?/ ?. ?% ?& ?| ?^ ??)))
                       (not (memq (char-after (1+ pos)) '(?= ?>))))
              (goto-char pos)
              (setq col (current-column)))))
        col))))

(defun php-indent--broken-arrow-terminates-line-p ()
  "Return non-nil if the current line's last real token is a `=>' arrow."
  (let ((from (point)))
    (end-of-line)
    (re-search-backward php-indent--line-terminating-arrow-re from t)))

;;; switch/case and alternative syntax

(defun php-indent--switch-block-p (open)
  "Return non-nil if OPEN is the opening brace of a `switch' block."
  (and (eq (char-after open) ?\{)
       (save-excursion
         (goto-char open)
         (skip-syntax-backward " ")
         (when (eq (char-before) ?\)) (backward-list))
         (skip-syntax-backward " ")
         (skip-syntax-backward "w_")
         (looking-at "\\_<switch\\_>"))))

(defun php-indent--switch-line-indent (open)
  "Return the indentation of the `switch' line whose brace is at OPEN."
  (save-excursion
    (goto-char open)
    (skip-syntax-backward " ")
    (when (eq (char-before) ?\)) (backward-list))
    (back-to-indentation)
    (current-column)))

(defun php-indent--altsyntax-indentation ()
  "Return the indentation for an alternative-syntax line, or nil.
Handles `endif', `endforeach', `endwhile', `endfor', `endswitch',
`enddeclare' and the colon forms of `else'/`elseif'."
  (save-excursion
    (back-to-indentation)
    (when (and (looking-at php-indent--altsyntax-re)
               (let ((kw (match-string-no-properties 1)))
                 (or (string-prefix-p "end" kw)
                     ;; else:/elseif (...): must end the line with a colon.
                     (save-excursion
                       (end-of-line)
                       (skip-syntax-backward " ")
                       (eq (char-before) ?:)))))
      (let* ((kw (match-string-no-properties 1))
             (open-re (cond
                       ((member kw '("else" "elseif" "endif")) "\\_<if\\_>")
                       ((string= kw "endforeach") "\\_<foreach\\_>")
                       ((string= kw "endwhile") "\\_<while\\_>")
                       ((string= kw "endfor") "\\_<for\\_>")
                       ((string= kw "endswitch") "\\_<switch\\_>")
                       ((string= kw "enddeclare") "\\_<declare\\_>"))))
        (when (and open-re
                   (php-indent--re-search-backward open-re nil t))
          (current-indentation))))))

;;; Core indentation

(defun php-indent--proper-indentation (parse-status)
  "Return the proper indentation for the current line.
PARSE-STATUS is the result of `syntax-ppss' at the line beginning."
  (save-excursion
    (back-to-indentation)
    (cond ((nth 4 parse-status)      ; inside comment
           (php-indent--comment-indentation parse-status))
          ((nth 3 parse-status) 0)   ; inside string/heredoc (kept by line fn)
          ;; PHP open/close tags anchor to column zero.
          ((looking-at "<\\?\\|\\?>") (prog-first-column))
          ((php-indent--altsyntax-indentation))
          ((php-indent--chained-expression-p))
          ((php-indent--ctrl-statement-indentation))
          ((php-indent--multi-line-declaration-indentation))
          ((php-indent--concat-continuation-indentation))
          ((and (not (php-indent--static-chain-in-list-p parse-status))
                (php-indent--statement-continuation-indentation)))
          ((nth 1 parse-status)
           (php-indent--bracket-indentation parse-status))
          ((php-indent--continued-expression-p)
           php-indent-offset)
          (t (prog-first-column)))))

(defun php-indent--comment-indentation (parse-status)
  "Return the indentation of a line inside a block comment.
PARSE-STATUS is the result of `syntax-ppss'.  Continuation lines of a
`/* ... */' comment are aligned one column past the star."
  (let ((start (nth 8 parse-status)))
    (save-excursion
      (goto-char start)
      (if (looking-at "/\\*")
          (1+ (current-column))
        (current-column)))))

(defun php-indent--bracket-indentation (parse-status)
  "Return the indentation for a line inside a bracketed group.
PARSE-STATUS is the result of `syntax-ppss'; point is at the line's
indentation."
  (let ((open (nth 1 parse-status))
        (same-indent-p (looking-at "[]})]"))
        (switch-label-p (looking-at "\\(?:case\\_>\\|default\\_>\\)"))
        (continued-expr-p (and (not (php-indent--static-chain-in-list-p parse-status))
                               (php-indent--continued-expression-p))))
    (if (php-indent--switch-block-p open)
        ;; PHP switch/case specific handling.
        (let ((switch-indent (php-indent--switch-line-indent open)))
          (cond
           (same-indent-p (+ switch-indent php-indent-switch-offset))
           (switch-label-p (+ switch-indent
                              php-indent-switch-case-offset
                              php-indent-switch-offset))
           (t (+ switch-indent
                 php-indent-switch-case-offset
                 php-indent-offset
                 php-indent-switch-offset))))
      ;; Generic bracket handling (ported from js.el).
      (goto-char open)
      (if (or (looking-at "[({[]\\s-*\\(/[/*]\\|$\\)")
              (save-excursion (forward-char)
                              (php-indent--broken-arrow-terminates-line-p)))
          (progn ; nothing following the opening paren/bracket
            (skip-syntax-backward " ")
            (when (eq (char-before) ?\))
              ;; The brace is preceded by a parameter/condition list; the
              ;; anchor is the start of the whole statement (which may be
              ;; on an earlier line than the `(' when the parameter list
              ;; is broken onto its own line).
              (backward-list)
              (php-indent--goto-statement-start))
            (back-to-indentation)
            (+ (current-column)
               (cond (same-indent-p 0)
                     (continued-expr-p (* 2 php-indent-offset))
                     (t (+ php-indent-offset
                           (pcase (char-after open)
                             (?\( php-indent-paren-offset)
                             (?\[ php-indent-square-offset)
                             (?\{ php-indent-curly-offset)
                             (_ 0)))))))
        ;; Something follows the opening bracket: align to it.
        (unless same-indent-p
          (forward-char)
          (skip-chars-forward " \t"))
        (current-column)))))

;;; Heredoc / HTML helpers

(defun php-indent--in-html-p ()
  "Return non-nil if the current line lies in an HTML (non-PHP) region.
A region between a `?>' and the next `<?php'/`<?='/`<?' is HTML.  When no
PHP open/close tag precedes the line, it is treated as PHP."
  (save-excursion
    (goto-char (line-beginning-position))
    (and (re-search-backward "<\\?php\\|<\\?=\\|<\\?\\|\\?>" nil t)
         (string= (match-string 0) "?>"))))

(defun php-indent--heredoc-closing-line-p ()
  "Return non-nil if the current line is a heredoc/nowdoc closing marker.
The closing marker is a bare identifier, optionally followed by `;', `,'
or `)', on its own line."
  (save-excursion
    (beginning-of-line)
    (looking-at "[ \t]*[A-Za-z_][A-Za-z0-9_]*[ \t]*[;,)]?[ \t]*$")))

;;; Entry points

;;;###autoload
(defun php-indent-line ()
  "Indent the current line as PHP.
Intended for use as `indent-line-function'.  Lines inside an HTML region
or inside a string/heredoc body are left unchanged; heredoc closing
markers are forced to column zero."
  (interactive)
  (let* ((parse-status
          (save-excursion (syntax-ppss (line-beginning-position))))
         (offset (- (point) (save-excursion (back-to-indentation) (point)))))
    (cond
     ;; Do not touch lines that belong to an HTML (?> ... <?php) region.
     ((php-indent--in-html-p) nil)
     ;; Heredoc/nowdoc closing marker: force column zero.
     ((and (nth 3 parse-status)
           (php-indent--heredoc-closing-line-p))
      (indent-line-to 0)
      (when (> offset 0) (forward-char offset)))
     ;; Inside a string or heredoc body: keep the current indentation.
     ((nth 3 parse-status) nil)
     (t
      (indent-line-to (php-indent--proper-indentation parse-status))
      (when (> offset 0) (forward-char offset))))))

;;;###autoload
(defun php-indent-region (start end)
  "Indent each PHP line between START and END."
  (interactive "r")
  (save-excursion
    (goto-char start)
    (let ((end-marker (copy-marker end)))
      (unwind-protect
          (while (< (point) end-marker)
            (unless (and (bolp) (eolp))
              (php-indent-line))
            (forward-line 1))
        (set-marker end-marker nil)))))

(provide 'php-indent)
;;; php-indent.el ends here

;;; php-mode.el --- major mode for editing PHP code

;; Copyright (C) 1999, 2000, 2001, 2003, 2004 Turadg Aleahmad
;;               2008 Aaron S. Hawley
;;               2011 Eric James Michael Ritz

;; Maintainer: Eric James Michael Ritz <Ren at lifesnotsimple dot com>
;; Original Author: Turadg Aleahmad, 1999-2004
;; Keywords: php languages oop
;; Created: 1999-05-17
;; X-URL:   https://github.com/ejmr/php-mode

(defconst php-mode-version-number "1.6.4"
  "PHP Mode version number.")

(defconst php-mode-modified "2011-09-25"
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

(require 'add-log)
(require 'speedbar)
(require 'font-lock)
(require 'cc-mode)
(require 'cc-langs)
(require 'custom)
(require 'etags)
(eval-when-compile
  (require 'regexp-opt))
(require 'flymake)

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
pla�n `php-mode'.  To get indentation to work you must use an
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

;;;###autoload
(define-derived-mode php-mode c-mode "PHP"
  "Major mode for editing PHP code.\n\n\\{php-mode-map}"
  (c-add-language 'php-mode 'c-mode)

  ;; PHP doesn't have C-style macros.
  ;; HACK: Overwrite this syntax with rules to match <?php and others.
  (set (make-local-variable 'c-opt-cpp-start) php-tags-key)
  (set (make-local-variable 'c-opt-cpp-prefix) php-tags-key)

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

  (modify-syntax-entry ?# "< b" php-mode-syntax-table)
  (modify-syntax-entry ?_ "_" php-mode-syntax-table)
  (modify-syntax-entry ?` "\"" php-mode-syntax-table)

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
  (add-hook 'php-mode-pear-hook
            (lambda ()
              (set (make-local-variable 'tab-width) 4)
              (set (make-local-variable 'c-basic-offset) 4)
              (set (make-local-variable 'indent-tabs-mode) nil)
              (c-set-offset 'block-open' - )
              (c-set-offset 'block-close' 0 )) nil t)

  (if (or php-mode-force-pear
          (and (stringp buffer-file-name)
               (string-match "PEAR\\|pear"
                             (buffer-file-name))
               (string-match "\\.php$" (buffer-file-name))))
      (run-hooks 'php-mode-pear-hook))

  (setq indent-line-function 'php-cautious-indent-line)
  (setq indent-region-function 'php-cautious-indent-region)
  (setq c-special-indent-hook nil)
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
       php-beginning-of-defun-regexp)

  (run-hooks 'php-mode-hook))

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
  (flet ((php-function-file-for (name)
                                (expand-file-name
                                 (format "function.%s.html"
                                         (replace-regexp-in-string "_" "-" name))
                                 php-manual-path)))
    (let ((doc-file (php-function-file-for (current-word))))
      (and (file-exists-p doc-file)
           (browse-url doc-file)))))

;; Define function documentation function
(defun php-search-documentation ()
  "Search PHP documentation for the word at point.  If
`php-manual-path' has a non-empty string value then the command
will first try searching the local documentation.  If the
requested documentation does not exist it will fallback to
searching the PHP website."
  (interactive)
  (flet ((php-search-web-documentation ()
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
       "__LINE__" "__FILE__"
       "__FUNCTION__" "__CLASS__" "__METHOD__"
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
       "ENT_NOQUOTES"
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
     '("and" "break" "continue" "declare" "default" "do" "echo" "else" "elseif"
       "endfor" "endforeach" "endif" "endswitch" "endwhile" "exit"
       "extends" "for" "foreach" "global" "if" "include" "include_once"
       "or" "require" "require_once" "return" "return new" "static" "switch"
       "then" "var" "while" "xor" "throw" "catch" "try"
       "instanceof" "catch all" "finally" "insteadof" "use" "as"
       "clone")))
  "PHP keywords.")

(defconst php-identifier
  (eval-when-compile
    '"[a-zA-Z\_\x7f-\xff][a-zA-Z0-9\_\x7f-\xff]*")
  "Characters in a PHP identifier.")

(defconst php-types
  (eval-when-compile
    (regexp-opt '("array" "bool" "boolean" "char" "const" "double" "float"
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
    '("\\<\\(use\\)\\s-+\\(\\(?:\\sw\\|\\\\\\)+\\)"
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

    ;; Fontify variables and function calls
    '("\\$\\(this\\|that\\)\\W" (1 font-lock-constant-face nil nil))

    ;; $_GET & co
    `(,(concat "\\$\\(" php-superglobals "\\)\\W")
      (1 font-lock-constant-face nil nil))

    ;; $variable
    '("\\$\\(\\sw+\\)" (1 font-lock-variable-name-face))

    ;; ->variable
    '("->\\(\\sw+\\)" (1 font-lock-variable-name-face t t))

    ;; ->function_call
    '("->\\(\\sw+\\)\\s-*(" . (1 php-function-call-face t t))

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


(provide 'php-mode)

;;; php-mode.el ends here

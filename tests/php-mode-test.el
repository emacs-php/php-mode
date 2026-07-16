;;; php-mode-test.el --- Tests for php-mode           -*- lexical-binding: t -*-

;; Copyright (C) 2018-2024  Friends of Emacs-PHP development
;; Copyright (C) 2013 Daniel Hackney
;;               2014, 2015 Eric James Michael Ritz

;; Author: Daniel Hackney <dan@haxney.org>
;; Maintainer: USAMI Kenta <tadsan@zonu.me>
;; URL: https://github.com/emacs-php/php-mode
;; License: GPL-3.0-or-later

;;; License

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

;; Automate tests from the "tests" directory using `ert', which comes bundled
;; with Emacs >= 24.1.

;;; Code:
(require 'php)
(require 'php-complete)
(require 'php-defs)
(require 'php-mode)
(require 'php-project)
(require 'php-ide)
(require 'ert)
(require 'cl-lib)
(require 'imenu)

(defvar php-mode-test-dir (if load-file-name
                           (file-name-directory load-file-name)
                         default-directory)
  "Directory containing the `php-mode' test files.")

(defvar php-mode-test-valid-magics '(indent)
  "List of allowed \"magic\" directives which can appear in test cases.")

(defvar php-mode-test-magic-regexp "###php-mode-test### \\((.+)\\)"
  "Regexp which identifies a magic comment.")

;; cl-letf does not work for global function on Emacs 24.3 or lower versions
(when (and (= emacs-major-version 24) (<= emacs-minor-version 3))
  (defun indent ()))

(defun php-mode-test-process-magics ()
  "Process the test directives in the current buffer.
These are the ###php-mode-test### comments. Valid magics are
listed in `php-mode-test-valid-magics'; no other directives will
be processed.

For backward compatibility the magic expressions may still refer to
`c-basic-offset'; it is bound to the current `php-indent-offset' so
that fixtures shared with the CC Mode based `php-cc-mode' keep working."
  (defvar c-basic-offset)
  (let ((c-basic-offset (if (boundp 'php-indent-offset) php-indent-offset 4)))
  (cl-letf (((symbol-function 'indent)
             (lambda (offset)
               (let ((current-offset (current-indentation)))
                 (unless (eq current-offset offset)
                   (warn "line: %d context: %S\n" (line-number-at-pos)
                         (save-excursion (back-to-indentation) (syntax-ppss)))
                   (list :line (line-number-at-pos)
                         :expected offset
                         :actual current-offset))))))
    (let (directives answers)
      (save-excursion
        (goto-char (point-min))
        (cl-loop while (re-search-forward php-mode-test-magic-regexp nil t)
                 for directives = (read (buffer-substring (match-beginning 1) (match-end 1)))
                 for result = (mapcar (lambda (expr)
                                        (let ((fn (car expr))
                                              (args (mapcar 'eval (cdr-safe expr))))
                                          (if (memq fn php-mode-test-valid-magics)
                                              (apply fn args))))
                                      directives)
                 append (cl-remove-if #'null result)))))))

(defun php-mode-test--buffer-face-list (buffer)
  "Return list of (STRING . FACE) from `BUFFER'."
  (with-temp-buffer
    (jit-lock-mode -1)
    (insert (with-current-buffer buffer (buffer-substring (point-min) (point-max))))
    (goto-char (point-min))
    (let (retval begin-pos last-face current-face str)
      (setq last-face (get-text-property (point) 'face))
      (setq begin-pos (point))
      (forward-char 1)

      (while (< (point) (point-max))
        (setq current-face (get-text-property (point) 'face))
        (unless (equal current-face last-face)
          (setq str (buffer-substring-no-properties begin-pos (point)))
          (setq retval (nconc retval (list (cons str last-face))))
          (setq begin-pos (point))
          (setq last-face current-face))
        (forward-char 1))
      (setq str (buffer-substring-no-properties begin-pos (point)))
      (nconc retval (list (cons str last-face))))))

(defun php-mode-test--parse-list-file (file-path)
  "Return list from `FILE-PATH'."
  (with-temp-buffer
    (insert-file-contents file-path)
    (let ((read-circle t))
      (read (current-buffer)))))

(cl-defmacro with-php-mode-test ((file &key style indent magic custom faces) &rest body)
  "Set up environment for testing `php-mode'.
Execute BODY in a temporary buffer containing the contents of
FILE, in `php-mode'. Optional keyword `:style' can be used to set
the coding style to one of the following:

1. `pear'
2. `drupal'
3. `wordpress'
4. `psr2'

Using any other symbol for STYLE results in undefined behavior.
The test will use the PER (\"php\") style by default.

If the `:custom' keyword is set, customized variables are not reset to
their default state prior to starting the test. Use this if the test should
run with specific customizations set.

If the `:faces' keyword is set, read the file with `.faces' added to that
file name and check that the faces of the fonts in the buffer match."
  (declare (indent 1))
  `(with-temp-buffer
     (insert-file-contents (expand-file-name ,file php-mode-test-dir))
     (setq default-directory
           (expand-file-name ".." (expand-file-name ,file php-mode-test-dir)))
     (php-mode)
     ,(if (fboundp 'font-lock-ensure)
          '(font-lock-ensure)
        '(with-no-warnings (font-lock-fontify-buffer)))

     ,(cl-case style
        (pear '(php-enable-pear-coding-style))
        (drupal '(php-enable-drupal-coding-style))
        (wordpress '(php-enable-wordpress-coding-style))
        (psr2 '(php-enable-psr2-coding-style))
        (t '(php-enable-default-coding-style)))

     ,(unless custom '(custom-set-variables '(php-indent-chain-indent nil)))

     ,(if indent
          '(let ((inhibit-message t)) (indent-region (point-min) (point-max))))
     ,(if magic
          `(should (equal
                    (cons ,file nil)
                    (cons ,file (php-mode-test-process-magics)))))
     ,(if faces
          `(should (equal
                    (cons ,file
                          (php-mode-test--parse-list-file
                           (concat (expand-file-name ,file php-mode-test-dir)
                                   (if (eq t ,faces) ".faces" ,faces))))
                    (cons ,file
                          (php-mode-test--buffer-face-list (current-buffer))))))
     (goto-char (point-min))
     (let ((case-fold-search nil))
       ,@body)))

(ert-deftest php-mode-test-namespace-block ()
  "Proper indentation for classs and functions in namespace block."
  (with-php-mode-test ("namespace-block.php" :indent t :magic t)))

(ert-deftest php-mode-test-issue-9 ()
  "Single quote in text in HTML misinterpreted.
The next character after \">We\" is a single quote. It should not
have a string face."
  :expected-result :failed
  (skip-unless (not (eq system-type darwin)))  ; TODO: Failed on macOS 28.2 or above!
  (with-php-mode-test ("issue-9.php")
    (search-forward ">We")
    (forward-char) ;; Jump to after the opening apostrophe
    (should-not (eq
                 (get-text-property (point) 'face)
                 'php-string))))

(ert-deftest php-mode-test-issue-14 ()
  "Array indentation."
  (with-php-mode-test ("issue-14.php" :indent t :magic t)))

(ert-deftest php-mode-test-issue-16 ()
  "Comma separated \"use\" (namespaces).
Gets the face of the text after the comma."
  (with-php-mode-test ("issue-16.php")
    (re-search-forward "^use " nil nil 3)
    (should (eq
             (get-text-property (search-forward ", ") 'face)
             'font-lock-type-face))))

(ert-deftest php-mode-test-issue-18 ()
  "Indentation of strings which include \"//\"."
  (with-php-mode-test ("issue-18.php" :indent t :magic t)))

(ert-deftest php-mode-test-issue-19 ()
  "Alignment of arrow operators."
  (custom-set-variables '(php-indent-chain-indent t))
  (with-php-mode-test ("issue-19.php" :indent t :custom t)
    (while (search-forward "$object->" (point-max) t)
      ;; Point is just after `->'
      (let ((col (current-column)))
        (search-forward "->")
        (should (= (current-column) col)))))

  ;; Test indentation again, but without php-indent-chain-indent enabled
  (with-php-mode-test ("issue-19.php" :indent t)
    (while (search-forward "\\($object->\\)" (point-max) t)
      (match-beginning 0)
      ;; Point is just on `$'
      (let ((col (current-column)))
        (search-forward "->")
        (should (= (current-column) (+ col php-indent-offset)))))))

(ert-deftest php-mode-test-issue-21 ()
  "Font locking multi-line string."
  (with-php-mode-test ("issue-21.php")
    (search-forward "= ")
    (while (not (looking-at ";"))
      (should (eq (get-text-property (point) 'face)
                  'php-string))
      (forward-char))))

(ert-deftest php-mode-test-issue-22 ()
  "Font lock quotes within comments as regular comments.
This applies for both single and double quotes."
  (with-php-mode-test ("issue-21.php")
    (while (search-forward "#" nil t)
      (while (not (looking-at "\n"))
        (should (eq (get-text-property (point) 'face)
                    'font-lock-comment-face))
        (forward-char)))))

(ert-deftest php-mode-test-issue-27 ()
  "Indentation in a file with a shebang."
  (with-php-mode-test ("issue-27.php" :indent t :magic t)))

(ert-deftest php-mode-test-issue-28 ()
  "Slowdown when scrolling.
No obvious way to test this. One possibility is to record time it
takes to scroll down the whole buffer using `next-line'. This may
not cause the desired fontification, and it could take different
amounts of time on different machines, so an absolute time would
not be very useful.

This doesn't test anything, for now."
  (should t))

(ert-deftest php-mode-test-issue-29 ()
  "Indentation of anonymous functions as arguments.
The closing brace and parenthesis should be at column 0."
  (with-php-mode-test ("issue-29.php" :indent t :magic t)))

(ert-deftest php-mode-test-issue-42 ()
  "Error while indenting closures.
If the bug has been fixed, indenting the buffer should not cause
an error."
  (with-php-mode-test ("issue-42.php" :indent t)))

(ert-deftest php-mode-test-issue-53 ()
  "Check if whitespace effects are undone when changing coding
style from Drupal."
  ;; Styles that do not enable `show-trailing-whitespace' (unlike drupal
  ;; and psr2), so switching to them from drupal must clear it again.
  (dolist (mode '(pear wordpress))
    ;; the file written to has no significance, only the buffer
    (let ((tmp-filename (concat (make-temp-name temporary-file-directory) ".php"))
          (auto-mode-alist '(("\\.php\\'" . php-mode))))
      (with-php-mode-test ("issue-53.php")
        (search-forward "return $this->bar;")
        (should (equal (list "before-write-file" mode nil)
                       (list "before-write-file" mode (looking-at-p "$"))))

        (php-set-style "drupal")
        (php-set-style (symbol-name mode))
        (should (equal (list "drupal" mode nil)
                       (list "drupal" mode show-trailing-whitespace)))
        (php-set-style "psr2")
        (php-set-style (symbol-name mode))
        (should (equal (list "psr2" mode nil)
                       (list "psr2" mode show-trailing-whitespace)))

        (php-set-style "drupal")
        (should (equal (list "drupal-2" mode t)
                       (list "drupal-2" mode show-trailing-whitespace)))
        (write-file tmp-filename)
        (should (equal (list "after-write-file" mode t)
                       (list "after-write-file" mode (looking-at-p "$"))))))))

(ert-deftest php-mode-test-legacy-c-basic-offset ()
  "Regression test for the CC Mode migration layer.
A buffer-local `c-basic-offset' (as still set by some projects through
directory/file local variables) must be reflected into
`php-indent-offset' by `php-style--honor-legacy-c-basic-offset', which
runs from `hack-local-variables-hook'."
  (defvar c-basic-offset)
  (with-temp-buffer
    (insert "<?php\n")
    (php-mode)
    ;; Emulate directory/file local variables applying `c-basic-offset'
    ;; after major-mode initialization, then the local-variables hook
    ;; firing (as it does for a real file visit).
    (setq-local c-basic-offset 8)
    (run-hooks 'hack-local-variables-hook)
    (should (local-variable-p 'php-indent-offset))
    (should (= php-indent-offset 8))))

(ert-deftest php-mode-test-issue-73 ()
  "The `delete-indentation' function should work properly for PHP.
 This means modifying the logic of `fixup-whitespace' so that it
 eliminates spaces before ',', ';', '->' amd '::' and after '->' and
 '::'."
  (with-php-mode-test ("issue-73.php")
    (when (search-forward "# Correct" nil t)
      (forward-line 1)
      (let ((correct-line (thing-at-point 'line)))
        (while (search-forward "# Test" nil t)
          (forward-line 1)
          (let ((current-line (line-number-at-pos)))
            (catch 'eob
              (while (not (looking-at-p "$"))
                (unless (zerop (forward-line 1))
                  (throw 'eob t))))
            (forward-line -1)
            (while (not (eq (line-number-at-pos) current-line))
              (delete-indentation))
            (beginning-of-line)
            (should (string= (thing-at-point 'line) correct-line))))))))

(ert-deftest php-mode-test-issue-83 ()
  "All static method should appear on imenu whether 'static' keyword is placed before or after visibility"
  (with-php-mode-test ("issue-83.php")
    (let* ((index-alist (imenu--make-index-alist))
           (all-methods (mapcar 'car (cdr (assoc "Methods" index-alist)))))
      (should (equal all-methods
                     (list
                      "static public function staticBeforeVisibility()"
                      "public static function staticAfterVisibility()"))))))

(ert-deftest php-mode-test-issue-99 ()
  "Proper indentation for 'foreach' statements without braces."
  (with-php-mode-test ("issue-99.php" :indent t :magic t)))

(ert-deftest php-mode-test-issue-115 ()
  "Proper alignment for chained method calls inside arrays."
  (custom-set-variables '(php-indent-chain-indent t))
  (with-php-mode-test ("issue-115.php" :indent t :magic t :custom t)))

(ert-deftest php-mode-test-issue-135 ()
  "Proper alignment multiline statements."
  (custom-set-variables '(php-indent-chain-indent t))
  (with-php-mode-test ("issue-135.php" :indent t :magic t :custom t)))

(ert-deftest php-mode-test-issue-130 ()
  "Proper alignment array elements."
  (with-php-mode-test ("issue-130.php" :indent t :magic t)))

(ert-deftest php-mode-test-issue-124 ()
  "Proper syntax propertizing when a quote appears in a heredoc."
  (with-php-mode-test ("issue-124.php" :indent t)
    (search-forward "Start of heredoc")
    ;; The heredoc should be recognized as a string (nth 3 of the
    ;; parser state is non-nil inside strings and here/nowdocs).
    (should (nth 3 (syntax-ppss)))
    (search-forward "function bar")
    ;; After the heredoc should *not* be recognized as a string.
    (should (not (nth 3 (syntax-ppss))))))

(ert-deftest php-mode-test-issue-136 ()
  "Proper highlighting for variable interpolation."
  (with-php-mode-test ("issue-136.php" :faces t)))

(ert-deftest php-mode-test-issue-144 ()
  "Indentation test '#' comment line has single quote."
  (with-php-mode-test ("issue-144.php" :indent t)
    (search-forward "$a" nil nil 3)
    (should (= (current-indentation) php-indent-offset))))

(ert-deftest php-mode-test-issue-145 ()
  "Closure indentation."
  (with-php-mode-test ("issue-145.php" :indent t)))

(ert-deftest php-mode-test-constants ()
  "Proper highlighting for constants."
  (with-php-mode-test ("constants.php" :faces t)))

(ert-deftest php-mode-test-identifiers()
  "Proper highlighting for identifiers including their namespace."
  (with-php-mode-test ("identifiers.php" :faces t)))

(ert-deftest php-mode-test-variables ()
  "Proper highlighting for variables."
  (with-php-mode-test ("variables.php" :faces t)
    (let ((variables '("regularVariable"
                       "variableVariable"
                       "staticVariable")))
      (dolist (variable variables)
        (save-excursion
          (search-forward variable)
          (goto-char (match-beginning 0))
          (should (eq 'php-variable-name
                      (get-text-property (point) 'face))))))

    (search-forward "memberVariable")
    (should (eq 'php-property-name
                (get-text-property (match-beginning 0) 'face)))

    (search-forward "funCall")
    (should-not (eq 'php-property-name
                    (get-text-property (match-beginning 0) 'face)))))

(ert-deftest php-mode-test-arrays()
  "Proper highlighting for array keyword."
  (with-php-mode-test ("arrays.php" :faces t)))

(ert-deftest php-mode-test-issue-174 ()
  "Test escaped quotes in string literals"
  (with-php-mode-test ("issue-174.php")
    (while (search-forward "quotation mark" nil t)
      (should (eq 'php-string
                  (get-text-property (- (point) 1) 'face))))))

(ert-deftest php-mode-test-issue-175 ()
  "Not highlight more than 2 digit number"
  (with-php-mode-test ("issue-175.php")
    (search-forward "10")
    (goto-char (match-beginning 0))
    (should-not (get-text-property (point) 'face))))


(ert-deftest php-mode-test-language-constructs()
  "Test highlighting of language constructs and reserved keywords"
  (with-php-mode-test ("language-constructs.php")
    (while (search-forward "ClassName" nil t)
      (backward-char)
      (let ((token (symbol-at-point)))
        (should (equal (list token 'font-lock-type-face)
                       (list token (get-text-property (point) 'face)))))))
  (with-php-mode-test ("language-constructs.php")
    (search-forward "Start:")
    (while (not (= (line-number-at-pos) (count-lines (point-min) (point-max))))
      (forward-line 1)
      (let ((token (symbol-at-point)))
        (should (equal (list token 'php-keyword)
                       (list token (get-text-property (point) 'face))))))))

(ert-deftest php-mode-test-issue-178 ()
  "Highligth as keyword and following symbol"
  (with-php-mode-test ("issue-178.php")
    (search-forward "use Test as")
    (should (eq 'php-keyword
                (get-text-property (- (point) 1) 'face)))
    (should (eq 'font-lock-type-face
                (get-text-property (+ (point) 1) 'face)))
    (search-forward "$values as")
    (should (eq 'php-keyword
                (get-text-property (- (point) 1) 'face)))
    (should (eq 'php-variable-name
                (get-text-property (+ (point) 2) 'face)))
    (search-forward "test as")
    (should (eq 'php-keyword
                (get-text-property (- (point) 1) 'face)))
    (should (eq 'php-keyword
                (get-text-property (+ (point) 1) 'face)))))

(ert-deftest php-mode-test-issue-186 ()
  "Indentation of switch case body preceeded by multiple case statements"
  (with-php-mode-test ("issue-186.php" :indent t :magic t)))

(ert-deftest php-mode-test-issue-197 ()
  "Test highlighting of member and function names (should not have type face)"
  (with-php-mode-test ("issue-197.php" :faces t)))

(ert-deftest php-mode-test-issue-200 ()
  "Test highlighting and elimination of extraneous whitespace in PSR-2 mode"
  (with-php-mode-test ("issue-200.php")
    (php-set-style "psr2")
    (should show-trailing-whitespace)
    (should (and (listp before-save-hook) (member 'delete-trailing-whitespace before-save-hook)))))

(ert-deftest php-mode-test-issue-201 ()
  "Test highlighting of special variables"
  (with-php-mode-test ("issue-201.php" :faces t)))

(ert-deftest php-mode-test-issue-211 ()
  "Test indentation of string concatination."
  (with-php-mode-test ("issue-211.php")
    (search-forward "\$str =")
    (let ((equal-indentation (1- (current-column)))) ;; because cursor is after '='
      (forward-line 1)
      (call-interactively 'indent-for-tab-command)
      (should (= (current-column) equal-indentation)))

    (search-forward "\$str_long_name =")
    (let ((equal-indentation (1- (current-column))))
      (forward-line 1)
      (call-interactively 'indent-for-tab-command)
      (should (= (current-column) equal-indentation)))

    (search-forward "\$sql =")
    (let ((equal-indentation (1- (current-column))))
      (forward-line 2)
      (call-interactively 'indent-for-tab-command)
      (should (= (current-column) equal-indentation)))))

(ert-deftest php-mode-test-issue-184()
  "Test indent-line for statements and heredoc end at beginning of lines"
  (with-php-mode-test ("issue-184.php")
    (search-forward "html;")
    (php-indent-line)
    (should (= (current-indentation) 0))
    (search-forward "return;")
    (php-indent-line)
    (should (= (current-indentation) php-indent-offset))))

(ert-deftest php-mode-test-switch-statements()
  "Test indentation inside switch statements"
  (with-php-mode-test ("switch-statements.php" :indent t :style pear)
    (search-forward "case true:")
    (should (eq (current-indentation) 0))
    (search-forward "break")
    (should (eq (current-indentation) php-indent-offset)))
  (with-php-mode-test ("switch-statements.php" :indent t :style psr2)
    (search-forward "case true:")
    (should (eq (current-indentation) php-indent-offset))
    (search-forward "break")
    (should (eq (current-indentation) (* 2 php-indent-offset)))
    (search-forward "return")
    (should (eq (current-indentation) (* 2 php-indent-offset)))))

(ert-deftest php-mode-test-issue-237 ()
  "Indent chaining method for PSR2."
  (with-php-mode-test ("issue-237.php" :indent t :style psr2 :magic t)))

(ert-deftest php-mode-test-issue-253 ()
  "Test highlight after string literal which contains many escaped quotes."
  (with-php-mode-test ("issue-253.php")
    (search-forward "$x" nil nil 3)
    (should (eq 'php-variable-name (get-text-property (1- (point)) 'face)))

    (search-forward "$this")
    (should (eq 'php-this-sigil (get-text-property (match-beginning 0) 'face)))
    (should (eq 'php-this (get-text-property (1+ (match-beginning 0)) 'face)))

    (search-forward "$x")
    (should (eq 'php-variable-sigil (get-text-property (match-beginning 0) 'face)))
    (should (eq 'php-variable-name (get-text-property (1+ (match-beginning 0)) 'face)))))

(ert-deftest php-mode-test-issue-305 ()
  "Test highlighting variables which contains 'this' or 'that'."
  (with-php-mode-test ("issue-305.php" :faces t)))

(ert-deftest php-mode-test-issue-307 ()
  "Activating php-mode should not mark the buffer as modified."
  (with-php-mode-test ("issue-307.php")
    (set-buffer-modified-p nil)
    (php-mode)
    (should-not (buffer-modified-p))))

(ert-deftest php-mode-test-issue-314 ()
  "Activating php-mode should not move point."
  (with-php-mode-test ("issue-314.php")
    (let ((orig-point (point)))
      (php-mode)
      (should (eq (point) orig-point)))))

(ert-deftest php-mode-test-issue-310 ()
  "Proper indentation after function with return type."
  (with-php-mode-test ("issue-310.php" :indent t :magic t)))

(ert-deftest php-mode-test-issue-333 ()
  "Do not freeze Emacs by font-lock regexp pattern."
  (with-php-mode-test ("issue-333.php")))

(ert-deftest php-mode-test-issue-357 ()
  "Match version-specific interpreters."
  (dolist (on '("php" "php3" "php5" "php7" "php-5" "php-5.5" "php7.0.1"))
    (with-temp-buffer
      (insert "#!" on)
      (set-auto-mode)
      (should (eq 'php-mode major-mode))))

  (dolist (off '("php2" "xphp5" "foo" "php7x"))
    (with-temp-buffer
      (insert "#!" off)
      (set-auto-mode)
      (should (not (eq 'php-mode major-mode))))))

(ert-deftest php-mode-test-issue-439 ()
  "Various heredoc/nowdoc formats are highlighted appropriately."
  (with-php-mode-test ("issue-439.php" :faces t)))

(ert-deftest php-mode-test-issue-443 ()
  "This case allows you to color things that are not authentic PHP tags
(ex.  `<?xml', `<?hh') as false positives."
  (with-php-mode-test ("issue-443.php" :faces t)))

(ert-deftest php-mode-test-type-hints ()
  "Test highlighting of type hints and return types."
  (with-php-mode-test ("type-hints.php" :faces t)))

(ert-deftest php-mode-test-static-method-calls ()
  "Test highlighting of static method calls which are named the same
as a keyword."
  (with-php-mode-test ("static-method-calls.php" :faces t)))

;; NOTE: `php-mode-debug' is now specific to the CC Mode based
;; `php-cc-mode' (it requires `cc-mode' internals such as
;; `c-offsets-alist').  Its regression test lives in
;; `tests/php-cc-mode-test.el'.

(ert-deftest php-project-root ()
  "Test for detection `php-project-root' by directory."
  (dolist (root (mapcar #'car php-project-available-root-files))
    (skip-unless (not (eq system-type windows-nt)))  ; TODO: Make test compatible to Windows!
    (with-php-mode-test ("project/1/src/functions.php")
      (let ((php-project-root root))
        (should (string= (expand-file-name "project/1/" php-mode-test-dir)
                         (expand-file-name (php-project-get-root-dir))))))))

(defun php-mode-test-in-function-p (&optional pos)
  "Determine whether POS is inside a function.
Meant for `php-mode-test-issue-503'."
  (let (bof (pos (or pos (point))))
    (save-excursion
      (when (beginning-of-defun)
        (setq bof (point))
        (end-of-defun)
        (and (> pos bof)
             (< pos (point)))))))

(ert-deftest php-mode-test-issue-503 ()
  "Function `php-beginning-of-defun' should return non-nil on success."
  (with-php-mode-test ("issue-503.php")
    (php-mode)
    (goto-char (point-max))
    (should (eq (php-mode-test-in-function-p) nil))
    (should (eq (php-mode-test-in-function-p (1- (point))) t))
    (should (eq (php-mode-test-in-function-p 1) nil))
    (should (eq (php-mode-test-in-function-p 24) t))
    (goto-char (point-min))
    (should (eq (php-mode-test-in-function-p nil) nil))))

(ert-deftest php-mode-test-indentation-issues ()
  ;; Proper alignment arglist.
  (with-php-mode-test ("indent/issue-702.php" :indent t :magic t))
  (with-php-mode-test ("indent/issue-726.php" :indent t :magic t))
  ;; Proper alignment arglist that contains empty lines.
  (with-php-mode-test ("indent/issue-793.php" :indent t :magic t)))

(ert-deftest php-mode-test-indentation-object-accessor ()
  "Alignment of chained object accessors split across lines."
  (with-php-mode-test ("indent/issue-623.php" :indent t :magic t)))

(ert-deftest php-mode-test-poly-php-html-indentation ()
  "Indentation must work inside PHP chunks of a PHP-in-HTML polymode.
Regression: `php-check-html-for-indentation' returned nil in polymode
buffers, which disabled indentation of the PHP chunks entirely.

The innermode is defined here (mirroring the `poly-php' package) rather
than depending on `poly-php', because that package pulls in a released
`php-mode' from an archive that would shadow the one under test."
  (skip-unless (require 'polymode nil t))
  ;; `php-in-poly-php-html-mode' checks the `poly-php-html-mode' variable
  ;; by name, so define the polymode under exactly that name.
  (eval '(progn
           (define-hostmode php-mode-test--poly-html-hostmode :mode 'html-mode)
           (define-innermode php-mode-test--poly-php-innermode
             :mode 'php-mode
             :head-matcher "<\\?php\\|<\\?="
             :tail-matcher "\\?>"
             :head-mode 'host :tail-mode 'host)
           (define-polymode poly-php-html-mode
             :hostmode 'php-mode-test--poly-html-hostmode
             :innermodes '(php-mode-test--poly-php-innermode)))
        t)
  (with-temp-buffer
    (insert "<div>\n<?php\nif ($x) {\necho 'hello';\n}\n?>\n</div>\n")
    (poly-php-html-mode)
    (font-lock-ensure)
    (goto-char (point-min))
    (search-forward "echo 'hello';")
    (beginning-of-line)
    (indent-according-to-mode)
    (should (= 4 (current-indentation)))))

(ert-deftest php-mode-test-derive-html-template-major-mode ()
  "A PHP file that contains HTML tags derives to `php-html-template-major-mode'."
  (skip-unless (fboundp 'web-mode))
  (let ((php-html-template-major-mode 'web-mode)
        (php-project-php-file-as-template 'auto))
    (with-temp-buffer
      (setq buffer-file-name (expand-file-name "template.php" temporary-file-directory))
      (unwind-protect
          (progn
            (insert "<div>\n<?php echo 'hi'; ?>\n</div>\n")
            (should (eq 'web-mode (php-derivation-major-mode))))
        (set-buffer-modified-p nil)
        (setq buffer-file-name nil)))))

(define-derived-mode php-mode-test--stub-ts-mode prog-mode "PHP/stub-ts"
  "Stand-in for `php-ts-mode' in `php-mode-test-mode-remap'.
Using a stub keeps the test independent of whether the PHP tree-sitter
grammar is installed.")

(ert-deftest php-mode-test-mode-remap ()
  "`php-mode-maybe' must honor `major-mode-remap-alist'.

Emacs's built-in `php-ts-mode' registers (php-mode . php-ts-mode) in
`treesit-major-mode-remap-alist' so that users can toggle between this
package and the core tree-sitter mode; `treesit-enabled-modes' copies
that entry into `major-mode-remap-alist' when they opt in.

`set-auto-mode' applies the remapping to whatever `auto-mode-alist'
names, so file patterns pointing straight at `php-mode' honored it while
those going through `php-mode-maybe' did not: the same buffer became
`php-mode' for \".php\" but the remapped mode for \".stub\"."
  (skip-unless (fboundp 'major-mode-remap))
  (dolist (probe '((((php-mode . php-mode-test--stub-ts-mode))
                    . php-mode-test--stub-ts-mode)
                   ;; Without a remapping the derived mode is used as-is.
                   (nil . php-mode)))
    (let ((major-mode-remap-alist (car probe)))
      (with-temp-buffer
        ;; `php-derivation-major-mode' consults `buffer-file-name'.
        (setq buffer-file-name (expand-file-name "remap.php" temporary-file-directory))
        (unwind-protect
            (progn
              (insert "<?php\necho 'hi';\n")
              (php-mode-maybe)
              (should (equal (cons (car probe) (cdr probe))
                             (cons (car probe) major-mode))))
          (set-buffer-modified-p nil)
          (setq buffer-file-name nil))))))

(defun php-mode-test--derive (file-name &rest body-text)
  "Return the mode `php-derivation-major-mode' picks for FILE-NAME.
BODY-TEXT is inserted into the buffer first."
  (with-temp-buffer
    (setq buffer-file-name (expand-file-name file-name temporary-file-directory))
    (unwind-protect
        (progn
          (apply #'insert body-text)
          (php-derivation-major-mode))
      (set-buffer-modified-p nil)
      (setq buffer-file-name nil))))

(ert-deftest php-mode-test-blade-template-fallback ()
  "A Blade template degrades to an HTML mode when `web-mode' is missing.

A `.blade.php' file is mostly HTML plus Blade directives, so it is not
PHP and `php-default-major-mode' cannot read it.  Before the fallback
existed, `php-derivation-major-mode' returned the unavailable
`web-mode' anyway and `php-mode-maybe' then failed with
`void-function'."
  (let ((php-blade-template-major-mode 'php-mode-test--absent-web-mode)
        (php-template-mode-alist '(("\\.blade" . php-mode-test--absent-web-mode))))
    (should-not (fboundp 'php-mode-test--absent-web-mode))
    ;; The chosen fallback must be usable: this is what `php-mode-maybe'
    ;; funcalls, so an unavailable mode would signal `void-function'.
    (let ((php-blade-template-major-mode-fallback '(html-mode)))
      (should (eq 'html-mode (php-mode-test--derive "welcome.blade.php" "@extends('x')\n"))))
    ;; Unavailable entries are skipped.
    (let ((php-blade-template-major-mode-fallback '(php-mode-test--absent-web-mode html-mode)))
      (should (eq 'html-mode (php-mode-test--derive "welcome.blade.php" "@extends('x')\n"))))
    ;; Opting out falls back on `php-default-major-mode'.
    (let ((php-blade-template-major-mode-fallback nil)
          (php-default-major-mode 'php-mode))
      (should (eq 'php-mode (php-mode-test--derive "welcome.blade.php" "@extends('x')\n"))))))

(ert-deftest php-mode-test-html-template-fallback-unchanged ()
  "A missing `php-html-template-major-mode' still derives to PHP.

Only Blade degrades to an HTML mode.  `php-project-php-file-as-template'
defaults to `auto', so any .php file holding an HTML tag reaches this
path; sending those to an HTML mode would take most PHP files away from
`php-mode' for anyone without `web-mode'."
  (let ((php-html-template-major-mode 'php-mode-test--absent-web-mode)
        (php-project-php-file-as-template 'auto)
        (php-default-major-mode 'php-mode)
        (php-template-mode-alist nil))
    (should-not (fboundp 'php-mode-test--absent-web-mode))
    (should (eq 'php-mode (php-mode-test--derive "page.php" "<div>\n<?php echo 'hi'; ?>\n</div>\n")))))

(ert-deftest php-mode-test-php74 ()
  "Test highlighting language constructs added in PHP 7.4."
  (with-php-mode-test ("7.4/arrow-function.php" :faces t))
  (with-php-mode-test ("7.4/typed-property.php" :faces t)))

(ert-deftest php-mode-test-php80 ()
  "Test highlighting language constructs added in PHP 8.0."
  (with-php-mode-test ("8.0/attribute/class.php" :faces t))
  (with-php-mode-test ("8.0/attribute/function.php" :faces t))
  (with-php-mode-test ("8.0/attribute/function2.php" :faces t)))

(ert-deftest php-mode-test-php81 ()
  "Test highlighting language constructs added in PHP 8.1."
  (with-php-mode-test ("8.1/enum.php" :faces t)))

(ert-deftest php-mode-test-php81-readonly ()
  "Test highlighting of PHP 8.1 readonly properties.
The fixture deliberately contains a syntactically invalid declaration
\(\"claas Err\"); the regexp-based font-lock leaves it unfontified."
  (with-php-mode-test ("8.1/readonly.php" :faces t)))

(ert-deftest php-mode-test-php84 ()
  "Test highlighting language constructs added in PHP 8.4."
  (with-php-mode-test ("8.4/property-hooks.php" :faces t)))

(defun php-mode-test--faces-of (code token)
  "Return the list of faces on TOKEN's characters after fontifying CODE."
  (with-temp-buffer
    (insert code)
    (php-mode)
    (font-lock-ensure)
    (goto-char (point-min))
    (should (search-forward token nil t))
    (let ((start (- (point) (length token))))
      (mapcar (lambda (i) (get-text-property (+ start i) 'face))
              (number-sequence 0 (1- (length token)))))))

(ert-deftest php-mode-test-php85-pipe-op ()
  "The PHP 8.5 pipe operator is fontified as `php-pipe-op'.

Both characters must get the face.  The comparison-operator matcher
claims a bare `>', so without a rule of its own `|>' came out
half-fontified: the `|' plain and the `>' as `php-comparison-op'."
  (should (equal '(php-pipe-op php-pipe-op)
                 (php-mode-test--faces-of "<?php\n$slug = $title |> trim(...);\n" "|>")))
  ;; Operators that the new rule must not steal from.
  (dolist (probe '(("<?php\n$a = $b || $c;\n"    "||"  (php-logical-op php-logical-op))
                   ("<?php\n$a = $b >= $c;\n"    ">="  (php-comparison-op php-comparison-op))
                   ("<?php\n$a = $b > $c;\n"     ">"   (php-comparison-op))
                   ("<?php\n$a = $b <=> $c;\n"   "<=>" (php-comparison-op php-comparison-op php-comparison-op))
                   ("<?php\n$a = $b | $c;\n"     "|"   (nil))))
    (cl-destructuring-bind (code token expected) probe
      (should (equal (cons token expected)
                     (cons token (php-mode-test--faces-of code token)))))))

(ert-deftest php-mode-test-lang ()
  "Test highlighting for language constructs."
  (with-php-mode-test ("lang/class/anonymous-class.php" :indent t :magic t :faces t))
  (with-php-mode-test ("lang/doc-comment/comments.php"
                       :faces (cond ((eq emacs-major-version 24) ".24.faces")
                                    ((version<= "27" emacs-version) ".27.faces")
                                    (t t))))
  (with-php-mode-test ("lang/doc-comment/annotation.php" :faces t))
  (with-php-mode-test ("lang/doc-comment/issue-8.php" :faces t))
  (with-php-mode-test ("lang/doc-comment/inheritdoc.php" :faces t))
  (with-php-mode-test ("lang/doc-comment/return-type.php" :faces t))
  (with-php-mode-test ("lang/function/calls.php" :faces t))
  (with-php-mode-test ("lang/function/closure.php" :indent t :magic t :faces t))
  (with-php-mode-test ("lang/import/import-constant.php" :faces t))
  (with-php-mode-test ("lang/import/import-function.php" :faces t))
  (with-php-mode-test ("lang/try-cactch/multiple.php" :faces t))
  (with-php-mode-test ("lang/types/cast.php" :faces t))
  (with-php-mode-test ("lang/types/function.php" :faces t))
  (with-php-mode-test ("lang/types/keywords.php" :faces t))
  (with-php-mode-test ("lang/errorcontrol.php" :faces t))
  (with-php-mode-test ("lang/magical-constants/echo.php" :faces t)))

(ert-deftest php-mode-test-pear ()
  "Tests for PEAR style."
  (with-php-mode-test ("indent/issue-227.php" :indent t :magic t :style pear))
  (with-php-mode-test ("indent/issue-774.php" :indent t :magic t :style pear)))

(ert-deftest php-complete-test-function-module-names-match-alist ()
  "`php-defs-function-module-names' must list every module of the alist.

The names are spelled out literally so that the autoloads copy of the
`php-complete-function-modules' `:safe' predicate can consult them
without php-defs.el being loaded; this keeps that literal honest."
  (should (equal php-defs-function-module-names
                 (mapcar #'car php-defs-functions-alist))))

(ert-deftest php-complete-test-function-modules-safe-local-variable ()
  "Only module names known to PHP Mode are safe for .dir-locals.el.

Regression test: the predicate looped over the standard Emacs variable
`values' instead of its own argument, so `cl-loop ... always' succeeded
vacuously and any list at all was accepted as safe."
  (let ((pred (get 'php-complete-function-modules 'safe-local-variable)))
    (should pred)
    (should (funcall pred '(core)))
    (should (funcall pred '(bcmath core pcntl)))
    (should (funcall pred nil))
    (should-not (funcall pred '(bogus-module)))
    (should-not (funcall pred '(core bogus-module)))
    (should-not (funcall pred '("anything" 42)))
    (should-not (funcall pred '(core . bcmath)))))

(ert-deftest php-complete-test-functions-returns-only-function-names ()
  "`php-complete--functions' must return function names and nothing else.

Regression test: it appended the whole (MODULE . FUNCTION-NAMES) entry of
`php-defs-functions-alist', so each enabled module leaked its own name
into the completion candidates as a symbol."
  (let* ((php-complete-function-modules '(bcmath))
         (php-complete--functions-cache (make-hash-table :test #'equal))
         (functions (php-complete--functions)))
    (should functions)
    (should-not (seq-remove #'stringp functions))
    (should (member "bcadd" functions))
    (should (equal functions (sort (copy-sequence (cdr (assq 'bcmath php-defs-functions-alist)))
                                   #'string<)))))

(ert-deftest php-complete-test-functions-does-not-mutate-user-option ()
  "`php-complete--functions' must leave `php-complete-function-modules' alone.

Regression test: it sorted the user option in place, so the first
completion silently reordered the value the user had set."
  (let* ((modules '(pcntl bcmath core))
         (php-complete-function-modules (copy-sequence modules))
         (php-complete--functions-cache (make-hash-table :test #'equal)))
    (php-complete--functions)
    (should (equal modules php-complete-function-modules))))

(ert-deftest php-complete-test-safe-local-variable-works-from-autoloads ()
  "The `:safe' predicate must work from the package autoloads file alone.

Emacs decides whether a .dir-locals.el value is safe while hacking local
variables, which happens before php-complete.el is loaded, so the
predicate runs as copied into php-mode-autoloads.el.  There it must not
depend on cl-lib nor on variables that only php-defs.el defines, or
`safe-local-variable-p' demotes the resulting error to nil and every
project setting this variable gets a confirmation prompt anyway."
  (let ((autoloads (expand-file-name "../lisp/php-mode-autoloads.el" php-mode-test-dir))
        (emacs (expand-file-name invocation-name invocation-directory)))
    (skip-unless (file-exists-p autoloads))
    ;; Emacs 28 was the first to copy a defcustom's `:safe' predicate into the
    ;; generated autoloads file; Emacs 27 drops it, so there is nothing to
    ;; check there.
    (skip-unless (with-temp-buffer
                   (insert-file-contents autoloads)
                   (search-forward "'php-complete-function-modules 'safe-local-variable" nil t)))
    (with-temp-buffer
      (let ((status (call-process
                     emacs nil t nil "-Q" "--batch"
                     "--load" autoloads
                     "--eval"
                     (prin1-to-string
                      '(progn
                         ;; Guard against the predicate quietly working only
                         ;; because php-complete.el got loaded after all.
                         (when (featurep 'php-complete)
                           (error "Feature php-complete must not be loaded in this check"))
                         (dolist (c '(((core) t)
                                      ((bcmath core pcntl) t)
                                      (nil t)
                                      ((bogus-module) nil)
                                      (("anything" 42) nil)))
                           (let* ((pred (get 'php-complete-function-modules
                                             'safe-local-variable))
                                  (got (and (funcall pred (nth 0 c)) t)))
                             (unless (eq got (nth 1 c))
                               (error "Value %S: got %S, want %S"
                                      (nth 0 c) got (nth 1 c)))))
                         (princ "OK"))))))
        ;; Check the output first: on failure ERT then reports what the
        ;; subprocess complained about, not just its exit status.
        (should (string-match-p "OK" (buffer-string)))
        (should (eq 0 status))))))

;;; php-ide tests

(ert-deftest php-ide-test-eglot-server-program ()
  "`php-ide-eglot-server-program' should resolve every shape of
`php-ide-eglot-executable' without touching Eglot itself."
  (let ((php-ide-eglot-executable nil))
    (should (null (php-ide-eglot-server-program))))
  (let ((php-ide-eglot-executable "psalm-language-server"))
    (should (equal '("psalm-language-server") (php-ide-eglot-server-program))))
  (let ((php-ide-eglot-executable '("php" "vendor/bin/path/to/server")))
    (should (equal '("php" "vendor/bin/path/to/server") (php-ide-eglot-server-program))))
  (let ((php-ide-eglot-executable 'intelephense))
    (should (equal '("intelephense" "--stdio") (php-ide-eglot-server-program))))
  (let ((php-ide-eglot-executable 'phpantom))
    (should (equal '("phpantom_lsp") (php-ide-eglot-server-program))))
  (let ((php-ide-eglot-executable 'php-lsp))
    (should (equal '("php-lsp") (php-ide-eglot-server-program))))
  (let ((php-ide-eglot-executable 'phpactor))
    (should (equal (list (if (fboundp 'phpactor--find-executable)
                             (phpactor--find-executable)
                           "phpactor")
                        "language-server")
                   (php-ide-eglot-server-program)))))

(defun php-mode-test--php-ide-stub-alist (log)
  "Return a `php-ide-feature-alist' of test doubles recording into LOG.
LOG is a symbol whose value is a list, appended to in call order."
  (list (list 'stub-ok :test (lambda () t)
              :activate (lambda () (push 'activated-ok (symbol-value log)))
              :deactivate (lambda () (push 'deactivated-ok (symbol-value log))))
        (list 'stub-boom :test (lambda () t)
              :activate (lambda () (error "Stub feature failed to start"))
              :deactivate (lambda () (push 'deactivated-boom (symbol-value log))))
        (list 'stub-unavailable :test (lambda () nil)
              :activate (lambda () (push 'activated-unavailable (symbol-value log)))
              :deactivate (lambda () (push 'deactivated-unavailable (symbol-value log))))))

(ert-deftest php-ide-test-failed-activation-leaves-mode-off ()
  "Regression test: a feature that cannot be activated must leave
`php-ide-mode' off.

`define-minor-mode' sets the mode variable before running the body, and
the body used to signal without undoing that, so the mode line claimed
PHP-IDE was running while nothing had been activated -- and turning it
back off then called `:deactivate' on an unavailable feature, which
signalled `void-function' and left the user stuck."
  (defvar php-mode-test--ide-log)
  (let ((php-mode-test--ide-log nil))
    (with-temp-buffer
      (php-mode)
      (let ((php-ide-feature-alist
             (php-mode-test--php-ide-stub-alist 'php-mode-test--ide-log)))
        (setq-local php-ide-features '(stub-unavailable))
        (should-error (php-ide-mode +1) :type 'user-error)
        (should-not php-ide-mode)
        (should-not php-ide--activated-features)
        ;; Nothing ran, and turning the mode off again must stay quiet.
        (should-not php-mode-test--ide-log)
        (php-ide-mode -1)
        (should-not php-ide-mode)))))

(ert-deftest php-ide-test-activation-rolls-back-on-error ()
  "A feature failing mid-list must not leave earlier ones activated."
  (defvar php-mode-test--ide-log)
  (let ((php-mode-test--ide-log nil))
    (with-temp-buffer
      (php-mode)
      (let ((php-ide-feature-alist
             (php-mode-test--php-ide-stub-alist 'php-mode-test--ide-log)))
        (setq-local php-ide-features '(stub-ok stub-boom))
        (should-error (php-ide-mode +1))
        (should-not php-ide-mode)
        (should-not php-ide--activated-features)
        ;; The one that did start must have been rolled back.
        (should (equal '(activated-ok deactivated-ok)
                       (reverse php-mode-test--ide-log)))))))

(ert-deftest php-ide-test-deactivates-what-it-activated ()
  "Regression test: deactivation must follow what was actually activated,
not the current value of `php-ide-features'.

Editing .dir-locals.el and re-applying it changes `php-ide-features' in a
live buffer; deactivation used to walk that new value, so it either
signalled on an unknown feature or turned off the wrong one, stranding
the feature that was really running."
  (defvar php-mode-test--ide-log)
  (let ((php-mode-test--ide-log nil))
    (with-temp-buffer
      (php-mode)
      (let ((php-ide-feature-alist
             (php-mode-test--php-ide-stub-alist 'php-mode-test--ide-log)))
        (setq-local php-ide-features '(stub-ok))
        (php-ide-mode +1)
        (should (equal '(stub-ok) php-ide--activated-features))
        ;; The project's configuration changes underneath the live buffer.
        (setq-local php-ide-features '(totally-unknown-feature))
        (php-ide-mode -1)
        (should-not php-ide-mode)
        (should-not php-ide--activated-features)
        (should (equal '(activated-ok deactivated-ok)
                       (reverse php-mode-test--ide-log)))))))

(ert-deftest php-ide-test-activation-is-idempotent ()
  "Re-enabling `php-ide-mode' must not activate a feature twice.

`hack-local-variables-hook' -- where the documented recipe puts
`php-ide-turn-on' -- runs again on `revert-buffer' and friends, and
`define-minor-mode' re-runs the body even when the mode is already on."
  (defvar php-mode-test--ide-log)
  (let ((php-mode-test--ide-log nil))
    (with-temp-buffer
      (php-mode)
      (let ((php-ide-feature-alist
             (php-mode-test--php-ide-stub-alist 'php-mode-test--ide-log)))
        (setq-local php-ide-features '(stub-ok))
        (php-ide-turn-on)
        (php-ide-turn-on)
        (php-ide-turn-on)
        (should (equal '(stub-ok) php-ide--activated-features))
        (should (equal '(activated-ok) (reverse php-mode-test--ide-log)))
        ;; And it must still deactivate exactly once.
        (php-ide-mode -1)
        (should (equal '(activated-ok deactivated-ok)
                       (reverse php-mode-test--ide-log)))))))

(ert-deftest php-ide-test-phpactor-feature-loads-its-own-bridge ()
  "The `phpactor' feature must load php-ide-phpactor.el itself.

php-ide.el only requires it at compile time, so without this the
`:activate'/`:deactivate' symbols resolve only when the package autoloads
happen to be loaded; loading php-ide.el directly gave `void-function'."
  (skip-unless (require 'phpactor nil t))
  (let ((plist (cdr (assq 'phpactor php-ide-feature-alist))))
    (should (funcall (plist-get plist :test)))
    (should (featurep 'php-ide-phpactor))
    (should (fboundp 'php-ide-phpactor-activate))
    (should (fboundp 'php-ide-phpactor-deactivate))))

(ert-deftest php-ide-test-feature-alist-arity ()
  "Regression test: `:test' must always be a callable 0-arg predicate, and
for every PHP-IDE feature actually available in this Emacs, `:activate'
and `:deactivate' must be callable with zero arguments too, since
`php-ide--activate-buffer'/`php-ide--deactivate-buffer' always `funcall'
them without arguments.  (Features whose backing package, e.g. lsp-mode
or Eglot, is not installed are skipped for the :activate/:deactivate
check, since their symbols are not `fboundp' until that package loads.)"
  (dolist (entry php-ide-feature-alist)
    (let* ((ide-plist (cdr entry))
           (test-fn (plist-get ide-plist :test)))
      (should (functionp test-fn))
      (should (eq 0 (car (func-arity test-fn))))
      (when (funcall test-fn)
        (dolist (key '(:activate :deactivate))
          (let ((fn (plist-get ide-plist key)))
            (should (functionp fn))
            (should (eq 0 (car (func-arity fn))))))))))

(ert-deftest php-ide-test-phpactor-activate-deactivate-not-swapped ()
  "Regression test: the `phpactor' feature's `:deactivate' must be
`php-ide-phpactor-deactivate', not (as it once was, by copy-paste)
`php-ide-phpactor-activate'."
  (let ((ide-plist (cdr (assq 'phpactor php-ide-feature-alist))))
    (should (eq #'php-ide-phpactor-deactivate (plist-get ide-plist :deactivate)))))

(ert-deftest php-ide-test-features-accepts-bare-symbol ()
  "Regression test: `php-ide-features' set to a bare symbol (as shown in
`php-ide.el's own Commentary) must not signal wrong-type-argument."
  (with-temp-buffer
    (php-mode)
    (setq-local php-ide-features 'none)
    (php-ide-mode +1)
    (should php-ide-mode)
    (php-ide-mode -1)
    (should-not php-ide-mode)))

(ert-deftest php-ide-test-turn-on-is-noop-when-unconfigured ()
  "Regression test: `php-ide-turn-on' must not signal an error (and must
not turn `php-ide-mode' on) when `php-ide-features' is unset, so it is
safe to add unconditionally to `hack-local-variables-hook'."
  (with-temp-buffer
    (php-mode)
    (should-not php-ide-features)
    (php-ide-turn-on)
    (should-not php-ide-mode)))

(ert-deftest php-ide-test-set-feature-and-status ()
  "`php-ide-set-feature' should set `php-ide-features' buffer-locally and
turn `php-ide-mode' on; `php-ide-status' should not error."
  (with-temp-buffer
    (php-mode)
    (php-ide-set-feature 'none)
    (should (equal '(none) php-ide-features))
    (should php-ide-mode)
    ;; Re-selecting the same feature while already on must not error.
    (php-ide-set-feature 'none)
    (should php-ide-mode)
    ;; `php-ide-status' just messages a summary; simply calling it must not error.
    (php-ide-status)))

(ert-deftest php-ide-test-safe-local-variables ()
  "`.dir-locals.el' safety predicates must only accept PHP-IDE's own
known feature symbols and bundled executable presets, never an
arbitrary function, path, or command list."
  (let ((features-safe (get 'php-ide-features 'safe-local-variable))
        (eglot-exe-safe (get 'php-ide-eglot-executable 'safe-local-variable)))
    (should (funcall features-safe '(none)))
    (should-not (funcall features-safe '(totally-bogus-feature)))
    (should (funcall eglot-exe-safe 'intelephense))
    (should (funcall eglot-exe-safe 'phpantom))
    (should (funcall eglot-exe-safe 'php-lsp))
    (should (funcall eglot-exe-safe 'phpactor))
    (should-not (funcall eglot-exe-safe "/bin/ls"))
    (should-not (funcall eglot-exe-safe '("curl" "https://example.com/x")))
    ;; `php-ide-mode-functions' can call arbitrary code, so it must have no
    ;; safe-local-variable predicate at all and always go through Emacs's
    ;; normal risky-variable confirmation.
    (should-not (get 'php-ide-mode-functions 'safe-local-variable))))

(ert-deftest php-ide-test-warns-only-on-exclusive-feature-clashes ()
  "Enabling two LSP clients at once should warn; other combinations should not.

Phpactor's bridge is not an LSP client, so pairing it with one is a
legitimate setup and must stay quiet."
  (let (warnings)
    (cl-letf (((symbol-function 'lwarn)
               (lambda (_class _level fmt &rest args)
                 (push (apply #'format fmt args) warnings))))
      (dolist (features '((eglot) (none) (none phpactor) (eglot phpactor)))
        (setq warnings nil)
        (php-ide--warn-about-exclusive-features features)
        (should-not warnings))
      (dolist (features '((eglot lsp-mode) (lsp-mode lsp-bridge)
                          (eglot lsp-mode lsp-bridge)))
        (setq warnings nil)
        (php-ide--warn-about-exclusive-features features)
        ;; One warning naming the clashing features, not one per feature.
        (should (= 1 (length warnings)))
        (dolist (feature features)
          (should (string-match-p (regexp-quote (symbol-name feature))
                                  (car warnings))))))))

(ert-deftest php-ide-test-eglot-deactivate-degrades-gracefully ()
  "`php-ide-eglot-deactivate' must warn, not signal, if Eglot's internal
buffer-scoped switch ever disappears."
  (skip-unless (require 'eglot nil t))
  (let (called warned)
    (cl-letf (((symbol-function 'eglot--managed-mode-off)
               (lambda () (setq called 'managed-mode-off))))
      (php-ide-eglot-deactivate)
      (should (eq 'managed-mode-off called)))
    ;; Fall back to the minor mode itself when the helper is gone.
    (setq called nil)
    (cl-letf (((symbol-function 'eglot--managed-mode-off) nil)
              ((symbol-function 'eglot--managed-mode)
               (lambda (arg) (setq called (cons 'managed-mode arg)))))
      (php-ide-eglot-deactivate)
      (should (equal '(managed-mode . -1) called)))
    ;; With neither available, warn instead of signalling `void-function'.
    (cl-letf (((symbol-function 'eglot--managed-mode-off) nil)
              ((symbol-function 'eglot--managed-mode) nil)
              ((symbol-function 'lwarn)
               (lambda (&rest _) (setq warned t))))
      (php-ide-eglot-deactivate)
      (should warned))))

(ert-deftest php-ide-test-command-holding-variables-are-risky ()
  "The alists that decide what PHP-IDE runs must be risky.

`php-ide-lsp-command-alist' holds the command lines Eglot executes, and
adding an entry to it also makes that entry pass the `:safe' predicate of
`php-ide-eglot-executable' -- so a directory-local value would choose
both the command and its own approval.  `php-ide-feature-alist' likewise
holds the functions `php-ide-mode' calls and backs the `:safe' predicate
of `php-ide-features'.  Risky variables are always confirmed and can
never be remembered as safe."
  (should (risky-local-variable-p 'php-ide-feature-alist))
  (should (risky-local-variable-p 'php-ide-lsp-command-alist))
  ;; And they must not also claim to be safe.
  (should-not (get 'php-ide-feature-alist 'safe-local-variable))
  (should-not (get 'php-ide-lsp-command-alist 'safe-local-variable)))

(ert-deftest php-ide-test-risky-local-variables-work-from-autoloads ()
  "Regression test: those variables must already be risky before
php-ide.el is loaded.

Emacs checks .dir-locals.el first, and unlike `:safe' the autoloads
generator does not copy a defcustom's `:risky' flag, so the flag has to
be stated where the check can see it."
  (let ((autoloads (expand-file-name "../lisp/php-mode-autoloads.el" php-mode-test-dir))
        (emacs (expand-file-name invocation-name invocation-directory)))
    (skip-unless (file-exists-p autoloads))
    (with-temp-buffer
      (let ((status (call-process
                     emacs nil t nil "-Q" "--batch"
                     "--load" autoloads
                     "--eval"
                     (prin1-to-string
                      '(progn
                         (when (featurep 'php-ide)
                           (error "php-ide must not be loaded in this check"))
                         (dolist (v '(php-ide-feature-alist
                                      php-ide-lsp-command-alist))
                           (unless (risky-local-variable-p v)
                             (error "%s is not risky without php-ide loaded" v)))
                         (princ "OK"))))))
        (should (string-match-p "OK" (buffer-string)))
        (should (eq 0 status))))))

(ert-deftest php-ide-test-safe-local-variables-work-from-autoloads ()
  "Regression test: the `:safe' predicates must work from the package
autoloads file alone.

Emacs decides whether a .dir-locals.el value is safe *before* php-ide.el
is loaded (the README recipe only pulls php-ide in from
`hack-local-variables-hook', which runs afterwards), so the predicates
run as copied into php-mode-autoloads.el.  There they must not depend on
cl-lib nor on variables that only php-ide.el defines, or
`safe-local-variable-p' demotes the resulting error to nil and every
project setting these variables gets a confirmation prompt anyway."
  (let ((autoloads (expand-file-name "../lisp/php-mode-autoloads.el" php-mode-test-dir))
        (emacs (expand-file-name invocation-name invocation-directory)))
    (skip-unless (file-exists-p autoloads))
    ;; Emacs 28 was the first to copy a defcustom's `:safe' predicate into the
    ;; generated autoloads file; Emacs 27 drops it, so there is nothing to
    ;; check there.
    (skip-unless (with-temp-buffer
                   (insert-file-contents autoloads)
                   (search-forward "'php-ide-features 'safe-local-variable" nil t)))
    (with-temp-buffer
      (let ((status (call-process
                     emacs nil t nil "-Q" "--batch"
                     "--load" autoloads
                     "--eval"
                     (prin1-to-string
                      '(progn
                         ;; Guard against the predicate quietly working only
                         ;; because php-ide.el got loaded after all.
                         (when (featurep 'php-ide)
                           (error "php-ide must not be loaded in this check"))
                         (dolist (c '((php-ide-features (eglot) t)
                                      (php-ide-features eglot t)
                                      (php-ide-features nil t)
                                      (php-ide-features (bogus-feature) nil)
                                      (php-ide-eglot-executable intelephense t)
                                      (php-ide-eglot-executable phpantom t)
                                      (php-ide-eglot-executable php-lsp t)
                                      (php-ide-eglot-executable phpactor t)
                                      (php-ide-eglot-executable "/bin/ls" nil)))
                           (let* ((pred (get (nth 0 c) 'safe-local-variable))
                                  (got (and (funcall pred (nth 1 c)) t)))
                             (unless (eq got (nth 2 c))
                               (error "%s with %S: got %S, want %S"
                                      (nth 0 c) (nth 1 c) got (nth 2 c)))))
                         (princ "OK"))))))
        ;; Check the output first: on failure ERT then reports what the
        ;; subprocess complained about, not just its exit status.
        (should (string-match-p "OK" (buffer-string)))
        (should (eq 0 status))))))

(ert-deftest php-ide-test-phpactor-disable-hover-at-point-p ()
  "Regression test: `php-ide-phpactor--disable-hover-at-point-p' must
suppress hover when *any* predicate matches, as its docstring says.

It used to be written with `never (not ...)', i.e. logical AND, so an
empty list disabled hover everywhere (the exact opposite of the intent)
and a list of several predicates only fired when all of them matched."
  (let ((always (lambda () t))
        (never (lambda () nil)))
    (let ((php-ide-phpactor-disable-hover-at-point-functions nil))
      (should-not (php-ide-phpactor--disable-hover-at-point-p)))
    (let ((php-ide-phpactor-disable-hover-at-point-functions (list always)))
      (should (php-ide-phpactor--disable-hover-at-point-p)))
    (let ((php-ide-phpactor-disable-hover-at-point-functions (list never)))
      (should-not (php-ide-phpactor--disable-hover-at-point-p)))
    (let ((php-ide-phpactor-disable-hover-at-point-functions (list always never)))
      (should (php-ide-phpactor--disable-hover-at-point-p)))
    (let ((php-ide-phpactor-disable-hover-at-point-functions (list never always)))
      (should (php-ide-phpactor--disable-hover-at-point-p)))
    (let ((php-ide-phpactor-disable-hover-at-point-functions (list never never)))
      (should-not (php-ide-phpactor--disable-hover-at-point-p)))))

(ert-deftest php-ide-test-phpactor-hover-timer-is-shared ()
  "Regression test: the Phpactor hover timer is shared by every buffer,
so deactivating one buffer must not stop hover in the others.

It used to be cancelled unconditionally, which silently killed hover in
every remaining PHP buffer.  A buffer killed while still active must not
strand the timer either."
  (let ((php-ide-phpactor-timer nil)
        (buffers nil))
    (unwind-protect
        (let ((a (generate-new-buffer " *php-ide-test-a*"))
              (b (generate-new-buffer " *php-ide-test-b*")))
          (setq buffers (list a b))
          (with-current-buffer a (php-ide-phpactor-activate))
          (with-current-buffer b (php-ide-phpactor-activate))
          (should php-ide-phpactor-timer)
          ;; Deactivating only A must leave the timer running for B.
          (with-current-buffer a (php-ide-phpactor-deactivate))
          (should php-ide-phpactor-timer)
          (should (buffer-local-value 'php-ide-phpactor-buffer b))
          ;; Once the last buffer goes, the timer must be cancelled.
          (with-current-buffer b (php-ide-phpactor-deactivate))
          (should-not php-ide-phpactor-timer)
          ;; A buffer killed while active must not strand the timer: the
          ;; timer function itself retires it on the next tick.
          (let ((c (generate-new-buffer " *php-ide-test-c*")))
            (push c buffers)
            (with-current-buffer c (php-ide-phpactor-activate))
            (should php-ide-phpactor-timer)
            (kill-buffer c)
            (php-ide-phpactor--hover-timer-function)
            (should-not php-ide-phpactor-timer)))
      (when php-ide-phpactor-timer
        (cancel-timer php-ide-phpactor-timer))
      (dolist (buf buffers)
        (when (buffer-live-p buf)
          (kill-buffer buf))))))

(ert-deftest php-ide-test-eglot-server-programs-registration ()
  "`php-ide-eglot-activate' should buffer-locally prepend an
`eglot-server-programs' entry only when `php-ide-eglot-executable' is
set, and must never mutate the global value."
  (skip-unless (require 'eglot nil t))
  (let ((global-before (copy-sequence eglot-server-programs)))
    (unwind-protect
        (progn
          (with-temp-buffer
            (php-mode)
            ;; Unconfigured: must not touch `eglot-server-programs' at all.
            (cl-letf (((symbol-function 'eglot-ensure) (lambda () nil)))
              (php-ide-eglot-activate))
            (should-not (local-variable-p 'eglot-server-programs)))
          (with-temp-buffer
            (php-mode)
            (setq-local php-ide-eglot-executable "psalm-language-server")
            (cl-letf (((symbol-function 'eglot-ensure) (lambda () nil)))
              (php-ide-eglot-activate))
            (should (equal '("psalm-language-server")
                           (funcall (cdr (assoc php-ide-eglot-managed-modes
                                                eglot-server-programs)))))
            ;; Re-activating must not prepend a duplicate entry.
            (let ((len (length eglot-server-programs)))
              (cl-letf (((symbol-function 'eglot-ensure) (lambda () nil)))
                (php-ide-eglot-activate))
              (should (= len (length eglot-server-programs))))))
      (should (equal global-before eglot-server-programs)))))

;; For developers: How to make .faces list file.
;;
;; 1. Press `M-x eval-buffer' in this file bufffer.
;; 2. Copy follows code snippet:
;;     (setq x (php-mode-test--buffer-face-list (current-buffer)))
;; 3. Visit target buffer of testing PHP file.
;; 4. Press `M-:' (or `M-x eval-expression') and yank killed the code snippet.
;; 5. Press `M-x ielm' and input `x' and RET key.
;; 6. Kill output list and yank list to .faces file.
;; 7. Execute `make test' in shell.

;;; php-mode-test.el ends here

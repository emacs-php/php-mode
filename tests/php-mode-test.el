;;; php-mode-test.el --- Tests for php-mode

;; Copyright (C) 2018-2019  Friends of Emacs-PHP development
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
(require 'php-mode)
(require 'php-mode-debug)
(require 'php-project)
(require 'ert)
(require 'cl-lib)
(require 'imenu)

;; Work around bug #14325
;; <http://debbugs.gnu.org/cgi/bugreport.cgi?bug=14325>.
(c-after-font-lock-init)

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
be processed."
  (cl-letf (((symbol-function 'indent)
             (lambda (offset)
               (let ((current-offset (current-indentation)))
                 (unless (eq current-offset offset)
                   (warn "line: %d context: %s\n" (line-number-at-pos) (c-guess-basic-syntax))
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
                 append (cl-remove-if #'null result))))))

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
4. `symfony2'
5. `psr2'

Using any other symbol for STYLE results in undefined behavior.
The test will use the PHP style by default.

If the `:custom' keyword is set, customized variables are not reset to
their default state prior to starting the test. Use this if the test should
run with specific customizations set.

If the `:faces' keyword is set, read the file with `.faces' added to that
file name and check that the faces of the fonts in the buffer match."
  (declare (indent 1))
  `(with-temp-buffer
     (setq php-mode-enable-backup-style-variables nil)
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
        (symfony2 '(php-enable-symfony2-coding-style))
        (psr2 '(php-enable-psr2-coding-style))
        (t '(php-enable-default-coding-style)))

     ,(unless custom '(custom-set-variables '(php-lineup-cascaded-calls nil)))

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
  (custom-set-variables '(php-lineup-cascaded-calls t))
  (with-php-mode-test ("issue-19.php" :indent t :custom t)
    (while (search-forward "$object->" (point-max) t)
      ;; Point is just after `->'
      (let ((col (current-column)))
        (search-forward "->")
        (should (= (current-column) col)))))

  ;; Test indentation again, but without php-lineup-cascaded-calls enabled
  (with-php-mode-test ("issue-19.php" :indent t)
    (while (search-forward "\\($object->\\)" (point-max) t)
      (match-beginning 0)
      ;; Point is just on `$'
      (let ((col (current-column)))
        (search-forward "->")
        (should (= (current-column) (+ col c-basic-offset)))))))

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
  (dolist (mode '(pear wordpress symfony2))
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
  (custom-set-variables '(php-lineup-cascaded-calls t))
  (with-php-mode-test ("issue-115.php" :indent t :magic t :custom t)))

(ert-deftest php-mode-test-issue-135 ()
  "Proper alignment multiline statements."
  (custom-set-variables '(php-lineup-cascaded-calls t))
  (with-php-mode-test ("issue-135.php" :indent t :magic t :custom t)))

(ert-deftest php-mode-test-issue-130 ()
  "Proper alignment array elements."
  (with-php-mode-test ("issue-130.php" :indent t :magic t)))

(ert-deftest php-mode-test-issue-124 ()
  "Proper syntax propertizing when a quote appears in a heredoc."
  (with-php-mode-test ("issue-124.php" :indent t)
    (search-forward "Start of heredoc")
    ;; The heredoc should be recognized as a string.
    (dolist (syntax (c-guess-basic-syntax))
      (should (eq (car syntax) 'string)))
    (search-forward "function bar")
    ;; After the heredoc should *not* be recognized as a string.
    (dolist (syntax (c-guess-basic-syntax))
      (should (not (eq (car syntax) 'string))))))

(ert-deftest php-mode-test-issue-136 ()
  "Proper highlighting for variable interpolation."
  (with-php-mode-test ("issue-136.php") :faces t))

(ert-deftest php-mode-test-issue-144 ()
  "Indentation test '#' comment line has single quote."
  (with-php-mode-test ("issue-144.php" :indent t)
    (search-forward "$a" nil nil 3)
    (should (= (current-indentation) c-basic-offset))))

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
  "Test indentation of string concatination"
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
    (php-cautious-indent-line)
    (should (= (current-indentation) 0))
    (search-forward "return;")
    (php-cautious-indent-line)
    (should (= (current-indentation) c-basic-offset))))

(ert-deftest php-mode-test-switch-statements()
  "Test indentation inside switch statements"
  (with-php-mode-test ("switch-statements.php" :indent t :style pear)
    (search-forward "case true:")
    (should (eq (current-indentation) 0))
    (search-forward "break")
    (should (eq (current-indentation) c-basic-offset)))
  (with-php-mode-test ("switch-statements.php" :indent t :style psr2)
    (search-forward "case true:")
    (should (eq (current-indentation) c-basic-offset))
    (search-forward "break")
    (should (eq (current-indentation) (* 2 c-basic-offset)))
    (search-forward "return")
    (should (eq (current-indentation) (* 2 c-basic-offset)))))

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
  (with-php-mode-test ("issue-443.php"
                       :faces (if (version<= "27" emacs-version) ".27.faces" t))))

(ert-deftest php-mode-test-type-hints ()
  "Test highlighting of type hints and return types."
  (with-php-mode-test ("type-hints.php" :faces (cond ((version<= "29" emacs-version) ".29.faces")
                                                     (t)))))

(ert-deftest php-mode-test-static-method-calls ()
  "Test highlighting of static method calls which are named the same
as a keyword."
  (with-php-mode-test ("static-method-calls.php" :faces t)))

(ert-deftest php-mode-debug-test ()
  "Test running php-mode-debug and php-mode-debug--buffer."
  (with-temp-buffer
    (php-mode)
    (php-mode-debug)
    (should (string= (buffer-name) "*PHP Mode DEBUG*"))
    (php-mode-debug--buffer 'top)
    (search-forward "--- PHP-MODE DEBUG BEGIN ---")
    (search-forward "--- PHP-MODE DEBUG END ---"))
  (with-current-buffer (php-mode-debug--buffer 'init)
    (should (eq 0 (- (point-max) (point-min))))))

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

(ert-deftest php-mode-test-issue-623 ()
  "Proper alignment object -> accessor."
  (with-php-mode-test ("indent/issue-623.php" :indent t :magic t)))

(ert-deftest php-mode-test-issue-702 ()
  "Proper alignment arglist."
  (with-php-mode-test ("indent/issue-702.php" :indent t :magic t))
  (with-php-mode-test ("indent/issue-726.php" :indent t :magic t)))

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
  (with-php-mode-test ("8.1/enum.php" :faces t))
  (with-php-mode-test ("8.1/readonly.php" :faces t)))

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

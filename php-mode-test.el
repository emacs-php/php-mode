;;; php-mode-test.el --- Tests for php-mode

;; Copyright (C) 2013 Daniel Hackney
;;               2014, 2015 Eric James Michael Ritz

;; Author: Daniel Hackney <dan@haxney.org>
;; URL: https://github.com/ejmr/php-mode

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

;;; Commentary:

;; Automate tests from the "tests" directory using `ert', which comes bundled
;; with Emacs >= 24.1.

;;; Code:

(require 'php-mode)
(require 'ert)
(require 'cl-lib)
(require 'imenu)

;; Work around bug #14325
;; <http://debbugs.gnu.org/cgi/bugreport.cgi?bug=14325>.
(c-after-font-lock-init)

(defvar php-mode-test-dir (expand-file-name "tests"
                                            (if load-file-name
                                                (file-name-directory load-file-name)
                                              default-directory))
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
             (lambda (offset) (equal (current-indentation) offset))))
    (let (directives answers)
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward php-mode-test-magic-regexp nil t)
          (setq directives (read (buffer-substring (match-beginning 1)
                                                   (match-end 1))))
          (setq answers
                (append (mapcar (lambda (curr)
                                  (let ((fn (car curr))
                                        (args (mapcar 'eval (cdr-safe curr))))
                                    (if (memq fn php-mode-test-valid-magics)
                                        (apply fn args))))
                                directives)
                        answers))))
      answers)))

(cl-defmacro with-php-mode-test ((file &key style indent magic custom) &rest body)
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
run with specific customizations set."
  (declare (indent 1))
  `(with-temp-buffer
     (insert-file-contents (expand-file-name ,file php-mode-test-dir))
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
          '(should (cl-reduce (lambda (l r) (and l r))
                              (php-mode-test-process-magics))))
     (goto-char (point-min))
     (let ((case-fold-search nil))
       ,@body)))

(ert-deftest php-mode-test-namespace-block ()
  "Proper indentation for classs and functions in namespace block."
  (with-php-mode-test ("namespace-block.php" :indent t :magic t)))

(ert-deftest php-mode-test-issue-8 ()
  "Annotation highlighting."
  (with-php-mode-test ("issue-8.php")
    (search-forward "@ORM")
    (should (equal
             (get-text-property (match-beginning 0) 'face)
             '(php-doc-annotation-tag font-lock-doc-face)))))

(ert-deftest php-mode-test-issue-9 ()
  "Single quote in text in HTML misinterpreted.
The next character after \">We\" is a single quote. It should not
have a string face."
  :expected-result :failed
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
  (with-php-mode-test
   ("issue-53.php")
   (search-forward "return $this->bar;")
   ;; the file written to has no significance, only the buffer
   (let ((tmp-filename (concat (make-temp-name temporary-file-directory) ".php")))
     (dolist (mode '(pear wordpress symfony2))
       (php-mode-custom-coding-style-set 'php-mode-coding-style 'drupal)
       (php-mode-custom-coding-style-set 'php-mode-coding-style mode)
       (should-not show-trailing-whitespace)
       (php-mode-custom-coding-style-set 'php-mode-coding-style 'psr2)
       (php-mode-custom-coding-style-set 'php-mode-coding-style mode)
       (should-not show-trailing-whitespace)

       (php-mode-custom-coding-style-set 'php-mode-coding-style 'drupal)
       (write-file tmp-filename)
       (should (looking-at-p "$"))))))

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
  (with-php-mode-test
   ("issue-83.php")
   (let* ((index-alist (imenu--make-index-alist))
          (public-methods (mapcar 'car (cdr (assoc "Public Methods" index-alist))))
          (all-methods (mapcar 'car (cdr (assoc "All Methods" index-alist)))))
     (should (member "staticBeforeVisibility" public-methods))
     (should (member "staticBeforeVisibility" all-methods))
     (should (member "staticAfterVisibility" public-methods))
     (should (member "staticAfterVisibility" all-methods)))))

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
  (with-php-mode-test ("issue-136.php")
    (let ((variables '("$name"
                       "${name}"
                       "{$name}"
                       "{$user->name}"
                       "{$user->getName()}"
                       "{$users[0]->name}"
                       "{$users[$index]->name}"
                       "{$users[$user->id]->name}"
                       "{$users[$user->getID()]->name}")))
      ;; All of the strings we want to test come after the call to
      ;; ob_start(), so we jump to there first.
      (search-forward "ob_start()")
      (dolist (variable variables)
        (search-forward variable)
        (goto-char (match-beginning 0))
        (should (eq 'php-variable-name
                    (get-text-property (point) 'face)))))))

(ert-deftest php-mode-test-issue-144 ()
  "Indentation test '#' comment line has single quote."
  (with-php-mode-test ("issue-144.php" :indent t)
    (search-forward "$a" nil nil 3)
    (should (= (current-indentation) c-basic-offset))))

(ert-deftest php-mode-test-issue-145 ()
  "Closure indentation."
  (with-php-mode-test ("issue-145.php" :indent t)))

(ert-deftest php-mode-test-comments ()
  "Proper highlighting for comments and doc-blocks."
  (with-php-mode-test ("comments.php")
    (search-forward "/**")
    (should (eq (get-text-property (match-beginning 0) 'face)
                'font-lock-doc-face))

    (search-forward "@copyright")
    (should (equal (get-text-property (match-beginning 0) 'face)
                   '(php-doc-annotation-tag font-lock-doc-face)))
    (search-forward-regexp "@link +\\(https://github.com/ejmr/php-mode\\)")
    (should (equal (get-text-property (match-beginning 1) 'face)
                   '(link font-lock-doc-face)))


    (search-forward-regexp "// \\(@annotation This is NOT annotation. 1\\)")
    (should (eq (get-text-property (match-beginning 0) 'face)
                'font-lock-comment-delimiter-face))
    (should (eq (get-text-property (match-beginning 1) 'face)
                'font-lock-comment-face))

    (search-forward-regexp "\\* \\(@annotation This is NOT annotation. 2\\)")
    (should (eq (get-text-property (match-beginning 0) 'face)
                'font-lock-comment-face))
    (should (eq (get-text-property (match-beginning 1) 'face)
                'font-lock-comment-face))

    ;; Comment outed doc-block
    (search-forward "/**")
    (should (eq (get-text-property (match-beginning 0) 'face)
                'font-lock-comment-face))

    (search-forward-regexp "\\* \\(@annotation This is NOT annotation. 3\\)")
    (should (equal (get-text-property (match-beginning 0) 'face)
                   'font-lock-comment-face))

    (should (equal (get-text-property (match-beginning 1) 'face)
                   'font-lock-comment-face))

    (search-forward "class CommentOuted")
    (should (eq (get-text-property (match-beginning 0) 'face)
                'font-lock-comment-face))

    ;; Class level doc-comment
    (search-forward-regexp "{@internal \\(Description\\)}")
    (should (equal (get-text-property (match-beginning 0) 'face)
                   '(php-doc-annotation-tag font-lock-doc-face)))
    (should (equal (get-text-property (match-beginning 1) 'face)
                   '(php-string php-doc-annotation-tag font-lock-doc-face)))

    (should (equal (get-text-property (1- (match-end 0)) 'face)
                   '(php-doc-annotation-tag font-lock-doc-face)))
    (should (equal (get-text-property (match-end 0) 'face)
                   'font-lock-doc-face))

    (search-forward-regexp "@property\\(-read\\)")
    (should (equal (get-text-property (match-beginning 0) 'face)
                   '(php-doc-annotation-tag font-lock-doc-face)))

    (should (equal (get-text-property (match-beginning 1) 'face)
                   '(php-doc-annotation-tag font-lock-doc-face)))

    (search-forward-regexp "@ORM\\(\\\\Table\\)")
    (should (equal (get-text-property (match-beginning 0) 'face)
                   '(php-doc-annotation-tag font-lock-doc-face)))
    (should (equal (get-text-property (match-beginning 1) 'face)
                   '(php-doc-annotation-tag font-lock-doc-face)))

    (search-forward-regexp "@var \\(string\\) \\(sample property doc-comment\\)")
    (should (equal (get-text-property (match-beginning 0) 'face)
                   '(php-doc-annotation-tag font-lock-doc-face)))
    (should (equal (get-text-property (match-beginning 1) 'face)
                   '(font-lock-type-face php-string font-lock-doc-face)))
    (should (equal (get-text-property (match-beginning 2) 'face)
                   'font-lock-doc-face))

    (search-forward-regexp "// \\(comment in after code\\)")
    (should (eq (get-text-property (match-beginning 0) 'face)
                'font-lock-comment-delimiter-face))
    (should (eq (get-text-property (match-beginning 1) 'face)
                'font-lock-comment-face))

    (search-forward-regexp "@var \\(string\\)|\\(bool\\)|\\(array\\)\\([[]]\\)|\\(ArrayObject\\) \\*/$")
    (should (equal (get-text-property (match-beginning 0) 'face) ;; matches `@'
                   '(php-doc-annotation-tag font-lock-doc-face)))
    (should (equal (get-text-property (match-beginning 1) 'face) ;; matches `s'
                   '(font-lock-type-face php-string font-lock-doc-face)))
    (should (equal (get-text-property (match-end 1) 'face)       ;; matches `|'
                   '(php-string font-lock-doc-face)))
    (should (equal (get-text-property (match-beginning 2) 'face) ;; matches `b'
                   '(font-lock-type-face php-string font-lock-doc-face)))
    (should (equal (get-text-property (match-end 2) 'face)       ;; matches `|'
                   '(php-string font-lock-doc-face)))
    (should (equal (get-text-property (match-beginning 3) 'face) ;; matches `a'
                   '(font-lock-type-face php-string font-lock-doc-face)))
    (should (equal (get-text-property (match-beginning 4) 'face) ;; matches `['
                   '(php-string font-lock-doc-face)))
    (should (equal (get-text-property (match-end 4) 'face)       ;; matches `|'
                   '(php-string font-lock-doc-face)))
    (should (equal (get-text-property (match-beginning 5) 'face) ;; matches `A'
                   '(php-string font-lock-doc-face)))
    (should (equal (get-text-property (match-end 5) 'face)       ;; matches ` '
                   ' font-lock-doc-face))

    (search-forward-regexp "// \\(one-line comment\\)")
    (should (eq (get-text-property (match-beginning 0) 'face)
                'font-lock-comment-delimiter-face))
    (should (eq (get-text-property (match-beginning 1) 'face)
                'font-lock-comment-face))

    (search-forward-regexp "// \\(@annotation This is NOT annotation. 4\\)")
    (should (eq (get-text-property (match-beginning 0) 'face)
                'font-lock-comment-delimiter-face))
    (should (eq (get-text-property (match-beginning 1) 'face)
                'font-lock-comment-face))

    (search-forward-regexp "@var \\(int\\) \\(internal linter variable\\) \\*/$")
    (should (equal (get-text-property (match-beginning 0) 'face) ;; matches `@'
                   '(php-doc-annotation-tag font-lock-doc-face)))
    (should (equal (get-text-property (match-beginning 1) 'face) ;; matches `i'
                   '(font-lock-type-face php-string font-lock-doc-face)))
    (should (equal (get-text-property (match-end 1) 'face)       ;; matches ` '
                   'font-lock-doc-face))
    (should (equal (get-text-property (match-beginning 2) 'face) ;; matches `i'
                   'font-lock-doc-face))
    (should (equal (get-text-property (match-end 2) 'face)       ;; matches ` '
                   'font-lock-doc-face))))

(ert-deftest php-mode-test-comment-return-type ()
  "Proper highlighting for type annotation in doc-block."
  (with-php-mode-test ("doc-comment-return-type.php")
    ;; Test for premitive type (int)
    (search-forward-regexp "@return \\(int\\) +\\(A integer value\\)")
    (should (equal (get-text-property (match-beginning 1) 'face) ;; matches `i'
                   '(font-lock-type-face php-string font-lock-doc-face)))
    (should (eq (get-text-property (match-beginning 2) 'face) ;; matches `A'
                'font-lock-doc-face))

    (search-forward-regexp "@return \\(\\?int\\) +\\(A nullable integer value\\)")
    (should (equal (get-text-property (match-beginning 1) 'face) ;; matches `?'
                   '(php-string font-lock-doc-face)))
    (should (equal (get-text-property (1+ (match-beginning 1)) 'face) ;; matches `i'
                   '(font-lock-type-face php-string font-lock-doc-face)))
    (should (eq (get-text-property (match-beginning 2) 'face) ;; matches `A'
                'font-lock-doc-face))

    (search-forward-regexp "@return \\(\\int\\[\\]\\) +\\(A list of integer values\\)")
    (should (equal (get-text-property (match-beginning 1) 'face) ;; matches `i'
                   '(font-lock-type-face php-string font-lock-doc-face)))
    (should (equal (get-text-property (1- (match-end 1)) 'face) ;; matches `]'
                   '(php-string font-lock-doc-face)))
    (should (eq (get-text-property (match-beginning 2) 'face) ;; matches `A'
                'font-lock-doc-face))

    ;; Test for class type (DateTime)
    (search-forward-regexp "@return \\(DateTime\\) +\\(A DateTime object value\\)")
    (should (equal (get-text-property (match-beginning 1) 'face) ;; matches `D'
                   '(php-string font-lock-doc-face)))
    (should (eq (get-text-property (match-beginning 2) 'face) ;; matches `A'
                'font-lock-doc-face))

    (search-forward-regexp "@return \\(\\?DateTime\\) +\\(A nullable DateTime object value\\)")
    (should (equal (get-text-property (match-beginning 1) 'face) ;; matches `?'
                   '(php-string font-lock-doc-face)))
    (should (equal (get-text-property (1+ (match-beginning 1)) 'face) ;; matches `D'
                   '(php-string font-lock-doc-face)))
    (should (eq (get-text-property (match-beginning 2) 'face) ;; matches `A'
                'font-lock-doc-face))

    (search-forward-regexp "@return \\(\\DateTime\\[]\\) +\\(A list of DateTime object values\\)")
    (should (equal (get-text-property (match-beginning 1) 'face) ;; matches `D'
                   '(php-string font-lock-doc-face)))
    (should (equal (get-text-property (1- (match-end 1)) 'face) ;; matches `]'
                   '(php-string font-lock-doc-face)))
    (should (eq (get-text-property (match-beginning 2) 'face) ;; matches `A'
                'font-lock-doc-face))

    ;; Test for class type (stdClass)
    (search-forward-regexp "@return \\(stdClass\\) +\\(A stdClass object value\\)")
    (should (equal (get-text-property (match-beginning 1) 'face) ;; matches `s'
                   '(php-string font-lock-doc-face)))
    (should (eq (get-text-property (match-beginning 2) 'face) ;; matches `A'
                'font-lock-doc-face))

    (search-forward-regexp "@return \\(\\?stdClass\\) +\\(A nullable stdClass object value\\)")
    (should (equal (get-text-property (match-beginning 1) 'face) ;; matches `?'
                   '(php-string font-lock-doc-face)))
    (should (equal (get-text-property (1+ (match-beginning 1)) 'face) ;; matches `s'
                   '(php-string font-lock-doc-face)))
    (should (eq (get-text-property (match-beginning 2) 'face) ;; matches `A'
                'font-lock-doc-face))

    (search-forward-regexp "@return \\(stdClass\\[]\\) +\\(A list of stdClass object values\\)")
    (should (equal (get-text-property (match-beginning 1) 'face) ;; matches `s'
                   '(php-string font-lock-doc-face)))
    (should (equal (get-text-property (1- (match-end 1)) 'face) ;; matches `]'
                   '(php-string font-lock-doc-face)))
    (should (eq (get-text-property (match-beginning 2) 'face) ;; matches `A'
                'font-lock-doc-face))

    ;; Test for class type (\App\User)
    (search-forward-regexp "@return \\(\\\\App\\\\User\\) +\\(A \\\\App\\\\User object value\\)")
    (should (equal (get-text-property (match-beginning 1) 'face) ;; matches `\'
                   '(php-string font-lock-doc-face)))
    (should (equal (get-text-property (1+ (match-beginning 1)) 'face) ;; matches `A'
                   '(php-string font-lock-doc-face)))
    (should (eq (get-text-property (match-beginning 2) 'face) ;; matches `A'
                'font-lock-doc-face))

    (search-forward-regexp "@return \\(\\?\\\\App\\\\User\\) +\\(A nullable \\\\App\\\\User object value\\)")
    (should (equal (get-text-property (match-beginning 1) 'face) ;; matches `?'
                   '(php-string font-lock-doc-face)))
    (should (equal (get-text-property (1+ (match-beginning 1)) 'face) ;; matches `\\'
                   '(php-string font-lock-doc-face)))
    (should (eq (get-text-property (match-beginning 2) 'face) ;; matches `A'
                'font-lock-doc-face))

    (search-forward-regexp "@return \\(\\\\App\\\\User\\[]\\) +\\(A list of \\\\App\\\\User object values\\)")
    (should (equal (get-text-property (match-beginning 1) 'face) ;; matches `\'
                   '(php-string font-lock-doc-face)))
    (should (equal (get-text-property (1- (match-end 1)) 'face) ;; matches `]'
                   '(php-string font-lock-doc-face)))
    (should (eq (get-text-property (match-beginning 2) 'face) ;; matches `A'
                'font-lock-doc-face))

    ;; Test for multiple types
    (search-forward-regexp "@return \\(int\\)\\(|\\)\\(string\\) +\\(Multiple types by int and string\\)")
    (should (equal (get-text-property (match-beginning 1) 'face) ;; matches `i'
                   '(font-lock-type-face php-string font-lock-doc-face)))
    (should (equal (get-text-property (match-beginning 2) 'face) ;; matches `|'
                   '(php-string font-lock-doc-face)))
    (should (equal (get-text-property (match-beginning 3) 'face) ;; matches `s'
                   '(font-lock-type-face php-string font-lock-doc-face)))
    (should (eq (get-text-property (match-beginning 4) 'face) ;; matches `M'
                'font-lock-doc-face))

    (search-forward-regexp "@return \\(int\\[]\\)\\(|\\)\\(string\\) +\\(Multiple types by list of int and string\\)")
    (should (equal (get-text-property (match-beginning 1) 'face) ;; matches `i'
                   '(font-lock-type-face php-string font-lock-doc-face)))
    (should (equal (get-text-property (1- (match-end 1)) 'face) ;; matches `]'
                   '(php-string font-lock-doc-face)))
    (should (equal (get-text-property (match-beginning 2) 'face) ;; matches `|'
                   '(php-string font-lock-doc-face)))
    (should (equal (get-text-property (match-beginning 3) 'face) ;; matches `s'
                   '(font-lock-type-face php-string font-lock-doc-face)))
    (should (eq (get-text-property (match-beginning 4) 'face) ;; matches `M'
                'font-lock-doc-face))

    (search-forward-regexp "@return \\(int\\)\\(|\\)\\(stdClass\\) +\\(Multiple types by int and stdClass\\)")
    (should (equal (get-text-property (match-beginning 1) 'face) ;; matches `i'
                   '(font-lock-type-face php-string font-lock-doc-face)))
    (should (equal (get-text-property (match-beginning 2) 'face) ;; matches `|'
                   '(php-string font-lock-doc-face)))
    (should (equal (get-text-property (match-beginning 3) 'face) ;; matches `s'
                   '(php-string font-lock-doc-face)))
    (should (eq (get-text-property (match-beginning 4) 'face) ;; matches `M'
                'font-lock-doc-face))

    (search-forward-regexp "@return \\(int\\)\\(|\\)\\(\\\\App\\\\User\\) +\\(Multiple types by int and \\\\App\\\\User\\)")
    (should (equal (get-text-property (match-beginning 1) 'face) ;; matches `i'
                   '(font-lock-type-face php-string font-lock-doc-face)))
    (should (equal (get-text-property (match-beginning 2) 'face) ;; matches `|'
                   '(php-string font-lock-doc-face)))
    (should (equal (get-text-property (match-beginning 3) 'face) ;; matches `\'
                   '(php-string font-lock-doc-face)))
    (should (eq (get-text-property (match-beginning 4) 'face) ;; matches `M'
                'font-lock-doc-face))

    (search-forward-regexp "@return \\(DateTime\\)\\(|\\)\\(int\\) +\\(Multiple types by DateTime and int\\)")
    (should (equal (get-text-property (match-beginning 1) 'face) ;; matches `D'
                   '(php-string font-lock-doc-face)))
    (should (equal (get-text-property (match-beginning 2) 'face) ;; matches `|'
                   '(php-string font-lock-doc-face)))
    (should (equal (get-text-property (match-beginning 3) 'face) ;; matches `i'
                   '(font-lock-type-face php-string font-lock-doc-face)))
    (should (eq (get-text-property (match-beginning 4) 'face) ;; matches `M'
                'font-lock-doc-face))

    (search-forward-regexp "@return \\(\\\\App\\\\User\\)\\(|\\)\\(int\\) +\\(Multiple types by \\\\App\\\\User and int\\)")
    (should (equal (get-text-property (match-beginning 1) 'face) ;; matches `\'
                   '(php-string font-lock-doc-face)))
    (should (equal (get-text-property (1+ (match-beginning 1)) 'face) ;; matches `A'
                   '(php-string font-lock-doc-face)))
    (should (equal (get-text-property (match-beginning 2) 'face) ;; matches `|'
                   '(php-string font-lock-doc-face)))
    (should (equal (get-text-property (match-beginning 3) 'face) ;; matches `i'
                   '(font-lock-type-face php-string font-lock-doc-face)))
    (should (eq (get-text-property (match-beginning 4) 'face) ;; matches `M'
                'font-lock-doc-face))

    (search-forward-regexp "@return \\(\\$this\\) +\\(this is special variable\\)")
    (should (equal (get-text-property (match-beginning 1) 'face) ;; matches `$'
                   '(php-doc-$this-sigil php-doc-variable-sigil font-lock-doc-face)))
    (should (equal (get-text-property (1+ (match-beginning 1)) 'face) ;; matches `t'
                   '(php-doc-$this php-variable-name font-lock-doc-face)))
    (should (eq (get-text-property (match-beginning 2) 'face) ;; matches `M'
                'font-lock-doc-face))

    (search-forward-regexp "@return \\(\\$that\\) +\\(that is NOT special variable\\)")
    (should (equal (get-text-property (match-beginning 1) 'face) ;; matches `$'
                   '(php-doc-variable-sigil font-lock-doc-face)))
    (should (equal (get-text-property (1+ (match-beginning 1)) 'face) ;; matches `t'
                   '(php-variable-name font-lock-doc-face)))
    (should (eq (get-text-property (match-beginning 2) 'face) ;; matches `M'
                'font-lock-doc-face))
    ))

(ert-deftest php-mode-test-constants ()
  "Proper highlighting for constants."
  (custom-set-variables '(php-extra-constants (quote ("extraconstant"))))
  (with-php-mode-test ("constants.php")
    (let ((variables '(
                       "true" "TRUE"
                       "false" "FALSE"
                       "null" "NULL"
                       "IS_CONSTANT"
                       "__IS_CONSTANT__"
                       "IS_CONSTANT99"
                       "extraconstant"
                       "ClassName"
                       "class;")))
      (dolist (variable variables)
        (search-forward variable)
        (goto-char (match-beginning 0))
        (should (eq 'php-constant
                    (get-text-property (point) 'face))))))
  (custom-set-variables '(php-extra-constants (quote ())))
  (with-php-mode-test ("constants.php")
    (let ((variables '("no_constant"
                       "no_CONSTANT"
                       "extraconstant"
                       "classIdentifier()"
                       "2FOO")))
      (dolist (variable variables)
        (search-forward variable)
        (goto-char (match-beginning 0))
        (should (not (eq 'php-constant
                     (get-text-property (point) 'face))))))))

(ert-deftest php-mode-test-identifiers()
  "Proper highlighting for identifiers including their namespace."
  (with-php-mode-test ("identifiers.php")
    (let ((variables '("UnqualifiedClassName"
                       "FullyQualifiedClassName"
                       "SpaceName")))
      (dolist (variable variables)
        (search-forward variable)
        (goto-char (match-beginning 0))
        (should (eq 'font-lock-type-face
                    (get-text-property (point) 'face)))))
    (search-forward "var")
    (goto-char (match-beginning 0))
    (should (eq 'php-variable-name
                (get-text-property (point) 'face)))
    (search-forward "syntaxerror")
    (goto-char (match-beginning 0))
    (should (not (eq 'php-variable-name
                     (get-text-property (point) 'face))))
    (search-forward "ClassName")
    (goto-char (match-beginning 0))
    (should (eq 'php-constant
                (get-text-property (point) 'face)))
    (search-forward "SpaceName")
    (goto-char (match-beginning 0))
    (should (eq 'php-constant
                (get-text-property (point) 'face)))))

(ert-deftest php-mode-test-variables()
  "Proper highlighting for variables."
  (with-php-mode-test ("variables.php")
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
  (with-php-mode-test ("arrays.php")
    ;; Keyword situations
    (let ((variables '("array();"
                       "array()")))
      (dolist (variable variables)
        (search-forward variable)
        (goto-char (match-beginning 0))
        (should (eq 'php-keyword
                    (get-text-property (point) 'face)))))
    ;; Type situations
    (let ((variables '("(array)"
                       "array $test"
                       ": array")))
      (dolist (variable variables)
        (search-forward variable)
        (search-backward "array")
        (should (eq 'font-lock-type-face
                    (get-text-property (point) 'face)))))))

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
      (should (eq 'font-lock-type-face
                  (get-text-property (point) 'face)))))
  (with-php-mode-test ("language-constructs.php")
    (search-forward "Start:")
    (while (not (= (line-number-at-pos) (count-lines (point-min) (point-max))))
      (forward-line 1)
      (should (eq 'php-keyword
                  (get-text-property (point) 'face))))))

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
  (with-php-mode-test ("issue-197.php")
    (while (search-forward "$test->" nil t)
      (should-not (eq 'font-lock-type-face
                      (get-text-property (point) 'face))))))

(ert-deftest php-mode-test-issue-200 ()
  "Test highlighting and elimination of extraneous whitespace in PSR-2 mode"
  (with-php-mode-test ("issue-200.php")
    (php-mode-custom-coding-style-set 'php-mode-coding-style 'psr2)
    (should show-trailing-whitespace)
    (should (and (listp before-save-hook) (member 'delete-trailing-whitespace before-save-hook)))))

(ert-deftest php-mode-test-issue-201 ()
  "Test highlighting of special variables"
  (with-php-mode-test ("issue-201.php")
    (search-forward "Start:")
    (search-forward "$this")
    (should (eq 'php-$this (get-text-property (- (point) 1) 'face)))
    (search-forward "$that")
    (should (eq 'php-$this (get-text-property (- (point) 1) 'face)))
    (search-forward "self")
    (should (eq 'php-keyword (get-text-property (- (point) 1) 'face)))
    (search-forward "static")
    (should (eq 'php-keyword (get-text-property (- (point) 1) 'face)))
    (search-forward "parent")
    (should (eq 'php-keyword (get-text-property (- (point) 1) 'face)))))

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

(ert-deftest php-mode-test-issue-227 ()
  "multi-line strings indents "
  (custom-set-variables '(php-lineup-cascaded-calls t))
  (with-php-mode-test ("issue-227.php" :indent t :style pear)))
(ert-deftest php-mode-test-issue-237 ()
  "Indent chaining method for PSR2."
  (with-php-mode-test ("issue-237.php" :indent t :style psr2 :magic t)))

(ert-deftest php-mode-test-issue-253 ()
  "Test highlight after string literal which contains many escaped quotes."
  (with-php-mode-test ("issue-253.php")
    (search-forward "$x" nil nil 3)
    (should (eq 'php-variable-name (get-text-property (1- (point)) 'face)))

    (search-forward "$this")
    (should (eq 'php-$this-sigil (get-text-property (match-beginning 0) 'face)))
    (should (eq 'php-$this (get-text-property (1+ (match-beginning 0)) 'face)))

    (search-forward "$x")
    (should (eq 'php-variable-sigil (get-text-property (match-beginning 0) 'face)))
    (should (eq 'php-variable-name (get-text-property (1+ (match-beginning 0)) 'face)))))

(ert-deftest psr-5-style-tag-annotation ()
  "PSR-5 style tag annotation."
  (with-php-mode-test ("annotation.php")
    (re-search-forward "@\\([^\\]+\\)\\\\\\([^\\]+\\)\\\\\\([^\r\n]+\\)")
    (cl-loop for i from 1 to 3
             do
             (progn
               (should (equal (get-text-property (match-beginning i) 'face)
                              '(php-doc-annotation-tag font-lock-doc-face)))
               (should (equal (get-text-property (1- (match-end i)) 'face)
                              '(php-doc-annotation-tag font-lock-doc-face)))))

    (search-forward "@property-read")
    (should (equal (get-text-property (match-beginning 0) 'face)
                   '(php-doc-annotation-tag font-lock-doc-face)))
    (should (equal (get-text-property (1- (match-end 0)) 'face)
                   '(php-doc-annotation-tag font-lock-doc-face)))))

(ert-deftest php-mode-test-issue-305 ()
  "Test highlighting variables which contains 'this' or 'that'."
  (with-php-mode-test ("issue-305.php")
    (search-forward "Start:")
    (search-forward "$this")
    (should-not (eq 'php-constant (get-text-property (- (point) 1) 'face)))
    (search-forward "$that")
    (should-not (eq 'php-constant (get-text-property (- (point) 1) 'face)))))

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
  (dolist (on (if (version< emacs-version "24.4")
                  '("php" "php5" "php7")
                '("php" "php3" "php5" "php7" "php-5" "php-5.5" "php7.0.1")))
    (with-temp-buffer
      (insert "#!" on)
      (set-auto-mode)
      (should (eq 'php-mode major-mode))))

  (dolist (off '("php2" "xphp5" "foo" "php8" "php7x"))
    (with-temp-buffer
      (insert "#!" off)
      (set-auto-mode)
      (should (not (eq 'php-mode major-mode))))))

(ert-deftest php-mode-test-type-hints ()
  "Test highlighting of type hints and return types."
  (with-php-mode-test ("type-hints.php")
    (search-forward "void")
    (should (eq 'font-lock-type-face (get-text-property (- (point) 1) 'face)))
    (dotimes (num 4)
      (search-forward "string")
      (should (eq 'font-lock-type-face (get-text-property (- (point) 1) 'face))))
    (dotimes (num 4)
      (search-forward "int")
      (should (eq 'font-lock-type-face (get-text-property (- (point) 1) 'face))))
    (dotimes (num 4)
      (search-forward "float")
      (should (eq 'font-lock-type-face (get-text-property (- (point) 1) 'face))))
    (dotimes (num 4)
      (search-forward "bool")
      (should (eq 'font-lock-type-face (get-text-property (- (point) 1) 'face))))
    (dotimes (num 4)
      (search-forward "array")
      (should (eq 'font-lock-type-face (get-text-property (- (point) 1) 'face))))
    (dotimes (num 4)
      (search-forward "stdClass")
      (should (eq 'font-lock-type-face (get-text-property (- (point) 1) 'face))))
    (dotimes (num 4)
      (search-forward "\\path\\to\\my\\Object")
      (should (eq 'font-lock-type-face (get-text-property (- (point) 1) 'face))))
    ;; Parameters on different lines
    (let ((variables '("string"
                       "int"
                       "bool"
                       "array"
                       "stdClass"
                       "\\path\\to\\my\\Object"
                       "void")))
      (dolist (variable variables)
        (search-forward variable)
        (should (eq 'font-lock-type-face (get-text-property (- (point) 1) 'face)))))
    ;; Return types on different lines
    (search-forward "void")
    (should (eq 'font-lock-type-face (get-text-property (- (point) 1) 'face)))
    (let ((variables '("string"
                       "int"
                       "float"
                       "bool"
                       "array"
                       "stdClass"
                       "\\path\\to\\my\\Object"
                       )))
      (dolist (variable variables)
        (dotimes (num 2)
          (search-forward variable)
          (should (eq 'font-lock-type-face (get-text-property (- (point) 1) 'face))))))))

;;; php-mode-test.el ends here

;; Local Variables:
;; firestarter: ert-run-tests-interactively
;; End:

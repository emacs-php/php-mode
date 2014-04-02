;;; php-mode-test.el --- Tests for php-mode

;; Copyright (C) 2013 Daniel Hackney
;;               2014 Eric James Michael Ritz

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
(eval-when-compile
  (require 'cl))

;; Work around bug #14325
;; <http://debbugs.gnu.org/cgi/bugreport.cgi?bug=14325>.
(c-after-font-lock-init)

(defvar php-mode-test-dir (expand-file-name "tests" (file-name-directory load-file-name))
  "Directory containing the `php-mode' test files.")

(defvar php-mode-test-valid-magics '(indent)
  "List of allowed \"magic\" directives which can appear in test cases.")

(defvar php-mode-test-magic-regexp "###php-mode-test### \\((.+)\\)"
  "Regexp which identifies a magic comment.")

(defun php-mode-test-process-magics ()
  "Process the test directives in the current buffer.
These are the ###php-mode-test### comments. Valid magics are
listed in `php-mode-test-valid-magics'; no other directives will
be processed."
  (flet ((indent (offset) (equal (current-indentation) offset)))
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

(defmacro* with-php-mode-test ((file &key style indent magic) &rest body)
  "Set up environment for testing `php-mode'.
Execute BODY in a temporary buffer containing the contents of
FILE, in `php-mode'. Optional keyword `:style' can be used to set
the coding style to one of the following:

1. `pear'
2. `drupal'
3. `wordpress'
4. `symfony2'

Using any other symbol for STYLE results in undefined behavior.
The test will use the PEAR style by default."
  (declare (indent 1))
  `(with-temp-buffer
     (insert-file-contents (expand-file-name ,file php-mode-test-dir))
     (php-mode)
     (font-lock-fontify-buffer)
     ,(case style
        (pear '(php-enable-pear-coding-style))
        (drupal '(php-enable-drupal-coding-style))
        (wordpress '(php-enable-wordpress-coding-style))
        (symfony2 '(php-enable-symfony2-coding-style))
        (t '(php-enable-pear-coding-style)))
     ,(if indent
          '(indent-region (point-min) (point-max)))
     ,(if magic
          '(should (reduce (lambda (l r) (and l r))
                           (php-mode-test-process-magics))))
     (goto-char (point-min))
     ,@body))

(ert-deftest php-mode-test-issue-8 ()
  "Annotation highlighting."
  (with-php-mode-test ("issue-8.php")
  (search-forward "@ORM")
  (should (eq
           (get-text-property (match-beginning 0) 'face)
           'php-annotations-annotation-face))))

(ert-deftest php-mode-test-issue-9 ()
  "Single quote in text in HTML misinterpreted.
The next character after \">We\" is a single quote. It should not
have a string face."
  :expected-result :failed
  (with-php-mode-test ("issue-9.php")
    (should-not (eq
                 (get-text-property (search-forward ">We") 'face)
                 'font-lock-string-face))))

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
  (with-php-mode-test ("issue-19.php" :indent t)
    (while (search-forward "$object->" (point-max) t)
      ;; Point is just after `->'
      (let ((col (current-column)))
        (search-forward "->")
        (should (= (current-column) col))))))

(ert-deftest php-mode-test-issue-21 ()
  "Font locking multi-line string."
  (with-php-mode-test ("issue-21.php")
    (search-forward "\"")
    (while (not (looking-at "\""))
      (should (eq (get-text-property (point) 'face)
                  'font-lock-string-face))
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

(ert-deftest php-mode-test-issue-99 ()
  "Proper indentation for 'foreach' statements without braces."
  (with-php-mode-test ("issue-99.php" :indent t :magic t)))

(ert-deftest php-mode-test-issue-115 ()
  "Proper alignment for chained method calls inside arrays."
  :expected-result :failed
  (with-php-mode-test ("issue-115.php" :indent t :magic t)))

(ert-deftest php-mode-test-issue-124 ()
  "Proper syntax propertizing when a quote appears in a heredoc."
  (with-php-mode-test ("issue-124.php" :indent t)
     (search-forward "Heredoc")
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
        (should (eq 'font-lock-variable-name-face
                    (get-text-property (point) 'face)))))))

(ert-deftest php-mode-test-issue-145 ()
  "Closure indentation."
  (with-php-mode-test ("issue-145.php" :indent t)))

;;; php-mode-test.el ends here

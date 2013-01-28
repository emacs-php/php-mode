;;; php-mode-test.el --- Tests for php-mode

;; Copyright (C) 2013 Daniel Hackney

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

(defvar php-mode-test-dir (expand-file-name "tests" (file-name-directory load-file-name))
  "Directory containing the `php-mode' test files.")

(defmacro* with-php-mode-test ((file &optional &key style) &rest body)
  "Set up environment for testing `php-mode'.
Execute BODY in a temporary buffer containing the contents of
FILE, in `php-mode'. Optional keyword `:style' can be used to set
the coding style to one of `pear', `drupal', or `wordpress'."
  (declare (indent 1))
  `(with-temp-buffer
     (insert-file-contents (expand-file-name ,file php-mode-test-dir))
     ,(case style
        (pear '(php-enable-pear-coding-style))
        (drupal '(php-enable-drupal-coding-style))
        (wordpress '(php-enable-wordpress-coding-style)))
     (php-mode)
     (font-lock-fontify-buffer)
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
  :expected-result :failed
  (with-php-mode-test ("issue-14.php")
    (let ((expected (concat "$post = Post::model()->find(array(\n"
                            "    'select' => 'title',\n"
                            "    'condition' => 'postID=:postID',\n"
                            "    'params' => array(':postID'=>10),\n"
                            "));")))
      (indent-region (point-min) (point-max))
      (goto-char (point-min))
      (re-search-forward "^\\$post")
      (should (string= (buffer-substring-no-properties (match-beginning 0) (point-max))
                       expected)))))

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
  (with-php-mode-test ("issue-18.php")
    (let ((expected (concat "if ($a === 'github') {\n"
                            "    header('Location: http://github.com');\n"
                            "}")))
      (indent-region (point-min) (point-max))
      (goto-char (point-min))
      (re-search-forward "^if ")
      (should (string= (buffer-substring-no-properties (match-beginning 0) (point-max))
                       expected)))))

(ert-deftest php-mode-test-issue-19 ()
  "Alignment of arrow operators."
  :expected-result :failed
  (with-php-mode-test ("issue-19.php")
    (indent-region (point-min) (point-max))
    (goto-char (point-min))
    (while (re-search-forward "^\\s-*\\$object->")
      ;; Point is just after `->'
      (let ((col (current-column)))
        (search-forward "->")
        (should (eq (current-column) col))))))

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
  (with-php-mode-test ("issue-27.php")
    (re-search-forward "^\\s-*// Tab")
    (indent-for-tab-command)
    (back-to-indentation)
    (should (eq (current-column) tab-width))))

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
  (with-php-mode-test ("issue-29.php")
    (indent-region (point-min) (point-max))
    (goto-char (point-min))
    (re-search-forward "^\\s-*});")
    (back-to-indentation)
    (should (eq (current-column) 0))))

(ert-deftest php-mode-test-issue-42 ()
  "Error while indenting closures.
If the bug has been fixed, indenting the buffer should not cause
an error."
  (with-php-mode-test ("issue-42.php")
    (indent-region (point-min) (point-max))))

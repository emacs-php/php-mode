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
  (with-php-mode-test ("issue-9.php")
    (should-not (eq
                 (get-text-property (search-forward ">We") 'face)
                 'font-lock-string-face))))

(ert-deftest php-mode-test-issue-14 ()
  "Array indentation."
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

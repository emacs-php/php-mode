;;; php-cc-mode-test.el --- Regression tests for php-cc-mode  -*- lexical-binding: t -*-

;; Copyright (C) 2018-2024  Friends of Emacs-PHP development

;; Author: USAMI Kenta <tadsan@zonu.me>
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

;; Regression tests for the frozen CC Mode based `php-cc-mode' (the old
;; `php-mode' implementation).  These mirror a representative subset of
;; the indentation and font-lock cases from `php-mode-test.el' but drive
;; `php-cc-mode' instead of the new cc-mode independent `php-mode', so we
;; keep exercising the legacy mode while both major modes coexist.
;;
;; The test fixtures under tests/ are shared with `php-mode-test.el'.
;; They were originally generated from CC Mode, so the `.faces' files
;; used here are the ones that were NOT regenerated for the new engine.

;;; Code:
(require 'php-cc-mode)
(require 'ert)
(require 'cl-lib)
(require 'imenu)

;; Work around bug #14325
;; <http://debbugs.gnu.org/cgi/bugreport.cgi?bug=14325>.
(c-after-font-lock-init)

(defvar php-cc-mode-test-dir (if load-file-name
                                 (file-name-directory load-file-name)
                               default-directory)
  "Directory containing the `php-cc-mode' test files.")

(defvar php-cc-mode-test-valid-magics '(indent)
  "List of allowed \"magic\" directives which can appear in test cases.")

(defvar php-cc-mode-test-magic-regexp "###php-mode-test### \\((.+)\\)"
  "Regexp which identifies a magic comment.")

(defun php-cc-mode-test-process-magics ()
  "Process the ###php-mode-test### directives in the current buffer."
  (cl-letf (((symbol-function 'indent)
             (lambda (offset)
               (let ((current-offset (current-indentation)))
                 (unless (eq current-offset offset)
                   (list :line (line-number-at-pos)
                         :expected offset
                         :actual current-offset))))))
    (let (directives)
      (save-excursion
        (goto-char (point-min))
        (cl-loop while (re-search-forward php-cc-mode-test-magic-regexp nil t)
                 for directives = (read (buffer-substring (match-beginning 1) (match-end 1)))
                 for result = (mapcar (lambda (expr)
                                        (let ((fn (car expr))
                                              (args (mapcar 'eval (cdr-safe expr))))
                                          (if (memq fn php-cc-mode-test-valid-magics)
                                              (apply fn args))))
                                      directives)
                 append (cl-remove-if #'null result))))))

(defun php-cc-mode-test--buffer-face-list (buffer)
  "Return list of (STRING . FACE) from BUFFER."
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

(defun php-cc-mode-test--parse-list-file (file-path)
  "Return list from FILE-PATH."
  (with-temp-buffer
    (insert-file-contents file-path)
    (let ((read-circle t))
      (read (current-buffer)))))

(cl-defmacro with-php-cc-mode-test ((file &key style indent magic faces) &rest body)
  "Set up environment for testing `php-cc-mode'.
Execute BODY in a temporary buffer containing the contents of FILE, in
`php-cc-mode'.  The keyword arguments mirror `with-php-mode-test' but
drive the CC Mode based mode and its `php-cc-set-style'."
  (declare (indent 1))
  `(with-temp-buffer
     (insert-file-contents (expand-file-name ,file php-cc-mode-test-dir))
     (setq default-directory
           (expand-file-name ".." (expand-file-name ,file php-cc-mode-test-dir)))
     (php-cc-mode)
     ,(if (fboundp 'font-lock-ensure)
          '(font-lock-ensure)
        '(with-no-warnings (font-lock-fontify-buffer)))
     ,(cl-case style
        (pear '(php-cc-set-style "pear"))
        (drupal '(php-cc-set-style "drupal"))
        (wordpress '(php-cc-set-style "wordpress"))
        (psr2 '(php-cc-set-style "psr2"))
        (t '(php-cc-set-style "php")))
     ,(if indent
          '(let ((inhibit-message t)) (indent-region (point-min) (point-max))))
     ,(if magic
          `(should (equal
                    (cons ,file nil)
                    (cons ,file (php-cc-mode-test-process-magics)))))
     ,(if faces
          `(should (equal
                    (cons ,file
                          (php-cc-mode-test--parse-list-file
                           (concat (expand-file-name ,file php-cc-mode-test-dir)
                                   (if (eq t ,faces) ".faces" ,faces))))
                    (cons ,file
                          (php-cc-mode-test--buffer-face-list (current-buffer))))))
     (goto-char (point-min))
     (let ((case-fold-search nil))
       ,@body)))

;;; Indentation regression tests

(ert-deftest php-cc-mode-test-namespace-block ()
  "Proper indentation for classes and functions in a namespace block."
  (with-php-cc-mode-test ("namespace-block.php" :indent t :magic t)))

(ert-deftest php-cc-mode-test-issue-14 ()
  "Array indentation."
  (with-php-cc-mode-test ("issue-14.php" :indent t :magic t)))

(ert-deftest php-cc-mode-test-issue-18 ()
  "Indentation of strings which include \"//\"."
  (with-php-cc-mode-test ("issue-18.php" :indent t :magic t)))

(ert-deftest php-cc-mode-test-issue-29 ()
  "Indentation of anonymous functions as arguments."
  (with-php-cc-mode-test ("issue-29.php" :indent t :magic t)))

(ert-deftest php-cc-mode-test-issue-99 ()
  "Proper indentation for `foreach' statements without braces."
  (with-php-cc-mode-test ("issue-99.php" :indent t :magic t)))

(ert-deftest php-cc-mode-test-issue-186 ()
  "Indentation of switch case body preceded by multiple case statements."
  (with-php-cc-mode-test ("issue-186.php" :indent t :magic t)))

(ert-deftest php-cc-mode-test-switch-statements ()
  "Test indentation inside switch statements."
  (with-php-cc-mode-test ("switch-statements.php" :indent t :style pear)
    (search-forward "case true:")
    (should (eq (current-indentation) 0)))
  (with-php-cc-mode-test ("switch-statements.php" :indent t :style psr2)
    (search-forward "case true:")
    (should (eq (current-indentation) c-basic-offset))))

;;; Font-lock regression tests
;;
;; These use `.faces' fixtures that were NOT regenerated for the new
;; engine, so they still describe CC Mode's output.

(ert-deftest php-cc-mode-test-constants ()
  "Proper highlighting for constants."
  (with-php-cc-mode-test ("constants.php" :faces t)))

(ert-deftest php-cc-mode-test-identifiers ()
  "Proper highlighting for identifiers including their namespace."
  (with-php-cc-mode-test ("identifiers.php" :faces t)))

(ert-deftest php-cc-mode-test-variables ()
  "Proper highlighting for variables."
  (with-php-cc-mode-test ("variables.php" :faces t)))

(ert-deftest php-cc-mode-test-arrays ()
  "Proper highlighting for the array keyword."
  (with-php-cc-mode-test ("arrays.php" :faces t)))

(ert-deftest php-cc-mode-test-static-method-calls ()
  "Highlighting of static method calls named the same as a keyword."
  (with-php-cc-mode-test ("static-method-calls.php" :faces t)))

(ert-deftest php-cc-mode-test-issue-305 ()
  "Highlighting of variables which contain `this' or `that'."
  (with-php-cc-mode-test ("issue-305.php" :faces t)))

(ert-deftest php-cc-mode-test-issue-197 ()
  "Highlighting of member and function names (should not have type face)."
  (with-php-cc-mode-test ("issue-197.php" :faces t)))

(ert-deftest php-cc-mode-test-issue-201 ()
  "Highlighting of special variables."
  (with-php-cc-mode-test ("issue-201.php" :faces t)))

;;; php-mode-debug (CC Mode specific) -----------------------------------------

(ert-deftest php-cc-mode-debug-test ()
  "Test running `php-mode-debug' and `php-mode-debug--buffer'.
`php-mode-debug' inspects CC Mode internals, so it is only valid in a
`php-cc-mode' buffer."
  (require 'php-mode-debug)
  (with-temp-buffer
    (php-cc-mode)
    (php-mode-debug)
    (should (string= (buffer-name) "*PHP Mode DEBUG*"))
    (php-mode-debug--buffer 'top)
    (search-forward "--- PHP-MODE DEBUG BEGIN ---")
    (search-forward "--- PHP-MODE DEBUG END ---"))
  (with-current-buffer (php-mode-debug--buffer 'init)
    (should (eq 0 (- (point-max) (point-min))))))

;;; php-cc-mode-test.el ends here

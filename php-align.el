;;; php-align.el --- Alignment configuration for PHP  -*- lexical-binding: t; -*-

;; Copyright (C) 2011  tetsujin (Yusuke Segawa)
;; Copyright (C) 2020  Friends of Emacs-PHP development

;; Author: tetsujin (Yusuke Segawa) <tetsujin85 (at) gmail.com>
;; Maintainer: USAMI Kenta <tadsan@zonu.me>
;; Keywords: php languages convenience align
;; Homepage: https://github.com/emacs-php/php-mode
;; Version: 1.23.0
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

;; This extension provides alignment for PHP.
;; Note that you must have Font Lock mode enabled.
;;
;; Put this file into your load-path.and the following code into your ~/.emacs
;;
;;     (add-hook 'php-mode-hook #'php-align-setup)

;;; TODO:
;; - Add test codes using el-expectations.

;;; Code:
(require 'align)
(require 'regexp-opt)
(require 'php-project)

(defvar php-align-rules-list
  `((php-comma-delimiter
     (regexp   . ",\\(\\s-*\\)[^/ \t\n]")
     (repeat   . t)
     (modes    . '(php-mode))
     (run-if   . ,(function (lambda () current-prefix-arg))))
    (php-assignment
     (regexp   . ,(concat "[^=!^&*-+<>/.| \t\n]\\(\\s-*[=!^&%*-+<>/.|]*\\)=>?"
                          "\\(\\s-*\\)\\([^= \t\n]\\|$\\)"))
     (group    . (1 2))
     (modes    . '(php-mode))
     (justify  . t)
     (tab-stop . nil))
    (php-comment
     (regexp   . "\\(\\s-*\\)\\(//.*\\|/\\*.*\\*/\\s-*\\)$")
     (modes    . (php-mode))
     (column   . comment-column)
     (valid    . ,(function
                   (lambda ()
                     (save-excursion
                       (goto-char (match-beginning 1))
                       (not (bolp)))))))
    (php-chain-logic
     (regexp   . "\\(\\s-*\\)\\(&&\\|||\\|\\<and\\>\\|\\<or\\>\\)")
     (modes    . (php-mode))
     (valid    . ,(function
                   (lambda ()
                     (save-excursion
                       (goto-char (match-end 2))
                       (looking-at "\\s-*\\(/[*/]\\|$\\)"))))))))

(defvar php-align-region-separate
  (eval-when-compile
    (concat
     ;; blank line
     "\\(?:" "^\\s-*$" "\\)"
     "\\|"
     ;; comment start or end line
     "\\(?:" "^\\s-*\\(?:/[/*]\\|\\*/\\)" "\\)"
     "\\|"
     ;; end of line are '[', '(', '{', '}', '/*'
     "\\(?:" "\\(?:[[({}]\\|/\\*+\\)\\s-*$" "\\)"
     "\\|"
     ;; beginning of line are ')', '}', ']' and trailing character are ',', ';'
     "\\(?:" "^\\s-*[)}]][ \t,;]?\\s-*$" "\\)"
     "\\|"
     ;;  beginning of line are some PHP keywrods
     "\\(?:"
     "^\\s-*"
     (regexp-opt
      '("for" "foreach" "while" "if" "else" "switch" "case" "break" "continue"
        "try" "catch" "declare" "do" "return" "namespace" "use"))
     "[ ;]"
     "\\)"
     "\\|"
     ;; function or method call
     "\\(?:" "^\\s-*" "\\(?:" "\\w\\|[->\\: \t]" "\\)+" "(" "\\)"))
  "Regexp of a section of PHP for alignment.")

;;;###autoload
(defun php-align-setup ()
  "Setup alignment configuration for PHP code."
  (when php-project-align-lines
    (php-align-mode 1)))

(defvar php-align-mode-lighter " PHP-Align")

;;;###autoload
(define-minor-mode php-align-mode
  "Alignment lines for PHP script."
  :lighter php-align-mode-lighter
  (add-to-list 'align-open-comment-modes 'php-mode)
  (add-to-list 'align-dq-string-modes 'php-mode)
  (add-to-list 'align-sq-string-modes 'php-mode)

  (if php-align-mode
      (progn
        (setq-local align-mode-rules-list php-align-rules-list)
        (setq-local align-region-separate php-align-region-separate))
    (setq-local align-mode-rules-list nil)
    (setq-local align-region-separate nil)))

(provide 'php-align)
;;; php-align.el ends here

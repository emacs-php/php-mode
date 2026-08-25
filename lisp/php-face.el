;;; php-face.el --- Face definitions for PHP script  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Friends of Emacs-PHP development

;; Author: USAMI Kenta <tadsan@zonu.me>
;; Created: 5 May 2019
;; Version: 1.28.0
;; Keywords: faces, php
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

;; Face definitions for PHP script.

;;; Code:

;;;###autoload
(defgroup php-faces nil
  "Faces used in PHP Mode."
  :tag "PHP Faces"
  :group 'php-mode
  :group 'faces)

(defface php-string '((t (:inherit font-lock-string-face)))
  "PHP Mode face used to highlight string literals."
  :tag "PHP String")

(defface php-keyword '((t (:inherit font-lock-keyword-face)))
  "PHP Mode face used to highlight keywords."
  :tag "PHP Keyword")

(defface php-builtin '((t (:inherit font-lock-builtin-face)))
  "PHP Mode face used to highlight builtins."
  :tag "PHP Built-in")

(defface php-function-name '((t (:inherit font-lock-function-name-face)))
  "PHP Mode face used to highlight function names."
  :tag "PHP Function Name")

(defface php-function-call-standard `((t ,(when (eval-when-compile (get 'font-lock-function-call-face 'face-defface-spec))
                                            '(:inherit font-lock-function-call-face))))
  "PHP Mode face used to highlight function names in calles."
  :tag "PHP Function Call Standard")

(defface php-function-call-traditional '((t ()))
  "PHP Mode face used to highlight function names in calles."
  :tag "PHP Function Call Traditional")

(define-obsolete-face-alias 'php-function-call 'php-function-call-traditional "1.26.0")

(defface php-method-call-standard '((t (:inherit php-function-call-standard)))
  "PHP Mode face used to highlight method names in calles."
  :tag "PHP Method Call Standard")

(defface php-method-call-traditional '((t (:inherit php-function-call-traditional)))
  "PHP Mode face used to highlight method names in calles."
  :tag "PHP Method Call Traditional")

(define-obsolete-face-alias 'php-method-call 'php-method-call-traditional "1.26.0")

(defface php-static-method-call-standard '((t (:inherit php-method-call-standard)))
  "PHP Mode face used to highlight static method names in calles."
  :tag "PHP Static Method Call Standard")

(defface php-static-method-call-traditional '((t (:inherit php-method-call-traditional)))
  "PHP Mode face used to highlight static method names in calles."
  :tag "PHP Static Method Call Traditional")

(define-obsolete-face-alias 'php-static-method-call 'php-static-method-call-traditional "1.26.0")

(defface php-variable-name '((t (:inherit font-lock-variable-name-face)))
  "PHP Mode face used to highlight variable names."
  :tag "PHP Variable Name")

(defface php-property-name '((t (:inherit php-variable-name)))
  "PHP Mode face used to highlight property names."
  :tag "PHP Property Name")

(defface php-variable-sigil '((t ()))
  "PHP Mode face used to highlight variable sigils ($)."
  :tag "PHP Variable Sigil")

(defface php-operator '((t ()))
  "PHP Mode face used to operators."
  :tag "PHP Operator")

(defface php-assignment-op '((t (:inherit php-operator)))
  "PHP Mode face used to assignment operators (=, +=, ...)."
  :tag "PHP Object Op")

(defface php-comparison-op '((t (:inherit php-operator)))
  "PHP Mode face used to comparison operators (==, !=, ===, ...)."
  :tag "PHP Comparison Op")

(defface php-logical-op '((t (:inherit php-operator)))
  "PHP Mode face used to logical operators (&&, ||, ?:)."
  :tag "PHP Logical Op")

(defface php-arithmetic-op '((t (:inherit php-operator)))
  "PHP Mode face used to arithmetic operators (+, -, %, ...)."
  :tag "PHP Arithmetic Op")

(defface php-inc-dec-op '((t (:inherit php-operator)))
  "PHP Mode face used to increment and decremt operators (--, ++)."
  :tag "PHP Increment/Decrement Op")

(defface php-string-op '((t (:inherit php-operator)))
  "PHP Mode face used to logical operators (.)."
  :tag "PHP String Op")

(defface php-object-op '((t (:inherit php-operator)))
  "PHP Mode face used to object operators (->)."
  :tag "PHP Object Op")

(defface php-pipe-op '((t (:inherit php-operator)))
  "PHP Mode face used to the pipe operator (|>).
The operator was added in PHP 8.5."
  :tag "PHP Pipe Op")

(defface php-paamayim-nekudotayim '((t ()))
  "PHP Mode face used to highlight scope resolution operators (::).
The operator is also knows as \"Paamayim Nekudotayim\"."
  :tag "PHP Paamayim Nekudotayim")

(defface php-type '((t (:inherit font-lock-type-face)))
  "PHP Mode face used to highlight types."
  :tag "PHP Type")

(defface php-class '((t (:inherit font-lock-type-face)))
  "PHP Mode face used to highlight class."
  :tag "PHP Class")

(defface php-constant '((t (:inherit font-lock-constant-face)))
  "PHP Mode face used to highlight constants."
  :tag "PHP Constant")

(defface php-constant-assign '((t (:inherit php-constant)))
  "PHP Mode face used to highlight constant assigning (\"const\" statement)."
  :tag "PHP Constant Assign")

(defface php-magical-constant '((t (:inherit font-lock-builtin-face)))
  "PHP Mode face used to highlight magical constants."
  :tag "PHP Magical Constant")

(defface php-this '((t (:inherit php-constant)))
  "PHP Mode face used to highlight $this variables."
  :tag "PHP $this")

(defface php-this-sigil '((t (:inherit php-constant)))
  "PHP Mode face used to highlight sigils($) of $this variable."
  :tag "PHP $this Sigil")

(define-obsolete-face-alias 'php-$this 'php-this "1.26.0")
(define-obsolete-face-alias 'php-$this-sigil 'php-this-sigil "1.26.0")

(defface php-errorcontrol-op '((t (:inherit font-lock-type-face)))
  "PHP Mode face used to highlight errorcontrol operators (@).."
  :tag "PHP ErrorControl Op")

(defface php-php-tag '((t (:inherit font-lock-preprocessor-face)))
  "PHP Mode face used to highlight PHP tags."
  :tag "PHP php Tag")

(defface php-doc-annotation-tag (eval-when-compile
                                  (if (eval-when-compile (boundp 'font-lock-doc-markup-face))
                                      '((t . (:inherit font-lock-doc-markup-face)))
                                    '((t . (:inherit font-lock-constant-face)))))
  "Face used to highlight annotation tags in doc-comment."
  :tag "PHPDoc Annotation Tag")

(defface php-doc-variable-sigil '((t (:inherit font-lock-variable-name-face)))
  "PHP Mode face used to highlight variable sigils($)."
  :tag "PHPDoc Variable Sigil")

(defface php-doc-$this '((t (:inherit php-type)))
  "PHP Mode face used to highlight $this variable in doc-comment."
  :tag "PHPDoc $this")

(defface php-doc-$this-sigil '((t (:inherit php-type)))
  "PHP Mode face used to highlight sigil of $this variable in doc-comment."
  :tag "PHPDoc $this Sigil")

(defface php-doc-class-name '((t (:inherit php-string)))
  "PHP Mode Face used to class names in doc-comment."
  :tag "PHPDoc Class Name")

(defface php-class-declaration '((t (:inherit php-keyword)))
  "PHP Mode Face used to class declarations."
  :tag "PHP Class Declaration")

(defface php-class-declaration-spec '((t (:inherit php-keyword)))
  "PHP Mode Face used to highlight class declaration specification keywords.
The keywords include: implements, extends."
  :tag "PHP Class Declaration Specification")

(defface php-namespace-declaration '((t (:inherit php-keyword)))
  "PHP Mode Face used to highlight namespace declaration keyword."
  :tag "PHP Namespace Declaration")

(defface php-import-declaration '((t (:inherit php-keyword)))
  "PHP Mode Face used to highlight import statements (use ... as ...)."
  :tag "PHP Import Statement")

(defface php-class-modifier '((t (:inherit php-keyword)))
  "PHP Mode Face used to highlight class modifiers (final, abstract)."
  :tag "PHP Class Modifier")

(defface php-method-modifier '((t (:inherit php-keyword)))
  "PHP Mode Face used to highlight method modifiers (final, abstract)."
  :tag "PHP Method Modifier")

(defface php-visibility-modifier '((t (:inherit php-keyword)))
  "PHP Mode Face used to highlight access keywords (public, protected, private)."
  :tag "PHP Visibility Modifier")

(defface php-control-structure '((t (:inherit php-keyword)))
 "PHP Mode Face used to highlight control structures.
The control structures include: if, foreach, while, switch, catch."
  :tag "PHP Control Structure")

(define-obsolete-face-alias 'php-annotations-annotation-face 'php-doc-annotation-tag "1.19.0")

(provide 'php-face)
;;; php-face.el ends here

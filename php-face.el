;;; php-face.el --- Face definitions for PHP script  -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Friends of Emacs-PHP development

;; Author: USAMI Kenta <tadsan@zonu.me>
;; Created: 5 May 2019
;; Version: 1.22.1
;; Keywords: faces, php
;; Homepage: https://github.com/emacs-php/php-mode
;; Package-Requires: ((emacs "24.3"))
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
  "Faces used in PHP Mode"
  :tag "PHP Faces"
  :group 'php-mode
  :group 'faces)

(defface php-string '((t (:inherit font-lock-string-face)))
  "PHP Mode face used to highlight string literals."
  :group 'php-faces
  :tag "PHP String")

(defface php-keyword '((t (:inherit font-lock-keyword-face)))
  "PHP Mode face used to highlight keywords."
  :group 'php-faces
  :tag "PHP Keyword")

(defface php-builtin '((t (:inherit font-lock-builtin-face)))
  "PHP Mode face used to highlight builtins."
  :group 'php-faces
  :tag "PHP Built-in")

(defface php-function-name '((t (:inherit font-lock-function-name-face)))
  "PHP Mode face used to highlight function names."
  :group 'php-faces
  :tag "PHP Function Name")

(defface php-function-call '((t (:inherit default)))
  "PHP Mode face used to highlight function names in calles."
  :group 'php-faces
  :tag "PHP Function Call")

(defface php-method-call '((t (:inherit php-function-call)))
  "PHP Mode face used to highlight method names in calles."
  :group 'php-faces
  :tag "PHP Method Call")

(defface php-static-method-call '((t (:inherit php-method-call)))
  "PHP Mode face used to highlight static method names in calles."
  :group 'php-faces
  :tag "PHP Static Method Call")

(defface php-variable-name '((t (:inherit font-lock-variable-name-face)))
  "PHP Mode face used to highlight variable names."
  :group 'php-faces
  :tag "PHP Variable Name")

(defface php-property-name '((t (:inherit php-variable-name)))
  "PHP Mode face used to highlight property names."
  :group 'php-faces
  :tag "PHP Property Name")

(defface php-variable-sigil '((t (:inherit default)))
  "PHP Mode face used to highlight variable sigils ($)."
  :group 'php-faces
  :tag "PHP Variable Sigil")

(defface php-operator '((t (:inherit default)))
  "PHP Mode face used to operators."
  :group 'php-faces
  :tag "PHP Operator")

(defface php-assignment-op '((t (:inherit php-operator)))
  "PHP Mode face used to assignment operators (=, +=, ...)."
  :group 'php-faces
  :tag "PHP Object Op")

(defface php-comparison-op '((t (:inherit php-operator)))
  "PHP Mode face used to comparison operators (==, !=, ===, ...)."
  :group 'php-faces
  :tag "PHP Comparison Op")

(defface php-logical-op '((t (:inherit php-operator)))
  "PHP Mode face used to logical operators (&&, ||, ?:)."
  :group 'php-faces
  :tag "PHP Logical Op")

(defface php-arithmetic-op '((t (:inherit php-operator)))
  "PHP Mode face used to arithmetic operators (+, -, %, ...)."
  :group 'php-faces
  :tag "PHP Arithmetic Op")

(defface php-inc-dec-op '((t (:inherit php-operator)))
  "PHP Mode face used to increment and decremt operators (--, ++)."
  :group 'php-faces
  :tag "PHP Increment/Decrement Op")

(defface php-string-op '((t (:inherit php-operator)))
  "PHP Mode face used to logical operators (.)."
  :group 'php-faces
  :tag "PHP String Op")

(defface php-object-op '((t (:inherit php-operator)))
  "PHP Mode face used to object operators (->)."
  :group 'php-faces
  :tag "PHP Object Op")

(defface php-paamayim-nekudotayim '((t (:inherit default)))
  "PHP Mode face used to highlight \"Paamayim Nekudotayim\" scope resolution operators (::)."
  :group 'php-faces
  :tag "PHP Paamayim Nekudotayim")

(defface php-type '((t (:inherit font-lock-type-face)))
  "PHP Mode face used to highlight types."
  :group 'php-faces
  :tag "PHP Type")

(defface php-class '((t (:inherit font-lock-type-face)))
  "PHP Mode face used to highlight class."
  :group 'php-faces
  :tag "PHP Class")

(defface php-constant '((t (:inherit font-lock-constant-face)))
  "PHP Mode face used to highlight constants."
  :group 'php-faces
  :tag "PHP Constant")

(defface php-constant-assign '((t (:inherit font-lock-type-face)))
  "PHP Mode face used to highlight constant assigning (\"const\" statement)."
  :group 'php-faces
  :tag "PHP Constant Assign")

(defface php-magical-constant '((t (:inherit font-lock-builtin-face)))
  "PHP Mode face used to highlight magical constants."
  :group 'php-faces
  :tag "PHP Magical Constant")

(defface php-$this '((t (:inherit php-constant)))
  "PHP Mode face used to highlight $this variables."
  :group 'php-faces
  :tag "PHP $this")

(defface php-$this-sigil '((t (:inherit php-constant)))
  "PHP Mode face used to highlight sigils($) of $this variable."
  :group 'php-faces
  :tag "PHP $this Sigil")

(defface php-errorcontrol-op '((t (:inherit font-lock-type-face)))
  "PHP Mode face used to highlight errorcontrol operators (@).."
  :group 'php-faces
  :tag "PHP ErrorControl Op")

(defface php-php-tag '((t (:inherit font-lock-preprocessor-face)))
  "PHP Mode face used to highlight PHP tags."
  :group 'php-faces
  :tag "PHP php Tag")

(defface php-doc-annotation-tag '((t . (:inherit font-lock-constant-face)))
  "Face used to highlight annotation tags in doc-comment."
  :group 'php-faces
  :tag "PHPDoc Annotation Tag")

(defface php-doc-variable-sigil '((t (:inherit font-lock-variable-name-face)))
  "PHP Mode face used to highlight variable sigils($)."
  :group 'php-faces
  :tag "PHPDoc Variable Sigil")

(defface php-doc-$this '((t (:inherit php-type)))
  "PHP Mode face used to highlight $this variable in doc-comment."
  :group 'php-faces
  :tag "PHPDoc $this")

(defface php-doc-$this-sigil '((t (:inherit php-type)))
  "PHP Mode face used to highlight sigil of $this variable in doc-comment."
  :group 'php-faces
  :tag "PHPDoc $this Sigil")

(defface php-doc-class-name '((t (:inherit php-string)))
  "Face used to class names in doc-comment."
  :group 'php-faces
  :tag "PHPDoc Class Name")

(defface php-class-declaration '((t (:inherit php-keyword)))
  "Face used to highlight class declaration keywords."
  :group 'php-faces
  :tag "PHP Class Declaration")

(defface php-class-declaration-spec '((t (:inherit php-keyword)))
  "Face used to highlight class declaration specification keywords (implements, extends)."
  :group 'php-faces
  :tag "PHP Class Declaration Specification")

(defface php-namespace-declaration '((t (:inherit php-keyword)))
  "Face used to highlight namespace declaration keyword."
  :group 'php-faces
  :tag "PHP Namespace Declaration")

(defface php-import-declaration '((t (:inherit php-keyword)))
  "Face used to highlight import statements (use ... as ...)."
  :group 'php-faces
  :tag "PHP Import Statement")

(defface php-class-modifiers '((t (:inherit php-keyword)))
  "Face used to highlight class modifiers (final, abstract)."
  :group 'php-faces
  :tag "PHP Class Modifiers")

(defface php-method-modifiers '((t (:inherit php-keyword)))
  "Face used to highlight method modifiers (final, abstract)."
  :group 'php-faces
  :tag "PHP Method Modifiers")

(defface php-method-access '((t (:inherit php-keyword)))
  "Face used to highlight method access keywords (public, protected, private)."
  :group 'php-faces
  :tag "PHP Method Access")

(defface php-method-static '((t (:inherit php-keyword)))
  "Face used to highlight static keyword in method declaration."
  :group 'php-faces
  :tag "PHP Method Static")

(defface php-property-access '((t (:inherit php-keyword)))
  "Face used to highlight property access keywords (public, protected, private)."
  :group 'php-faces
  :tag "PHP Property Access")

(defface php-property-const '((t (:inherit php-keyword)))
  "Face used to highlight const keyword in property declaration."
  :group 'php-faces
  :tag "PHP Property Const")

(defface php-property-static '((t (:inherit php-keyword)))
  "Face used to highlight static keyword in property declaration."
  :group 'php-faces
  :tag "PHP Property Static")

(defface php-block-statement '((t (:inherit php-keyword)))
  "Face used to highlight block statements (if, elseif, for, foreach, while, declare, switch, catch)."
  :group 'php-faces
  :tag "PHP Block Statement")

(defface php-flow-control-statement '((t (:inherit php-keyword)))
  "Face used to highlight flow control statements (break, continue, die, exit, goto, return, throw)."
  :group 'php-faces
  :tag "PHP Flow Control Statement")

(defface php-print-statement '((t (:inherit php-keyword)))
  "Face used to highlight print statements (echo, print, var_dump)."
  :group 'php-faces
  :tag "PHP Print Statement")

(defface php-include-statement '((t (:inherit php-keyword)))
  "Face used to highlight include statements (include, include_once, require, require_once)."
  :group 'php-faces
  :tag "PHP Include Statement")

(defface php-constant-keyword '((t (:inherit php-keyword)))
  "Face used to highlight constant keywords (true, false, null)."
  :group 'php-faces
  :tag "PHP Constant Keywords")

(defface php-number '((t (:inherit default)))
  "Face used to highlight numbers."
  :group 'php-faces
  :tag "PHP Numbers")

(defface php-string-quote '((t (:inherit php-string)))
  "Face used to highlight quotes surrounding a string."
  :group 'php-faces
  :tag "PHP String Quotes")

(define-obsolete-face-alias 'php-annotations-annotation-face 'php-doc-annotation-tag "1.19.0")

(provide 'php-face)
;;; php-face.el ends here

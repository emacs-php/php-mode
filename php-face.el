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

(defface php-object-op '((t (:inherit default)))
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

(defface php-errorcontrol-op '((t (:inherit  font-lock-type-face)))
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

(define-obsolete-face-alias 'php-annotations-annotation-face 'php-doc-annotation-tag "1.19.0")

(provide 'php-face)
;;; php-face.el ends here

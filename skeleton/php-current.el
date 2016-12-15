;; php-current.el --- Skeleton for insert current lanaguage element

;; Copyright (C) 2016  USAMI Kenta

;; Author: USAMI Kenta <tadsan@zonu.me>

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301 USA,

;;; Commentary:

;; Add to your .emacs file:
;;
;;   (with-eval-after-load 'php-mode
;;     (require 'php-current)
;;     (define-key php-mode-map (kbd "C-c C--") 'php-insert-current-class)
;;     (define-key php-mode-map (kbd "C-c C-=") 'php-insert-current-namespace))

;;; Code:

(require 'php-mode)
(require 'skeleton)

;;;###autoload
(define-skeleton php-current-class
  "Insert current class name if cursor in class context."
  > (let ((matched (php-get-current-element php--re-classlike-pattern)))
      (if matched
          (concat matched php-class-suffix-when-insert)
        "")))

;;;###autoload
(define-skeleton php-current-namespace
  "Insert current namespace if cursor in in namespace context."
  > (let ((matched (php-get-current-element php--re-namespace-pattern)))
      (if matched
          (concat matched php-namespace-suffix-when-insert)
        "")))

(provide 'php-current)
;;; php-current.el ends here

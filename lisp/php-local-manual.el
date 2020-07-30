;;; php-local-manual.el --- Tools for local PHP manual -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Friends of Emacs-PHP development

;; Author: Eric James Michael Ritz
;; Maintainer: USAMI Kenta <tadsan@zonu.me>
;; URL: https://github.com/emacs-php/php-mode
;; Keywords: docs, php
;; Version: 2.0.0
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

;; This package helps you search the locally installed PHP Manual.
;; If you're only developing online, this feature is probably unnecessary.

;;; Code:
(require 'php-mode)

(defalias 'php-local-manual-search #'php-search-local-documentation)

;; TODO: move implementation
;; (define-obsolete-function-alias 'php-search-local-documentation #'php-local-manual-search)

(provide 'php-local-manual)
;;; php-local-manual.el ends here

;;; php-cc-mode.el --- Compatibility alias for the CC Mode based php-mode  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Friends of Emacs-PHP development

;; Author: USAMI Kenta <tadsan@zonu.me>
;; Maintainer: USAMI Kenta <tadsan@zonu.me>
;; URL: https://github.com/emacs-php/php-mode
;; Keywords: languages php
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

;; This file is a forward-compatibility shim introduced while PHP Mode is
;; being reworked to no longer depend on CC Mode.
;;
;; In the target design, the CC Mode based implementation is renamed to
;; `php-cc-mode' and the name `php-mode' is reused for a cc-mode
;; independent major mode.  During the transition `php-mode' (in
;; php-mode.el) is still the CC Mode based implementation, so this file
;; only establishes the *names* that code will migrate to, without
;; changing any behavior:
;;
;;   - `php-cc-mode' is provided as an alias for `php-mode', so that
;;     configuration or third-party code written against the new name
;;     works now and keeps working after the eventual swap.
;;   - Variable names that will belong to `php-cc-mode' are provided as
;;     aliases of their current `php-mode-*' counterparts.
;;
;; Loading this file has no effect on `php-mode' itself: it does not
;; register any `auto-mode-alist' / `interpreter-mode-alist' entry, and
;; it does not redefine `php-mode'.  It is intentionally kept out of the
;; `php-mode' require graph and is loaded only when something asks for
;; `php-cc-mode'.

;;; Code:

(require 'php-mode)

;;;###autoload
(defalias 'php-cc-mode 'php-mode
  "Major mode for editing PHP code, based on CC Mode.

This is currently an alias for `php-mode'.  It exists so that code can
already refer to the CC Mode based implementation by the name it will
keep (`php-cc-mode') once the default `php-mode' becomes the cc-mode
independent implementation.")

;; Hook: configuration hung on `php-cc-mode-hook' should run for the CC
;; Mode based mode.  While `php-cc-mode' is an alias for `php-mode', the
;; hook is an alias too, so setting either one has the same effect.
(defvaralias 'php-cc-mode-hook 'php-mode-hook
  "Hook run when entering `php-cc-mode'.
Currently an alias for `php-mode-hook'.")

;; Variables that will be owned by `php-cc-mode' in the target design.
;; They are aliased to the current `php-mode-*' names so that either
;; spelling works during the transition.
(defvaralias 'php-cc-mode-lineup-cascaded-calls 'php-mode-lineup-cascaded-calls
  "Indent chained method calls to the previous line.
Currently an alias for `php-mode-lineup-cascaded-calls'.")

(defvaralias 'php-cc-mode-enable-backup-style-variables 'php-mode-enable-backup-style-variables
  "When non-nil, back up values set by hook and buffer local variables.
Currently an alias for `php-mode-enable-backup-style-variables'.")

(provide 'php-cc-mode)
;;; php-cc-mode.el ends here

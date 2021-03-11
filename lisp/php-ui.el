;;; php-ui.el --- UI support for PHP development     -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Friends of Emacs-PHP development

;; Author: USAMI Kenta <tadsan@zonu.me>
;; Keywords: tools, files
;; URL: https://github.com/emacs-php/php-mode
;; Version: 1.24.0
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

;; PHP Mode integrates LSP Mode (lsp-mode), Phpactor (phpactor.el) and IDE-like tools.
;;
;; **Note**:
;; This feature is under development and experimental.
;; All of these functions, modes and terms are subject to change without notice.
;;
;; ## Motivations
;;
;; There are some IDE-like features / packages for PHP development.
;; PHP-UI bridges projects and their IDE-like features.
;;
;; ## IDE Features
;;
;; We don't recommend features, but bundle some feature bridges.
;; They are sorted alphabetically except "none."
;;
;;  - none
;;      Does not launch any IDE features.
;;  - lsp-mode
;;      https://emacs-lsp.github.io/lsp-mode/
;;      https://github.com/emacs-lsp/lsp-mode
;;  - phpactor
;;      https://phpactor.readthedocs.io/
;;      https://github.com/phpactor/phpactor
;;      https://github.com/emacs-php/phpactor.el
;;
;; ## Configuration
;;
;; Put follows code into your .emacs (~/.emacs.d/init.el) file:
;;
;;     (defun my-php-mode-setup ()
;;       (add-hook 'hack-local-variables-hook #'php-ui-mode t t))
;;
;;     (with-eval-after-load 'php-ui
;;       (custom-set-variables
;;        '(php-ui-feature 'eglot) ;; or 'none, 'phpactor, 'lsp-mode
;;        '(php-ui-eglot-executable '("psalm-language-server")) ;; or "intelephense", '("php" "vendor/bin/path/to/server")
;;        ;; If you want to hide php-ui-mode from the mode line, set an empty string
;;        '(php-ui-mode-lighter ""))
;;
;;       ;; Only Eglot users
;;       (add-to-list 'php-ui-eglot-executable '(php-mode . php-ui-eglot-server-program))
;;
;;       (add-hook 'php-mode #'my-php-mode-setup))
;;
;; If you don't enable IDE support by default, set '(php-ui-feature 'none)
;;
;; ### For per project configuration
;;
;; Put follows code into .dir-locals.el in project directory:
;;
;;     ((nil (php-project-root . git)
;;           (php-ui-eglot-executable . ("psalm-language-server"))
;;           ;; or (php-ui-eglot-executable . ("php" "vendor/bin/path/to/server"))
;;           (php-ui-feature . lsp-mode)))
;;
;; If you can't put .dir-locals.el in your project directory, consider the sidecar-locals package.
;;     https://melpa.org/#/sidecar-locals
;;     https://gitlab.com/ideasman42/emacs-sidecar-locals
;;

;;; Code:
(eval-when-compile
  (require 'php-ui-phpactor))

(defvar php-ui-feature-alist
  '((none :test (lambda () t)
          :activate (lambda () t)
          :deactivate (lambda () t))
    (phpactor :test (lambda () (and (require 'phpactor nil t) (featurep 'phpactor)))
              :activate php-ui-phpactor-activate
              :deactivate php-ui-phpactor-activate)
    (lsp-mode :test (lambda () (and (require 'lsp nil t) (featurep 'lsp)))
              :activate lsp
              :deactivate lsp-workspace-shutdown)))

(defgroup php-ui nil
  "UI support for PHP developing."
  :tag "PHP-UI"
  :prefix "php-ui-"
  :group 'php)

(defcustom php-ui-feature nil
  "A symbol of PHP-UI feature"
  :tag "PHP-UI Feature"
  :group 'php-ui
  :type 'symbol
  :safe #'symbolp)

(defcustom php-ui-mode-lighter " PHP-UI"
  "A symbol of PHP-UI feature"
  :tag "PHP-UI Mode Lighter"
  :group 'php-ui
  :type 'string
  :safe #'stringp)

;;;###autoload
(define-minor-mode php-ui-mode
  "Minor mode for integrate IDE-like tools."
  :lighter php-ui-mode-lighter
  (let ((ui-plist (cdr-safe (assq php-ui-feature php-ui-feature-alist))))
    (if (null ui-plist)
        (message "Please set `php-ui-feature' variable in .dir-locals.el or custom variable")
      (if php-ui-mode
          (php-ui--activate-buffer ui-plist)
        (php-ui--deactivate-buffer ui-plist)))))

;;;###autoload
(defun php-ui (feature)
  "Select a PHP-UI feature and execute `php-ui-mode'."
  (interactive (list (php-ui--select-feature)))
  (unless feature
    (user-error "No PHP-UI feature is installed.  Install the lsp-mode, eglot or phpactor package"))
  (unless (assq feature php-ui-feature-alist)
    (user-error "`%s' does not include in available PHP-UI features. (%s)"
                feature
                (mapconcat #'symbol-name (php-ui--avilable-features) ", ")))
  (when (and php-ui-mode feature php-ui-feature
             (not (eq php-ui-feature php-ui-feature)))
    (php-ui-mode -1))
  (let ((php-ui-feature feature))
    (php-ui-mode +1)))

(defun php-ui--activate-buffer (ui-plist)
  "Activate php-ui implementation by UI-PLIST."
  (funcall (plist-get ui-plist :activate)))

(defun php-ui--deactivate-buffer (ui-plist)
  "Deactivate php-ui implementation by UI-PLIST."
  (funcall (plist-get ui-plist :deactivate)))

(defun php-ui--avilable-features ()
  "Return list of available PHP-UI features."
  (cl-loop for (ui . plist) in php-ui-feature-alist
           if (funcall (plist-get plist :test))
           collect ui))

(defun php-ui--select-feature ()
  "Choose PHP-UI feature"
  (let* ((features (php-ui--avilable-features))
         (count (length features)))
    (cond
     ((eq count 0) nil)
     ((eq count 1) (car features))
     (t
      (intern (completing-read "Select PHP-UI feature: " features nil t))))))

(provide 'php-ui)
;;; php-ui.el ends here

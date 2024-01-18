;;; php-ide.el --- IDE-like UI support for PHP development -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Friends of Emacs-PHP development

;; Author: USAMI Kenta <tadsan@zonu.me>
;; Keywords: tools, files
;; URL: https://github.com/emacs-php/php-mode
;; Version: 1.25.1
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
;; PHP-IDE bridges projects and their IDE-like features.
;;
;; ## IDE Features
;;
;; We don't recommend features, but bundle some feature bridges.
;; They are sorted alphabetically except "none."
;;
;;  - none
;;      Does not launch any IDE features.
;;  - eglot
;;      https://github.com/joaotavora/eglot
;;  - lsp-bridge
;;      https://github.com/manateelazycat/lsp-bridge
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
;;     (defun init-php-mode-setup ()
;;       (add-hook 'hack-local-variables-hook #'php-ide-mode t t))
;;
;;     (defun init-php-ide-mode-setup (feature activate)
;;         (pcase feature
;;           (`lsp-bridge
;;            (if activate
;;                (progn (yas-minor-mode +1)
;;                       (corfu-mode -1))
;;              (yas-minor-mode -1)
;;              (corfu-mode +1)))))
;;
;;     (with-eval-after-load 'php-ide
;;       (custom-set-variables
;;        '(php-ide-features . 'eglot) ;; and/or 'none, 'phpactor, 'lsp-mode
;;        '(php-ide-eglot-executable "psalm-language-server") ;; or "intelephense", '("php" "vendor/bin/path/to/server")
;;        ;; If you want to hide php-ide-mode from the mode line, set an empty string
;;        '(php-ide-mode-lighter ""))
;;
;;       (add-hook 'php-mode-hook #'init-php-mode-setup)
;;       (add-hook 'php-ide-mode-functions #'init-php-ide-mode-setup))
;;
;; If you don't enable IDE support by default, set '(php-ide-feature 'none)
;;
;; ### For per project configuration
;;
;; Put follows code into .dir-locals.el in project directory:
;;
;;     ((nil (php-project-root . git)
;;           (php-ide-features . (lsp-mode))))
;;
;; If you can't put .dir-locals.el in your project directory, consider the sidecar-locals package.
;;     https://melpa.org/#/sidecar-locals
;;     https://codeberg.org/ideasman42/emacs-sidecar-locals
;;

;;; Code:
(require 'cl-lib)
(require 'php-project)

(eval-when-compile
  (require 'php-ide-phpactor)
  (defvar eglot-server-programs)
  (declare-function lsp-bridge-mode "ext:lsp-bridge" ())
  (declare-function eglot-ensure "ext:eglot" ())
  (declare-function eglot--managed-mode-off "ext:eglot" ())
  (declare-function phpactor--find-executable "ext:phpactor" ()))

(defvar php-ide-feature-alist
  '((none :test (lambda () t)
          :activate (lambda () t)
          :deactivate (lambda () t))
    (phpactor :test (lambda () (and (require 'phpactor nil t) (featurep 'phpactor)))
              :activate php-ide-phpactor-activate
              :deactivate php-ide-phpactor-activate)
    (eglot :test (lambda () (and (require 'eglot nil t) (featurep 'eglot)))
           :activate eglot-ensure
           :deactivate eglot--managed-mode-off)
    (lsp-bridge :test (lambda () (and (require 'lsp-bridge nil t) (featurep 'lsp-bridge)))
                :activate (lambda () (lsp-bridge-mode +1))
                :deactivate (lambda () (lsp-bridge-mode -1)))
    (lsp-mode :test (lambda () (and (require 'lsp nil t) (featurep 'lsp)))
              :activate lsp
              :deactivate lsp-workspace-shutdown)))

(defvar php-ide-lsp-command-alist
  '((intelephense "intelephense" "--stdio")
    (phpactor . (lambda () (list (if (fboundp 'phpactor--find-executable)
                                     (phpactor--find-executable)
                                   "phpactor")
                                 "language-server")))))

(defgroup php-ide nil
  "IDE-like support for PHP developing."
  :tag "PHP-IDE"
  :prefix "php-ide-"
  :group 'php)

;;;###autoload
(defcustom php-ide-features nil
  "A set of PHP-IDE features symbol."
  :tag "PHP-IDE Feature"
  :group 'php-ide
  :type `(set ,@(mapcar (lambda (feature) (list 'const (car feature)))
                       php-ide-feature-alist)
              symbol)
  :safe (lambda (v) (cl-loop for feature in (if (listp v) v (list v))
                             always (symbolp feature))))

;;;###autoload
(defcustom php-ide-eglot-executable nil
  "Command name or path to the command of Eglot LSP executable."
  :tag "PHP-IDE Eglot Executable"
  :group 'php-ide
  :type '(choice
          (const intelephense)
          (const phpactor)
          string (repeat string))
  :safe (lambda (v) (cond
                     ((stringp v) (file-exists-p v))
                     ((listp v) (cl-every #'stringp v))
                     ((assq v php-ide-lsp-command-alist)))))

;;;###autoload
(defun php-ide-eglot-server-program ()
  "Return a list of command to execute LSP Server."
  (cond
   ((stringp php-ide-eglot-executable) (list php-ide-eglot-executable))
   ((listp php-ide-eglot-executable) php-ide-eglot-executable)
   ((when-let (command (assq php-ide-eglot-executable php-ide-lsp-command-alist))
      (cond
       ((functionp command) (funcall command))
       ((listp command) command))))))

(defcustom php-ide-mode-lighter " PHP-IDE"
  "A symbol of PHP-IDE feature."
  :tag "PHP-IDE Mode Lighter"
  :group 'php-ide
  :type 'string
  :safe #'stringp)

;;;###autoload
(defcustom php-ide-mode-functions nil
  "Hook functions called when before activating or deactivating PHP-IDE.
Notice that two arguments (FEATURE ACTIVATE) are given.

FEATURE: A symbol, like \\='lsp-mode.
ACTIVATE: T is given when activeting, NIL when deactivating PHP-IDE."
  :tag "PHP-IDE Mode Functions"
  :group 'php-ide
  :type '(repeat function)
  :safe (lambda (functions)
          (and (listp functions) (cl-every #'functionp functions))))

;;;###autoload
(define-minor-mode php-ide-mode
  "Minor mode for integrate IDE-like tools."
  :lighter php-ide-mode-lighter
  (let ((ide-features php-ide-features))
    (when-let (unavailable-features (cl-loop for feature in ide-features
                                             unless (assq feature php-ide-feature-alist)
                                             collect feature))
      (user-error "%s includes unavailable PHP-IDE features.  (available features are: %s)"
                  ide-features
                  (mapconcat (lambda (feature) (concat "'" (symbol-name feature)))
                             (php-ide--avilable-features) ", ")))
    (cl-loop for feature in ide-features
             for ide-plist = (cdr-safe (assq feature php-ide-feature-alist))
             do (if (null ide-plist)
                    (message "Please set `php-ide-feature' variable in .dir-locals.el or custom variable")
                  (run-hook-with-args 'php-ide-mode-functions feature php-ide-mode)
                  (if php-ide-mode
                      (php-ide--activate-buffer feature ide-plist)
                    (php-ide--deactivate-buffer ide-plist))))))

;;;###autoload
(defun php-ide-turn-on ()
  "Turn on PHP IDE-FEATURES and execute `php-ide-mode'."
  (unless php-ide-features
    (user-error "No PHP-IDE feature is installed.  Install the lsp-mode, lsp-bridge, eglot or phpactor package"))
  (php-ide-mode +1))

(defun php-ide--activate-buffer (name ide-plist)
  "Activate php-ide implementation by NAME and IDE-PLIST."
  (unless (funcall (plist-get ide-plist :test))
    (user-error "PHP-IDE feature `%s' is not available" name))
  (funcall (plist-get ide-plist :activate)))

(defun php-ide--deactivate-buffer (ide-plist)
  "Deactivate php-ide implementation by IDE-PLIST."
  (funcall (plist-get ide-plist :deactivate)))

(defun php-ide--avilable-features ()
  "Return list of available PHP-IDE features."
  (cl-loop for (ide . plist) in php-ide-feature-alist
           if (funcall (plist-get plist :test))
           collect ide))

(provide 'php-ide)
;;; php-ide.el ends here

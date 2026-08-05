;;; php-ide.el --- IDE-like UI support for PHP development -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Friends of Emacs-PHP development

;; Author: USAMI Kenta <tadsan@zonu.me>
;; Keywords: tools, files
;; URL: https://github.com/emacs-php/php-mode
;; Version: 1.26.1
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
;; Put the following code into your .emacs (~/.emacs.d/init.el) file:
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
;;        '(php-ide-features '(eglot)) ;; and/or '(none), '(phpactor), '(lsp-mode)
;;        '(php-ide-eglot-executable "psalm-language-server") ;; or "intelephense", '("php" "vendor/bin/path/to/server")
;;        ;; If you want to hide php-ide-mode from the mode line, set an empty string
;;        '(php-ide-mode-lighter ""))
;;
;;       (add-hook 'php-mode-hook #'init-php-mode-setup)
;;       (add-hook 'php-ide-mode-functions #'init-php-ide-mode-setup))
;;
;; If you don't want to enable any IDE support by default, leave `php-ide-features'
;; unset (its default, nil) or set it to '(none) explicitly.
;;
;; ### For per project configuration
;;
;; Put the following code into .dir-locals.el in project directory:
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

;; Autoloaded because the `:safe' predicate of `php-ide-features' consults this
;; alist, and that predicate is copied into the package autoloads file, where it
;; runs while Emacs checks .dir-locals.el — long before php-ide.el itself loads.
;;;###autoload
(defvar php-ide-feature-alist
  '((none :test (lambda () t)
          :activate (lambda () t)
          :deactivate (lambda () t))
    (phpactor :test (lambda () (and (require 'phpactor nil t) (featurep 'phpactor)))
              :activate php-ide-phpactor-activate
              :deactivate php-ide-phpactor-deactivate)
    (eglot :test (lambda () (and (require 'eglot nil t) (featurep 'eglot)))
           :activate php-ide-eglot-activate
           ;; `eglot--managed-mode-off' is Eglot's own internal (and unexported) function,
           ;; but it is the only operation that turns Eglot off for just the current buffer
           ;; without shutting down a server that other buffers may still be using.  The
           ;; public `eglot-shutdown' always kills the whole server, which would be a much
           ;; more disruptive (and asymmetric) deactivation than `php-ide-eglot-activate'.
           :deactivate eglot--managed-mode-off)
    (lsp-bridge :test (lambda () (and (require 'lsp-bridge nil t) (featurep 'lsp-bridge)))
                :activate (lambda () (lsp-bridge-mode +1))
                :deactivate (lambda () (lsp-bridge-mode -1)))
    (lsp-mode :test (lambda () (and (require 'lsp nil t) (featurep 'lsp)))
              :activate lsp
              :deactivate lsp-disconnect))
  "Alist of PHP-IDE features and how to probe and (de)activate each one.

Each element is (FEATURE . PLIST), where PLIST holds these keywords,
each bound to a function called with no arguments:

`:test'        Return non-nil when FEATURE is usable in this Emacs,
               loading its backing package if necessary.
`:activate'    Turn FEATURE on in the current buffer.
`:deactivate'  Turn FEATURE off in the current buffer.")

;; Autoloaded for the same reason as `php-ide-feature-alist'; the `:safe'
;; predicate of `php-ide-eglot-executable' consults this alist.
;;;###autoload
(defvar php-ide-lsp-command-alist
  '((intelephense "intelephense" "--stdio")
    (phpactor . (lambda () (list (if (fboundp 'phpactor--find-executable)
                                     (phpactor--find-executable)
                                   "phpactor")
                                 "language-server"))))
  "Alist of bundled LSP server presets for `php-ide-eglot-executable'.

Each element is (NAME . COMMAND), where COMMAND is either a list of
strings to execute or a function of no arguments returning such a list.
Only the NAME symbols listed here are accepted as safe directory-local
values; see `php-ide-eglot-executable'.")

(defgroup php-ide nil
  "IDE-like support for PHP developing."
  :tag "PHP-IDE"
  :prefix "php-ide-"
  :group 'php)

;;;###autoload
(defcustom php-ide-features nil
  "A set of PHP-IDE features symbol."
  :tag "PHP-IDE Feature"
  :type `(set ,@(mapcar (lambda (feature) (list 'const (car feature)))
                       php-ide-feature-alist)
              symbol)
  ;; Only accept feature symbols already known to `php-ide-feature-alist' as safe
  ;; for .dir-locals.el; an arbitrary symbol here could name a feature added by
  ;; some future or third-party extension with its own (unvetted) side effects.
  ;;
  ;; Deliberately written without `cl-lib': this predicate is copied verbatim
  ;; into the package autoloads file and runs there while Emacs checks
  ;; .dir-locals.el, where cl-lib may not be loaded yet.
  :safe (lambda (v)
          (let ((features (if (proper-list-p v) v (list v))))
            (not (memq nil (mapcar (lambda (feature)
                                     (and (assq feature php-ide-feature-alist) t))
                                   features))))))

;;;###autoload
(defcustom php-ide-eglot-executable nil
  "Command name or path to the command of Eglot LSP executable."
  :tag "PHP-IDE Eglot Executable"
  :type '(choice
          (const intelephense)
          (const phpactor)
          string (repeat string))
  ;; Only a symbol naming one of the bundled presets in `php-ide-lsp-command-alist'
  ;; is safe for .dir-locals.el: the actual command is then fully determined by
  ;; this package, not by the (untrusted) directory-local value.  A literal string
  ;; or argument list lets the directory choose the executable/arguments outright,
  ;; which `php-ide-eglot-server-program' would later pass straight to
  ;; `start-process' — that must go through Emacs's normal unsafe-variable
  ;; confirmation prompt rather than apply silently.
  :safe (lambda (v) (and (assq v php-ide-lsp-command-alist) t)))

;;;###autoload
(defun php-ide-eglot-server-program ()
  "Return a list of command to execute LSP Server."
  (cond
   ((stringp php-ide-eglot-executable) (list php-ide-eglot-executable))
   ((listp php-ide-eglot-executable) php-ide-eglot-executable)
   ((when-let* ((command (cdr (assq php-ide-eglot-executable php-ide-lsp-command-alist))))
      (cond
       ((functionp command) (funcall command))
       ((listp command) command))))))

(defvar php-ide-eglot-managed-modes '(php-mode phps-mode php-ts-mode)
  "Major modes keyed by the `eglot-server-programs' entry php-ide adds.

`php-ide-eglot-activate' registers `php-ide-eglot-executable' for exactly
these modes.")

(defun php-ide-eglot--contact-function (&optional _interactive _project)
  "CONTACT function registered into `eglot-server-programs' by php-ide.
Ignores the INTERACTIVE and PROJECT arguments Eglot may pass; see
`php-ide-eglot-server-program' for the actual command lookup."
  (php-ide-eglot-server-program))

;;;###autoload
(defun php-ide-eglot-activate ()
  "Activate Eglot for `php-ide-mode', honoring `php-ide-eglot-executable'.

When `php-ide-eglot-executable' is set, this buffer-locally prepends
an entry to `eglot-server-programs' so Eglot uses it instead of its
own bundled default for PHP.  Buffers where `php-ide-eglot-executable'
is unset are unaffected and keep using Eglot's default."
  (when (and php-ide-eglot-executable
             (not (eq (cdr (assoc php-ide-eglot-managed-modes eglot-server-programs))
                      #'php-ide-eglot--contact-function)))
    (setq-local eglot-server-programs
                (cons (cons php-ide-eglot-managed-modes #'php-ide-eglot--contact-function)
                      eglot-server-programs)))
  (eglot-ensure))

(defcustom php-ide-mode-lighter " PHP-IDE"
  "Mode line indicator for `php-ide-mode'.

Set it to an empty string to hide `php-ide-mode' from the mode line."
  :tag "PHP-IDE Mode Lighter"
  :type 'string
  :safe #'stringp)

;;;###autoload
(defcustom php-ide-mode-functions nil
  "Hook functions called when before activating or deactivating PHP-IDE.
Notice that two arguments (FEATURE ACTIVATE) are given.

FEATURE: A symbol, like \\='lsp-mode.
ACTIVATE: T is given when activating, NIL when deactivating PHP-IDE."
  :tag "PHP-IDE Mode Functions"
  :type '(repeat function)
  ;; Deliberately has no :safe predicate.  This variable holds functions that
  ;; `php-ide-mode' calls automatically, so a directory-local value naming an
  ;; arbitrary (but already-`fboundp') function would let any repo run code in
  ;; the visitor's Emacs just by having them open a file; that must always go
  ;; through Emacs's normal unsafe-variable confirmation, never apply silently.
  )

;;;###autoload
(define-minor-mode php-ide-mode
  "Minor mode for integrate IDE-like tools."
  :lighter php-ide-mode-lighter
  (let ((ide-features (if (listp php-ide-features) php-ide-features (list php-ide-features))))
    (when-let* ((unavailable-features (cl-loop for feature in ide-features
                                               unless (assq feature php-ide-feature-alist)
                                               collect feature)))
      (user-error "%s includes unavailable PHP-IDE features.  (available features are: %s)"
                  ide-features
                  (mapconcat (lambda (feature) (concat "'" (symbol-name feature)))
                             (php-ide--available-features) ", ")))
    ;; Every feature in IDE-FEATURES is guaranteed to be in `php-ide-feature-alist' here,
    ;; because the loop above already signals a `user-error' otherwise.
    (cl-loop for feature in ide-features
             for ide-plist = (cdr (assq feature php-ide-feature-alist))
             do (progn
                  (run-hook-with-args 'php-ide-mode-functions feature php-ide-mode)
                  (if php-ide-mode
                      (php-ide--activate-buffer feature ide-plist)
                    (php-ide--deactivate-buffer ide-plist))))))

;;;###autoload
(defun php-ide-turn-on ()
  "Turn on `php-ide-mode' if `php-ide-features' is set, otherwise do nothing.

Unlike calling `php-ide-mode' directly, this never signals an error when
`php-ide-features' is unset, so it is safe to add unconditionally to
`php-mode-hook' or `hack-local-variables-hook'; buffers/projects that
never configure `php-ide-features' are silently left alone."
  (when php-ide-features
    (php-ide-mode +1)))

(defun php-ide--activate-buffer (name ide-plist)
  "Activate php-ide implementation by NAME and IDE-PLIST."
  (unless (funcall (plist-get ide-plist :test))
    (user-error "PHP-IDE feature `%s' is not available" name))
  (funcall (plist-get ide-plist :activate)))

(defun php-ide--deactivate-buffer (ide-plist)
  "Deactivate php-ide implementation by IDE-PLIST."
  (funcall (plist-get ide-plist :deactivate)))

(defun php-ide--available-features ()
  "Return list of available PHP-IDE features."
  (cl-loop for (ide . plist) in php-ide-feature-alist
           if (funcall (plist-get plist :test))
           collect ide))

;;;###autoload
(defun php-ide-set-feature (feature)
  "Set `php-ide-features' to FEATURE for the current buffer and enable it.

Interactively, prompt among the PHP-IDE features currently available on
this system (see `php-ide--available-features'); features whose backing
package (lsp-mode, lsp-bridge, Eglot or phpactor.el) is not installed
are not offered.

This sets `php-ide-features' buffer-locally, so the choice does not
persist beyond the current buffer; put a matching entry in
\".dir-locals.el\" (or your `php-mode-hook') to make it stick."
  (interactive
   (let ((available (php-ide--available-features)))
     (unless available
       (user-error "No PHP-IDE feature is available.  Install lsp-mode, lsp-bridge, eglot or phpactor"))
     (list (intern (completing-read "PHP-IDE feature: "
                                    (mapcar #'symbol-name available) nil t)))))
  (when php-ide-mode
    (php-ide-mode -1))
  (setq-local php-ide-features (list feature))
  (php-ide-mode +1))

;;;###autoload
(defun php-ide-status ()
  "Show `php-ide-mode' status for the current buffer in the echo area."
  (interactive)
  (let ((configured (if (listp php-ide-features) php-ide-features (list php-ide-features))))
    (message "PHP-IDE: %s (configured: %s; available on this system: %s)"
             (if php-ide-mode "on" "off")
             (if configured (mapconcat #'symbol-name configured ", ") "none")
             (or (mapconcat #'symbol-name (php-ide--available-features) ", ") "none"))))

(provide 'php-ide)
;;; php-ide.el ends here

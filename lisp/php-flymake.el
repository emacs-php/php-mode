;;; php-flymake.el --- Flymake backend for PHP       -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Friends of Emacs-PHP development

;; Author: USAMI Kenta <tadsan@zonu.me>
;; Created: 5 Mar 2022
;; Version: 1.24.2
;; Keywords: tools, languages, php

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

;; Flymake backend for PHP.

;;; Code:
(require 'flymake)
(require 'cl-lib)
(eval-when-compile
  (require 'pcase)
  (require 'rx))

(defgroup php-flymake nil
  "Flymake backend for PHP."
  :tag "PHP Flymake"
  :group 'php)

(defcustom php-flymake-executable-command-args nil
  "List of command and arguments for `php -l'."
  :group 'php-flymake
  :type '(repeat string)
  :safe (lambda (v) (and (listp v) (cl-every v #'stringp))))

(defconst php-flymake--diaggnostics-pattern
  (eval-when-compile
    (rx bol (? "PHP ")
        (group (or "Parse" "Fatal")) ;; 1: type, not used
        " error:" (+ (syntax whitespace))
        (group (+? any)) ;; 2: msg
        " in " (group (+? any)) ;; 3: file, not used
        " on line " (group (+ num)) ;; 4: line
        eol)))

(defvar-local php-flymake--proc nil)

;;;###autoload
(defun php-flymake (report-fn &rest args)
  "Flymake backend for PHP syntax check.

See `flymake-diagnostic-functions' about REPORT-FN and ARGS parameters."
  (setq-local flymake-proc-allowed-file-name-masks nil)
  (when (process-live-p php-flymake--proc)
    (if (plist-get args :interactive)
        (user-error "There's already a Flymake process running in this buffer")
      (kill-process php-flymake--proc)))
  (save-restriction
    (widen)
    (cl-multiple-value-bind (use-stdin skip) (php-flymake--buffer-status)
      (unless skip
        (setq php-flymake--proc (php-flymake--make-process report-fn buffer-file-name (current-buffer) use-stdin))
        (when use-stdin
          (process-send-region php-flymake--proc (point-min) (point-max)))
        (process-send-eof php-flymake--proc)))))

(defun php-flymake--buffer-status ()
  "Return buffer status about \"use STDIN\" and \"Skip diagnostic\"."
  (let* ((use-stdin (or (null buffer-file-name)
                         (buffer-modified-p (current-buffer))
                         (file-remote-p buffer-file-name)))
         (skip (and (not use-stdin)
                    (save-excursion (goto-char (point-min)) (looking-at-p "#!")))))
    (cl-values use-stdin skip)))

(defun php-flymake--diagnostics (locus source)
  "Parse output of `php -l' command in SOURCE buffer.  LOCUS means filename."
  (unless (eval-when-compile (and (fboundp 'flymake-make-diagnostic)
                                  (fboundp 'flymake-diag-region)))
    (error "`php-flymake' requires Emacs 26.1 or later"))
  (cl-loop
   while (search-forward-regexp php-flymake--diaggnostics-pattern nil t)
   for msg = (match-string 2)
   for line = (string-to-number (match-string 4))
   for diag = (or (pcase-let ((`(,beg . ,end)
                               (flymake-diag-region source line)))
                    (flymake-make-diagnostic source beg end :error msg))
                  (flymake-make-diagnostic locus (cons line nil) nil :error msg))
     return (list diag)))

(defun php-flymake--build-command-line (file)
  "Return the command line for `php -l' FILE."
  (let* ((command-args (or php-flymake-executable-command-args
                           (list (or (bound-and-true-p php-executable) "php"))))
         (cmd (car-safe command-args))
         (default-directory (expand-file-name "~")))
    (unless (or (file-executable-p cmd)
                (executable-find cmd))
      (user-error "`%s' is not valid command" cmd))
    (nconc command-args
           (list "-d" "display_errors=0")
           (when file (list "-f" file))
           (list "-l"))))

(defun php-flymake--make-process (report-fn locus source use-stdin)
  "Make PHP process for syntax check SOURCE buffer.

See `flymake-diagnostic-functions' about REPORT-FN parameter.
See `flymake-make-diagnostic' about LOCUS parameter."
  (make-process
   :name "php-flymake"
   :buffer (generate-new-buffer "*flymake-php-flymake*")
   :command (php-flymake--build-command-line (unless use-stdin locus))
   :noquery t :connection-type 'pipe
   :sentinel
   (lambda (p _ev)
     (unwind-protect
         (when (and (eq 'exit (process-status p))
                    (with-current-buffer source (eq p php-flymake--proc)))
           (with-current-buffer (process-buffer p)
             (goto-char (point-min))
             (funcall report-fn
                      (if (zerop (process-exit-status p))
                          nil
                        (php-flymake--diagnostics locus source)))))
       (unless (process-live-p p)
         ;; (display-buffer (process-buffer p)) ; uncomment to debug
         (kill-buffer (process-buffer p)))))))

(provide 'php-flymake)
;;; php-flymake.el ends here

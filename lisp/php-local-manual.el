;;; php-local-manual.el --- Tools for local PHP manual -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Friends of Emacs-PHP development

;; Author: phil-s
;; Maintainer: USAMI Kenta <tadsan@zonu.me>
;; URL: https://github.com/emacs-php/php-mode/wiki/Local-PHP-Manual
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
;;
;; ## Notice
;;
;; This file is marked as an unmaintained feature.
;; https://github.com/emacs-php/php-mode/wiki/Unmaintained-Features
;;
;; ## How to use
;;
;; see https://github.com/emacs-php/php-mode/wiki/Local-PHP-Manual
;;
;; ### php-local-manual-search
;;
;; Put follows code into your .emacs (~/.emacs.d/init.el) file:
;;
;;     (custom-set-variables
;;      '(php-manual-path (expand-file-name "~/local/share/php-manual"))
;;      '(php-search-documentation-function #'php-local-manual-search))
;;

;;; Code:
(require 'php)

(defconst php-local-manual-documentation-types
  '("function" "control-structures" "class" "book")
  ;; "intro" and "ref" also look interesting, but for all practical purposes
  ;; their terms are sub-sets of the "book" terms (with the few exceptions
  ;; being very unlikely search terms).
  "The set (and priority sequence) of documentation file prefixes
under which to search for files in the local documentation directory.")

(defvar php-local-manual--words-cache nil)

(defun php-local-manual--read-arg ()
  "Obtain interactive argument for searching documentation."
  ;; Cache the list of documentation words available for completion,
  ;; based on the defined types-of-interest.
  (let ((types-list php-local-manual-documentation-types)
        (words-cache php-local-manual--words-cache)
        (local-manual (and (stringp php-manual-path)
                           (not (string= php-manual-path "")))))
    (when (and local-manual
               (not (assq types-list words-cache)))
      ;; Generate the cache on the first run, or if the types changed.
      ;; We read the filenames matching our types list in the local
      ;; documentation directory, and extract the 'middle' component
      ;; of each. e.g. "function.array-map.html" => "array_map".
      (let* ((types-opt (regexp-opt types-list))
             (pattern (concat "\\`" types-opt "\\.\\(.+\\)\\.html\\'"))
             (collection
              (mapcar (lambda (filename)
                        (subst-char-in-string ?- ?_ (replace-regexp-in-string
                                                     pattern "\\1" filename)))
                      (directory-files php-manual-path nil pattern))))
        ;; Replace the entire cache. If the types changed, we don't need
        ;; to retain the collection for the previous value.
        (setq words-cache (list (cons types-list collection)))
        (setq php-local-manual--words-cache words-cache)))
    ;; By default we search for (current-word) immediately, without prompting.
    ;; With a prefix argument, or if there is no (current-word), we perform a
    ;; completing read for a word from the cached collection.
    (let* ((default (current-word))
           (prompt (if default
                       (format "Search PHP docs (%s): " default)
                     "Search PHP docs: "))
           (collection (and local-manual
                            (cdr (assq types-list words-cache))))
           (word (if (or current-prefix-arg (not default))
                     (completing-read prompt collection nil nil nil nil default)
                   default)))
      ;; Return interactive argument list.
      (list word))))

;;;###autoload
(defun php-local-manual-search (word)
  "Search the local PHP documentation (i.e. in `php-manual-path') for
the word at point.  The function returns t if the requested documentation
exists, and nil otherwise.

With a prefix argument, prompt (with completion) for a word to search for."
  (interactive (php-local-manual--read-arg))
  (let ((file (catch 'found
                (cl-loop for type in php-local-manual-documentation-types do
                         (let* ((doc-html (format "%s.%s.html"
                                                  type
                                                  (replace-regexp-in-string
                                                   "_" "-" (downcase word))))
                                (file (expand-file-name doc-html  php-manual-path)))
                           (when (file-exists-p file)
                             (throw 'found file)))))))
    (when file
      (let ((file-url (if (string-prefix-p "file://" file)
                          file
                        (concat "file://" file))))
        (php-browse-documentation-url file-url))
      t)))

;;;###autoload
(define-obsolete-function-alias 'php-search-local-documentation #'php-local-manual-search "2.0.0")

(provide 'php-local-manual)
;;; php-local-manual.el ends here

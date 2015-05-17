;; Copyright (C) 2015  David Arroyo Menéndez

;; Author: David Arroyo Menéndez <davidam@gnu.org>
;; Maintainer: David Arroyo Menéndez <davidam@gnu.org>

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

;; http://php.net/manual/en/ref.strings.php
;; file:///usr/share/doc/php-doc/html/ref.strings.html

(define-skeleton php-addcslashes 
  "Insert an addcslashes statement"
  ""
  '(setq str (skeleton-read "String? "))
  '(setq charlist (skeleton-read "Charlist? "))
  > "addclslashes(" str ", " charlist ");" \n)

(define-skeleton php-addslashes 
  "Insert an addcslashes statement"
  ""
  '(setq str (skeleton-read "String? "))
  > "addclslashes(" str ");" \n)

(define-skeleton php-bin2hex
  "Insert a bin2hex statement"
  ""
  '(setq str (skeleton-read "String? "))
  > "bin2hex(" str ");" \n)

(define-skeleton php-convert_uudecode
  "Insert a bin2hex statement"
  ""
  '(setq str (skeleton-read "String? "))
  > "convert_uudecode(" str ");" \n)

(define-skeleton php-convert_uuencode
  "Insert a bin2hex statement"
  ""
  '(setq str (skeleton-read "String? "))
  > "convert_uuencode(" str ");" \n)

(define-skeleton php-explode
  "Insert an explode statement"
  ""
  '(setq separator (skeleton-read "Explode separator? "))
  '(setq var (skeleton-read "Explode variable? "))
  > "explode('" separator "', " var ");" \n)

(define-skeleton php-implode
  "Insert an implode statement"
  ""
  '(setq separator (skeleton-read "Implode separator? "))
  '(setq var (skeleton-read "Implode variable? "))
  > "implode('" separator "', " var 
  (skeleton-read 
   > ", " str )
  > ");"
  )

(define-skeleton php-rtrim
  "Insert a bin2hex statement"
  ""
  '(setq str (skeleton-read "String? "))
  > "rtrim(" str ");" \n)

(define-skeleton php-strlen
  "Insert a strlen statement"
  ""
  '(setq str (skeleton-read "String? "))
  > "strlen(" str ");" \n)

(define-skeleton php-strtolower
  "Insert a strlower statement"
  ""
  '(setq str (skeleton-read "String? "))
  > "strtolower(" str ");" \n)

(define-skeleton php-strtotime
  "Insert a strtotime statement"
  ""
  '(setq str (skeleton-read "String? "))
  > "strtotime(" str ");" \n)

(define-skeleton php-strtoupper
  "Insert a strtoupper statement"
  ""
  '(setq str (skeleton-read "String? "))
  > "strtoupper(" str ");" \n)

(define-skeleton php-strrev
  "Insert a strrev statement"
  ""
  '(setq str (skeleton-read "String? "))
  > "strrev('" str "');" \n)

(define-skeleton php-ucfirst
  "Insert a ucfirst statement"
  ""
  '(setq str (skeleton-read "String? "))
  > "ucfirst('" str "');" \n)

(define-skeleton php-ucwords
  "Insert a ucfirst statement"
  ""
  '(setq str (skeleton-read "String? "))
  > "ucwords('" str "');" \n)

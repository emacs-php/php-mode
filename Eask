(package "php-mode"
         "1.26.1"
         "Major mode for editing PHP code")

(website-url "https://github.com/emacs-php/php-mode")
(keywords "languages" "php")

(package-file "lisp/php-mode.el")
(files
 "lisp/php.el"
 "lisp/php-complete.el"
 "lisp/php-defs.el"
 "lisp/php-face.el"
 "lisp/php-format.el"
 "lisp/php-project.el"
 "lisp/php-local-manual.el"
 "lisp/php-ide-phpactor.el"
 "lisp/php-ide.el"
 "lisp/php-mode-debug.el")

(script "test" "echo \"Error: no test specified\" && exit 1")

(source 'melpa)
(source 'gnu)

(depends-on "emacs" "26.1")

(development
 (depends-on "phpactor")
 (depends-on "pkg-info")
 (depends-on "projectile")
 (depends-on "smart-jump")
 (depends-on "shut-up")
 )

(setq network-security-level 'low)  ; see https://github.com/jcs090218/setup-emacs-windows/issues/156#issuecomment-932956432

<?php

/**
 * GitHub Issue:    https://github.com/emacs-php/php-mode/issues/129
 *
 * PHP Mode should use 'font-lock-keyword-face' to highlight 'final'
 * in the method below.
 * 
 */

class MyClass
{
        public final function foo() 
        {
                return null;
        }
}

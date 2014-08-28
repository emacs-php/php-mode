<?php

/**
 * Github Issue:    https://github.com/ejmr/php-mode/issues/66
 *
 * Namespace examples from user comment on
 * http://www.php.net/manual/en/language.namespaces.definitionmultiple.php
 *
 * From the comment: "use" statements are required to be placed after the
 * "namespace my\space" but before the "{".
 */

namespace foo\bar;
use my\space\MyClass;
{
  $str = "hi";
  class TheClass {
    public function __construct() {
        $object = new StdClass();
        $object->call()
               ->something();
    }
  }
 // place code here

} // end of namespace foo\bar

namespace another\bar;
use my\space\MyClass;
use my\space\AnotherClass;
{
  $more = "other";

} // end of namespace another\bar
?>
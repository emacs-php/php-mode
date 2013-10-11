<?php

/**
 * GitHub Issue:    https://github.com/ejmr/php-mode/issues/136
 *
 * The code below contains strings with interpolated variables.  PHP
 * Mode must use font-lock-variable-name-face to highlight all of
 * those variables.  The page
 *
 *     http://www.php.net/manual/en/language.types.string.php#language.types.string.parsing
 *
 * describes all of the variable syntaxes which PHP Mode must
 * recognize and highlight appropriately.
 * 
 */

$name = "Eric";

class User
{
        public $name;
        public $id = 0;

        public function getName()
        {
                return $this->name;
        }

        public function getID()
        {
                return $this->id;
        }
}

$user = new User();
$user->name = "Eric";

$users = array($user);
$index = 0;

ob_start();

echo "My name is $name";
echo "My name is ${name}";
echo "My name is {$name}";
echo "My name is {$user->name}";
echo "My name is {$user->getName()}";
echo "My name is {$users[0]->name}";
echo "My name is {$users[$index]->name}";
echo "My name is {$users[$user->id]->name}";
echo "My name is {$users[$user->getID()]->name}";

ob_end_clean();

<?php

/**
 * A `=>' left dangling at the end of a line.
 *
 * `=>' is excluded from `php-indent--indent-operator-re' because it is
 * PHP's array key operator, so nothing treated the line after it as a
 * continuation: an arrow function's body fell all the way back to
 * column zero.
 */

// An arrow function's body on the next line.
$f = fn($x) =>
    $x + 1;                                          // ###php-mode-test### ((indent 4))

$g = static fn(int $n): bool =>
    $n > 0;                                          // ###php-mode-test### ((indent 4))

// An array value on the next line.
$config = [
    'key' =>                                         // ###php-mode-test### ((indent 4))
        'a value long enough to wrap',               // ###php-mode-test### ((indent 8))
    'other' => 1,                                    // ###php-mode-test### ((indent 4))
];                                                   // ###php-mode-test### ((indent 0))

// The opening bracket on the same line as the arrow: the value anchors
// to the line holding the bracket, not to the key's column.
$inline = ['key' =>
    'value'];                                        // ###php-mode-test### ((indent 4))

// A property hook's expression on the next line.
class Person
{
    public string $label {                           // ###php-mode-test### ((indent 4))
        get =>                                       // ###php-mode-test### ((indent 8))
            $this->firstName                         // ###php-mode-test### ((indent 12))
            . ' '                                    // ###php-mode-test### ((indent 12))
            . $this->lastName;                       // ###php-mode-test### ((indent 12))
    }                                                // ###php-mode-test### ((indent 4))
}

// A match arm's result on the next line.
$label = match ($n) {
    1 =>                                             // ###php-mode-test### ((indent 4))
        'one',                                       // ###php-mode-test### ((indent 8))
    default => 'many',                               // ###php-mode-test### ((indent 4))
};                                                   // ###php-mode-test### ((indent 0))

// Tokens that end in `>' but are not `=>' keep their own handling.
if ($a >=
    $b                                               // ###php-mode-test### ((indent 4))
) {
    echo 1;                                          // ###php-mode-test### ((indent 4))
}

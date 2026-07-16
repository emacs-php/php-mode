<?php

/**
 * PHP 8.4: indentation of property hooks.
 *
 * `get'/`set' hooks put a brace block after a property declaration
 * rather than after a function signature, and the short forms end the
 * line with `=>'.
 *
 * @see https://www.php.net/releases/8.4/en.php
 */

class Person
{
    // A "virtual" property.  It may not be set explicitly.
    public string $fullName {                        // ###php-mode-test### ((indent 4))
        get => $this->firstName . ' ' . $this->lastName; // ###php-mode-test### ((indent 8))
    }                                                // ###php-mode-test### ((indent 4))

    public string $firstName {                       // ###php-mode-test### ((indent 4))
        set => ucfirst(strtolower($value));          // ###php-mode-test### ((indent 8))
    }                                                // ###php-mode-test### ((indent 4))

    public string $lastName {                        // ###php-mode-test### ((indent 4))
        set {                                        // ###php-mode-test### ((indent 8))
            if (strlen($value) < 2) {                // ###php-mode-test### ((indent 12))
                throw new \InvalidArgumentException('Too short'); // ###php-mode-test### ((indent 16))
            }                                        // ###php-mode-test### ((indent 12))
            $this->lastName = $value;                // ###php-mode-test### ((indent 12))
        }                                            // ###php-mode-test### ((indent 8))
    }                                                // ###php-mode-test### ((indent 4))

    // Both hooks on one property.
    public string $email {                           // ###php-mode-test### ((indent 4))
        get {                                        // ###php-mode-test### ((indent 8))
            return strtolower($this->email);         // ###php-mode-test### ((indent 12))
        }                                            // ###php-mode-test### ((indent 8))
        set {                                        // ###php-mode-test### ((indent 8))
            $this->email = trim($value);             // ###php-mode-test### ((indent 12))
        }                                            // ###php-mode-test### ((indent 8))
    }                                                // ###php-mode-test### ((indent 4))

    // A short `get' whose expression runs on past the first line.
    public string $label {                           // ###php-mode-test### ((indent 4))
        get => $this->firstName                      // ###php-mode-test### ((indent 8))
            . ' '                                    // ###php-mode-test### ((indent 12))
            . $this->lastName;                       // ###php-mode-test### ((indent 12))
    }                                                // ###php-mode-test### ((indent 4))

    // An interface-style abstract hook.
    abstract public string $slug { get; }            // ###php-mode-test### ((indent 4))

    // A hook with an explicit parameter type.
    public string $nickname {                        // ###php-mode-test### ((indent 4))
        set(string $value) {                         // ###php-mode-test### ((indent 8))
            $this->nickname = $value;                // ###php-mode-test### ((indent 12))
        }                                            // ###php-mode-test### ((indent 8))
    }                                                // ###php-mode-test### ((indent 4))
}

// Hooks in a constructor-promoted property.
class Point
{
    public function __construct(                     // ###php-mode-test### ((indent 4))
        public int $x {                              // ###php-mode-test### ((indent 8))
            set => abs($value);                      // ###php-mode-test### ((indent 12))
        },                                           // ###php-mode-test### ((indent 8))
    ) {}                                             // ###php-mode-test### ((indent 4))
}

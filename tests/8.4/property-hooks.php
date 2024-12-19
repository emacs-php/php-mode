<?php

class Person
{
    // A "virtual" property.  It may not be set explicitly.
    public string $fullName {
        get => $this->firstName . ' ' . $this->lastName;
    }

    // All write operations go through this hook, and the result is what is written.
    // Read access happens normally.
    public string $firstName {
        set => ucfirst(strtolower($value));
    }

    // All write operations go through this hook, which has to write to the backing value itself.
    // Read access happens normally.
    public string $lastName {
        set {
            if (strlen($value) < 2) {
                throw new \InvalidArgumentException('Too short');
            }
            $this->lastName = $value;
        }
    }
}

$p = new Person();

$p->firstName = 'peter';
print $p->firstName; // Prints "Peter"
$p->lastName = 'Peterson';
print $p->fullName; // Prints "Peter Peterson"

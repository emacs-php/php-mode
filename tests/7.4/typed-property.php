<?php

class Typed
{
    private ?string $string;

    private Hoge $hoge;

    public function __construct($var = null)
    {
        $this->string = $var;
    }

    public function print()
    {
        var_dump($this->string);
    }
}

(new Typed)->print();

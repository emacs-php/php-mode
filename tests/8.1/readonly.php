<?php

class Foo
{
    private readonly string $p1;
    public readonly int $p2;

    public function __construct(
        private readonly string $a1,
        public readonly int $a2,
    ) {}
}

__halt_compiler();

claas Err
{
    /** Readonly property must have type */
    public readonly $e1;
}

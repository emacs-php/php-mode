<?php

class SomeClass
{
    public function none($title): void
    {
    }

    public function title(string $title): string
    {
    }

    public function nullableTitle(?string $title): ?string
    {
    }

    public function number(int $number): int
    {
    }

    public function nullableNumber(?int $number): ?int
    {
    }

    public function otherNumber(float $number): float
    {
    }

    public function nullableOtherNumber(?float $number): ?float
    {
    }

    public function flag(bool $flag): bool
    {
    }

    public function nullableFlag(?bool $flag): ?bool
    {
    }

    public function options(array $options): array
    {
    }

    public function nullableOptions(?array $options): ?array
    {
    }

    public function object(stdClass $object): stdClass
    {
    }

    public function nullableObject(?stdClass $object): ?stdClass
    {
    }

    public function nsObject(\path\to\my\Object $object): \path\to\my\Object
    {
    }

    public function nullableNsObject(?\path\to\my\Object $object): ?\path\to\my\Object
    {
    }
}

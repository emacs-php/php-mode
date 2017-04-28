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

    public function someFunction(
        $any,
        string $name,
        int $value,
        bool $flag,
        array $options,
        stdClass $object,
        \path\to\my\Object $nsObject
    ): void {
    }

    public function getNone(
    ): void {
    }

    public function getTitle(
    ): string {
    }

    public function getNullableTitle(
    ): ?string {
    }

    public function getNumber(
    ): int {
    }

    public function getNullableNumber(
    ): ?int {
    }

    public function getOtherNumber(
    ): float {
    }

    public function getNullableOtherNumber(
    ): ?float {
    }

    public function getFlag(
    ): bool {
    }

    public function getNullableFlag(
    ): ?bool {
    }

    public function getOptions(
    ): array {
    }

    public function getNullableOptions(
    ): ?array {
    }

    public function getObject(
    ): stdClass {
    }

    public function getNullableObject(
    ): ?stdClass {
    }

    public function getNsObject(
    ): \path\to\my\Object {
    }

    public function getNullableNsObject(
    ): ?\path\to\my\Object {
    }
}

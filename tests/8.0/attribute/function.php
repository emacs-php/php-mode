<?php

#[ExampleAttribute]
function f1() { }

#[WithoutArgument]
#[SingleArgument(0)]
#[FewArguments('Hello', 'World')]
function foo() {}

#[WithoutArgument]#[SingleArgument(0)]#[FewArguments('Hello', 'World')]
function bar() {}

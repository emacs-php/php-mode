<?php

# Comment

#[ExampleAttribute]
#[Attr1, Attr2]
class Klass
{
    #[ExampleAttribute]
    function f1() { }

    #[WithoutArgument]
    #[SingleArgument(0)]
    #[FewArguments('Hello', 'World')]
    function foo() {}

    #[WithoutArgument] #[SingleArgument(0)] #[FewArguments('Hello', 'World')]
    function bar() {}

    #[Attr2("foo"), Attr2("bar")]
    function buz() {}
}

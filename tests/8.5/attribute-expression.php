<?php

/**
 * Expressions and closures inside attribute arguments.
 *
 * PHP 8.5 allows static closures and first-class callables in constant
 * expressions, which means an attribute argument can now hold a closure
 * body.  The pre-8.5 spelling wrapped the same logic in an object.
 *
 * @see https://www.php.net/releases/8.5/en.php
 */

// PHP 8.4 and earlier: an expression object as the argument.
final class PostsController
{
    #[AccessControl(                                 // ###php-mode-test### ((indent 4))
        new Expression('request.user === post.getAuthor()'), // ###php-mode-test### ((indent 8))
    )]                                               // ###php-mode-test### ((indent 4))
    public function update(                          // ###php-mode-test### ((indent 4))
        Request $request,                            // ###php-mode-test### ((indent 8))
        Post $post,                                  // ###php-mode-test### ((indent 8))
    ): Response {                                    // ###php-mode-test### ((indent 4))
        // ...                                       // ###php-mode-test### ((indent 8))
    }                                                // ###php-mode-test### ((indent 4))
}

// PHP 8.5: a static closure as the argument.
final class PostsController85
{
    #[AccessControl(static function (                // ###php-mode-test### ((indent 4))
        Request $request,                            // ###php-mode-test### ((indent 8))
        Post $post,                                  // ###php-mode-test### ((indent 8))
    ): bool {                                        // ###php-mode-test### ((indent 4))
        return $request->user === $post->getAuthor(); // ###php-mode-test### ((indent 8))
    })]                                              // ###php-mode-test### ((indent 4))
    public function update(                          // ###php-mode-test### ((indent 4))
        Request $request,                            // ###php-mode-test### ((indent 8))
        Post $post,                                  // ###php-mode-test### ((indent 8))
    ): Response {                                    // ###php-mode-test### ((indent 4))
        // ...                                       // ###php-mode-test### ((indent 8))
    }                                                // ###php-mode-test### ((indent 4))
}

// A first-class callable and an arrow function as arguments.
final class Handlers
{
    #[Listener(strlen(...))]                         // ###php-mode-test### ((indent 4))
    #[Validator(static fn(int $n): bool => $n > 0)]  // ###php-mode-test### ((indent 4))
    public int $count = 0;                           // ###php-mode-test### ((indent 4))
}

// An attribute on a plain function, with a nested array argument.
#[Route('/posts', methods: [                         // ###php-mode-test### ((indent 0))
    'GET',                                           // ###php-mode-test### ((indent 4))
    'HEAD',                                          // ###php-mode-test### ((indent 4))
])]                                                  // ###php-mode-test### ((indent 0))
function index(): Response                           // ###php-mode-test### ((indent 0))
{
    return new Response();                           // ###php-mode-test### ((indent 4))
}

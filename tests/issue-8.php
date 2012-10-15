<?php

/**
 * Github Issue:    https://github.com/ejmr/php-mode/issues/8
 *
 * The code below contains annotations in the comments which begin
 * with the '@' character.  The highlighting for these annotations
 * should be different from that used for comments.  At the very least
 * php-mode should highlight the part up to the first backslash.
 */

/**
 * @ORM\Entity
 * @ORM\Table(name="product")
 */
class Product
{
    /**
     * @ORM\Id
     * @ORM\Column(type="integer")
     * @ORM\GeneratedValue(strategy="AUTO")
     */
    protected $id;

    /**
     * @ORM\Column(type="string", length=100)
     */
    protected $name;

    /**
     * @ORM\Column(type="decimal", scale=2)
     */
    protected $price;

    /**
     * @ORM\Column(type="text")
     */
    protected $description;
}
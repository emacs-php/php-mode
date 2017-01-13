<?php

/**
 * File level doc-comment
 *
 * @copyright 1999, 2000, 2001, 2003, 2004 Turadg Aleahmad
 * @copyright 2008 Aaron S. Hawley
 * @copyright 2011, 2012, 2013, 2014, 2015, 2016 Eric James Michael Ritz
 * @author    USAMI Kenta <tadsan@pixiv.com>
 */

// one-line comment
// @annotation This is NOT annotation.

/*------------------------------------------------
  Multi-line comment

 * @annotation This is NOT annotation.
 -------------------------------------------------*/

// /**
//  * Comment outed class implementation
//  */
// class CommentOuted
// {
// }

/**
 * Class level doc-comment
 *
 * Description {@internal Description} inline tag.
 *
 * @property-read string[] $name
 * @ORM\Table(name="majormodes")
 * @ORM\Entity(repositoryClass="Emacs\Repository\MajorModeRepository")
 */
final class SampleClass
{
    /** Const doc-comment */
    const SAMPLE = 'SAMPLE';
    /** @var string sample property doc-comment */
    private $name;

    /**
     * @param string $name
     */
    public function __construct($name)
    {
        $this->name = $name; // comment

        // one-line comment
        // @annotation This is NOT annotation.
    }
}

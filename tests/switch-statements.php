<?php

/**
 * Test indentation of switch statement and case labels
 */

switch (true) {
    case true:
        echo 'hello';
        break;

    case true:
        return;
}
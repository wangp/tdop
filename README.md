Top-down operator precedence parsing
====================================

Learning about Pratt parsing.  (A similar, if not the same, technique is used
to parse Mercury terms, and is presumably common to Prolog systems.)

The toy grammar implemented is:

    e --> NAME
        ; INTEGER
        ; name = e
        ; e ? e : e
        ; prefixop e
        ; e postfixop
        ; e infixop e
        ; e ( args )
        ; ( e )

    args --> zero or more comma-separated e

    prefixop --> + ; - ; !

    postfixop --> !

    infixop --> + ; - ; * ; / ; ^

This is a better introduction than most:
[Pratt Parsers: Expression Parsing Made Easy][1]

[1]: http://journal.stuffwithstuff.com/2011/03/19/pratt-parsers-expression-parsing-made-easy/


Author
======

Peter Wang <novalazy@gmail.com>

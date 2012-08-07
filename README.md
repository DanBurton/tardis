# TardisT

The State monad allows you
to send information forwards to the future,
or to receive such information from the past.
The Reverse State monad allows you to do the reverse:
send information backwards to the past,
or receive information from the future. 

TardisT is a monad transformer
that provides state operations that allow you
to send information to both the future *and* the past,
as well as receive information from both directions.
It is isomorphic to a StateT on top of a ReverseStateT,
or vice-versa.

See Control/Monad/Tardis/Example.hs for an example.

----

You can decide how evil you would like this library to be.
By default, it will not include typeclass instances
that require "scary" language extensions.

    cabal install tardis

You can instead install it with the additional instances
by indicating that it is OK:

    cabal install -f use-undecidable-instances

See Control/Monad/Tardis.hs for details.

----

This was inspired by Luke Palmer's blog post on
the "reverse state monad".

http://lukepalmer.wordpress.com/2008/08/10/mindfuck-the-reverse-state-monad/

See also:

http://panicsonic.blogspot.com/2007/12/backwards-state-or-power-of-laziness.html

----

Currently, this is hugely lacking in test coverage and documentation.
I'll put some more effort into documentation soon,
but I'm not really sure how to test this other than by example.

# TardisT

![build status](https://github.com/DanBurton/tardis/actions/workflows/haskell.yml/badge.svg?branch=master)

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

See test/Example.hs for an example.

----

This was inspired by Luke Palmer's blog post on
the "reverse state monad".

http://lukepalmer.wordpress.com/2008/08/10/mindfuck-the-reverse-state-monad/

See also:

http://panicsonic.blogspot.com/2007/12/backwards-state-or-power-of-laziness.html

----

(c) 2012 Dan Burton

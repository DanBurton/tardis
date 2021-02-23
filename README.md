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

See test/Example.hs for an example.

----

You can decide how evil you would like this library to be.
By default, it will not include typeclass instances
that require "scary" language extensions.

    cabal install tardis

You can instead install it with the additional instances
by indicating that it is OK:

    cabal install -f use-undecidable-instances

You can tell whether you have the extra instances with
this ghci session:

    ghci> :m +Control.Monad.Tardis
    ghci> :m +Control.Monad.Trans.Identity
    ghci> :t IdentityT (return ()) >> getPast

If you *don't* have the additional instances, it will say the type is:

    (Monad m, MonadTardis bw b (IdentityT m)) => IdentityT m b

If you *do* have the additional instances, it will say the type is:

    MonadTardis bw b m => IdentityT m b


See Control/Monad/Tardis.hs for details.

----

This was inspired by Luke Palmer's blog post on
the "reverse state monad".

http://lukepalmer.wordpress.com/2008/08/10/mindfuck-the-reverse-state-monad/

See also:

http://panicsonic.blogspot.com/2007/12/backwards-state-or-power-of-laziness.html

----

(c) 2012 Dan Burton

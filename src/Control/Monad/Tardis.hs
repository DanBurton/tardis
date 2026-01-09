{-# OPTIONS_GHC -Wall #-}

-- | This module re-exports both 'MonadTardis' and 'TardisT'
-- (Wherever there is overlap, the 'MonadTardis' version is preferred.)
-- 
-- The recommended usage of a Tardis is to import this module.
module Control.Monad.Tardis
  ( -- * Re-exports
    module Control.Monad.Trans.Tardis
  , module Control.Monad.Tardis.Class

    -- * What is a Tardis?
    -- $whatis
    
    -- * How do you use a Tardis?
    -- $howuse
  ) where


import Control.Monad.Tardis.Class
import Control.Monad.Trans.Tardis
  ( TardisT
  , runTardisT
  , evalTardisT
  , execTardisT

  , Tardis
  , runTardis
  , evalTardis
  , execTardis
  
  , noState
  )


{- $whatis
    A Tardis is the combination of the State monad transformer
    and the Reverse State monad transformer.
    
    The State monad transformer features a forwards-traveling state.
    You can retrieve the current value of the state,
    and you can set its value, affecting any future attempts
    to retrieve it.

    The Reverse State monad transformer is just the opposite:
    it features a backwards-traveling state.
    You can retrieve the current value of the state,
    and you can set its value, affecting any /past/ attempts
    to retrieve it. This is a bit weirder than its
    forwards-traveling counterpart, so its Monad instance
    additionally requires that the underlying Monad it transforms
    must be an instance of MonadFix.

    A Tardis is nothing more than mashing these two things together.
    A Tardis gives you /two/ states: one which travels /backwards/
    (or /upwards/) through your code (referred to as @bw@),
    and one which travels /forwards/ (or /downwards/) through your code
    (referred to as @fw@). You can retrieve the current
    value of either state, and you can set the value of either state.
    Setting the forwards-traveling state will affect the /future/,
    while setting the backwards-traveling state will affect the /past/.
    Take a look at how Monadic bind is implemented for 'TardisT':

> m >>= f  = TardisT $ \ ~(bw, fw) -> mdo
>   (x,  ~(bw'', fw' )) <- runTardisT m (bw', fw)
>   (x', ~(bw' , fw'')) <- runTardisT (f x) (bw, fw')
>   return (x', (bw'', fw''))

    Like the Reverse State monad transformer, TardisT's Monad instance
    requires that the monad it transforms is an instance of MonadFix,
    as is evidenced by the use of @mdo@.
    Notice how the forwards-traveling state travels /normally/:
    first it is fed to @m@, producing @fw'@, and then it is fed to @f x@,
    producing @fw''@. The backwards-traveling state travels in the opposite
    direction: first it is fed to @f x@, producing @bw'@, and then
    it is fed to @m@, producing @bw''@.

-}

{- $howuse
    A Tardis provides four primitive operations,
    corresponding to the /get/ and /put/ for each of its two states.
    The most concise way to explain it is this:
    'getPast' retrieves the value from the latest 'sendFuture',
    while 'getFuture' retrieves the value from the next 'sendPast'.
    Beware the pitfall of performing send and get in the wrong order.
    Let's consider forwards-traveling state:

> do sendFuture "foo"
>    x <- getPast

    In this code snippet, @x@ will be @\"foo\"@, because 'getPast'
    grabs the value from the latest 'sendFuture'. If you wanted
    to observe that state /before/ overwriting it with @\"foo\"@,
    then re-arrange the code so that 'getPast' happens earlier
    than 'sendFuture'. Now let's consider backwards-traveling state:

> do x <- getFuture
>    sendPast "bar"

    In this code snippet, @x@ will be @\"bar\"@, because 'getFuture'
    grabs the value from the next 'sendPast'. If you wanted
    to observe that state /before/ overwriting it with @\"bar\"@,
    then re-arrange the code so that 'getFuture' happens later
    than 'sendPast'.

    TardisT is an instance of MonadFix. This is especially important
    when attempting to write backwards-traveling code, because
    the name binding occurs later than its usage.
    The result of the following code will be @(11, \"Dan Burton\")@.

> flip execTardis (10, "Dan") $ do
>   name <- getPast
>   sendFuture (name ++ " Burton")
>   rec
>     sendPast (score + 1)
>     score <- getFuture
>   return ()

    To avoid using @rec@, you may find 'modifyBackwards' to be useful.
    This code is equivalent to the previous example:

> flip execTardis (10, "Dan") $ do
>   modifyForwards (++ " Burton")
>   modifyBackwards (+ 1)

-}


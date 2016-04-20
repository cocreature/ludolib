
{-| Provides handy looping functions.
-}
module Control.Loops where

{-| Acts like a C-style for loop. You have to do the initializers on your
own. After that, you can call this with a "test", "body", and "after"
action. The test is performed first, and if it gives a True then the body
happens, the after happens, and it all loops back. Sure, your body could include
the after stuff, there's no literal reason that they need to be broken up, but
it makes it more C-like to have your body and updates separate.
-}
forLoop :: Monad m => m Bool -> m body -> m after -> m ()
forLoop test body after = do
    b <- test
    if b
        then body >> after >> forLoop test body after
        else return ()

{-| Like 'when' from Control.Monad, but instead of taking a Bool, it takes an
action that will produce a Bool. If the action produces True, it runs the
body, otherwise it does not run the body.
-}
whenM :: Monad m => m Bool -> m body -> m ()
whenM test body = do
    b <- test
    if b
        then body >> return ()
        else return ()

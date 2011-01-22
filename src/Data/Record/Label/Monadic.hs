{-# LANGUAGE TypeOperators, TypeSynonymInstances, TemplateHaskell #-}
module Data.Record.Label.Monadic
(
-- * Failure-handling monadic operations.
-- ...
-- * State and Reader Monad lens operations.
  getsL, putsL, modifiesL, (=:) 
, asksL, localL
-- ** Deprecated names for compatibility
, getM, setM, modM
, askM, localM
)
where

-- Brandon Simmons, 1/12/2011:
--- NOTES ON STATE/READER FUNCTION NAME CHANGES:
--        getsL, putsL, modifiesL, (=:) 
--      , asksL, localL
--
-- These names are better IMHO because:
--    1) the 'M' at the end of a function usually implies polymorphism 
--        over the Monad class not MonadState
--
--    2) In the same way that (:->) is meant to resemble (->) we can have
--        State monad functions share similar names:
--            getsL              <--->    gets 
--            :: (s :-> a) -> m a         :: (s -> a) -> m a
--
--            asksL              <--->    asks
--            :: (r :-> a) -> m a         :: (r -> a) -> m a
--        
--       Furthermore, the proposed name "putsL" mirrors what would be the
--       "put" equivalent of "gets". Only that function doesn't exist in 
--       the standard libraries (because it can't be easily defined without
--       something like 'fclabels'. Likewise for "modifiesL".
--
--    3) I would love to use the 'M' suffix for the functions that rely on 
--        the new MaybePoint constructor, and will have polymorphic Monad
--        return types.
--


import Control.Monad.State
import Control.Monad.Reader
import Data.Record.Label.Core

-- | Get a value out of state pointed to by the specified lens.

getsL :: MonadState s m => s :-> b -> m b
getsL = gets . getL

-- | Set a value somewhere in state pointed to by the specified lens.

putsL :: MonadState s m => s :-> b -> b -> m ()
putsL l = modify . setL l

-- | Alias for `putsL' that reads like an assignment.

infixr 7 =:
(=:) :: MonadState s m => s :-> b -> b -> m ()
(=:) = putsL

-- | Modify a value with a function somewhere in state pointed to by the
-- specified lens.

modifiesL :: MonadState s m => s :-> b -> (b -> b) -> m ()
modifiesL l = modify . modL l

-- | Fetch a value pointed to by a lens out of a reader environment.

asksL :: MonadReader r m => (r :-> b) -> m b
asksL = asks . getL

-- | Execute a computation in a modified environment. The lens is used to
-- point out the part to modify.

localL :: MonadReader r m => (r :-> b) -> (b -> b) -> m a -> m a
localL l = local . modL l
 
 
 ------------ OLD NAMES FOR BACKWORDS COMPATIBILITY? ----------

-- | DEPRECATED. use `getsL`.

getM :: MonadState s m => s :-> b -> m b
getM = getsL

-- | DEPRECATED. use `putsL`.

setM :: MonadState s m => s :-> b -> b -> m ()
setM = putsL

-- | DEPRECATED. use `modifiesL`.

modM :: MonadState s m => s :-> b -> (b -> b) -> m ()
modM = modifiesL

-- | DEPRECATED. use `asksL`.

askM :: MonadReader r m => (r :-> b) -> m b
askM = asksL

-- | DEPRECATED. use `localL`.

localM :: MonadReader r m => (r :-> b) -> (b -> b) -> m a -> m a
localM = localL


{-
 ------------ FAILURE-HANDLING MONADIC FUNCTIONS --------------

---- These functions can be used to catch errors such as occur when using
---- a lens on the wrong constructor of a multi-constructor type. Currently 
---- we return results in polymorphic Monad class, rather than just Maybe,
---- because:
--       1) it is more general and possibly useful
--       2) it mirrors the liftML function (sister to fmapL but supporting 
--           our failure-handling functions) 

-- | Get a modifier function from a lens that returns its value in a monad.
-- If a lens function fails we call the monad 'fail' method.

modLM :: (Monad m)=> (f :-> a) -> (a -> a) -> f -> m f
modLM l m = _liftMaybe . _maybeMod (unLens l) (return . m)


-- | Lift a lens into the Monad class. Any failures raised by lenses using the
-- MaybePoint constructor will call the monad's 'fail' method.

liftML :: Monad m => (a :-> b) -> m a :-> m b
liftML = undefined

-}

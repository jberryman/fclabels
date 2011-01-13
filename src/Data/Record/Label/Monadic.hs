{-# LANGUAGE TypeOperators, TypeSynonymInstances, TemplateHaskell #-}
module Data.Record.Label.Monadic
(
-- * Monadic lens operations.
  getM, setM, modM, (=:)
, askM, localM
)
where

-- Brandon Simmons, 1/12/2011:
--- PROPOSED NAME CHANGES:
--     getsL, putsL, modifiesL, (=:) 
--   , asksL, localL
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

getM :: MonadState s m => s :-> b -> m b
getM = gets . getL

-- | Set a value somewhere in state pointed to by the specified lens.

setM :: MonadState s m => s :-> b -> b -> m ()
setM l = modify . setL l

-- | Alias for `setM' that reads like an assignment.

infixr 7 =:
(=:) :: MonadState s m => s :-> b -> b -> m ()
(=:) = setM

-- | Modify a value with a function somewhere in state pointed to by the
-- specified lens.

modM :: MonadState s m => s :-> b -> (b -> b) -> m ()
modM l = modify . modL l

-- | Fetch a value pointed to by a lens out of a reader environment.

askM :: MonadReader r m => (r :-> b) -> m b
askM = asks . getL

-- | Execute a computation in a modified environment. The lens is used to
-- point out the part to modify.

localM :: MonadReader r m => (r :-> b) -> (b -> b) -> m a -> m a
localM l f = local (modL l f)


{-# LANGUAGE TypeOperators, TypeSynonymInstances, TemplateHaskell #-}
module Data.Record.Label.Monadic
(
-- * Monadic lens operations.
  getM, setM, modM, (=:)
, askM, localM
)
where

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


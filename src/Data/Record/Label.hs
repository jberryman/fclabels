module Data.Record.Label
(
-- * Lens types.
  Point (Point,MaybePoint)
, (:->) (Lens)
, lens, maybeLens
, getL, setL, modL

, fmapL

-- * Bidirectional functor.
, (:<->:) (..)
, Iso (..)
, lmap
, for

-- * Monadic lens operations.
-- ** Failure-handling monadic operations.
, getML, setML, modML
, liftML 
-- ** State and Reader Monad lens operations.
, getsL, putsL, modifiesL, (=:) 
, asksL, localL
-- *** Deprecated names for compatibility
, getM, setM, modM
, askM, localM

-- * Derive labels using Template Haskell.
, module Data.Record.Label.TH
)
where

import Data.Record.Label.Core
import Data.Record.Label.Monadic
import Data.Record.Label.TH

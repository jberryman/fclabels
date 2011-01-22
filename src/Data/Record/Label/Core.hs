{-# LANGUAGE TypeOperators #-}
module Data.Record.Label.Core where

import Prelude hiding ((.), id)
import Control.Applicative
import Control.Category
import Control.Monad.State
import Control.Monad.Reader

data Point f i o = Point 
                       { _get :: f -> o
                       , _set :: i -> f -> f }
                 -- Multi-constructor types will generate lenses built
                 -- with this constructor:
                 | MaybePoint
                       { _getSafe :: f -> Maybe o
                       , _setSafe :: i -> f -> Maybe f }

-- defined in such a way to be general enough to use in Category instance
-- declaration and in `modLM`:
_maybeMod :: Point f i o -> (o -> Maybe i) -> f -> Maybe f
_maybeMod (MaybePoint g s) f a = flip s a =<< (f =<< g a) 
_maybeMod p f a                = _maybeMod (_toMaybePoint p) f a

_mod :: Point f i o -> (o -> i) -> f -> f
_mod (Point g s) f a = s (f (g a)) a
_mod mp          f a = _mod (_toPoint mp) f a

newtype (f :-> a) = Lens { unLens :: Point f a a }

-- | Create a lens out of a simple getter and setter. See also `maybeLens`.

lens :: (f -> a) -> (a -> f -> f) -> f :-> a
lens g s = Lens (Point g s)

-- | Get a simple getter function from a lens. See also `safeGetL`.

getL :: (f :-> a) -> f -> a
getL = _get . _toPoint . unLens

-- | Get a simple setter function from a lens. See also `safeSetL`.

setL :: (f :-> a) -> a -> f -> f
setL = _set . _toPoint . unLens

-- | Get a simple modifier function from a lens. See also `modLM`.

modL :: (f :-> a) -> (a -> a) -> f -> f
modL = _mod . unLens

instance Category (:->) where
  id = lens id const
  -- compose two error-handling Point types, threading Maybes
  Lens (MaybePoint g s) . Lens b@(MaybePoint g' s') = 
        maybeLens (\f-> g =<< g' f) (_maybeMod b . s)
  -- etc.
  Lens (Point g s) . Lens b@(MaybePoint g' s') = 
        maybeLens (fmap g . g') (\i-> _maybeMod b (return . s i))
  -- 
  Lens (MaybePoint g s) . Lens b = 
        maybeLens (g . _get b) (_maybeMod b . s)
  -- original definition:
  Lens a . Lens b = lens (_get a . _get b) (_mod b . _set a)

instance Functor (Point f i) where
  fmap f (Point g s) = Point (f . g) s
  fmap f (MaybePoint g s) = MaybePoint (fmap f . g) s


instance Applicative (Point f i) where
  pure a = Point (const a) (const id)
  -- MaybePoint & Point mix in the same manner as in Category above
  (MaybePoint g s) <*> (MaybePoint g' s') = 
      MaybePoint (\f -> g f <*> g' f)  (\r f-> s' r =<< s r f)
  --
  (Point g s) <*> (MaybePoint g' s') = 
      MaybePoint (\f -> pure (g f) <*> g' f)  (\r-> s' r . s r)
  --
  (MaybePoint g s) <*> (Point g' s') = 
      MaybePoint (\f -> g f <*> pure (g' f)) (\r-> fmap (s' r) . s r)
  -- original definition:
  a <*> b = Point (_get a <*> _get b) (\r -> _set b r . _set a r)


-- | Lift a lens into the Applicative class. Failure-handling with the 
-- MaybePoint type is abandoned. See also `liftML`.

fmapL :: Applicative f => (a :-> b) -> f a :-> f b
fmapL (Lens (Point g s)) = lens (fmap g) (\x f -> s <$> x <*> f)
fmapL (Lens mp) = fmapL (Lens $ _toPoint mp) -- SEEMS TO BE NO WAY TO WRAP
                                             -- MAYBE AROUND APPLICATIVE HERE

--
-- TODO: CONSIDER MAKING A MONADIC/FAILURE-HANDLING BIDIRECTIONAL FUNCTOR 
--       CALLED  (:<~>:) 
--

-- | This isomorphism type class is like a `Functor' but works in two directions.

class Iso f where
  (%) :: a :<->: b -> f a -> f b

-- | The bijections datatype, a function that works in two directions. 

infixr 7 :<->:
data a :<->: b = (:<->:) { fw :: a -> b, bw :: b -> a }

-- | Constructor for bijections.

instance Category (:<->:) where
  id = id :<->: id
  (a :<->: b) . (c :<->: d) = a . c :<->: d . b

infixr 8 %

instance Iso ((:->) i) where
  l % Lens (MaybePoint g s) = maybeLens (fmap (fw l) . g) (s . bw l)
  l % Lens a = lens (fw l . _get a) (_set a . bw l)

instance Iso ((:<->:) i) where
  (%) = (.)

lmap :: Functor f => (a :<->: b) -> f a :<->: f b 
lmap l = let a :<->: b = l in fmap a :<->: fmap b

dimap :: (o' -> o) -> (i -> i') -> Point f i' o' -> Point f i o
dimap fo fi (MaybePoint g s) = MaybePoint (fmap fo . g) (s . fi)
dimap fo fi l = Point (fo . _get l) (_set l . fi)

-- | Combine a partial destructor with a lens into something easily used in the
-- applicative instance for the hidden `Point' datatype. Internally uses the
-- covariant in getter, contravariant in setter bi-functioral-map function.
-- (Please refer to the example because this function is just not explainable
-- on its own.)

for :: (i -> o) -> (f :-> o) -> Point f i o
for a b = dimap id a (unLens b)

 
 ------------  Point TYPE AND Maybe CONVERSIONS  --------------

-- Point type conversions, wrapping non-error-catching functions in the Maybe 
-- type, or discarding Maybe wrappers in the MaybePoint constructor, raising
-- an error on Nothing:
_toPoint :: Point f i o -> Point f i o
_toPoint (MaybePoint g s) = Point (_maybeGetErr . g) (\i-> _maybeSetErr . s i)
_toPoint p                = p

_toMaybePoint :: Point f i o -> Point f i o
_toMaybePoint (Point g s) = MaybePoint (return . g) (\i-> return . s i)
_toMaybePoint mp          = mp


-- Like fromJust but with a more appropriate error message for lens failures:
_maybeGetErr, _maybeSetErr :: Maybe f -> f
_maybeGetErr = _maybeErr "Getter function failed."
_maybeSetErr = _maybeErr "Setter function failed."
_maybeErr str = maybe (error str) id

-- lifts Maybe into Monad class:
_liftMaybe :: (Monad m)=> Maybe a -> m a
_liftMaybe = maybe (fail "A lens function failed.") return


 ------------  NEW FAILURE-HANDLING-SUPPORTING FUNCTIONS  --------------

-- | Create an error handling lens. Useful for multi-constructor types or 
-- other situations where a setter or getter might fail.

maybeLens :: (f -> Maybe a) -> (a -> f -> Maybe f) -> f :-> a
maybeLens g s = Lens (MaybePoint g s)



 ------------ MISCELLENEOUS / DEMO FUNCTIONS --------------

-- | Given a predicate and a lens, returns a new lens that uses MaybePoint
-- internally. When used with the monadic setter and getter functions, the
-- operation will fail if the predicate is not satisfied. When used with 
-- `getL` and `setL` an error will be raised if the predicate returns False.

--validateWith :: (b -> Bool) -> (a :-> b) -> (a :-> b)
--validateWith p l = undefined --todo later


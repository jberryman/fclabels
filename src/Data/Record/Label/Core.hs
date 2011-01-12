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
                 | MaybePoint
                       { _getSafe :: f -> Maybe o
                       , _setSafe :: i -> f -> Maybe f }

-- defined in such a way to be general enough to use in Category instance
-- declaration and in `safeModL`:
_maybeMod :: Point f i o -> (o -> Maybe i) -> f -> Maybe f
_maybeMod (MaybePoint g s) f a = flip s a =<< (f =<< g a) 
_maybeMod p f a                = _maybeMod (_toMaybePoint p) f a

_mod :: Point f i o -> (o -> i) -> f -> f
_mod (Point g s) f a = s (f (g a)) a
_mod mp          f a = _mod (_toPoint mp) f a

newtype (f :-> a) = Lens { unLens :: Point f a a }

-- | Create a lens out of a simple getter and setter. see also `safeLens`.

lens :: (f -> a) -> (a -> f -> f) -> f :-> a
lens g s = Lens (Point g s)

-- | Get the getter function from a lens.

getL :: (f :-> a) -> f -> a
getL = _get . _toPoint . unLens

-- | Get the setter function from a lens.

setL :: (f :-> a) -> a -> f -> f
setL = _set . _toPoint . unLens

-- | Get a non-failure-handling modifier function from a lens. See also 
-- `safeModL`.

modL :: (f :-> a) -> (a -> a) -> f -> f
modL = _mod . unLens

instance Category (:->) where
  id = lens id const
  -- compose two error-handling Point types, threading Maybes
  Lens (MaybePoint g s) . Lens b@(MaybePoint g' s') = 
        safeLens (\f-> g =<< g' f) (_maybeMod b . s)
  -- etc.
  Lens (Point g s) . Lens b@(MaybePoint g' s') = 
        safeLens (fmap g . g') (\i-> _maybeMod b (return . s i))
  -- 
  Lens (MaybePoint g s) . Lens b = 
        safeLens (g . _get b) (_maybeMod b . s)
  -- original definition:
  Lens a . Lens b = lens (_get a . _get b) (_mod b . _set a)

instance Functor (Point f i) where
  fmap f (Point g s) = Point (f . g) s
  fmap f (MaybePoint g s) = MaybePoint (fmap f . g) s


-- TODO: THESE NEED TO BE DOUBLE-CHECKED:
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

fmapL :: Applicative f => (a :-> b) -> f a :-> f b
fmapL l = lens (fmap (getL l)) (\x f -> setL l <$> x <*> f)

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
  l % Lens a = lens (fw l . _get a) (_set a . bw l)

instance Iso ((:<->:) i) where
  (%) = (.)

lmap :: Functor f => (a :<->: b) -> f a :<->: f b 
lmap l = let a :<->: b = l in fmap a :<->: fmap b

dimap :: (o' -> o) -> (i -> i') -> Point f i' o' -> Point f i o
dimap f g l = Point (f . _get l) (_set l . g)

-- | Combine a partial destructor with a lens into something easily used in the
-- applicative instance for the hidden `Point' datatype. Internally uses the
-- covariant in getter, contravariant in setter bi-functioral-map function.
-- (Please refer to the example because this function is just not explainable
-- on its own.)

for :: (i -> o) -> (f :-> o) -> Point f i o
for a b = dimap id a (unLens b)


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


 ------------ NEW SAFE FUNCTIONS (NOT YET EXPORTED) --------------

-- | Create an error handling lens. Useful for multi-constructor types or 
-- other situations where  a setter or getter might fail.

safeLens :: (f -> Maybe a) -> (a -> f -> Maybe f) -> f :-> a
safeLens g s = Lens (MaybePoint g s)


-- | Get a modifier function from a lens that catches errors of lens 
-- composition in the Maybe type.

safeModL :: (f :-> a) -> (a -> a) -> f -> Maybe f
safeModL l m = _maybeMod (unLens l) (return . m)

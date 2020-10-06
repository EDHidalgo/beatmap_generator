{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}

module BeatMapGenTypes where

import           Data.Coerce  (coerce)
import           Data.Proxy
import           GHC.TypeLits (KnownNat, Nat, natVal)


{- TRACKED DATA -}

-- newtype Tracker tS s f tE e = Tracker (Song tS s, FrameNum f, SongEvents tE e)
--   deriving stock (Show)


{- SONG DATA -}

-- TODO : Change to use coerce
newtype Song t s = Song { unSong :: t s }
  deriving stock (Show)
  deriving newtype (Functor, Applicative, Monad, Bounded, Enum, Eq, Semigroup, Monoid)


{- FRAME -}

class FrameCounter c s f | c s -> f where
  getEventTime :: KnownNat s => c s -> f
  incFrame :: c s -> c s

{- Frame Counter

Frame counter converts frame to time in seconds by using the skip counter 's'
-}

instance (KnownNat s) => FrameCounter Frames s Int where
  {-# INLINE getEventTime #-}
  getEventTime (Frames f) = f * fromIntegral (natVal (Proxy @s))

  {-# INLINE incFrame #-}
  incFrame = succ . coerce

newtype Frames (s :: Nat) = Frames Int
  deriving stock (Show)
  deriving newtype (Enum)


{- SONG EVENTS -}

newtype SongEvents t e = SongEvents { unEvents :: t e }
  deriving stock (Show)
  deriving newtype (Functor, Applicative, Monad, Bounded, Enum, Eq, Semigroup, Monoid)

class (Foldable t, Traversable t) => SongManip t where
  splitSections :: t s -> ((s, s), c s)
  songIsOver :: t s -> Bool

class (Traversable t) => EventLog t where
  appendEvent :: s -> t s
  allEvents :: t s


{- GAME TYPES -}

-- TODO : Add difficulties (more rails)
data Rails = W | D | L | R
  deriving stock (Bounded, Enum, Show)

{-# INLINE mkRB #-}
mkRB :: (Eq r, Ord r, Fractional r, RealFrac r) => r -> Rails
mkRB pct = toEnum $ ceiling pct `mod` fromEnum (maxBound @Rails)


{- Osu style buttons -}
data Osu = PlaceholderA | PlaceholderB
  deriving stock (Show)

mkOsu :: a -> Osu
mkOsu _ = PlaceholderA


{- Event container -}
data Event = Event { actTime    :: Int
                   , eventRails :: Rails
                   , eventOsu   :: Osu  }
  deriving (Show)

genBuilder :: Num n => n -> Int -> Event
genBuilder secRes frame = Event { actTime    = undefined frame
                                , eventRails = undefined
                                , eventOsu   = mkOsu secRes }

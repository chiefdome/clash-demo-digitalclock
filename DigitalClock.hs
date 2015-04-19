{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE ScopedTypeVariables        #-}
module Main where

import           Prelude                 () -- no implicit Prelude!
import           Control.Applicative
import           Control.Arrow
--import           Data.Foldable(toList)
import qualified Data.List as List
import           Data.Traversable

import           Language.Literals.Binary
import           CLaSH.Prelude
import           CLaSH.Sized.Vector
import           CLaSH.Signal
import           CLaSH.Signal.Explicit
import           CLaSH.Signal.Bundle

-- | Clock implicit
every :: (Num t, Eq t) => t -> Unbundled Bool -> Unbundled Bool
every x = fsm <^> 0
  where
    fsm _  True          = (0,    False)
    fsm st _   | x == st = (0,    True )
    fsm st _             = (st+1, False)

type Word17 = Unsigned 17

everySecond :: Signal Bool -- ^ trigger every one second
everySecond  = counter' fpgaFrequency
-- $ (2 :: Word17) ^ (15 :: Word17) * (1000 :: Word17) -- or Signed 16?

-- using 32 bit arithmetic is a bit lazy
fpgaFrequency :: Unsigned 27
--fpgaFrequency  = 1000 -- reduced for debugging
fpgaFrequency  = 32768000 -- real 32MHz

-- TODO: These have extra clocks!
counter      :: (Num s, Eq s) =>
                 s            -> -- ^ number of clock cycles before overflow and reset
                 Signal Bool  -> -- ^ trigger for clock cycle (not the clock domain!)
                 Unbundled (s, Bool)
counter limit = fsm <^> 0
  where
    fsm st False              = (st,   (st,   False))
    fsm st True | limit == st = (0,    (0,    True ))
    fsm st True               = (st+1, (st+1, False))

counter'      :: (Num s, Eq s) =>
                  s            -> -- ^ number of clock cycles before overflow and reset
                  Signal Bool
counter' limit = (fsm <^> 0) $ signal ()
  where
    fsm st () | limit == st = (0,    True)
              | otherwise   = (st+1, False)

mapper ::  Applicative f => (a1 -> a -> b) -> (f a1, f a) -> f b
mapper f (a, b) = f <$> a <*> b
-- Q: do we need to add clock everywhere?

-- | Given an input clock, this clock gives four digits of the clock
-- for minutes and seconds.
hourClock :: (Num a, Eq a) =>
              Signal (Vec 4 a)
hourClock = bundle (  secondsCounter
                   :> tenSecondsCounter
                   :> minutesCounter
                   :> tenMinutesCounter
                   :> Nil )
  where
    secondPulse                         = counter' 32768 -- $ fpgaFrequency
    (secondsCounter,    tenSecondPulse) = counter 10 secondPulse
    (tenSecondsCounter, minutePulse   ) = counter 6  tenSecondPulse
    (minutesCounter,    tenMinutePulse) = counter 10 minutePulse
    (tenMinutesCounter, _hourPulse    ) = counter 6  tenMinutePulse
    -- (hoursCounter,      _             ) = counter 24 (hourPulse,      rst)

newtype BCDDigit = BCDDigit { bcdDigit :: Unsigned 4 }
  deriving (Eq, Ord, Enum, Bounded, Show, Num)

newtype SevenSegDigit = SevenSegDigit { ssDigit :: Unsigned 7 }
  deriving (Eq, Ord, Enum, Bounded, Show, Num)

-- | Encoding a BCD digit to seven segment display with active-low.
sevenSegmentDigit :: BCDDigit -> SevenSegDigit
sevenSegmentDigit 0 = [b| 1000000 |]
sevenSegmentDigit 1 = [b| 1111001 |]
sevenSegmentDigit 2 = [b| 0100100 |]
sevenSegmentDigit 3 = [b| 0110000 |]
sevenSegmentDigit 4 = [b| 0011001 |]
sevenSegmentDigit 5 = [b| 0010010 |]
sevenSegmentDigit 6 = [b| 0000010 |]
sevenSegmentDigit 7 = [b| 1111000 |]
sevenSegmentDigit 8 = [b| 0000000 |]
sevenSegmentDigit 9 = [b| 0010000 |]
sevenSegmentDigit _ = [b| 1111111 |] -- empty


hz1000 :: Signal Bool
hz1000 = counter' (1 :: Unsigned 2) -- (32768 :: Unsigned 17)
-- ((2^15) :: Word17)

-- | Interface to seven segment display.
data SevenSegmentDisplay n = SevenSegmentDisplay {
                               anodeIndex   :: Unsigned n    -- ^ Anode index
                             , currentDigit :: SevenSegDigit -- ^ Seven segment signal for th current anode
                             }
 deriving (Show)

instance (KnownNat n) => Bundle (SevenSegmentDisplay n) where
  type Unbundled' t (SevenSegmentDisplay n) = (Signal' t (Unsigned n),
                                               Signal' t (SevenSegDigit))
  unbundle' _  s                                = (anodeIndex <$> s, currentDigit <$> s)
  bundle'   _ (anode,                    digit) = SevenSegmentDisplay <$> anode <*> digit

digitAnode 0 = [b|0001|]
digitAnode 1 = [b|0010|]
digitAnode 2 = [b|0100|]
digitAnode 3 = [b|1000|]

-- | Given an array of numbers and clock for changing anode state,
-- drives @SevenSegmentDisplay@ interface.
sevenSegmentDisplay :: KnownNat n              =>
                       Signal (Vec n BCDDigit) ->
                       Signal (SevenSegmentDisplay n)
sevenSegmentDisplay digits = bundle (whichDigit,
                                         sevenSegmentDigit <$> currentDigit)
  where
    (whichDigit, _) = counter (myMaxIndex $ unbundle digits) hz1000
    rst             = signal False -- no resetting for now...
    currentDigit   :: Signal BCDDigit
    currentDigit    = (!!) <$> digits <*> whichDigit --(!!) <$> digits <*> whichDigit
    myMaxIndex     :: (KnownNat n) => Unbundled (Vec n a) -> Unsigned n
    myMaxIndex      = fromIntegral . maxIndex

-- | Top entity to implement
topEntity ::  Signal (SevenSegmentDisplay 4)
topEntity  = sevenSegmentDisplay hourClock 

takeEvery :: Int -> [a] -> [a]
takeEvery n = go
  where
    go []     = []
    go (b:bs) = b:go (List.drop (fromIntegral n) bs)

main = print $ takeEvery ((fromIntegral fpgaFrequency `div` 10)) $ sampleN (10*fromIntegral fpgaFrequency) topEntity 

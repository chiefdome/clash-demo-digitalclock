{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module DigitalClock where

import Prelude                 () -- no implicit Prelude!
import Control.Applicative
import Control.Arrow
import Data.Traversable

import Language.Literals.Binary
import CLaSH.Prelude
import CLaSH.Sized.Vector
import CLaSH.Signal
import CLaSH.Signal.Explicit
import CLaSH.Signal.Bundle

-- | Clock implicit
every :: (Num t, Eq t) => t -> Unbundled Bool -> Unbundled Bool
every x = fsm <^> 0
  where
    fsm _  True          = (0,    False)
    fsm st _   | x == st = (0,    True )
    fsm st _             = (st+1, False)

type Word17 = Unsigned 17

everySecond :: Signal Bool -- ^ reset signal
            -> Signal Bool -- ^ trigger every one second
everySecond = every fpgaFrequency
-- $ (2 :: Word17) ^ (15 :: Word17) * (1000 :: Word17) -- or Signed 16?

-- using 32 bit arithmetic is a bit lazy
fpgaFrequency :: Unsigned 27
fpgaFrequency  = 32768000

-- TODO: These have extra clocks!
counter      :: (Num s, Eq s) =>
                 s            -> -- ^ number of clock cycles before overflow and reset
                 Signal Bool  -> -- ^ counter clock (not the clock domain!)
                 Signal Bool  -> -- ^ reset
                 Unbundled (s, Bool)
counter limit = curry (fsm <^> 0)
  where
    fsm st (_    , True)               = (0,    (st,   False))
    fsm st (False, _   )               = (0,    (st,   False))
    fsm st (_    , _   ) | limit == st = (0,    (0,    True ))
    fsm st (_    , _   )               = (st+1, (st+1, False))

mapper ::  Applicative f => (a1 -> a -> b) -> (f a1, f a) -> f b
mapper f (a, b) = f <$> a <*> b
-- Q: do we need to add clock everywhere?

-- | Given an input clock, this clock gives four digits of the clock
-- for minutes and seconds.
hourClock :: (Num a, Eq a) =>
              Signal Bool  ->
              Signal (Vec 4 a)
hourClock rst = bundle (  secondsCounter
                       :> tenSecondsCounter
                       :> minutesCounter
                       :> tenMinutesCounter
                       :> Nil )
  where
    secondPulse                         = everySecond rst
    (secondsCounter,    tenSecondPulse) = counter 10 secondPulse    rst
    (tenSecondsCounter, minutePulse   ) = counter 6  tenSecondPulse rst
    (minutesCounter,    tenMinutePulse) = counter 10 minutePulse    rst
    (tenMinutesCounter, _hourPulse    ) = counter 6  tenMinutePulse rst
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


hz1000 :: Signal Bool -> Signal Bool
hz1000 = every (32768 :: Unsigned 17)
 -- ((2^15) :: Word17)


-- | Interface to seven segment display.
data SevenSegmentDisplay n = SevenSegmentDisplay {
                               anodeIndex   :: Unsigned n    -- ^ Anode index
                             , currentDigit :: SevenSegDigit -- ^ Seven segment signal for th current anode
                             }

instance (KnownNat n) => Bundle (SevenSegmentDisplay n) where
  type Unbundled' t (SevenSegmentDisplay n) = (Signal' t (Unsigned n),
                                               Signal' t (SevenSegDigit))
  unbundle' _  s                                = (anodeIndex <$> s, currentDigit <$> s)
  bundle'   _ (anode,                    digit) = SevenSegmentDisplay <$> anode <*> digit

-- | Given an array of numbers and clock for changing anode state,
-- drives @SevenSegmentDisplay@ interface.
sevenSegmentDisplay :: KnownNat n              =>
                       Signal (Vec n BCDDigit) ->
                       Signal Bool             ->
                       Signal (SevenSegmentDisplay n)
sevenSegmentDisplay digits clk = bundle (15, -- whichDigit,
                                         sevenSegmentDigit <$> currentDigit)
  where
    (whichDigit, _) = counter (fromIntegral $ maxIndex $ unbundle digits) clk rst
    rst             = signal False -- no resetting for now...
    currentDigit   :: Signal BCDDigit
    currentDigit    = (!!0 ) <$> digits -- <*> whichDigit --(!!) <$> digits <*> whichDigit


-- | Top entity to implement
topEntity    ::  Signal Bool -> Signal (SevenSegmentDisplay 4)
topEntity clk = sevenSegmentDisplay (hourClock clk) (hz1000 clk)


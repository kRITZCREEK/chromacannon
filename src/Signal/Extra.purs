module Signal.Extra where

import Prelude
import Signal
import Signal.Time

-- |Creates a signal which only yields values equal to the specified value.
whenEqual :: forall a. (Eq a) => a -> Signal a -> Signal a
whenEqual value input = filter ((==) value) value input

-- |Creates a signal which only yields a single value any time the given
-- |signal changes to a particular value from a non-equal value.
whenChangeTo :: forall a. (Eq a) => a -> Signal a -> Signal a
whenChangeTo value input = whenEqual value $ dropRepeats input

-- |Takes a signal and a time value, and creates a signal which waits to yield
-- |the next result until the specified amount of time has elapsed. It then
-- |yields only the newest value from that period. New events during the debounce
-- |period reset the delay.
debounce :: forall a. Time -> Signal a -> Signal a
debounce t s =
  let leading = whenChangeTo false $ since t s
  in sampleOn leading s

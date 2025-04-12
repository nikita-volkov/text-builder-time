module TextBuilderTime.Iso8601
  ( day,
    utcTime,
  )
where

import Data.Time
import TextBuilder
import Prelude

-- |
-- Convert 'Day' to ISO-8601 format.
--
-- >>> day (fromGregorian 2022 6 16)
-- "2022-06-16"
{-# INLINE day #-}
day :: Day -> TextBuilder
day (toGregorian -> (year, month, day)) =
  mconcat
    [ fixedLengthDecimal 4 year,
      "-",
      fixedLengthDecimal 2 month,
      "-",
      fixedLengthDecimal 2 day
    ]

-- |
-- Convert 'UTCTime' to ISO-8601 format.
--
-- >>> utcTime (UTCTime (fromGregorian 2022 6 16) 0)
-- "2022-06-16T00:00:00Z"
--
-- >>> utcTime (UTCTime (fromGregorian 2022 6 16) 0.000001)
-- "2022-06-16T00:00:00.000001Z"
--
-- >>> utcTime (UTCTime (fromGregorian 2022 6 16) 0.000000001)
-- "2022-06-16T00:00:00.000000001Z"
--
-- >>> utcTime (UTCTime (fromGregorian 2022 6 16) (3 * 60 * 60 + 4 * 60 + 5))
-- "2022-06-16T03:04:05Z"
{-# INLINE utcTime #-}
utcTime :: UTCTime -> TextBuilder
utcTime UTCTime {..} =
  let picoseconds = diffTimeToPicoseconds utctDayTime
      (seconds, picosecond) = divMod picoseconds 1_000_000_000_000
      seconds' = fromInteger seconds :: Int
      (dayMinutes, second) = divMod seconds' 60
      (hour, minute) = divMod dayMinutes 60
   in mconcat
        [ day utctDay,
          "T",
          fixedLengthDecimal 2 hour,
          ":",
          fixedLengthDecimal 2 minute,
          ":",
          fixedLengthDecimal 2 second,
          picosecondsSubsecondsComponent (fromIntegral picosecond),
          "Z"
        ]

-- |
-- Subseconds component of the ISO-8601 format compiled from picoseconds.
--
-- >>> picosecondsSubsecondsComponent 0
-- ""
--
-- >>> picosecondsSubsecondsComponent 000_000_000_001
-- ".000000000001"
--
-- >>> picosecondsSubsecondsComponent 000_000_000_010
-- ".00000000001"
--
-- >>> picosecondsSubsecondsComponent 100_000_000_000
-- ".1"
--
-- __WARNING__. This function is only supposed to be used internally, because it doesn't check for the number overflowing the picoseconds range:
-- >>> picosecondsSubsecondsComponent 100_000_000_000_000
-- "."
{-# INLINE picosecondsSubsecondsComponent #-}
picosecondsSubsecondsComponent ::
  -- | Picoseconds.
  Int ->
  TextBuilder
picosecondsSubsecondsComponent =
  skipTrail 12
  where
    skipTrail pos val =
      if val == 0
        then
          mempty
        else case divMod val 10 of
          (quotient, remainder) ->
            if remainder == 0
              then
                skipTrail (pred pos) quotient
              else
                "." <> fixedLengthDecimal pos val

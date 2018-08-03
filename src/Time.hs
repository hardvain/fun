module Time where

import qualified Data.Time.Clock.POSIX as Time
import Data.Ratio


timeInMicros :: IO Integer
timeInMicros = numerator . toRational . (* 1000000) <$> Time.getPOSIXTime

timeInMillis :: IO Integer
timeInMillis = (`div` 1000) <$> timeInMicros

timeInSeconds :: IO Integer
timeInSeconds = (`div` 1000) <$> timeInMillis

timeInSeconds' :: IO Double
timeInSeconds' = (/ 1000000) . fromIntegral <$> timeInMicros
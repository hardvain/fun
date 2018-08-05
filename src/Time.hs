module Time where

import qualified Data.Time.Clock.POSIX as Time
import Data.Ratio

timeInMicros :: IO Int
timeInMicros = fromIntegral . numerator . toRational . (* 1000000) <$> Time.getPOSIXTime

timeInMillis :: IO Int
timeInMillis = (`div` 1000) <$> timeInMicros

timeInSeconds :: IO Int
timeInSeconds = (`div` 1000) <$> timeInMillis

timeInSeconds' :: IO Double
timeInSeconds' = (/ 1000000) . fromIntegral <$> timeInMicros

elapsedTimeFrom :: Int -> IO Int
elapsedTimeFrom from = do
  currentTime <- timeInMillis
  return $ currentTime - from
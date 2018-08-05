module Animation where
import Ease
data AnimationTarget = PositionX | PositionY | PositionZ
                      | RotationX | RotationY | RotationZ
                      | ScaleX | ScaleY | ScaleZ 

data Animation = Animation {
  target :: AnimationTarget,
  from :: Float,
  to :: Float,
  function :: EasingFunction,
  startTime :: Int,
  duration :: Int
}

runAnimation :: Animation -> FrameNumber -> MillisElapsed -> Float
runAnimation animation@(Animation _ from _ _ startTime duration) frameNumber millisElapsed = if shouldParticipate
  then calculateValue animation frameNumber millisElapsed 
  else from
    where 
       shouldParticipate = millisElapsed > startTime && millisElapsed < (startTime + duration)

calculateValue :: Animation -> FrameNumber -> MillisElapsed -> Float
calculateValue animation@(Animation target from to function startTime duration) frameNumber millisElapsed = from + delta
  where 
    elapsedValue = millisElapsed - startTime
    normalisedProgress = (fromIntegral elapsedValue) / (fromIntegral duration)
    easedValue = bounceInOut normalisedProgress
    valueDifference = to - from
    delta = valueDifference * easedValue

{-
  x
  from 
  to
  fromTime
  duration
  function
-}
module AST where

import Shape 
import Ease

data Position = Position Float Float Float
data Rotation = Rotation Float Float Float
data Scale = Scale Float Float Float

type FrameNumber = Int
type MillisElapsed = Int

type Animation a = (FrameNumber, MillisElapsed) -> a

data EasingFunction = BounceOut | BounceIn | BounceInOut

data EasingState = EasingState {
  easingFunction :: EasingFunction,
  activeTime :: Maybe (Int, Int),
  activeFrame :: Maybe (Int, Int),
  from :: Float,
  to :: Float
}
{-
  Check if current current framenumber is inbetween active frame & active frame + duration / or do the same for time
    if yes,
      Get the elapsed value by subtracting the current value - start value
      Convert elapsed value to a range from 0 to 1
      pass the result to easing function and get the result 
      get the difference between from and to, multiply it by above result and add to from. The resulting value is the final value
-}
getValue :: FrameNumber -> MillisElapsed -> EasingState -> Float
getValue frameNumber millisElapsed state@(EasingState _ (Just (startTime, duration)) _ from _ ) = 
  if shouldParticipate
    then applyEasing frameNumber millisElapsed state
    else from
  where 
    shouldParticipate = millisElapsed > startTime && millisElapsed < (startTime + duration)
    

applyEasing :: FrameNumber -> MillisElapsed -> EasingState -> Float
applyEasing fromeNumber millisElapsed state@(EasingState easingFunction (Just (startTime, duration)) _ from to ) = from + delta
  where 
    elapsedValue = millisElapsed - startTime
    normalisedProgress = (fromIntegral elapsedValue) / (fromIntegral duration)
    easedValue = bounceInOut normalisedProgress
    valueDifference = to - from
    delta = valueDifference * easedValue

data Transformation = Transformation Position Rotation Scale
data Tree a = Empty | Node a [Tree a] (Maybe Transformation)
data SceneGraph = SceneGraph (Tree Drawable)
  
module Animation where
  
import Ease
import Transformation

data AnimationTarget = PositionX | PositionY | PositionZ | RotationX | RotationY | RotationZ | ScaleX | ScaleY | ScaleZ 

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
    easedValue = (getEasing function) normalisedProgress
    valueDifference = to - from
    delta = valueDifference * easedValue

sampleAnimation :: AnimationTarget -> Animation 
sampleAnimation target = Animation target (0) (6.28)  BounceInOut 0 5000
  
getTransformation :: FrameNumber -> MillisElapsed -> Animation -> Transformation
getTransformation frameNumber millisElpased anim = getTransformationForTarget (target anim) $ runAnimation anim frameNumber millisElpased

getTransformationForTarget :: AnimationTarget -> (Float -> Transformation)
getTransformationForTarget PositionX = (\v -> Transformation (Position v 0 0) defaultRotation defaultScale)
getTransformationForTarget PositionY = (\v -> Transformation (Position 0 v 0) defaultRotation defaultScale)
getTransformationForTarget PositionZ = (\v -> Transformation (Position 0 0 v) defaultRotation defaultScale)
getTransformationForTarget ScaleX = (\v -> Transformation defaultPosition defaultRotation (Scale v 1 1))
getTransformationForTarget ScaleY = (\v -> Transformation defaultPosition defaultRotation (Scale 1 v 1))
getTransformationForTarget ScaleZ = (\v -> Transformation defaultPosition defaultRotation (Scale 1 1 v))
getTransformationForTarget RotationX = (\v -> Transformation defaultPosition (Rotation v 0 0) defaultScale)
getTransformationForTarget RotationY = (\v -> Transformation defaultPosition (Rotation 0 v 0) defaultScale)
getTransformationForTarget RotationZ = (\v -> Transformation defaultPosition (Rotation 0 0 v) defaultScale)

transformAnimations :: Int -> Int -> [Animation] -> Transformation
transformAnimations frameNumber millisElapsed anims = mconcat $ fmap (getTransformation frameNumber millisElapsed) anims
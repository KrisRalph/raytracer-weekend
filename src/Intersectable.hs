module Intersectable where
import Ray
import Vector

class Intersectable a where
    intersect :: a -> Ray -> Float


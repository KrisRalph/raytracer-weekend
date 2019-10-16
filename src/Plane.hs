module Plane where
import Vector
import Ray
import Intersectable

data Plane = Plane { point :: Vec3f
                   , normal :: Vec3f
                   }

instance Intersectable Plane where
    intersect = intersects

intersects :: Plane -> Ray -> Float
intersects p r = t
  where denom = direction r `dot` normal p
        t     = (point p `minus` origin r `dot` normal p) / denom
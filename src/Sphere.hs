module Sphere where
import Vector
import Ray
import Intersectable

data Sphere = Sphere { center :: Vec3f
                     , radius :: Float
                     }

instance Intersectable Sphere where
    intersect = intersects

intersects :: Sphere -> Ray -> Float
intersects s r | d > 0 = ((-b) - sqrt d) / (2.0*a)
               | otherwise = -1.0
    where oc = origin r `minus` center s
          a = dot (direction r) (direction r)
          b = 2.0 * dot oc (direction r)
          c = dot oc oc - radius s**2
          d = b*b - 4*a*c

-- todo tomorrow make things actually random
--randomInUnitSphere :: Sphere -> Float -> Float -> Float -> Vec3f
--randomInUnitSphere sph r
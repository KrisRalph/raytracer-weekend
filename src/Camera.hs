module Camera where
import Vector
import Ray hiding (origin)

data Camera = Camera { corner :: Vec3f 
                     , horizontal :: Vec3f 
                     , vertical :: Vec3f
                     , origin :: Vec3f 
                     }

mkCamera :: Camera
mkCamera = Camera { corner     = Vec3f (-2) (-1) (-1)
                  , horizontal = Vec3f 4 0 0
                  , vertical   = Vec3f 0 2 0
                  , origin       = Vec3f 0 0 0
                  }

getRay :: Camera -> Float -> Float -> Ray
getRay c u v = Ray (origin c) (dir u v)
    where dir u v = (corner c) `plus` (u `scalarMult` horizontal c)
                               `plus` (v `scalarMult` vertical c `minus` origin c)
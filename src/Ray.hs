module Ray where
import Vector

data Ray = Ray { a :: Vec3f
               , b :: Vec3f
               }

origin :: Ray -> Vec3f
origin (Ray a _) = a

direction :: Ray -> Vec3f
direction (Ray _ b) = b

pointAt :: Float -> Ray -> Vec3f
pointAt t (Ray a b) = a `plus` (t `scalarMult` b)
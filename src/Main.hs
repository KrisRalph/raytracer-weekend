module Main where
import Vector
import Ray
import Sphere 
import Plane
import Intersectable
import Camera
import Data.Fixed (mod')

colour :: Ray -> Vec3f
colour r | Sphere.intersects s r > 0 = intersection s r
         | Sphere.intersects p r > 0 = intersection p r
         | otherwise = drawBackground r
  where s = Sphere (Vec3f 0 0 (-1)) 0.5
        p = Sphere (Vec3f 0 100.5 0) 100
        --p = Plane (Vec3f 0 0.25 0) (Vec3f 0 1 0)

intersection :: Intersectable i => i -> Ray -> Vec3f
intersection s r = 0.5 `scalarMult` Vec3f (x n+1) (y n+1) (z n+1)
  where n = toUnit (pointAt (s `intersect` r) r `minus` Vec3f 0 0 (-1))

drawBackground :: Ray -> Vec3f
drawBackground (Ray a b) = t `scalarMult` unit `plus` ((1.0-t) `scalarMult` Vec3f 0.5 0.7 1.0)
  where t = 0.25 * ((y . toUnit) b + 1.0)

scene :: Int -> Int -> [Vec3f]
scene w h = [col u v | v <- linspace h (0.0, 1.0), u <- linspace w (0.0, 1.0)]
  where c = mkCamera
        col u v = colour $ getRay c u v

main :: IO ()
main = do
  putStrLn "hello world"
  let w = 2000
  let h = 1000
  let image = scene w h
  --print image
  writeFile "output.ppm" $ outputPPM image w h


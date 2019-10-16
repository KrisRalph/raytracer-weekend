module Vector where

data Vec3f = Vec3f { x :: Float
                   , y :: Float
                   , z :: Float
                   }

instance Show Vec3f where
  show (Vec3f r g b) = show r ++ " " ++
                       show g ++ " " ++
                       show b ++ " "

showRGB :: Vec3f -> String
showRGB (Vec3f r g b) = show (round $ 255*r) ++ " " ++
                        show (round $ 255*g) ++ " " ++
                        show (round $ 255*b) ++ "\n"

outputPPM :: [Vec3f] -> Int -> Int -> String
outputPPM pxs w h = "P3\n" ++ show w ++ " " ++ show h ++ "\n255\n" 
                    ++ concatMap showRGB pxs ++ "\n"

-- harrumph no numpy harrumph
linspace :: (Fractional a, Num a)=> Int -> (a,a) ->  [a]
linspace 0 _ = []
linspace 1 (a,b) = [(a+b)/2]
linspace n (a,b) = map ((s*) . fromIntegral) [0..(n-1)]
  where s = (b-a) / (fromIntegral n-1)

-- implement a functor or use negate 3 times?
negateVec :: Vec3f -> Vec3f
negateVec (Vec3f x y z) = Vec3f (negate x) (negate y) (negate z)

vecLength :: Vec3f -> Float
vecLength (Vec3f x y z) = sqrt (x*x + y*y + z*z)

squaredLength :: Vec3f -> Float
squaredLength (Vec3f x y z) = x*x + y*y + z*z

gradient :: Int -> Int -> [Vec3f]
gradient w h = [Vec3f r g 0.2| r <- linspace h (0.0, 1.0), g <- linspace w (0.0, 1.0)]

unit :: Vec3f
unit = Vec3f 1 1 1

toUnit :: Vec3f -> Vec3f
toUnit v = vecLength v `scalarDiv` v

plus :: Vec3f -> Vec3f -> Vec3f
plus (Vec3f x y z) (Vec3f x' y' z') = Vec3f (x+x') (y+y') (z+z')

minus :: Vec3f -> Vec3f -> Vec3f
minus (Vec3f x y z) (Vec3f x' y' z') = Vec3f (x-x') (y-y') (z-z')

mult :: Vec3f -> Vec3f -> Vec3f
mult (Vec3f x y z) (Vec3f x' y' z') = Vec3f (x*x') (y*y') (z*z')

scalarMult :: Float -> Vec3f -> Vec3f
scalarMult s (Vec3f x y z) = Vec3f (s*x) (s*y) (s*z)

divide :: Vec3f -> Vec3f -> Vec3f
divide (Vec3f  x y z) (Vec3f x' y' z') = Vec3f (x/x') (y/y') (z/z')

scalarDiv :: Float -> Vec3f -> Vec3f
scalarDiv s (Vec3f x y z) = Vec3f (x/s) (y/s) (z/s)

dot :: Vec3f -> Vec3f -> Float
dot (Vec3f x y z) (Vec3f x' y' z') = x*x' + y*y' + z*z'

-- well this is a bit messy
cross :: Vec3f -> Vec3f -> Vec3f 
cross a b = Vec3f (y a * z b - z a * y b)
                  (x a * z b - z a * x b)
                  (x a * y b - y a * x b)

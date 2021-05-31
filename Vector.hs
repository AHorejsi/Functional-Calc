module Vector (
    Vector(Vector),
    dimensionsv,
    vplusv,
    vminusv,
    vmultr,
    rmultv,
    vdotv,
    vcrossv,
    vdivr,
    absv,
    normv,
    anglev,
    vdistv
) where

newtype Vector a = Vector { pos :: [a] } deriving (Show, Eq)

dimensionsv :: (Num a) => Vector a -> Int
dimensionsv (Vector pos) = length pos

vplusv :: (Num a) => Vector a -> Vector a -> Vector a
vplusv (Vector leftPos) (Vector rightPos)
    | length leftPos /= length rightPos = error "Vectors must be of equal dimensions"
    | otherwise = Vector $ zipWith (+) leftPos rightPos

vminusv :: (Num a) => Vector a -> Vector a -> Vector a
vminusv (Vector leftPos) (Vector rightPos)
    | length leftPos /= length rightPos = error "Vectors must be of equal dimensions"
    | otherwise = Vector $ zipWith (-) leftPos rightPos

vmultr :: (Num a) => Vector a -> a -> Vector a
vmultr (Vector leftPos) right = Vector $ map (*right) leftPos

rmultv :: (Num a) => a -> Vector a -> Vector a
rmultv left right = vmultr right left

vdotv :: (Num a) => Vector a -> Vector a -> a
vdotv (Vector leftPos) (Vector rightPos)
    | length leftPos /= length rightPos = error "Vectors must be of equal dimensions"
    | otherwise = sum $ zipWith (*) leftPos rightPos

vcrossv :: (Num a) => Vector a -> Vector a -> Vector a
vcrossv (Vector leftPos) (Vector rightPos)
    | 3 /= length leftPos || 3 /= length rightPos = error "Vectors must be of 3 dimensions"
    | otherwise = Vector [xPos, yPos, zPos]
    where leftXPos = head leftPos
          leftYPos = leftPos !! 1
          leftZPos = leftPos !! 2
          rightXPos = head rightPos
          rightYPos = rightPos !! 1
          rightZPos = rightPos !! 2
          xPos = leftYPos * rightZPos - leftZPos * rightYPos
          yPos = leftZPos * rightXPos - leftXPos * rightZPos
          zPos = leftXPos * rightYPos - leftYPos * rightXPos

vdivr :: (Fractional a) => Vector a -> a -> Vector a
vdivr left right = vmultr left (1 / right)

absv :: (Floating a) => Vector a -> a
absv (Vector pos) = sqrt $ sum $ map (**2) pos

normv :: (Floating a) => Vector a -> Vector a
normv vec = vdivr vec (absv vec)

anglev :: (Floating a) => Vector a -> Vector a -> a
anglev left right
    | dimensionsv left /= dimensionsv right = error "Vectors must be of equal dimensions"
    | otherwise = acos $ dotProd / (absv left * absv right)
    where dotProd = vdotv left right

vdistv :: (Floating a) => Vector a -> Vector a -> a
vdistv left right
    | dimensionsv left /= dimensionsv right = error "Vectors must be of equal dimensions" 
    | otherwise = sqrt $ sum $ map (**2) (pos $ vminusv left right)

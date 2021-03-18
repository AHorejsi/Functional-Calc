module Imaginary (
    Complex(Complex),
    Quaternion(Quaternion),
    realC,
    imag0C,
    realQ,
    imag0Q,
    imag1Q,
    imag2Q,
    rplusc,
    cplusr,
    cplusc,
    rplusq,
    qplusr,
    cplusq,
    qplusc,
    qplusq,
    rminusc,
    cminusr,
    cminusc,
    rminusq,
    qminusr,
    cminusq,
    qminusc,
    qminusq,
    rmultc,
    cmultr,
    cmultc,
    rmultq,
    qmultr,
    cmultq,
    qmultc,
    qmultq,
    negc
) where

data Complex a = Complex { realC  :: a,
                           imag0C :: a
                         } deriving (Show, Eq)

data Quaternion a = Quaternion { realQ  :: a,
                                 imag0Q :: a,
                                 imag1Q :: a,
                                 imag2Q :: a
                               } deriving (Show, Eq)

rplusc :: (Num a) => a -> Complex a -> Complex a
rplusc left (Complex rightReal rightImag0) = Complex (left + rightReal) rightImag0

cplusr :: (Num a) => Complex a -> a -> Complex a
cplusr left right = rplusc right left

cplusc :: (Num a) => Complex a -> Complex a -> Complex a
cplusc (Complex leftReal leftImag0) (Complex rightReal rightImag0) = Complex (leftReal + rightReal) (leftImag0 + rightImag0)

rplusq :: (Num a) => a -> Quaternion a -> Quaternion a
rplusq left (Quaternion rightReal rightImag0 rightImag1 rightImag2) = Quaternion (left + rightReal) rightImag0 rightImag1 rightImag2

qplusr :: (Num a) => Quaternion a -> a -> Quaternion a
qplusr left right = rplusq right left

cplusq :: (Num a) => Complex a -> Quaternion a -> Quaternion a
cplusq (Complex leftReal leftImag0) (Quaternion rightReal rightImag0 rightImag1 rightImag2) = Quaternion (leftReal + rightReal) (leftImag0 + rightImag0) rightImag1 rightImag2

qplusc :: (Num a) => Quaternion a -> Complex a -> Quaternion a
qplusc left right = cplusq right left

qplusq :: (Num a) => Quaternion a -> Quaternion a -> Quaternion a
qplusq (Quaternion leftReal leftImag0 leftImag1 leftImag2) (Quaternion rightReal rightImag0 rightImag1 rightImag2) = Quaternion (leftReal + rightReal) (leftImag0 + rightImag0) (leftImag1 + rightImag1) (leftImag2 + rightImag2)

rminusc :: (Num a) => a -> Complex a -> Complex a
rminusc left (Complex rightReal rightImag0) = Complex (left - rightReal) (-rightImag0)

cminusr :: (Num a) => Complex a -> a -> Complex a
cminusr (Complex leftReal leftImag0) right = Complex (leftReal - right) leftImag0

cminusc :: (Num a) => Complex a -> Complex a -> Complex a
cminusc (Complex leftReal leftImag0) (Complex rightReal rightImag0) = Complex (leftReal - rightReal) (leftImag0 - rightImag0)

rminusq :: (Num a) => a -> Quaternion a -> Quaternion a
rminusq left (Quaternion rightReal rightImag0 rightImag1 rightImag2) = Quaternion (left - rightReal) (-rightImag0) (-rightImag1) (-rightImag2)

qminusr :: (Num a) => Quaternion a -> a -> Quaternion a
qminusr (Quaternion leftReal leftImag0 leftImag1 leftImag2) right = Quaternion (leftReal - right) leftImag0 leftImag1 leftImag2

cminusq :: (Num a) => Complex a -> Quaternion a -> Quaternion a
cminusq (Complex leftReal leftImag0) (Quaternion rightReal rightImag0 rightImag1 rightImag2) = Quaternion (leftReal - rightReal) (leftImag0 - rightImag0) (-rightImag1) (-rightImag2)

qminusc :: (Num a) => Quaternion a -> Complex a -> Quaternion a
qminusc (Quaternion leftReal leftImag0 leftImag1 leftImag2) (Complex rightReal rightImag0) = Quaternion (leftReal - rightReal) (leftImag0 - rightImag0) leftImag1 leftImag2

qminusq :: (Num a) => Quaternion a -> Quaternion a -> Quaternion a
qminusq (Quaternion leftReal leftImag0 leftImag1 leftImag2) (Quaternion rightReal rightImag0 rightImag1 rightImag2) = Quaternion (leftReal - rightReal) (leftImag0 - rightImag0) (leftImag1 - rightImag1) (leftImag2 - rightImag2)

rmultc :: (Num a) => a -> Complex a -> Complex a
rmultc left (Complex rightReal rightImag0) = Complex (left * rightReal) (left * rightImag0)

cmultr :: (Num a) => Complex a -> a -> Complex a
cmultr left right = rmultc right left

cmultc :: (Num a) => Complex a -> Complex a -> Complex a
cmultc (Complex leftReal leftImag0) (Complex rightReal rightImag0) = Complex (leftReal * rightReal - leftImag0 * rightImag0) (leftReal * rightImag0 + leftImag0 * rightReal)

rmultq :: (Num a) => a -> Quaternion a -> Quaternion a
rmultq left (Quaternion rightReal rightImag0 rightImag1 rightImag2) = Quaternion (left * rightReal) (left * rightImag0) (left * rightImag1) (left * rightImag2)

qmultr :: (Num a) => Quaternion a -> a -> Quaternion a
qmultr left right = rmultq right left

cmultq :: (Num a) => Complex a -> Quaternion a -> Quaternion a
cmultq (Complex leftReal leftImag0) (Quaternion rightReal rightImag0 rightImag1 rightImag2) = Quaternion (leftReal * rightReal - leftImag0 * rightImag0) (leftReal * rightImag0 + rightReal * leftImag0) (leftReal * rightImag1 - leftImag0 * rightImag2) (leftReal * rightImag2 + leftImag0 * rightImag1)

qmultc :: (Num a) => Quaternion a -> Complex a -> Quaternion a
qmultc (Quaternion leftReal leftImag0 leftImag1 leftImag2) (Complex rightReal rightImag0) = Quaternion (leftReal * rightReal - leftImag0 * rightImag0) (leftReal * rightImag0 + leftImag0 * rightReal) (leftImag1 * rightReal + leftImag2 * rightImag0) (-leftImag1 * rightImag0 + leftImag2 * rightReal)

qmultq :: (Num a) => Quaternion a -> Quaternion a -> Quaternion a
qmultq (Quaternion leftReal leftImag0 leftImag1 leftImag2) (Quaternion rightReal rightImag0 rightImag1 rightImag2) = Quaternion (leftReal * rightReal - leftImag0 * rightImag0 - leftImag1 * rightImag1 - leftImag2 * rightImag2) (leftReal * rightImag0 + leftImag0 * rightReal - leftImag1 * rightImag2 + leftImag2 * rightImag1) (leftReal * rightImag1 + leftImag0 * rightImag2 + leftImag1 * rightReal - leftImag2 * rightImag0) (leftReal * rightImag2 - leftImag0 * rightImag1 + leftImag1 * rightImag0 + leftImag2 * rightReal)

cdivr :: (Num a) => Complex a -> a -> Complex a
cdivr (Complex leftReal leftImag0) right = Complex (leftReal / right) (leftImag0 / right)

qdivr :: (Num a) => Quaternion a -> a -> Quaternion a
qdivr (Quaternion leftReal leftImag0 leftImag1 leftImag2) right = 

negc :: (Num a) => Complex a -> Complex a
negc = rmultc $ -1

negq :: (Num a) => Quaternion a -> Quaternion a
negq = rmultq $ -1

ntoc :: (Num a) => a -> Complex a
ntoc num = Complex num 0

ntoq :: (Num a) => a -> Quaternion a
ntoq num = Quaternion num 0 0 0

ctoq :: (Num a) => Complex a -> Quaternion a
ctoq (Complex real imag0) = Quaternion real imag0 0 0

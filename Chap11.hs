{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}

data Trivial = Trivial'
data UnaryTypeCon a = UnaryValueCon a

data PugType = PugData
data HuskyType a = HuskyData
data DogueDeBordeaux doge = DogueDeBordeaux doge

myPug = PugData :: PugType

myHusky :: HuskyType a
myHusky = HuskyData

myOtherHusky :: Num a => HuskyType a
myOtherHusky = HuskyData

myOtherOtherHusky :: HuskyType [[[[[[Int]]]]]]
myOtherOtherHusky = HuskyData

myDoge :: DogueDeBordeaux Int
myDoge = DogueDeBordeaux 10

badDoge :: DogueDeBordeaux String
badDoge = DogueDeBordeaux "10"

data Doggies a = 
    Husky a
  | Mastiff a
  deriving (Eq, Show)


data Price =
  Price Integer deriving (Eq, Show)

data Manufacturer =
    Mini
  | Mazda
  | Tata
  deriving (Eq, Show)

data Airline =
    PapuAir
  | CatapultsR'Us
  | TakeYourChancesUnited
  deriving (Eq, Show)

data Size =
  Size Integer deriving (Eq, Show)

data Vehicle = Car Manufacturer Price
             | Plane Airline Size
             deriving (Eq, Show)

myCar = Car Mini (Price 14000)
urCar = Car Mazda (Price 20000)
clownCar = Car Tata (Price 7000)
doge = Plane PapuAir (Size 100)

isCar :: Vehicle -> Bool
isCar (Car _ _) = True
isCar _ = False

isPlane :: Vehicle -> Bool
isPlane (Plane _ _) = True
isPlane _ = False

areCars :: [Vehicle] -> [Bool]
areCars = map isCar

getManu :: Vehicle -> Manufacturer
getManu (Car x _) = x
getManu _ = undefined

data Example0 =
  Example0 deriving (Eq, Show)

data Example1 =
  Example1 Int deriving (Eq, Show)

data Example2 =
  Example2 Int String deriving (Eq, Show)

data Example = MakeExample deriving Show

class TooMany a where
  tooMany :: a -> Bool

instance TooMany Int where
  tooMany = (> 42)

newtype Goats = Goats Int deriving (Eq, Show, TooMany)

instance TooMany (Int, String) where
  tooMany = (> 42) . fst

instance (Num a, TooMany a) => TooMany (a, a) where
  tooMany = tooMany . (uncurry (+))


data QuantumBool = QuantumTrue
                 | QuantumFalse
                 | QuantumBoth deriving (Eq, Show)

data TwoQs =
  MkTwoQs QuantumBool QuantumBool
  deriving (Eq, Show)

data Person =
  Person { name :: String
         , age :: Int}
         deriving (Eq, Show)

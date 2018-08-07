import Data.Time

data DatabaseItem = DbString String
                  | DbNumber Integer
                  | DbDate UTCTime
                  deriving (Eq, Ord, Show)

theDatabase :: [DatabaseItem]
theDatabase =
  [ DbDate (UTCTime
            (fromGregorian 1911 5 1)
    (secondsToDiffTime 34123))
  , DbNumber 9001
  , DbString "Hello, world!"
  , DbDate (UTCTime
            (fromGregorian 1921 6 1)
            (secondsToDiffTime 34123))
  ]

filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate dbItem = map g $ filter f dbItem
                      where f (DbDate x) = True
                            f _ = False
                            g (DbDate x) = x

filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber dbItem = map g $ filter f dbItem
                      where f (DbNumber x) = True
                            f _ = False
                            g (DbNumber x) = x

mostRecent :: [DatabaseItem] -> UTCTime
mostRecent dbItem = foldr (\val acc -> if val > acc then val else acc) dayZero (filterDbDate dbItem)
                    where dayZero = UTCTime (fromGregorian 1900 1 1) (secondsToDiffTime 0)

sumDb :: [DatabaseItem] -> Integer
sumDb dbItem = foldr (+) 0 (filterDbNumber dbItem)

avgDb :: [DatabaseItem] -> Double
avgDb dbItem = fromIntegral(sumDb dbItem) / fromIntegral(length (filterDbNumber dbItem))

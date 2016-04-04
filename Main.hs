{-# LANGUAGE DeriveDataTypeable #-}
module Main where
 
import Happstack.Server
import Text.JSON.Generic
import GHC.Float

data Customer =
    Customer {
        cid :: String,
        onTimePayments :: Int,
        profit :: Double
    }
    deriving (Data, Typeable, Show)

data CustomerRanking =
    CustomerRanking {
        customerId :: String,
        positiveRanking :: Bool
    }
    deriving (Data, Typeable, Show)

type Score = Double

score :: Customer -> Float
score (Customer _ x y) = double2Float $ fromIntegral x * y

mean xs = (sum xs) / (fromIntegral $ length xs)

stdDev :: [Float] -> Float
stdDev xs =
    let variance = ((sum $ map (\x -> (x - mean xs)^2) xs) / (fromIntegral $ length xs))
    in sqrt variance

calculateRanking :: [Customer] -> [CustomerRanking]
calculateRanking cs =
    let scores = map score cs
        m = mean scores
        sd = stdDev scores
    in
        map (\c -> CustomerRanking (cid c) (score c > (m - sd))) cs

c1 = Customer "Bob" 80 100
c2 = Customer "Anton" 85 99
c3 = Customer "Alice" 90 120
c4 = Customer "Jack" 44 80

testProjectJSON = encodeJSON $ calculateRanking [c1,c2,c3,c4]

main = simpleHTTP nullConf $ ok $ toResponse testProjectJSON

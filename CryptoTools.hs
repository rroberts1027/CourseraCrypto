module CryptoTools where

import Data.Text.Lazy.Read
import Data.Char
import Data.List
import Data.Bits


data Message = M String
data HexString = HS String
data HexList = HL [String] 
data IntList = IL [Int]

class Crypto a where
     toMessage :: a -> Message
     toHexString :: a -> HexString
     toHexList :: a -> HexList
     toIntList :: a -> IntList

{- ------------------------------------------------------------------ -}

--Bitwise xor function
(<+>) :: (Crypto a, Crypto b) => a -> b -> IntList
(<+>) x y = helper (toIntList x) (toIntList y)
                where helper (IL x') (IL y') = IL $ zipWith xor x' y'

{- ------------------------------------------------------------------ -}


instance Crypto Message where
     toMessage = id
     toHexString = toHexString . toIntList 
     toHexList = toHexList . toIntList
     toIntList (M x) = IL $ map ord x

instance Crypto HexString where
     toMessage = toMessage . toIntList
     toHexString = id
     toHexList (HS x) = HL $ breakUpHex x
     toIntList = toIntList . toHexList

instance Crypto HexList where
     toMessage = toMessage . toIntList
     toHexString (HL x) = HS . concat $ x
     toHexList = id
     toIntList (HL x) = IL $ map (\y -> fromBase 16 $ fromAlphaDigits $ y) x

instance Crypto IntList where
     toMessage (IL x) = M $ map chr x
     toHexString = toHexString . toHexList
     toHexList (IL x) = HL $ map (\y -> headingZero . toAlphaDigits $ toBase 16 $ y) x
     toIntList = id

instance Show Message where
    show (M x) = '\"':x ++ ['\"']

instance Show HexString where
    show (HS x) = "hex: \"" ++ x ++ "\""

instance Show HexList where
    show (HL x) = concat $ map (\y -> " . " ++ y) x

instance Show IntList where
    show (IL x) = concat $ map (\y -> " . " ++ (show y)) x

breakUpHex :: String -> [String]
breakUpHex (x:(y:tl)) = (x:(y:[])):(breakUpHex tl)
breakUpHex _ = []

headingZero :: String -> String
headingZero (x:[]) = '0':[x]
headingZero x = x


{- ------------------------------------------------------------------ -}
--Source: http://rosettacode.org/wiki/Non-decimal_radices/Convert#Haskell


toBase :: Int -> Int -> [Int]
toBase b v = toBase' [] v where
  toBase' a 0 = a
  toBase' a v = toBase' (r:a) q where (q,r) = v `divMod` b
 
fromBase :: Int -> [Int] -> Int
fromBase b ds = foldl' (\n k -> n * b + k) 0 ds
 
toAlphaDigits :: [Int] -> String
toAlphaDigits = map convert where
  convert n | n < 10    = chr (n + ord '0')
            | otherwise = chr (n + ord 'a' - 10)
 
fromAlphaDigits :: String -> [Int]
fromAlphaDigits = map convert where
 convert c | isDigit c = ord c - ord '0'
           | isUpper c = ord c - ord 'A' + 10
           | isLower c = ord c - ord 'a' + 10


{- ------------------------------------------------------------------ -}
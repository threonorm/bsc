{-# LANGUAGE DeriveDataTypeable #-}
module FStringCompat(FString, getFString,
                     tmpFString, cloneFString, concatFString,
                     mkNumFString, mkStrFString, mkFString,
                     filterFString, toString, fromString
                    ) where

-- wrapper to make SStrings look like FStrings

import Prelude hiding((++))
import qualified SpeedyString as S
import PPrint(PPrint(..), text)
import Util(itos)
import qualified Data.Generics as Generic


newtype FString = FString S.SString deriving (Eq,Ord,Generic.Data, Generic.Typeable)

fromString :: String -> FString
fromString = FString . S.fromString

toString :: FString -> String
toString (FString s) = S.toString s

(++) :: FString -> FString -> FString
(FString s) ++ (FString s') = FString $ s S.++ s'

instance Show FString where
    show (FString s) = show s

instance PPrint FString where
    pPrint _ _ x = text (show x)

getFString :: FString -> String
getFString = toString

mkFString :: String -> FString
mkFString s = fromString s

cloneFString :: [FString] -> FString -> FString
cloneFString fs f = head [f' | n <- [1..] :: [Integer],
                          let f' = f ++ fromString ('_':'_':'_':itos n),
                          f' `notElem` fs]

tmpFString :: Int -> String -> FString
tmpFString _ = fromString

concatFString :: [FString] -> FString
concatFString fs = FString $ S.concat [s | FString s <- fs]

filterFString :: (Char -> Bool) -> FString -> FString
filterFString pred (FString s) = FString $ S.filter pred s

mkNumFString :: Integer -> FString
mkNumFString i = fromString (itos i)

mkStrFString :: String -> FString
mkStrFString s = fromString (show s)

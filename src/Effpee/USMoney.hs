{-# OPTIONS -Wno-unused-matches #-}

module Effpee.USMoney
  ( evalCoins
  , evalBills
  , getPresident
  , reallyPresident
  , President (..)
  , USCoin (..)
  , USBill (..)
  , Many (..)
  ) where

import Data.Int
import Effpee
import Effpee.ADT
import GHC.Num    ((+))

evalCoin :: USCoin -> Int
evalCoin Penny         = 1
evalCoin Nickel        = 5
evalCoin Quarter        = 25
evalCoin Dime          = 10
evalCoin OneDollarCoin = 100
evalCoin TwoDollarCoin = 200

evalCoins
  :: Many USCoin
  -> Int
evalCoins Empty     = 0
evalCoins (x :. xs) = evalCoin x + evalCoins xs

evalBill :: USBill -> Int
evalBill OneDollar  = 1
evalBill TwoDollar  = 2
evalBill FiveDollar  = 5
evalBill TenDollar  = 10
evalBill TwentyDollar  = 20
evalBill FiftyDollar  = 50
evalBill OneHundredDollar = 100

-- Use @evalBill@ in this definition
evalBills
  :: Many USBill
  -> Int
evalBills Empty = 0
evalBills (x :. xs) = evalBill x + evalBills xs

-- Given a US bill/note produce the presient whose portrait appears on it.
-- * $1   => Washington
-- * $2   => Jefferson
-- * $5   => Lincoln
-- * $10  => Hamilton
-- * $20  => Jackson
-- * $50  => Grant
-- * $100 => Franklin
getPresident
  :: USBill
  -> President
getPresident OneDollar = Washington
getPresident TwoDollar = Jefferson
getPresident FiveDollar = Lincoln
getPresident TenDollar = Hamilton
getPresident TwentyDollar = Jackson
getPresident FiftyDollar = Grant
getPresident OneHundredDollar = Franklin

reallyPresident
  :: President
  -> Boolean     -- ^ this is the @Boolean@ from the ADT module NOT the builtin @Bool@ type
reallyPresident Franklin = Nah
reallyPresident _ = Yeah

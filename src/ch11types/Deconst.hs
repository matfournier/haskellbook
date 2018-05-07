module Deconst where

newtype Name  = Name String deriving Show
newtype Acres = Acres Int deriving Show

-- FarmerType is a sum

data FarmerType = DairyFarmer
                | WheatFarmer
                | SoybeanFarmer
                deriving Show

-- Farmer is aplain ole product of name, acres, farmertype
data Farmer =
  Farmer Name Acres FarmerType
  deriving Show

isDairyFarmer :: Farmer -> Bool
isDairyFarmer (Farmer _ _ DairyFarmer) = True
isDairyFarmer _ = False

-- alt. using records

data FarmerRec =
  FarmerRec { name               :: Name
            , acres              :: Acres
            , farmerType         :: FarmerType }
  deriving Show

isDairyFarmerRec :: FarmerRec -> Bool
isDairyFarmerRec farmer =
  case farmerType farmer of
    DairyFarmer -> True
    _           -> False

  


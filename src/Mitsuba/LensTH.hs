{-# LANGUAGE LambdaCase #-}
module Mitsuba.LensTH where
import Control.Lens
import Control.Lens.TH
import Language.Haskell.TH

dataName :: Dec -> Maybe Name
dataName = \case
  DataD    _ n _ _ _ -> Just n
  NewtypeD _ n _ _ _ -> Just n
  _ -> Nothing
  
dataDataName :: Dec -> Maybe Name
dataDataName = \case
  DataD    _ n _ (x:y:_) _ -> Just n
  _ -> Nothing

makeLensesL :: Name -> Q [Dec]
makeLensesL = makeLensesWith 
            $ defaultRules & lensField .~ (Just . (++ "L"))
module Mitsuba.LensTH where
import Control.Lens
import Control.Lens.TH
import Language.Haskell.TH

makeLensesL :: Name -> Q [Dec]
makeLensesL = makeLensesWith 
            $ defaultRules & lensField .~ (Just . (++ "L"))
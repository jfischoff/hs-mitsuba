{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE ExtendedDefaultRules #-}
module Mitsuba.Element.Types where
import qualified Data.Text as T
import Data.Text (Text)
import Control.Lens hiding ((#))
import Control.Lens.At
import Text.Blaze.Internal hiding (Tag, String)
import Text.Blaze hiding (Tag, String)
import Mitsuba.LensTH
import Data.List
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as H
import Control.Applicative
import Data.Monoid
import Data.Maybe
import Data.Either
import qualified Data.Foldable as F
default (Text, Integer, Double)

type Name = Text

data Visibility = Shown | Hidden
   deriving (Eq, Show, Read, Ord, Bounded, Enum)

data ChildType 
   = Nested Visibility 
   -- ^ Is the name visible?
   | Attribute (Element -> Text)
   -- ^ This is here as a way to combine the children of the attribute 
   --   to the attribute value. Sometimes they are " " seperatored
   --   usually it 

data ChildItem = ChildItem 
   { childItemType          :: ChildType
   -- ^ Is it an attribute of a nested tag
   , childItemElement       :: Element
   }

type Children = HashMap Name ChildItem

data Element = Element 
   { elementTag      :: Name
   -- ^ It is either the tag or it is primitive string
   , elementChildren :: Children
   -- ^ empty for primitive
   }

makePrisms ''Visibility
makePrisms ''ChildType
makeLensesL ''ChildItem
makeLensesL ''Element


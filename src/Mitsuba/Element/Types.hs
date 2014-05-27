{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE DeriveDataTypeable   #-}
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
import Data.Data
import qualified Data.Foldable as F
import Text.Show.Functions
default (Text, Integer, Double)

type Name = Text

-- Get rid of KnownMaterial as a way to store the index of refraction
-- 

--instance Show (a -> b) where
--  show _ = "show f"

-- I can fix this by creating a enumeration

data Visibility = Shown | Hidden
   deriving (Eq, Show, Read, Ord, Bounded, Enum, Typeable)

data ChildType 
   = Nested Visibility 
   -- ^ Is the name visible?
   | Attribute (Element -> Text)
   -- ^ This is here as a way to combine the children of the attribute 
   --   to the attribute value. Sometimes they are " " seperatored
   --   usually it 
   deriving (Typeable)
   
instance Show ChildType where
  show x = case x of
    Nested    v  -> "Nested " ++ show v
    Attribute {} -> "Attribute"

data ChildItem = ChildItem 
   { childItemType          :: ChildType
   -- ^ Is it an attribute of a nested tag
   , childItemElement       :: Element
   } deriving (Show, Typeable)

type Children = HashMap Name ChildItem

data Element = Element 
   { elementTag      :: Name
   -- ^ It is either the tag or it is primitive string
   , elementChildren :: Children
   -- ^ empty for primitive
   } deriving (Show, Typeable)

makePrisms ''Visibility
makePrisms ''ChildType
makeLensesL ''ChildItem
makeLensesL ''Element


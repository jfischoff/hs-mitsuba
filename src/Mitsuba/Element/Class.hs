{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE DefaultSignatures    #-}
module Mitsuba.Element.Class where
import Mitsuba.Element.Types
import qualified Data.Text as T
import Data.Text (Text)
import Control.Lens hiding ((#), children, from, (.>))
import Control.Lens.At
import Control.Lens.Setter
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
import Data.Proxy
import GHC.Generics
import Mitsuba.Generic
import Data.Char
import Control.Monad.State
import Control.Arrow
default (Text, Integer, Double)

class ToElement a where
   toElement :: a -> Element
   
   default toElement :: (Generic a, GToElement (Rep a)) => a -> Element
   toElement = defaultGeneric
   
defaultGeneric :: (Generic a, GToElement (Rep a)) => a -> Element
defaultGeneric = makeGElement . gToElement . from
   
instance ToElement Text where
   toElement = primitive "string"

instance ToElement String where
   toElement = primitive "string" . T.pack

instance ToElement Int where
   toElement = primitive "integer" . T.pack . show

instance ToElement Integer where
   toElement = primitive "integer" . T.pack . show

instance ToElement Double where
   toElement = primitive "float" . T.pack . show

instance ToElement Bool where
   toElement = primitive "bool" . T.pack . show

instance ToElement Element where
   toElement = id
   
class ToAttributeValue a where
   toAttributeValue :: a -> Text
   
instance ToAttributeValue Double where
   toAttributeValue = T.pack . show

instance ToAttributeValue String where
   toAttributeValue = T.pack
   
instance ToAttributeValue Text where
   toAttributeValue = id

t :: Text -> MarkupM b -> MarkupM a 
t = CustomParent . Text

ta :: Text -> [Attribute] -> MarkupM a -> MarkupM b
ta tag as child = foldl' (!) (t tag child) as

cta :: Text -> [Attribute] -> MarkupM a
cta tag = foldl' (!) (ct tag)

ct :: Text -> MarkupM a
ct x = CustomLeaf (Text x) True

-- TODO 
-- This should take a ToPrimtiveValue in
a :: ToValue a => Text -> a -> Attribute
a x = customAttribute (textTag x) . toValue

addChild x name e ct 
   = x & elementChildrenL.at name .~ (Just $ ChildItem ct e)

tag tagName = Element tagName mempty
primitive typ value = tag typ # ("value", value)

defNested = ChildItem (Nested Shown)

addAttribute x name e f = addChild x name e $ Attribute f

defaultAttribute = Attribute elementTag

addPrimitiveAttribute :: ToAttributeValue a
                      => Element
                      -> Name
                      -> a
                      -> Element 
addPrimitiveAttribute x name value 
   = addAttribute x name (tag (toAttributeValue value)) elementTag

infixl 2 #

x # (n, v) = addPrimitiveAttribute x n v

-- I want to add an elements children 
appendChildren :: (ToElement a, ToElement b)
               => a
               -> b
               -> Element
appendChildren x y 
   = toElement x 
   & elementChildrenL <>~ (toElement y ^. elementChildrenL)

addChildList :: ToElement a 
             => Element 
             -> [a]
             -> Element
addChildList i 
   = foldr (\(n, x) e -> addNested e n x Hidden) i
   . zip (map (T.pack . show) [0..])
   . map toElement

addCompoundAttribute x n e sep 
   = addAttribute x n e ( T.intercalate sep
                        . map (elementTag . childItemElement) 
                        . F.toList
                        . elementChildren
                        )

addNested :: Element -> Name -> Element -> Visibility -> Element
addNested x n e v = addChild x n e $ Nested v


infixl 1 .>

addNestedShown :: ToElement a => Element -> (Text, a) -> Element
addNestedShown = (.>)

(.>) :: ToElement a => Element -> (Text, a) -> Element
x .> (n, e) = addNested x n (toElement e) Shown

typeL :: Traversal' Element Name
typeL = elementChildrenL . at "type" . _Just . childItemElementL . elementTagL

setType :: Name -> Element -> Element 
setType n = set typeL n

getType :: Element -> Maybe Name
getType = preview typeL

allAttribute :: Element -> Element
allAttribute = set (elementChildrenL . mapped . childItemTypeL) defaultAttribute

allNested :: Element -> Element
allNested = set (elementChildrenL . mapped . childItemTypeL) (Nested Shown)

--child :: Name -> Traversal' Element (Element, ChildType)
--child name = elementChildrenL . at name . _Just

toXML :: Element -> Markup
toXML = toXML' Nothing

toXMLChild :: Name -> ChildType -> Element -> Either Attribute Markup
toXMLChild name typ e = case typ of
   Nested visibility -> 
      let maybeName = case visibility of
            Shown  -> Just name
            Hidden -> Nothing
      in Right $ toXML' maybeName e
   Attribute renderer -> Left $ a name $ renderer e

toXML' :: Maybe Name -> Element -> Markup
toXML' maybeName Element {..} = 
   let (asAttrs, children) 
         = partitionEithers 
         $ map (\(k, cd) -> 
                  toXMLChild k (cd ^. childItemTypeL) (cd ^. childItemElementL)
               )
         $ H.toList elementChildren

       maybeNameAttr = maybeToList $ a "name" <$> maybeName
       attrs = maybeNameAttr ++ asAttrs

   in case children of
      [] -> cta elementTag attrs
      xs -> ta  elementTag attrs $ foldr1 Append children

lowerFirst = (\([x], xs) -> toLower x : xs) . splitAt 1

renameConstructor :: String -> String -> String
renameConstructor dataType 
   =  lowerFirst . drop capitalCount where
       capitalCount = length $ filter isUpper dataType

renameDataType :: String -> String
renameDataType = map toLower

renameSelector :: String -> String -> String
renameSelector dataTypeName = lowerFirst . drop (length dataTypeName)

-- This needs to be state
data GToElementLog = GToElementLog 
   { _dataTypeName    :: Last String
   , _constrName      :: Last String
   , _children        :: [(String, Element)] -- used for records
   , _result          :: Last Element -- used for sum types
   } 

instance Monoid GToElementLog where
   mempty = GToElementLog mempty mempty mempty mempty
   GToElementLog a0 b0 c0 d0 `mappend` GToElementLog a1 b1 c1 d1 = 
      GToElementLog (a0 <> a1) (b0 <> b1) (c0 <> c1) (d0 <> d1)

class GToElement f where
   gToElement :: f a -> State GToElementLog ()

makeSumType :: String -> String -> Element -> Element
makeSumType dataTypeName constructorName elem 
   = tag (T.pack dataTypeName) # ("type", constructorName)
   & elementChildrenL <>~ elementChildren elem 

makeProductType :: String -> [(String, Element)] -> Element
makeProductType dataTypeName cs
   = Element (T.pack dataTypeName)
   $ H.fromList 
   $ map (T.pack *** ChildItem (Nested Shown)) cs

makeGElement :: State GToElementLog () -> Element
makeGElement w = result where
   GToElementLog {..} = execState w mempty

   Last (Just dtName) = _dataTypeName
   Last (Just ctName) = _constrName

   result = case ( _children, getLast _result) of
      ([], Just x ) -> makeSumType     dtName ctName x
      (_, Nothing ) -> makeProductType dtName _children
      _ -> error "logic error in GToElement (D1 d (C1 c f)) gToTag"

makeLenses ''GToElementLog

instance (Datatype d, GToElement f) => GToElement (D1 d f) where
   gToElement (M1 x) = do 
      assign dataTypeName 
         $ last' 
         $ renameDataType 
         $ datatypeName (undefined :: t d f p)
      gToElement x

instance (Constructor c, GToElement f) => GToElement (C1 c f) where
   gToElement (M1 x) = do
      Just dt <- getLast <$> use dataTypeName
      assign constrName 
         $ last' 
         $ renameConstructor dt 
         $ conName (undefined :: t c f p)
      gToElement x

instance (GToElement f, GToElement g) => GToElement (f :+: g) where
   gToElement = \case
      L1 x -> gToElement x
      R1 x -> gToElement x

instance (GToElement f, GToElement g) => GToElement (f :*: g) where
   gToElement (x :*: y) = gToElement x >> gToElement y


instance GToElement U1 where
   gToElement U1 = return ()

last' = Last . Just

instance ToElement x => GToElement (S1 NoSelector (Rec0 x)) where
   gToElement (M1 (K1 x)) = assign result 
                          $ last' $ toElement x 

instance (Selector s, ToElement x) => GToElement (S1 s (Rec0 x)) where
   gToElement (M1 (K1 x)) = do 
      Just dt <- getLast <$> use dataTypeName
      let sName = renameSelector dt 
                $ selName (undefined :: t s (Rec0 x) p) 

      children <>= [(sName, toElement x)]



-- TODO rename gfold to sumFold or something without the word
-- fold in it
forwardToElement :: (Generic a, GFold (Rep a) ToElement Element) 
                 => a -> Element
forwardToElement = gfold (Proxy :: Proxy ToElement) toElement

forwardToAttributeValue :: (Generic a, GFold (Rep a) ToAttributeValue Text) => a -> Text
forwardToAttributeValue = gfold (Proxy :: Proxy ToAttributeValue) toAttributeValue

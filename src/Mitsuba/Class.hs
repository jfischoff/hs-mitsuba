{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE RecordWildCards        #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE DefaultSignatures      #-}
{-# LANGUAGE TupleSections          #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE OverlappingInstances   #-}
{-# LANGUAGE ConstraintKinds        #-}
module Mitsuba.Class where
import Control.Lens hiding (from, children, (#))
import Control.Lens.Setter
import Control.Applicative
import Data.Text.Format
import Data.Double.Conversion.Text
import Control.Arrow hiding (left)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as H
import Control.Lens.At 
import Data.Monoid
import GHC.Generics
import Control.Monad.Writer
import Control.Monad.State
import Data.Maybe
import Data.List 
import Data.Char
import Text.Blaze.Internal hiding (Tag, String)
import Text.Blaze hiding (Tag, String)
import Data.Text (Text)
import qualified Data.Text as T
import Text.Blaze.Renderer.Pretty
import qualified Data.Foldable as F
import Mitsuba.Generic
import Data.Proxy
import GHC.Exts
import Mitsuba.Primitive
import qualified Data.Text.Lazy as TL

-- TODO rename logic!

lowerFirst = (\([x], xs) -> toLower x : xs) . splitAt 1

renameConstructor :: String -> String -> String
renameConstructor dataType 
   =  lowerFirst . drop capitalCount where
       capitalCount = length $ filter isUpper dataType

renameDataType :: String -> String
renameDataType = map toLower

renameSelector :: String -> String -> String
renameSelector dataTypeName = lowerFirst . drop (length dataTypeName)

-- Whether or not to render the names
data Visiblity = Show | Hide deriving (Show, Eq)

-- the type of names
type Name = String

-- Names aan be rendered or not
data Named = Named 
   { _name       :: Name
   , _visibility :: Visiblity
   } deriving (Show, Eq)

class ToNamed a where
   toNamed :: Traversal' a Named

-- The primitive types
data PrimitiveType
   = Float    Double
   | String   String
   | Int      Int
   | Bool     Bool
   | Spectrum Spectrum
   deriving (Show, Eq) 

-- The children type
-- the name key is also in the Element, which is too bad
type Children = HashMap Name (Element, ChildType)

-- traverse the children
class ToChildren a where
   toChildren :: Traversal' a Children

instance ToChildren Children where
   toChildren = id

-- 
data Primitive = Primitive 
   { _primitiveName :: Named 
   , _primitiveType :: PrimitiveType
   } deriving (Show, Eq)
   

data ChildType = Attribute | Nested
   deriving (Show, Eq)

-- I don't there is any value in storing the Maybe seperately for the type
data Tag = Tag 
   { _tagTag      :: Name
   , _tagName     :: Maybe  Named
   , _tagType     :: Maybe String
   , _tagChildren :: Children 
   } deriving (Show, Eq)
   
tag name = Tag name Nothing Nothing mempty
   
data Element
   = T Tag
   | P Primitive
   deriving (Show, Eq)
   
makeLenses ''Tag
makePrisms ''PrimitiveType
makeLenses ''Primitive
makePrisms ''ChildType
makePrisms ''Visiblity
makePrisms ''Element
makeLenses ''Named

instance ToNamed Tag where
   toNamed = tagName . _Just

instance ToNamed Primitive where
   toNamed = primitiveName
   
instance ToNamed Element where
   toNamed f = \case 
      T x -> T <$> toNamed f x
      P x -> P <$> toNamed f x

instance ToChildren Element where
   toChildren f = \case
      T x -> T <$> toChildren f x
      P x -> pure $ P x

instance ToChildren Tag where
   toChildren = tagChildren
      
allAttribute :: ToChildren a => a -> a
allAttribute = over toChildren $ fmap (second (const Attribute))

allNested :: ToChildren a => a -> a
allNested = over toChildren $ fmap (second (const Nested))

child :: ToChildren a => Name -> Traversal' a (Element, ChildType)
child name = toChildren . at name . _Just 

-- An example
setChildType :: ToChildren a => String -> ChildType -> a -> a
setChildType = setChildPart _2

setChildNameVisibility :: ToChildren a => String -> Visiblity -> a -> a
setChildNameVisibility = setChildPart (_1 . _T . tagName . _Just . visibility)

setChildPart :: ToChildren a => Setter' (Element, ChildType) b -> String -> b -> a -> a
setChildPart setter name part x = set (child name . setter) part x

class ToElement a where
   toElement :: a -> Element
   
   default toElement :: (Generic a, GToElement (Rep a)) => a -> Element
   toElement = defaultGeneric
   
infixl 1 # 

class AppendAttribute a where
   (#) :: ToElement b => a -> (Name, b) -> a

instance AppendAttribute Tag where
   x # (n, a) = set (tagChildren . at n) (Just (toElement a, Attribute)) x
   
instance AppendAttribute Primitive where
   (#) x _ = x
   
instance AppendAttribute Element where
   (#) x y = case x of 
      P p -> P $ (#) p y
      T t -> T $ (#) t y
      
instance ToElement Element where
   toElement = id
   
defaultGeneric = makeGElement . gToElement . from

instance ToElement Int where
   toElement = P . Primitive (Named "" Hide) . Int

instance ToElement Double where
   toElement = P . Primitive (Named "" Hide) . Float
   
instance ToElement Bool where
   toElement = P . Primitive (Named "" Hide) . Bool   
   
instance ToElement String where
   toElement = P . Primitive (Named "" Hide) . String
   
instance ToElement Text where
   toElement = P . Primitive (Named "" Hide) . String . T.unpack
   
instance ToElement Spectrum where
   toElement = P . Primitive (Named "" Hide) . Spectrum

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

defaultPrimitiveName = "__primitive__"

makeSumType :: String -> String -> Element -> Tag
makeSumType dataTypeName constructorName elem 
   = Tag dataTypeName Nothing (Just constructorName)
   $ case elem of
       P x -> H.singleton defaultPrimitiveName (P x, Nested) -- Not that useful
       T x -> x ^. toChildren

makeProductType :: String -> [(String, Element)] -> Tag
makeProductType dataTypeName cs
   = Tag dataTypeName Nothing Nothing
   -- turn the [(id, x)] -> Maybe x
   -- all the children default as Nested 
   $ H.fromList $ map (second (,Nested)) cs

makeGElement :: State GToElementLog () -> Element
makeGElement w = result where
   GToElementLog {..} = execState w mempty
   
   Last (Just dtName) = _dataTypeName
   Last (Just ctName) = _constrName
   
   result = T $ case ( _children, getLast _result) of
      ([], Just x ) -> makeSumType     dtName ctName x
      (_, Nothing )  -> makeProductType dtName _children
      _ -> error "logic error in GToElement (D1 d (C1 c f)) gToTag"

makeLenses ''GToElementLog

instance (Datatype d, GToElement f) => GToElement (D1 d f) where
   gToElement (M1 x) = do 
      assign dataTypeName $ last' $ renameDataType $ datatypeName (undefined :: t d f p)
      gToElement x

instance (Constructor c, GToElement f) => GToElement (C1 c f) where
   gToElement (M1 x) = do
      Just dt <- getLast <$> use dataTypeName
      assign constrName $ last' $ renameConstructor dt $ conName (undefined :: t c f p)
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
   gToElement (M1 (K1 x)) = assign result $ last' $ toElement x 

instance (Selector s, ToElement x) => GToElement (S1 s (Rec0 x)) where
   gToElement (M1 (K1 x)) = do 
      Just dt <- getLast <$> use dataTypeName
      let sName = renameSelector dt $ selName (undefined :: t s (Rec0 x) p) 
      children <>= [( sName
                         , set toNamed (Named sName Show) $ toElement x
                         )]
                         
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

--
format' x = TL.toStrict . format x

class ToAttributeValue a where
   toAttributeValue :: a -> Text

instance ToAttributeValue Double where
   toAttributeValue = T.pack . show

instance ToAttributeValue String where
   toAttributeValue = T.pack

instance ToAttributeValue PrimitiveType where
   toAttributeValue = \case
         Float  x -> T.pack    $ show x
         String x -> T.pack    $ show x
         Int    x -> T.pack    $ show x
         Bool   x -> T.toLower $ T.pack $ show x
         Spectrum _ -> error "can't convert spectrum to attribute value!"

instance ToAttributeValue WavelengthStyle where
   toAttributeValue = T.intercalate ", "
           . map (\(x, y) -> toFixed 0 x <> ":" <> toFixed 2 y)
           . unWavelengthStyle

instance ToAttributeValue InternalSpectralFormat where
  toAttributeValue 
      = T.intercalate ", "
      . map (T.drop 1 . toFixed 1) 
      . toListOf each

instance ToAttributeValue RGBTriple where
   toAttributeValue 
      = T.intercalate ", "
      . map (toFixed 3)
      . toListOf each
      
instance ToAttributeValue RGBLike where
   toAttributeValue = forwardToAttributeValue
   
instance ToAttributeValue Temperature where
   toAttributeValue = format' "{}k" . Only . unTemperature

instance ToAttributeValue Hex where
   toAttributeValue x = 
      let formatter = left 2 '0' . hex 
      in TL.toStrict . format "#{}{}{}" $ map formatter $ listOf x each


instance ToAttributeValue Primitive where
   toAttributeValue = toAttributeValue . _primitiveType
   
instance ToAttributeValue Tag where
   toAttributeValue Tag {..} = 
      T.unwords $ fmap (toAttributeValue . fst) $ F.toList _tagChildren 
      
instance ToAttributeValue Element where
   toAttributeValue = \case
      P x -> toAttributeValue x
      T x -> toAttributeValue x

class ToAttribute a where
   toAttribute :: a -> Attribute

instance ToAttribute Primitive where
   toAttribute Primitive {..} = 
      a (T.pack $ _name _primitiveName) $ toAttributeValue _primitiveType 

instance ToAttribute Tag where
   toAttribute Tag {..} 
      = a (T.pack _tagTag)
      $ T.unwords 
      $ map (toAttributeValue . fst)
      $ F.toList _tagChildren
      
instance ToAttribute Element where
   toAttribute = \case
      P x -> toAttribute x
      T x -> toAttribute x
      
   
-- TODO make instances for Element, Primitive, Tag

class ToXML a where
   toXML :: a -> Markup
   
   default toXML :: ToElement a => a -> Markup
   toXML = toXML . toElement 

instance ToXML Primitive where
   toXML Primitive {..} = 
      let nameAttr = if _visibility _primitiveName == Show then 
                        [a "name" $ T.pack $ _name _primitiveName]
                     else
                        []
      in case _primitiveType of
            Float    x -> cta "float"   $ nameAttr ++ [a "value" (show x)]
            String   x -> cta "string"  $ nameAttr ++ [a "value" x]
            Int      x -> cta "integer" $ nameAttr ++ [a "value" (show x)]
            Bool     x -> cta "bool"    $ nameAttr ++ [a "value" (T.toLower $ T.pack $ show x)]
            Spectrum x -> case x of
               SWavelengths s -> cta "spectrum"  $ nameAttr ++ [a "value" (toAttributeValue s)]
               SUniform     s -> cta "spectrum"  $ nameAttr ++ [a "value" (toAttributeValue s)]
               SInternal    s -> cta "spectrum"  $ nameAttr ++ [a "value" (toAttributeValue s)]
               SRGB         s -> cta "rgb"       $ nameAttr ++ [a "value" (toAttributeValue s)]
               SSRGB        s -> cta "rgb"       $ nameAttr ++ [a "value" (toAttributeValue s)]
               SFile        s -> cta "spectrum"  $ nameAttr ++ [a "filename" (toAttributeValue s)]
               SBlackbody   s -> cta "blackbody" $ nameAttr ++ 
                                                    [ a "temperature" 
                                                         ( toAttributeValue
                                                         $ _blackBodyTemperature s
                                                         )
                                                    , a "scale" 
                                                          ( toAttributeValue
                                                          $ _blackBodyScale s
                                                          )
                                                    ]
      

instance ToXML Tag where
   toXML Tag {..} = 
      let (asAttrs, children) = partition (has _Attribute . snd . snd) $ H.toList _tagChildren
          
          attrs = catMaybes 
            [ do tn <- _tagName
                 n  <- if _visibility tn == Show then
                          return $ _name tn
                       else
                          Nothing
                 return $ a "name" n 
            , a "type" <$> _tagType
            ] ++ map (toAttribute . fst . snd) asAttrs
      
      in case map (fst . snd) children of
         [] -> cta (T.pack _tagTag) attrs
         xs -> ta (T.pack _tagTag) attrs $ foldr1 Append $ map toXML xs  

instance ToXML Element where
   toXML = \case
      P x -> toXML x
      T x -> toXML x

data Test = Test 
   { testBar :: Int
   , testFoo :: Int
   } deriving (Show, Eq, Generic)
   
instance ToElement Test

data Bar = Bar 
   { barThing :: Test
   } deriving (Show, Eq, Generic)
   
instance ToElement Bar

testXML = putStr . renderMarkup . toXML . toElement

data SumType 
   = S1 Bar
   | S2 AttributeThing
   deriving (Show, Eq, Generic)
   
instance ToElement SumType

data AttributeThing = AttributeThing
   { attributeThingX :: Int
   , attributeThingY :: Int
   } deriving (Show, Eq, Generic)
   
instance ToElement AttributeThing where
   toElement = allAttribute . defaultGeneric


forwardToXML :: (Generic a, GFold (Rep a) ToXML Markup) => a -> Markup
forwardToXML = gfold (Proxy :: Proxy ToXML) toXML

forwardToElement :: (Generic a, GFold (Rep a) ToElement Element) => a -> Element
forwardToElement = gfold (Proxy :: Proxy ToElement) toElement

forwardToAttributeValue :: (Generic a, GFold (Rep a) ToAttributeValue Text) => a -> Text
forwardToAttributeValue = gfold (Proxy :: Proxy ToAttributeValue) toAttributeValue




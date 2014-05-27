{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Mitsuba.Types.Primitives where
import Mitsuba.Element hiding (Visibility (Hidden))
import Control.Lens hiding ((#), (.>))
import Control.Lens.Fold
import Data.Word
import Data.Text.Format 
import Data.Double.Conversion.Text
import Control.Applicative
import Data.Monoid
import Data.Data
import GHC.Generics
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Text (Text)
import Text.Blaze
import Data.List
import qualified Data.HashMap.Strict as H
import Control.Arrow hiding (left)
import qualified Data.Foldable as F
import Data.Maybe
import Data.Default.Generics
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Function
import Data.Map (Map)
import qualified Data.Map as M
default (Text, Integer, Double)

newtype RefractiveValue = RefractiveValue { unRefractiveValue :: Double }
  deriving (Show, Eq, Ord, Read, Data, Generic, Typeable, ToElement)
  
instance Default RefractiveValue where
  def = RefractiveValue 1.000100
  
newtype PositiveDouble = PositiveDouble { unPositiveDouble :: Double }
  deriving (Show, Eq, Ord, Read, Data, Generic, Typeable, ToElement)
  
instance Default PositiveDouble where
  def = PositiveDouble 1
  
newtype NormalizedDouble = NormalizedDouble { unNormalizedDouble :: Double } 
  deriving (Show, Eq, Ord, Read, Data, Generic, Typeable, ToElement)
  
newtype RoughnessAlpha = RoughnessAlpha { unRoughnessAlpha :: Double }
  deriving (Show, Eq, Ord, Read, Data, Generic, Typeable, ToElement)

type Wavelength = Double
type Amplitude  = Double

data AtLeastTwo k v = AtLeastTwo { unAtLeastTwo :: Map k v }
  deriving (Eq, Show, Read, Ord, Generic, Typeable, Data)

makeAtLeastTwo k v k' v'
  | k /= k' = Just $ H.fromList [(k, v), (k', v')]
  | otherwise = Nothing

atleastTwoToList = M.toList . unAtLeastTwo


newtype WavelengthStyle = 
   WavelengthStyle { unWavelengthStyle :: AtLeastTwo PositiveDouble Amplitude }
      deriving(Eq, Show, Ord, Read, Data, Typeable, Generic)
      
instance Default WavelengthStyle where
  def = WavelengthStyle $ AtLeastTwo 
      $ M.fromList [(PositiveDouble 600, 0.5), (PositiveDouble  800, 0.5)]

data InternalSpectralFormat = InternalSpectralFormat
   { isfA :: Double
   , isfB :: Double
   , isfC :: Double
   , isfD :: Double
   , isfE :: Double
   , isfF :: Double
   , isfG :: Double
   , isfH :: Double
   , isfI :: Double
   , isfJ :: Double
   } deriving(Eq, Show, Ord, Read, Data, Typeable, Generic)

instance Default InternalSpectralFormat

instance Each InternalSpectralFormat InternalSpectralFormat Double Double where
   each func InternalSpectralFormat {..} 
       =  InternalSpectralFormat
      <$> func isfA
      <*> func isfB
      <*> func isfC
      <*> func isfD
      <*> func isfE
      <*> func isfF
      <*> func isfG
      <*> func isfH
      <*> func isfI 
      <*> func isfJ 

data RGBTriple = RGBTriple
   { rgbTripleR :: Double
   , rgbTripleG :: Double
   , rgbTripleB :: Double
   } deriving(Eq, Show, Ord, Read, Data, Typeable, Generic)
   
instance Default RGBTriple

instance Each RGBTriple RGBTriple Double Double where
   each f RGBTriple {..} 
       =  RGBTriple 
      <$> f rgbTripleR
      <*> f rgbTripleG
      <*> f rgbTripleB

data Hex = Hex Word8 Word8 Word8
   deriving(Eq, Show, Ord, Read, Data, Typeable, Generic)
   
instance Default Hex

instance Each Hex Hex Word8 Word8 where
   each f (Hex a b c) = Hex <$> f a <*> f b <*> f c

listOf :: s -> Getting (Data.Monoid.Endo [a]) s a -> [a]
listOf = (^..)

data RGBLike 
   = RGBLTriple RGBTriple
   | RGBLHex    Hex
   deriving(Eq, Show, Ord, Read, Data, Typeable, Generic)
   
instance Default RGBLike

newtype Temperature = Temperature { unTemperature :: Integer }
   deriving(Eq, Show, Ord, Read, Data, Typeable, Generic, Num)
   
instance Default Temperature

data Blackbody = Blackbody 
   { blackBodyTemperature :: Temperature
   , blackBodyScale       :: Double
   } deriving(Eq, Show, Ord, Read, Data, Typeable, Generic)
   
instance Default Blackbody

data Spectrum 
   = SWavelengths WavelengthStyle
   | SUniform     Double
   | SInternal    InternalSpectralFormat
   | SRGB         RGBLike
   | SSRGB        RGBLike
   | SFile        FilePath
   | SBlackbody   Blackbody
   deriving(Eq, Show, Ord, Read, Data, Typeable, Generic)
   
instance Default Spectrum

format' x = TL.toStrict . format x

-- must be in increasing order
instance ToAttributeValue WavelengthStyle where
   toAttributeValue = T.intercalate ", "
           . map (\(PositiveDouble x, y) -> (T.pack . show) x <> ":" <> (T.pack . show) y) 
           . sortBy (\x y -> fst x `compare` fst y) . atleastTwoToList . unWavelengthStyle

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

instance ToElement Spectrum where
   toElement = \case
      SWavelengths x -> primitive "spectrum" x
      SUniform     x -> primitive "spectrum" x
      SInternal    x -> primitive "spectrum" x
      SRGB         x -> primitive "rgb"      x
      SSRGB        x -> primitive "srgb"      x
      SFile        x -> tag "spectrum"  # ("filename", x)
      SBlackbody   x -> tag "blackbody" 
                           # ("temperature", blackBodyTemperature x)
                           # ("scale"      , blackBodyScale       x)
                           
makePrisms ''Spectrum
   
newtype Ref a = Ref { unRef :: String }
   deriving(Show, Read, Data, Typeable, Generic)
   
instance Default a => Default (Ref a)
   
instance Eq (Ref a) where
   Ref x == Ref y = x == y
   
instance Ord (Ref a) where
   Ref x `compare` Ref y = compare x y

instance ToAttributeValue (Ref a) where
   toAttributeValue = toAttributeValue . unRef

instance ToElement (Ref a) where
   toElement Ref {..} = tag "ref" # ("id", unRef) 

data Child a 
   = CRef    (Ref a)
   | CNested a
   deriving(Eq, Show, Ord, Read, Data, Typeable, Generic)

instance Default a => Default (Child a)

makePrisms ''Child
  
instance ToElement a => ToElement (Child a) where
   toElement = forwardToElement

data Point = Point 
   { pointX :: Double
   , pointY :: Double
   , pointZ :: Double
   } deriving(Eq, Show, Ord, Read, Data, Typeable, Generic)

instance Default Point   

zeroPoint = Point 0 0 0
   
instance Each Point Point Double Double where
   each f Point {..} = Point <$> f pointX <*> f pointY <*> f pointZ
   
instance ToElement Point where
   toElement Point {..} 
      = tag "point"
         # ("x", pointX)
         # ("y", pointY)
         # ("z", pointZ)

data Vector = Vector 
   { vectorX :: Double
   , vectorY :: Double
   , vectorZ :: Double
   } deriving(Eq, Show, Ord, Read, Data, Typeable, Generic)

instance Default Vector

instance Each Vector Vector Double Double where
   each f Vector {..} = Vector <$> f vectorX <*> f vectorY <*> f vectorZ
   
instance ToElement Vector where
   toElement Vector {..}
      = tag "vector" 
         # ("x", vectorX)
         # ("y", vectorY)
         # ("z", vectorZ)

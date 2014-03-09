{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
module Mitsuba.Primitive where
import Control.Lens hiding ((#))
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
default (Text, Integer, Double)

type Wavelength = Double
type Amplitude  = Double

newtype WavelengthStyle = 
   WavelengthStyle { unWavelengthStyle :: [(Wavelength, Amplitude)] }
      deriving(Eq, Show, Ord, Read, Data, Typeable, Generic)

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

instance Each RGBTriple RGBTriple Double Double where
   each f RGBTriple {..} 
       =  RGBTriple 
      <$> f rgbTripleR
      <*> f rgbTripleG
      <*> f rgbTripleB

data Hex = Hex Word8 Word8 Word8
   deriving(Eq, Show, Ord, Read, Data, Typeable, Generic)

instance Each Hex Hex Word8 Word8 where
   each f (Hex a b c) = Hex <$> f a <*> f b <*> f c

listOf :: s -> Getting (Data.Monoid.Endo [a]) s a -> [a]
listOf = (^..)

data RGBLike 
   = RGBLTriple RGBTriple
   | RGBLHex    Hex
   deriving(Eq, Show, Ord, Read, Data, Typeable, Generic)

newtype Temperature = Temperature { unTemperature :: Integer }
   deriving(Eq, Show, Ord, Read, Data, Typeable, Generic)

data Blackbody = Blackbody 
   { _blackBodyTemperature :: Temperature
   , _blackBodyScale       :: Double
   } deriving(Eq, Show, Ord, Read, Data, Typeable, Generic)

data Spectrum 
   = SWavelengths WavelengthStyle
   | SUniform     Double
   | SInternal    InternalSpectralFormat
   | SRGB         RGBLike
   | SSRGB        RGBLike
   | SFile        FilePath
   | SBlackbody   Blackbody
   deriving(Eq, Show, Ord, Read, Data, Typeable, Generic)
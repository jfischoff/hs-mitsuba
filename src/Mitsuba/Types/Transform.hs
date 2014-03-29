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
module Mitsuba.Types.Transform where
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
import Mitsuba.Types.Primitives
import Data.Default.Generics
default (Text, Integer, Double)
-- I want to be able to monoid on transforms
-- I think the write way to think about it is that the
-- transforms are always with times
-- so how does that work?
-- if they are the same days 

-- I think I need a user interface for this stuff

data Translate = Translate 
   { translateX :: Double
   , translateY :: Double
   , translateZ :: Double
   } deriving(Eq, Show, Ord, Read, Data, Typeable, Generic)
   
instance Default Translate

instance Each Translate Translate Double Double where
   each f Translate {..} 
       =  Translate
      <$> f translateX
      <*> f translateY
      <*> f translateZ    

instance ToElement Translate where
   toElement Translate {..} 
      = tag "translate" 
         # ("x", translateX)
         # ("y", translateY)
         # ("z", translateZ)

data Rotate = Rotate
   { rotateAxis  :: Vector
   , rotateAngle :: Double
   } deriving(Eq, Show, Ord, Read, Data, Typeable, Generic)

instance Default Rotate

instance ToElement Rotate where
   toElement Rotate {..} 
      = tag "rotate"
         # ("x"    , vectorX rotateAxis    )
         # ("y"    , vectorY rotateAxis    )
         # ("z"    , vectorZ rotateAxis    )
         # ("angle", rotateAngle)

data Scale 
   = SUniformScale Double
   | SScaleAxis    Vector
    deriving(Eq, Show, Ord, Read, Data, Typeable, Generic)
    
instance Default Scale

instance ToElement Scale where
   toElement = \case
     SUniformScale x -> primitive "scale" x
     SScaleAxis    x -> tag "scale" `appendChildren` x

data Matrix = Matrix
   { m00 :: Double
   , m01 :: Double
   , m02 :: Double
   , m03 :: Double
   , m10 :: Double
   , m11 :: Double
   , m12 :: Double
   , m13 :: Double
   , m20 :: Double
   , m21 :: Double
   , m22 :: Double
   , m23 :: Double 
   , m30 :: Double
   , m31 :: Double
   , m32 :: Double
   , m33 :: Double
   } deriving(Eq, Show, Ord, Read, Data, Typeable, Generic)
   
instance Default Matrix

instance Each Matrix Matrix Double Double where
   each f Matrix {..} 
       =  Matrix    
      <$> f m00
      <*> f m01
      <*> f m02
      <*> f m03
      <*> f m10
      <*> f m11
      <*> f m12
      <*> f m13
      <*> f m20
      <*> f m21
      <*> f m22
      <*> f m23
      <*> f m30
      <*> f m31
      <*> f m32
      <*> f m33

instance ToElement Matrix where
   toElement m = 
      let values = T.intercalate " " $ map (T.pack . show) $ toListOf each m
      in tag "matrix" # ("value", values)

data Lookat = Lookat 
   { origin :: Point
   , target :: Point
   , up     :: Vector
   } deriving(Eq, Show, Ord, Read, Data, Typeable, Generic)
   
instance Default Lookat

commaSeperateValues = T.intercalate ", " . map (T.pack . show)   

instance ToElement Lookat where
   toElement Lookat {..} = 
      let originPoint = commaSeperateValues $ toListOf each origin
          targetPoint = commaSeperateValues $ toListOf each target
          upVector    = commaSeperateValues $ toListOf each up

      in tag "lookat"
             # ("origin", originPoint) 
             # ("target", targetPoint) 
             # ("up"    , upVector   )

data TransformCmd 
   = TCTranslate Translate
   | TCRotate    Rotate
   | TCScale     Scale
   | TCMatrix    Matrix
   | TCLookat    Lookat
    deriving(Eq, Show, Ord, Read, Data, Typeable, Generic)
    
instance Default TransformCmd

translate :: Double -> Double -> Double -> RegularTransform
translate x y z = RegularTransform $ pure $ TCTranslate $ Translate x y z

rotateX :: Double -> RegularTransform
rotateX = RegularTransform . pure . TCRotate . Rotate (Vector 1 0 0)

rotateY :: Double -> RegularTransform
rotateY = RegularTransform . pure . TCRotate . Rotate (Vector 0 1 0)

rotateZ :: Double -> RegularTransform
rotateZ = RegularTransform . pure . TCRotate . Rotate (Vector 0 0 1)

uniformScale :: Double -> RegularTransform 
uniformScale = RegularTransform . pure . TCScale . SUniformScale 

scale :: Double -> Double -> Double -> RegularTransform
scale x y z = RegularTransform . pure . TCScale . SScaleAxis $ Vector x y z

uniformScaleZ :: Double -> RegularTransform
uniformScaleZ = RegularTransform . pure . TCScale . SScaleAxis . Vector 1 1

instance ToElement TransformCmd where
   toElement = forwardToElement

newtype RegularTransform = 
   RegularTransform { unRegularTransform :: [TransformCmd] }
    deriving(Eq, Show, Ord, Read, Data, Typeable, Generic)
    
instance Default RegularTransform

instance Monoid RegularTransform where
   mempty = RegularTransform mempty
   RegularTransform xs `mappend` RegularTransform ys
      = RegularTransform $ xs <> ys

instance ToElement RegularTransform where
   toElement (RegularTransform xs) = 
       tag "transform" `addChildList` xs

newtype Animation = Animation {unAnimation :: [(Double, RegularTransform)] }
    deriving(Eq, Show, Ord, Read, Data, Typeable, Generic)
    
instance Default Animation

instance ToElement Animation where
   toElement (Animation xs) 
      = let toKeyframe (k, trans) = toElement trans # ("time", k) 
      in tag "animation" `addChildList` map toKeyframe xs

data Transform 
   = TAnimated Animation
   | TRegular  RegularTransform
    deriving(Eq, Show, Ord, Read, Data, Typeable, Generic)
    
instance Default Transform

-- Need to use a different Eq instance for this to have a monoid, but I am lazy
instance Monoid Transform where
  mempty = TRegular mempty
  mappend a b = case (a, b) of
    (TRegular x, TRegular y) -> TRegular $ x <> y

instance ToElement Transform where
   toElement = forwardToElement










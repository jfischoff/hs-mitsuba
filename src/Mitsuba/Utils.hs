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
module Mitsuba.Utils where
import Mitsuba.Types
import Mitsuba.Element hiding (Visibility (Hidden))
import qualified Mitsuba.Element as E
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
import Data.HashMap.Strict (HashMap)
import Control.Arrow hiding (left)
import qualified Data.Foldable as F
import Data.Maybe
import Mitsuba.Types.Primitives
import Mitsuba.Types.Transform
import Mitsuba.LensTH
import Data.Default.Generics
import Data.Map (Map)
import qualified Data.Map as M
import Language.Haskell.TH.Module.Magic

class HasToWorld a where
  toWorld :: Traversal' a Transform

class HasBSDF a where
  bsdf :: Traversal' a BSDF
  
instance HasToWorld ShapeLeaf where
  toWorld = shapeLeafToWorldL

instance HasBSDF (Child BSDF) where
  bsdf f = \case
    CRef x -> CRef <$> pure x
    CNested x -> CNested <$> f x
    
instance HasBSDF ShapeLeaf where
  bsdf = shapeLeafLeafL.bsdf

instance HasBSDF SimpleShapeLeaf where
  bsdf = simpleShapeLeafMaterialL . bsdf
  
instance (HasBSDF a, HasBSDF b) => HasBSDF (Either a b) where
  bsdf f = \case
    Left  x -> Left  <$> bsdf f x
    Right x -> Right <$> bsdf f x
    
instance HasBSDF OBJLeaf where
  bsdf f OBJLeaf {..} 
     =  OBJLeaf objLeafObj 
    <$> case objLeafMaterial of
          Left  x -> Left  <$> bsdf f x
          Right x -> Right <$> pure x
    
twosided :: NonTransmission -> BSDF
twosided = BSDFTwosided . Twosided . CNested

diffuse :: BSDF
diffuse = BSDFDiffuse $ Diffuse $ CSpectrum $ SUniform 1.0

class DielectricI a where
  dielectric :: a -> a -> BSDF
  
instance DielectricI RefractiveValue where
  dielectric x y = BSDFDielectric $ Dielectric 
     { dielectricIntIOR                = IOR x
     , dielectricExtIOR                = IOR y
     , dielectricSpecularReflectance   = def
     , dielectricSpecularTransmittance = def
     }

instance DielectricI KnownMaterial where
  dielectric x y = BSDFDielectric $ Dielectric 
     { dielectricIntIOR                = RKM x
     , dielectricExtIOR                = RKM y
     , dielectricSpecularReflectance   = def
     , dielectricSpecularTransmittance = def
     }
  
instance HasToWorld Shape where
  toWorld = _SShapeLeaf . toWorld

instance HasBSDF Shape where
  bsdf f = \case
    SShapeGroup xs -> SShapeGroup     <$> traverse (bsdf f) xs
    SShapeLeaf x   -> SShapeLeaf <$> bsdf f x

objMaterialMap :: Traversal' Shape (Map Text (Child BSDF)) 
objMaterialMap = _SShapeLeaf . shapeLeafLeafL . _Right . objLeafMaterialL . _Right

shapeLeaf :: ShapeType -> Shape 
shapeLeaf st = SShapeLeaf
    $ ShapeLeaf
        ( Left
        $ SimpleShapeLeaf 
            st
            (CNested diffuse)
        )
        mempty
        Nothing
        Nothing

cube :: Shape
cube = shapeLeaf $ STCube $ Cube False

sphere :: PositiveDouble -> Shape 
sphere radius 
  = SShapeLeaf 
  $ ShapeLeaf 
    ( Left
    $ SimpleShapeLeaf 
        (STSphere $ Sphere zeroPoint radius True)
        (CNested diffuse) 
    )
    mempty
    Nothing
    Nothing

cylinder :: PositiveDouble -> Shape 
cylinder radius 
  = SShapeLeaf
  $ ShapeLeaf
      ( Left
      $ SimpleShapeLeaf
          (STCylinder $ Cylinder zeroPoint (Point 0 1 0) radius True)
          (CNested diffuse)
      )
      mempty
      Nothing
      Nothing

-- TODO make the rectangle command
square :: Shape
square 
  = SShapeLeaf
  $ ShapeLeaf 
      ( Left 
      $ SimpleShapeLeaf 
          (STRectangle $ Rectangle True)
          (CNested diffuse)
      )
      mempty
      Nothing
      Nothing

--rectangle :: Double -> Double -> Shape 
--rectangle width height 
--  = Shape (STRectangle )

disk :: Shape
disk 
  = SShapeLeaf
  $ ShapeLeaf
    ( Left
    $ SimpleShapeLeaf 
        (STDisk $ Disk True)
        (CNested diffuse)
    )
    mempty
    Nothing
    Nothing  

obj :: FilePath -> Shape 
obj filePath
  = SShapeLeaf
  $ ShapeLeaf
      ( Right
      $ OBJLeaf
          ( OBJ 
              filePath
              False
              0.0
              False
              False
          )
         (Left $ CNested diffuse)
      )
      mempty
      Nothing
      Nothing

ply :: FilePath -> Shape
ply filePath = SShapeLeaf
    $ ShapeLeaf
        ( Left
        $ SimpleShapeLeaf 
            (STPLY $ PLY filePath True 0.0 False False)
            (CNested diffuse)
        )
        mempty
        Nothing
        Nothing

objMultiMaterial :: FilePath -> Shape
objMultiMaterial filePath
  = SShapeLeaf
  $ ShapeLeaf
      ( Right
      $ OBJLeaf
          ( OBJ 
              filePath
              False
              0.0
              False
              False
          )
         (Right mempty)
      )
      mempty
      Nothing
      Nothing

serialized :: FilePath -> Shape
serialized filepath = shapeLeaf $ STSerialized $ Serialized
  { serializedFilename       = filepath
  , serializedShapeIndex     = 0
  , serializedFaceNormals    = True
  , serializedMaxSmoothAngle = 0.0
  , serializedFlipNormals    = False
  }

instanceShape :: Ref Shape -> Shape
instanceShape = shapeLeaf . STInstance . Instance

hair :: FilePath -> Shape 
hair filePath = shapeLeaf $ STHair $ Hair
  { hairFilename       = filePath
  , hairRadius         = 1.0
  , hairAngleThreshold = 0.0
  , hairReduction      = 0.0
  , hairWidth          = 1
  , hairHeight         = 1
  , hairTexture        = TCheckerboard $ def
  }

heightField :: FilePath -> Shape 
heightField filePath = shapeLeaf $ STHeightField $ HeightField
  { heightFieldShadingNormals = True
  , heightFieldFlipNormals    = False
  , heightFieldWidth          = 1
  , heightFieldHeight         = 1
  , heightFieldScale          = 1
  , heightFieldFilename       = filePath
  , heightFieldTexture        = TCheckerboard $ def
  }


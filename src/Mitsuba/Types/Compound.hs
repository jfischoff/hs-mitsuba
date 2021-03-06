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

module Mitsuba.Types.Compound where
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
import MonadUtils
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
default (Text, Integer, Double)

-- positive, non zero, integer
newtype Whole = Whole { unWhole :: Integer }
  deriving (Eq, Show, Ord, Read, Data, Generic, Typeable, ToElement)
  
instance Default Whole where
  def = Whole 1
  
newtype NonZero = NonZero { unNonZero :: Double }
  deriving (Eq, Show, Ord, Read, Data, Generic, Typeable, ToElement)
  
instance Default NonZero where
  def = NonZero 1

toWhole :: Integer -> Maybe Whole  
toWhole x 
  | x > 0     = Just $ Whole x
  | otherwise = Nothing
  
tshow :: Show a => a -> Text
tshow = T.pack . show

data Alias a = Alias
   { aliasId :: Ref a
   , aliasAs :: String
   } deriving(Show, Read, Data, Typeable, Generic)
   
instance Default a => Default (Alias a)
   
instance Eq (Alias a) where
   Alias x0 y0 == Alias x1 y1 
       = x0 == x1 
      && y0 == y1
      
instance Ord (Alias a) where
   Alias x0 y0 `compare` Alias x1 y1 = 
      compare (compare x0 x1) (compare y0 y1)

instance ToElement (Alias a) where
   toElement Alias {..} = 
      tag "alias" # ("id", aliasId) # ("as", aliasAs)

data Channel 
   = R
   | G
   | B
   | A
   | X
   | Y
   | Z
   | All
   deriving(Eq, Show, Ord, Read, Data, Typeable, Generic, Bounded, Enum)
   
instance Default Channel

defaultShowInstance :: Show a => a -> Element
defaultShowInstance = toElement . T.toLower . T.pack . show
   
instance ToElement Channel where
   toElement = defaultShowInstance   
   
data WrapMode
   = Repeat
   | Mirror
   | Clamp
   | Zero
   | One
   deriving(Eq, Show, Ord, Read, Data, Typeable, Generic, Bounded, Enum)
   
instance Default WrapMode
   
instance ToElement WrapMode where
   toElement = defaultShowInstance
   
data FilterType
   = EWA
   | Trilinear
   | Nearest
   deriving(Eq, Show, Ord, Read, Data, Typeable, Generic, Bounded, Enum)
   
instance Default FilterType
   
instance ToElement FilterType where
   toElement = defaultShowInstance

{-
data CachePolicy = Cache | DontCache
   deriving(Eq, Show, Ord, Read, Data, Typeable, Generic, Bounded, Enum)
   
instance Default CachePolicy   
   
instance ToElement CachePolicy where
   toElement = defaultShowInstance
-}

data WrapUV = WrapUV 
  { wrapUVWrapModeU :: WrapMode
  , wrapUVWrapModeV :: WrapMode
  } deriving (Show, Eq, Read, Ord, Generic, Data, Typeable)

instance Default WrapUV  
instance ToElement WrapUV

data Bitmap = Bitmap 
   { bitmapFilename      :: FilePath
   , bitmapWrapMode      :: Either WrapMode WrapUV
   , bitmapGamma         :: Maybe Double
   , bitmapFilterType    :: FilterType
   , bitmapMaxAnisotropy :: Double
   , bitmapCache         :: Bool
   , bitmapUoffset       :: Double
   , bitmapVoffset       :: Double
   , bitmapUscale        :: Double
   , bitmapVscale        :: Double
   , bitmapChannel       :: Channel 
   } deriving (Show, Eq, Read, Ord, Generic, Data, Typeable)

instance Default Bitmap
instance ToElement Bitmap where
  toElement Bitmap {..} = 
    ((case bitmapWrapMode of
         Left  x -> (.> ("wrapMode", x))
         Right x -> (`appendChildren` x)) 
    (tag "bitmap" .> ("filename", bitmapFilename)))
    .?> ("gamma"        , bitmapGamma)
    .>  ("filterType"   , bitmapFilterType   )
    .>  ("maxAnisotropy", bitmapMaxAnisotropy)
    .>  ("cache"        , bitmapCache        )
    .>  ("uoffset"      , bitmapUoffset      )
    .>  ("voffset"      , bitmapVoffset      )
    .>  ("uscale"       , bitmapUscale       )
    .>  ("vscale"       , bitmapVscale       )
    .>  ("channel"      , bitmapChannel      )

data Checkerboard = Checkerboard
   { checkerboardColor0  :: Spectrum
   , checkerboardColor1  :: Spectrum
   , checkerboardUoffset :: Double
   , checkerboardVoffset :: Double
   , checkerboardUscale  :: Double
   , checkerboardVscale  :: Double
   } deriving (Show, Eq, Read, Ord, Generic, Data, Typeable)

instance Default Checkerboard
instance ToElement Checkerboard 
  

data GridTexture = GridTexture
   { gridTextureColor0    :: Spectrum
   , gridTextureColor1    :: Spectrum
   , gridTextrueLineWidth :: Double
   , gridTextureUScale    :: Double
   , gridTextureVScale    :: Double
   , gridTextureUOffset   :: Double
   , gridTextureVOffset   :: Double
   } deriving (Show, Eq, Read, Ord, Generic, Data, Typeable)

instance Default GridTexture
instance ToElement GridTexture

data Color = CSpectrum Spectrum | CTexture Texture
   deriving(Eq, Show, Ord, Read, Data, Typeable, Generic)

instance Default Color
instance ToElement Color where
   toElement = forwardToElement

data ScaleTexture = ScaleTexture
   { scaleTextureTexture :: Color
   , scaleTextureScale   :: Double
   } deriving (Show, Eq, Read, Ord, Generic, Data, Typeable)
   
instance Default ScaleTexture
instance ToElement ScaleTexture where
  toElement ScaleTexture {..} 
    = tag "dummy" 
    .>  ("scale", scaleTextureScale)
    .!> ("texture", scaleTextureTexture)

data Wireframe = Wireframe
   { wireframeInteriorColor :: Spectrum
   , wireframeEdgeColor     :: Spectrum
   , wireframeLineWidth     :: Double
   , wireframeStepWidth     :: Double
   } deriving (Show, Eq, Read, Ord, Generic, Data, Typeable)

instance Default Wireframe
instance ToElement Wireframe

data CurvatureType = Mean | Gaussian   
   deriving (Show, Eq, Read, Ord, Generic, Data, Typeable)

instance Default CurvatureType
instance ToElement CurvatureType where
   toElement = defaultShowInstance

data Curvature = Curvature
   { curvatureCurvature :: CurvatureType
   , curvatureScale     :: Double
   } deriving (Show, Eq, Read, Ord, Generic, Data, Typeable)

instance Default Curvature
instance ToElement Curvature

data Texture 
   = TBitmap       Bitmap
   | TCheckerboard Checkerboard
   | TGridtexture  GridTexture
   | TScale        ScaleTexture
   | TVertexcolors 
   | TWireframe    Wireframe
   | TCurvature    Curvature
   deriving (Show, Eq, Read, Ord, Generic, Data, Typeable)

instance Default Texture
instance ToElement Texture where
  toElement e = case e of
    TBitmap       x -> (tag "texture" # ("type", "bitmap"))
                    `appendChildren` x
    TCheckerboard x -> (tag "texture" # ("type", "checkerboard"))
                    `appendChildren` x
    TGridtexture  x -> (tag "texture" # ("type", "gridtexture"))
                    `appendChildren` x
    TScale        x -> (tag "texture" # ("type", "scale"))
                    `appendChildren` x
    TVertexcolors   -> (tag "texture" # ("type", "vertexcolors"))
    TWireframe    x -> (tag "texture" # ("type", "wireframe"))
                    `appendChildren` x
    TCurvature    x -> (tag "texture" # ("type", "curvature"))
                    `appendChildren` x
    

data Cube = Cube 
   { cubeFlipNormals :: Bool
   } deriving(Eq, Show, Ord, Read, Data, Typeable, Generic)

instance Default Cube
instance ToElement Cube  
   
data Sphere = Sphere 
   { sphereCenter      :: Point
   , sphereRadius      :: PositiveDouble
   , sphereFlipNormals :: Bool
   } deriving(Eq, Show, Ord, Read, Data, Typeable, Generic) 
   
instance Default Sphere
instance ToElement Sphere

data Cylinder = Cylinder 
   { cylinderP0          :: Point
   , cylinderP1          :: Point
   , cylinderRadius      :: PositiveDouble
   , cylinderFlipNormals :: Bool
   } deriving(Eq, Show, Ord, Read, Data, Typeable, Generic)

instance Default Cylinder
instance ToElement Cylinder
   
data Rectangle = Rectangle 
   { rectangleFlipNormals :: Bool
   } deriving(Eq, Show, Ord, Read, Data, Typeable, Generic)

instance Default Rectangle   
instance ToElement Rectangle
   
data Disk = Disk 
   { diskFlipNormals :: Bool
   } deriving(Eq, Show, Ord, Read, Data, Typeable, Generic)
   
instance Default Disk
instance ToElement Disk

data OBJ = OBJ
   { objFilename       :: String
   , objFaceNormals    :: Bool
   , objMaxSmoothAngle :: Double
   , objFlipNormals    :: Bool
   , objFlipTexCoords  :: Bool
   } deriving(Eq, Show, Ord, Read, Data, Typeable, Generic)
   
instance Default OBJ
instance ToElement OBJ

data Serialized = Serialized 
   { serializedFilename       :: String
   , serializedShapeIndex     :: Int
   , serializedFaceNormals    :: Bool
   , serializedMaxSmoothAngle :: Double
   , serializedFlipNormals    :: Bool
   } deriving(Eq, Show, Ord, Read, Data, Typeable, Generic)
   
instance Default Serialized
instance ToElement Serialized

data PLY = PLY 
   { plyFilename       :: String
   , plyFaceNormals    :: Bool
   , plyMaxSmoothAngle :: Double
   , plyFlipNormals    :: Bool
   , plySrgb           :: Bool
   } deriving(Eq, Show, Ord, Read, Data, Typeable, Generic)
   
instance Default PLY
instance ToElement PLY

data Instance = Instance 
  { instanceRef     :: Ref Shape
  } deriving(Eq, Show, Ord, Read, Data, Typeable, Generic)
  
instance Default Instance

instance ToElement Instance where
  toElement Instance {..} = tag "dummy" 
    .!> ("ref"    , instanceRef)

data Hair = Hair 
   { hairFilename       :: String
   , hairRadius         :: Double
   , hairAngleThreshold :: Double
   , hairReduction      :: Double
   , hairWidth          :: Int
   , hairHeight         :: Int
   , hairTexture        :: Texture
   } deriving(Eq, Show, Ord, Read, Data, Typeable, Generic)
   
instance Default Hair   
instance ToElement Hair

data HeightField = HeightField
  { heightFieldShadingNormals :: Bool
  , heightFieldFlipNormals    :: Bool
  , heightFieldWidth          :: Integer
  , heightFieldHeight         :: Integer
  , heightFieldScale          :: Double
  , heightFieldFilename       :: FilePath
  , heightFieldTexture        :: Texture
  } deriving(Eq, Show, Ord, Read, Data, Typeable, Generic)

instance Default HeightField   
instance ToElement HeightField

data Diffuse = Diffuse 
   { diffuseReflectance :: Color
   } deriving(Eq, Show, Ord, Read, Data, Typeable, Generic)
   
instance Default Diffuse
instance ToElement Diffuse
   
data RoughDiffuse = RoughDiffuse 
   { roughDiffuseReflectance   :: Color
   , roughDiffuseAlpha         :: Luminance
   , roughDiffuseUseFastApprox :: Bool
   } deriving(Eq, Show, Ord, Read, Data, Typeable, Generic)

instance Default RoughDiffuse
instance ToElement RoughDiffuse
   
data KnownMaterial 
   = Vacuum
   | Helium
   | Hydrogen
   | Air
   | CarbonDioxide
   | Water
   | Acetone
   | Ethanol
   | CarbonTetrachloride
   | Glycerol
   | Benzene
   | SiliconeOil
   | Bromine
   | WaterIce
   | FusedQuartz
   | Pyrex
   | AcrylicGlass
   | Polypropylene
   | Bk7
   | SodiumChloride
   | Amber
   | Pet
   | Diamond
   deriving(Eq, Show, Ord, Read, Data, Typeable, Generic, Enum, Bounded)

instance Default KnownMaterial

instance ToAttributeValue KnownMaterial where
   toAttributeValue = \case
      Vacuum              -> "vacuum"
      Helium              -> "helium"
      Hydrogen            -> "hydrogen"
      Air                 -> "air"
      CarbonDioxide       -> "carbon dioxide"
      Water               -> "water"
      Acetone             -> "acetone"
      Ethanol             -> "ethanol"
      CarbonTetrachloride -> "carbon tetrachloride"
      Glycerol            -> "glycerol"
      Benzene             -> "benzene"
      SiliconeOil         -> "silicone oil"
      Bromine             -> "bromine"
      WaterIce            -> "water ice"
      FusedQuartz         -> "fused quartz"
      Pyrex               -> "pyrex"
      AcrylicGlass        -> "acrylic glass"
      Polypropylene       -> "polypropylene"
      Bk7                 -> "bk7"
      SodiumChloride      -> "sodium chloride"
      Amber               -> "amber"
      Pet                 -> "pet"
      Diamond             -> "diamond"

instance ToElement KnownMaterial where
   toElement = toElement . toAttributeValue

data Refraction 
   = RKM KnownMaterial 
   | IOR RefractiveValue 
   deriving(Eq, Show, Ord, Read, Data, Typeable, Generic)
   
instance Default Refraction   
instance ToElement Refraction where
   toElement = forwardToElement
   
data RefractionPair = RefractionPair 
  { refractionPairIntIOR :: Refraction
  , refractionPairExtIOR :: Refraction
  } deriving(Eq, Show, Ord, Read, Data, Typeable, Generic)
  
instance Default RefractionPair   
instance ToElement RefractionPair

data Dielectric = Dielectric 
   { dielectricIntIOR                :: Refraction
   , dielectricExtIOR                :: Refraction
   , dielectricSpecularReflectance   :: Color
   , dielectricSpecularTransmittance :: Color
   } deriving(Eq, Show, Ord, Read, Data, Typeable, Generic)

instance Default Dielectric
instance ToElement Dielectric where
  toElement Dielectric {..} 
    =  tag "dielectric"
    .> ("intIOR", dielectricIntIOR)
    .> ("extIOR", dielectricExtIOR)
    .> ("specularReflectance"  , dielectricSpecularReflectance)
    .> ("specularTransmittance", dielectricSpecularTransmittance)

data ThinDielectric = ThinDielectric
   { thinDielectricIntIOR                :: Refraction
   , thinDielectricExtIOR                :: Refraction
   , thinDielectricSpecularReflectance   :: Color
   , thinDielectricSpecularTransmittance :: Color
   } deriving(Eq, Show, Ord, Read, Data, Typeable, Generic)

instance Default ThinDielectric
instance ToElement ThinDielectric where
  toElement ThinDielectric {..} 
    =  tag "thindielectric"
    .> ("intIOR"               , thinDielectricIntIOR)
    .> ("extIOR"               , thinDielectricExtIOR)
    .> ("specularReflectance"  , thinDielectricSpecularReflectance)
    .> ("specularTransmittance", thinDielectricSpecularTransmittance)
   
data Distribution 
   = Beckmann
   | GGX
   | PhongDistribution
    deriving(Eq, Show, Ord, Read, Data, Typeable, Generic, Enum, Bounded)

instance Default Distribution
instance ToAttributeValue Distribution where
   toAttributeValue = \case
      Beckmann          -> "beckmann"
      GGX               -> "ggx"
      PhongDistribution -> "phong"

instance ToElement Distribution where
   toElement = toElement . toAttributeValue

data Luminance 
   = UniformLuminance Double
   | TextureLuminance Texture
   deriving (Eq, Show, Ord, Read, Data, Typeable, Generic)
   
instance Default Luminance
instance ToElement Luminance where
   toElement = forwardToElement

data AnistrophicAlpha = AnistrophicAlpha
  { anistrophicAlphaU :: Luminance
  , anistrophicAlphaV :: Luminance
  } deriving(Eq, Show, Ord, Read, Data, Typeable, Generic)
  
instance ToElement AnistrophicAlpha where
  toElement AnistrophicAlpha {..} 
    =  tag "dummy"
    .> ("alphaU", anistrophicAlphaU)
    .> ("alphaV", anistrophicAlphaV)
  
instance Default AnistrophicAlpha

data UniformAlpha = UniformAlpha 
  { uniformAlphaDistribution :: Distribution
  -- TODO this must be in a range 0 - 4
  , uniformAlphaAlpha        :: Luminance
  } deriving(Eq, Show, Ord, Read, Data, Typeable, Generic)
  
instance ToElement UniformAlpha  
instance Default UniformAlpha

data AlphaDistribution 
  = ADAnistrophicAlpha AnistrophicAlpha
  | ADUniformAlpha     UniformAlpha 
   deriving(Eq, Show, Ord, Read, Data, Typeable, Generic)
   
instance ToElement AlphaDistribution where
  toElement = \case 
    ADAnistrophicAlpha x 
      ->  tag "dummy" 
      .>  ("distribution", toElement "as")
      ..> x
    ADUniformAlpha x -> toElement x
    
instance Default AlphaDistribution
   
data RoughDielectric = RoughDielectric 
   { roughDielectricAlpha                 :: AlphaDistribution
   , roughDielectricRefraction            :: RefractionPair
   , roughDielectricSpecularReflectance   :: Color
   , roughDielectricSpecularTransmittance :: Color
   } deriving(Eq, Show, Ord, Read, Data, Typeable, Generic)
   
instance Default RoughDielectric   
instance ToElement RoughDielectric where
   toElement RoughDielectric {..} 
       =  tag "roughDielectric" 
      ..> roughDielectricAlpha
      ..> roughDielectricRefraction
      .>  ("specularReflectance"  , roughDielectricSpecularReflectance)
      .>  ("specularTransmittance", roughDielectricSpecularTransmittance)

data ConductorType
   = AmorphousCarbon
   | Silver
   | Aluminium
   | CubicAluminiumArsenide
   | CubicAluminiumAntimonide
   | Gold
   | PolycrystallineBeryllium
   | Chromium
   | CubicCaesiumIodide
   | Copper
   | CopperIOxide
   | CopperIIOxide
   | CubicDiamond
   | Mercury
   | MercurTelluride
   | Iridium
   | PolycrystallinePotassium
   | Lithium
   | MagnesiumOxide
   | Molybdenum
   | Sodium
   | Niobium
   | Nickel
   | Rhodium
   | HexagonalSiliconCarbide
   | TinTelluride
   | Tantalum
   | PolycrystThoriumFluoride
   | PolycrystallineTitaniumCarbide
   | TitaniumNitride
   | TetragonalTitanDioxide
   | Vanadium
   | VanadiumNitride
   | Tungsten
   | None
   deriving(Eq, Show, Ord, Read, Data, Typeable, Generic, Enum, Bounded)
   
instance Default ConductorType   
   
instance ToAttributeValue ConductorType where
   toAttributeValue = \case
      AmorphousCarbon                -> "a-C"
      Silver                         -> "Ag"
      Aluminium                      -> "Al"
      CubicAluminiumArsenide         -> "AlAs"
      CubicAluminiumAntimonide       -> "AlSb"
      Gold                           -> "Au"
      PolycrystallineBeryllium       -> "Be"
      Chromium                       -> "Cr"
      CubicCaesiumIodide             -> "CsI"
      Copper                         -> "Cu"
      CopperIOxide                   -> "Cu2O"
      CopperIIOxide                  -> "CuO"
      CubicDiamond                   -> "d-C"
      Mercury                        -> "Hg"
      MercurTelluride                -> "HgTe"
      Iridium                        -> "Ir"
      PolycrystallinePotassium       -> "K"
      Lithium                        -> "Li"
      MagnesiumOxide                 -> "MgO"
      Molybdenum                     -> "Mo"
      Sodium                         -> "Na_palik"
      Niobium                        -> "Nb"
      Nickel                         -> "Ni_palik"
      Rhodium                        -> "Rh"
-- This causes a crash
--      Selenium                       -> "Se"
      HexagonalSiliconCarbide        -> "SiC"
      TinTelluride                   -> "SnTe"
      Tantalum                       -> "Ta"
      PolycrystThoriumFluoride       -> "TiC"
      PolycrystallineTitaniumCarbide -> "TiN"
      TitaniumNitride                -> "TiO2"
      TetragonalTitanDioxide         -> "VC"
      Vanadium                       -> "V_palik"
      VanadiumNitride                -> "VN"
      Tungsten                       -> "W"
      None                           -> "none"
   
instance ToElement ConductorType where
   toElement = toElement . toAttributeValue
   
data ManualConductance = ManualConductance
   { manualConductanceK   :: Spectrum
   , manualConductanceEta :: Spectrum
   } deriving(Eq, Show, Ord, Read, Data, Typeable, Generic)

instance Default ManualConductance
instance ToElement ManualConductance
   
data Conductance 
  = CConductorType ConductorType
  | CManualConductance ManualConductance
   deriving(Eq, Show, Ord, Read, Data, Typeable, Generic)
   
instance Default Conductance
instance ToElement Conductance where
  toElement = \case
    CConductorType x -> tag "dummy" .> ("material", x)
    CManualConductance x -> toElement x
  
data Conductor = Conductor 
   { conductorConductance         :: Conductance
   , conductorExtEta              :: Refraction
   , conductorSpecularReflectance :: Color
   } deriving(Eq, Show, Ord, Read, Data, Typeable, Generic)
   

instance ToElement Conductor where
  toElement Conductor {..} 
     = tag "conductor" 
    `appendChildren` conductorConductance
    .> ("extEta", conductorExtEta)
    .> ("specularReflectance", conductorSpecularReflectance)
  
  
instance Default Conductor

--TODO make luminance
-- Luminance
-- and a instance that forwards

data RoughConductor = RoughConductor 
  { roughConductorAlpha               :: AlphaDistribution
  , roughConductorConductance         :: Conductance
  , roughConductorExtEta              :: Refraction
  , roughConductorSpecularReflectance :: Color
  } deriving(Eq, Show, Ord, Read, Data, Typeable, Generic)
   
instance Default RoughConductor
instance ToElement RoughConductor where
   toElement RoughConductor {..}
     =   tag "roughconductor" 
     ..> roughConductorAlpha
     ..> roughConductorConductance
     .>  ("extEta", roughConductorExtEta)
     .>  ("specularReflectance", roughConductorSpecularReflectance)
   

data Plastic = Plastic
   { plasticRefraction          :: RefractionPair
   , plasticSpecularReflectance :: Color
   , plasticDiffuseReflectance  :: Color
   , plasticNonlinear           :: Bool
   } deriving(Eq, Show, Ord, Read, Data, Typeable, Generic)

instance Default Plastic
instance ToElement Plastic where
  toElement Plastic {..}
    =  tag "plastic"
    ..> plasticRefraction
    .>  ("specularReflectance", plasticSpecularReflectance)
    .>  ("diffuseReflectance" , plasticDiffuseReflectance)
    .>  ("nonlinear"          , plasticNonlinear)

-- TODO the atleast the plastic roughness must between 0-0.5 it is 
-- for a specific distribution
-- Beckmann is 0-4
data RoughPlastic = RoughPlastic
   { roughPlasticAlpha               :: UniformAlpha
   , roughPlasticRefraction          :: RefractionPair
   , roughPlasticSpecularReflectance :: Color
   , roughPlasticDiffuseReflectance  :: Color
   , roughPlasticNonlinear           :: Bool
   } deriving(Eq, Show, Ord, Read, Data, Typeable, Generic)
   
instance Default RoughPlastic
instance ToElement RoughPlastic where
  toElement RoughPlastic {..} 
    =   tag "roughplastic"
    ..> roughPlasticAlpha
    ..> roughPlasticRefraction
    .>  ("specularReflectance", roughPlasticSpecularReflectance)
    .>  ("diffuseReflectance" , roughPlasticDiffuseReflectance)
    .>  ("nonlinear"          , roughPlasticNonlinear)

-- Refraction type should be both
-- because they must be different
data Coating = Coating
   { coatingRefraction         :: RefractionPair
   , coatingThickness          :: Double
   , coatingSigmaA             :: Color
   , coatingSpecularReflection :: Color
   , coatingChild              :: Child BSDF
   } deriving(Eq, Show, Ord, Read, Data, Typeable, Generic)
   

instance Default Coating
instance ToElement Coating where
  toElement Coating {..}
    =   tag "coating"
    ..> coatingRefraction
    .>  ("thickness"         , coatingThickness)
    .>  ("sigmaA"            , coatingSigmaA)
    .>  ("specularReflection", coatingSpecularReflection)
    .!> ("child"             , coatingChild)
      
data RoughCoating = RoughCoating
   { roughCoatingAlpha               :: UniformAlpha
   , roughCoatingRefraction       :: RefractionPair
   , roughCoatingThickness           :: Double
   , roughCoatingSigmaA              :: Color
   , roughCoatingSpecularReflectance :: Color
   , roughCoatingChild               :: Child BSDF
   } deriving(Eq, Show, Ord, Read, Data, Typeable, Generic)

instance Default RoughCoating
instance ToElement RoughCoating where
  toElement RoughCoating {..} 
     =   (tag "roughcoating" 
     ..> roughCoatingRefraction
     .>  ("thickness", roughCoatingThickness)
     .>  ("specularReflectance", roughCoatingSpecularReflectance)
     .>  ("sigmaA", roughCoatingSigmaA))
     `appendChildren` roughCoatingChild
     `appendChildren` roughCoatingAlpha

data BumpMap = BumpMap 
   { bumpMapMap  :: Texture
   , bumpMapBSDF :: Child BSDF
   } deriving(Eq, Show, Ord, Read, Data, Typeable, Generic)

instance Default BumpMap
instance ToElement BumpMap where
  toElement = allInvisible . defaultGeneric 

data Phong = Phong
   { phongExponent            :: Luminance
   , phongSpecularReflectance :: Color
   , phongDiffuseReflectance  :: Color 
   } deriving(Eq, Show, Ord, Read, Data, Typeable, Generic)
   
instance Default Phong
instance ToElement Phong
   
data WardType
   = WTWard
   | WTWardDuer
   | WTBalanced
   deriving(Eq, Show, Ord, Read, Data, Typeable, Generic, Bounded, Enum)
   
instance Default WardType
   
instance ToAttributeValue WardType where
   toAttributeValue = \case
      WTWard     -> "ward"
      WTWardDuer -> "ward-duer"
      WTBalanced -> "balanced"

instance ToElement WardType where
   toElement = toElement . toAttributeValue
   
data Ward = Ward 
   { wardVariant             :: WardType
   , wardAlphaU              :: Luminance
   , wardAlphaV              :: Luminance
   , wardSpecularReflectance :: Color
   , wardDiffuseReflectance  :: Color
   } deriving(Eq, Show, Ord, Read, Data, Typeable, Generic)
   
instance Default Ward
instance ToElement Ward
   
data MixtureBSDF = MixtureBSDF 
   { mixtureBSDFChildren :: NonEmpty (PositiveDouble, Child BSDF)
   } deriving(Eq, Show, Ord, Read, Data, Typeable, Generic)

instance Default MixtureBSDF where
  def = MixtureBSDF $ return (def, def)
  

instance ToElement MixtureBSDF where
   toElement MixtureBSDF {..} 
      = let (weights, children) = unzip $ NonEmpty.toList mixtureBSDFChildren
      in addChildList (tag "mixturebsdf")
         $ (primitive "string" (intercalate ", " $ map (show . unPositiveDouble) weights) # 
            ("name", "weights" :: Text)) :
           map toElement children

data BlendBSDF = BlendBSDF
   { blendBSDFWeight  :: Luminance
   , blendBSDFChild0  :: Child BSDF
   , blendBSDFChild1  :: Child BSDF
   } deriving(Eq, Show, Ord, Read, Data, Typeable, Generic)

instance Default BlendBSDF
instance ToElement BlendBSDF where
  toElement 
    = hideChild "child0"
    . hideChild "child1"
    . defaultGeneric
   
data Mask = Mask 
   { maskOpacity :: Color
   , maskChild   :: Child BSDF
   } deriving(Eq, Show, Ord, Read, Data, Typeable, Generic)

instance Default Mask
instance ToElement Mask where
  toElement 
    = hideChild "child" 
    . defaultGeneric

data NonTransmission 
  = NTDiffuse         Diffuse
  | NTRoughDiffuse    RoughDiffuse
  | NTConductor       Conductor
  | NTRoughconductor  RoughConductor
  | NTPlastic         Plastic
  | NTRoughplastic    RoughPlastic
  | NTCoating         Coating
  | NTRoughcoating    RoughCoating
  | NTBumpmap         BumpMap
  | NTPhong           Phong
  | NTWard            Ward
  deriving(Eq, Show, Ord, Read, Data, Typeable, Generic)
  
instance Default NonTransmission
instance ToElement NonTransmission where
  toElement x = (tag "bsdf" # ("type", typeName)) `appendChildren` forwardToElement x where
    typeName = case x of
      NTDiffuse         {} -> "diffuse"
      NTRoughDiffuse    {} -> "roughdiffuse"
      NTConductor       {} -> "conductor"
      NTRoughconductor  {} -> "roughconductor"
      NTPlastic         {} -> "plastic"
      NTRoughplastic    {} -> "roughplastic"
      NTCoating         {} -> "coating"
      NTRoughcoating    {} -> "roughcoating"
      NTBumpmap         {} -> "bumpmap"
      NTPhong           {} -> "phong"
      NTWard            {} -> "ward"
-- 
data Twosided = Twosided 
   { twosidedChild :: Child NonTransmission
   } deriving(Eq, Show, Ord, Read, Data, Typeable, Generic)
   
instance Default Twosided
instance ToElement Twosided where
  toElement = allInvisible . defaultGeneric

data Difftrans = Difftrans
   { diffTransTransmittance :: Color
   } deriving(Eq, Show, Ord, Read, Data, Typeable, Generic)
   
instance Default Difftrans 
instance ToElement Difftrans
   
   
data ScatteringMethod 
   = SMMaterial KnownMaterial
   | SMSA { sigmaS :: Color, sigmaA :: Color}
   | SMTA { sigmaT :: Color, albedo :: Color}
   deriving(Eq, Show, Ord, Read, Data, Typeable, Generic)
   
instance Default ScatteringMethod
instance ToElement ScatteringMethod where
  toElement e = ($ tag "dummy" ) $ case e of
    SMMaterial m -> (.> ("material", m))
    SMSA s a -> (\x -> x .> ("sigmaS", s) .> ("sigmaA", a))
    SMTA s a -> (\x -> x .> ("sigmaT", s) .> ("albedo", a))
   
data HK = HK 
   { hkScatteringMethod :: ScatteringMethod
   , hkThickness        :: Double
   , hkChild            :: Child BSDF
   } deriving(Eq, Show, Ord, Read, Data, Typeable, Generic)
   
instance Default HK
instance ToElement HK where
  toElement HK {..}
    =  (tag "hk"
    .>  ("thickness", hkThickness)
    .!> ("child", hkChild))
    `appendChildren` hkScatteringMethod

   
data Irawan = Irawan
   { irawanFilename             :: FilePath
   , irawanRepeatU              :: Double
   , irawanRepeatV              :: Double
   , irawanAdditionalParameters :: [(String, Either Spectrum Double)]
   } deriving(Eq, Show, Ord, Read, Data, Typeable, Generic)

instance Default Irawan

instance ToElement Irawan where
   toElement Irawan {..} 
      = foldl' (.>) (tag "irawan") 
      $ [ ("filename", toElement irawanFilename)
        , ("repeatU" , toElement irawanRepeatU)
        , ("repeatV" , toElement irawanRepeatV)
        ] ++ map (T.pack *** forwardToElement) irawanAdditionalParameters

data BSDF 
   = BSDFDiffuse         Diffuse 
   | BSDFRoughdiffuse    RoughDiffuse
   | BSDFDielectric      Dielectric
   | BSDFThindielectric  ThinDielectric
   | BSDFRoughdielectric RoughDielectric
   | BSDFConductor       Conductor
   | BSDFRoughconductor  RoughConductor
   | BSDFPlastic         Plastic
   | BSDFRoughplastic    RoughPlastic
   | BSDFCoating         Coating
   | BSDFRoughcoating    RoughCoating
   | BSDFBumpmap         BumpMap
   | BSDFPhong           Phong
   | BSDFWard            Ward
   | BSDFMixturebsdf     MixtureBSDF
   | BSDFBlendbsdf       BlendBSDF
   | BSDFMask            Mask
   | BSDFTwosided        Twosided
   | BSDFDifftrans       Difftrans
   | BSDFHk              HK
   | BSDFIrawan          Irawan
   deriving(Eq, Show, Ord, Read, Data, Typeable, Generic)

instance Default BSDF
instance ToElement BSDF

data PointLight = PointLight
   { pointLightLocation       :: Either Transform Point
   , pointLightIntensity      :: Spectrum
   , pointLightSamplingWeight :: NonZero
   } deriving (Show, Eq, Read, Ord, Generic, Data, Typeable)
   
instance Default PointLight
instance ToElement PointLight where
  toElement PointLight {..}
    =  ($ tag "point"     
    .> ("intensity"     , pointLightIntensity     )
    .> ("samplingWeight", pointLightSamplingWeight))
    $ case pointLightLocation of
      Left  t -> (.> ("toWorld", t))
      Right t -> (.> ("position", t))
    
data AreaLight = AreaLight 
   { areaLightRadiance       :: Spectrum
   , areaLightSamplingWeight :: Double
   } deriving (Show, Eq, Read, Ord, Generic, Data, Typeable)

instance Default AreaLight
instance ToElement AreaLight 

data SpotLight = SpotLight
   { spotLightToWorld        :: Transform
   , spotLightIntensity      :: Spectrum
   , spotLightCutoffAngle    :: Double
   , spotLightBeamWidth      :: Double
   , spotLightTexture        :: Texture
   , spotLightSamplingWeight :: Double
   } deriving (Show, Eq, Read, Ord, Generic, Data, Typeable)

instance Default SpotLight
instance ToElement SpotLight

data DirectionalLight = DirectionalLight
   { directionalLightToWorld        :: Transform
   , directionalLightDirection      :: Vector
   , directionalLightIrradiance     :: Spectrum
   , directionalLightSamplingWeight :: Double
   } deriving (Show, Eq, Read, Ord, Generic, Data, Typeable)

instance Default DirectionalLight
instance ToElement DirectionalLight

data Collimated = Collimated 
   { collimatedToWorld        :: Transform
   , collimatedPower          :: Spectrum
   , collimatedSamplingWeight :: Double
   } deriving (Show, Eq, Read, Ord, Generic, Data, Typeable)

instance Default Collimated
instance ToElement Collimated

data Sky = Sky
   { skyTurbidity      :: Double
   , skyAlbedo         :: Spectrum
   , skyYear           :: Integer
   , skyMonth          :: Integer
   , skyDay            :: Integer
   , skyHour           :: Double
   , skyMinute         :: Double
   , skySecond         :: Double
   , skyLatitude       :: Double
   , skyLongitude      :: Double
   , skyTimezone       :: Double
   , skySunDirection   :: Vector
   , skyStretch        :: Double
   , skyResolution     :: Integer
   , skyScale          :: Double
   , skySamplingWeight :: Double
   , skyToWorld        :: Transform
   } deriving (Show, Eq, Read, Ord, Generic, Data, Typeable)

instance Default Sky
instance ToElement Sky

data Sun = Sun 
   { sunTurbidity    :: Double
   , sunYear         :: Integer
   , sunMonth        :: Integer
   , sunDay          :: Integer
   , sunHour         :: Double
   , sunMinute       :: Double
   , sunSecond       :: Double
   , sunLatitude     :: Double
   , sunLongitude    :: Double
   , sunTimezone     :: Double
   , sunResolution   :: Integer
   , sunScale        :: Double
   , sunRadiusScale  :: Double
   , sunSamplingWeight :: Double
   } deriving (Show, Eq, Read, Ord, Generic, Data, Typeable)
   
instance Default Sun   
instance ToElement Sun   
   
data Sunsky = Sunsky
   { sunskyTurbidity      :: Double
   , sunskyAlbedo         :: Spectrum
   , sunskyYear           :: Integer
   , sunskyMonth          :: Integer
   , sunskyDay            :: Integer
   , sunskyHour           :: Double
   , sunskyMinute         :: Double
   , sunskySecond         :: Double
   , sunskyLatitude       :: Double
   , sunskyLongitude      :: Double
   , sunskyTimezone       :: Double
   , sunskySunDirection   :: Vector
   , sunskyStretch        :: Double
   , sunskyResolution     :: Integer
   , sunskySunScale       :: Double
   , sunskySkyScale       :: Double
   , sunskySunRadiusScale :: Double
   } deriving (Show, Eq, Read, Ord, Generic, Data, Typeable)
   
instance Default Sunsky
instance ToElement Sunsky   
   
data Envmap = Envmap
   { envmapFilename       :: FilePath
   , envmapScale          :: Double
   , envmapToWorld        :: Transform
   , envmapGamma          :: Double
   , envmapCache          :: Bool
   , envmapSamplingWeight :: Double
   } deriving (Show, Eq, Read, Ord, Generic, Data, Typeable)
   
instance Default Envmap
instance ToElement Envmap  
   
data Constant = Constant
   { constantRadiance       :: Spectrum
   , constantSamplingWeight :: Double
   } deriving (Show, Eq, Read, Ord, Generic, Data, Typeable)

instance Default Constant
instance ToElement Constant

data Emitter
   = EPoint            PointLight
   | EArea             AreaLight
   | ESpot             SpotLight
   | EDirectional      DirectionalLight
   | ECollimated       Collimated
   | ESky              Sky
   | ESun              Sun
   | ESunsky           Sunsky
   | EEnvmap           Envmap
   | EConstant         Constant
   deriving (Show, Eq, Read, Ord, Generic, Data, Typeable)

instance Default Emitter
instance ToElement Emitter



-- There are three types of shapes
-- node (shapegroup)
-- leaf 
--    multishader
--    singleshader
data ShapeType 
   = STCube        Cube
   | STSphere      Sphere
   | STCylinder    Cylinder
   | STRectangle   Rectangle
   | STDisk        Disk
   | STPLY         PLY
   | STSerialized  Serialized
   | STInstance    Instance 
   | STHair        Hair
   | STHeightField HeightField
    deriving(Eq, Show, Ord, Read, Data, Typeable, Generic)

instance Default ShapeType
instance ToElement a => ToElement [a] where
   toElement = addChildList (tag "list") 
    
instance ToElement ShapeType where

class ToShapeType a where
  toShapeType :: a -> Text

instance ToShapeType ShapeType where
  toShapeType = \case
     STCube        {} -> "cube"
     STSphere      {} -> "sphere"
     STCylinder    {} -> "cylinder"
     STRectangle   {} -> "rectangle"
     STDisk        {} -> "disk"
     STPLY         {} -> "ply"
     STSerialized  {} -> "serialized"
     STInstance    {} -> "instance"
     STHair        {} -> "hair"
     STHeightField {} -> "heightfield"

  
--TODO instances

data MicroFlake = MicroFlake
   { microFlakeStddev :: Double
   } deriving (Show, Eq, Read, Ord, Generic, Data, Typeable)

instance Default MicroFlake
instance ToElement MicroFlake

-- Combine with other mixture phases
data MixturePhase = MixturePhase
   { mixturePhaseChildren :: [(Double, Child Phase)]
   } deriving (Show, Eq, Read, Ord, Generic, Data, Typeable)

instance Default MixturePhase
instance ToElement MixturePhase where
   toElement MixturePhase {..} = 
      let (weights, phaseFunctions) = unzip mixturePhaseChildren
      in addChildList (tag "mixturephase" 
                      .> ( "weights"
                         , T.intercalate ", " 
                             $ map (T.pack . show) weights
                         ))
                      $ map toElement phaseFunctions
         
data SSSMaterial 
   = Apple
   | Cream
   | Skimmilk
   | Spectralon
   | LowfatMilk
   | ReducedMilk
   | RegularMilk
   | Espresso
   | MintMochaCoffee
   | LowfatSoyMilk
   | RegularSoyMilk
   | LowfatChocolateMilk
   | RegularChocolateMilk
   | Coke
   | Pepsi
   | Sprite
   | Chicken1
   | Ketchup
   | Skin1
   | Wholemilk
   | Gatorade
   | Chardonnay
   | WhiteZinfandel
   | Merlot
   | BudwieserBeer
   | CoorsLightBeer
   | Clorox
   | AppleJuice
   | CranberryJuice
   | GrapeJuice
   | RubyGrapefruitJuice
   | Chicken2
   | Potato
   | Skin2
   | WhiteGrapefruitJuice
   | Shampoo
   | StrawberryShampoo
   | HeadShouldersShampoo
   | LemonTeaPowder
   | OrangeJuicePowder
   | PinkLemonadPowder
   | CappuccinoPowder
   | SaltPowder
   | SugarPowder
   | SuisseMocha
   deriving (Show, Eq, Read, Ord, Generic, Data, Typeable, Enum, Bounded)
   
instance Default SSSMaterial   
instance ToElement SSSMaterial where
   toElement x = toElement $ case x of
      Apple                -> "Apple" :: Text
      Cream                -> "Cream"
      Skimmilk             -> "Skimmilk"
      Spectralon           -> "Spectralon"
      LowfatMilk           -> "Lowfat Milk"
      ReducedMilk          -> "Reduced Milk"
      RegularMilk          -> "Regular Milk"
      Espresso             -> "Expresso"
      MintMochaCoffee      -> "Mint Mocha Coffee"
      LowfatSoyMilk        -> "Lowfat Soy Milk"
      RegularSoyMilk       -> "Regular Soy Milk"
      LowfatChocolateMilk  -> "Lowfat Chocolate Milk"
      RegularChocolateMilk -> "Regular Chocolate Milk"
      Coke                 -> "Coke"
      Pepsi                -> "Pepsi"
      Sprite               -> "Sprite"
      Chicken1             -> "Chicken1"
      Ketchup              -> "Ketchup"
      Skin1                -> "Skin1"
      Wholemilk            -> "Wholemilk"
      Gatorade             -> "Gatorade"
      Chardonnay           -> "Chardonnay"
      WhiteZinfandel       -> "White Zinfandel"
      Merlot               -> "Merlot"
      BudwieserBeer        -> "Budweiser Beer"
      CoorsLightBeer       -> "Coors Light Beer"
      Clorox               -> "Clorox"
      AppleJuice           -> "Apple Juice"
      CranberryJuice       -> "Cranberry Juice"
      GrapeJuice           -> "Grape Juice"
      RubyGrapefruitJuice  -> "Ruby Grapefruit Juice"
      Chicken2             -> "Chicken2"
      Potato               -> "Potato"
      Skin2                -> "Skin2"
      WhiteGrapefruitJuice -> "White Grapefruit Juice"
      Shampoo              -> "Shampoo"
      StrawberryShampoo    -> "Strawberry Shampoo"
      HeadShouldersShampoo -> "Head & Shoulders Shampoo"
      LemonTeaPowder       -> "Lemon Tea Powder"
      OrangeJuicePowder    -> "Orange Juice Powder"
      PinkLemonadPowder    -> "Pink Lemonade Powder"
      CappuccinoPowder     -> "Cappuccino Powder"
      SaltPowder           -> "Salt Powder"
      SugarPowder          -> "Sugar Powder"
      SuisseMocha          -> "Suisse Mocha"

data SigmaAS = SigmaAS 
   { sigmaASSigmaA :: Spectrum
   , sigmaASSigmaS :: Spectrum
   }
   deriving (Show, Eq, Read, Ord, Generic, Data, Typeable)

instance Default SigmaAS   
instance ToElement SigmaAS

data SigmaTAlbedo = SigmaTAlbedo 
   { sigmaTAlbedoT      :: Spectrum
   , sigmaTAlbedoAlbedo :: Spectrum
   } deriving (Show, Eq, Read, Ord, Generic, Data, Typeable)

instance Default SigmaTAlbedo
instance ToElement SigmaTAlbedo

data MaterialStyle 
   = MSMaterial      SSSMaterial
   | MSSigmaAS       SigmaAS
   | MSSigmaTAlbedo  SigmaTAlbedo
   deriving (Show, Eq, Read, Ord, Generic, Data, Typeable)

instance Default MaterialStyle
instance ToElement MaterialStyle where
   toElement = forwardToElement

newtype HG = HG { hgg :: Double }
  deriving (Show, Eq, Read, Ord, Generic, Data, Typeable)

instance Default HG
instance ToElement HG

data Phase 
   = PIsotropic 
   | PHg        HG
   | PRayleigh  
   | PKay      
   | PMicroflake   MicroFlake
   | PMixturephase MixturePhase
   deriving (Show, Eq, Read, Ord, Generic, Data, Typeable)

instance Default Phase
instance ToElement Phase where
  toElement = \case
    PIsotropic      -> tag "phase" # ("type", "isotropic") 
    PHg        x    -> (tag "phase" # ("type", "hg")) `appendChildren` x
    PRayleigh       -> tag "phase" # ("type", "rayleigh")
    PKay           -> tag "phase" # ("type", "kay")
    PMicroflake   x -> (tag "phase" # ("type", "microflake"  )) `appendChildren` x
    PMixturephase x -> (tag "phase" # ("type", "mixturephase")) `appendChildren` x

data Homogeneous = Homogeneous 
   { homogeneousMaterialStyle :: MaterialStyle
   , homogeneousScale         :: Double
   , homogeneousPhase         :: Phase
   } deriving (Show, Eq, Read, Ord, Generic, Data, Typeable)

instance Default Homogeneous
instance ToElement Homogeneous where
  toElement Homogeneous {..} 
    =  (tag "homogeneous" 
    .> ("scale", homogeneousScale)
    .!> ("phase", homogeneousPhase))
    `appendChildren` homogeneousMaterialStyle
   
data HeterogeneousSampling 
   = Simpson
   | Woodcock
   deriving (Show, Eq, Read, Ord, Generic, Data, Typeable, Enum, Bounded)
   
instance Default HeterogeneousSampling
instance ToElement HeterogeneousSampling where
   toElement x = toElement $ case x of
      Simpson  -> "simpson" :: Text
      Woodcock -> "woodcock"

data ConstVolume 
   = CVDouble   Double
   | CVSpectrum Spectrum
   | CVVector   Vector
   deriving (Show, Eq, Read, Ord, Generic, Data, Typeable)

instance Default ConstVolume
instance ToElement ConstVolume where
  toElement e = tag "constvolume" .> case e of
    CVDouble   x -> ("value", toElement x)
    CVSpectrum x -> ("value", toElement x)
    CVVector   x -> ("value", toElement x)
   
data SendDataType = SendAcrossNetwork | AssumeAvailable
   deriving (Show, Eq, Read, Ord, Generic, Data, Typeable, Enum, Bounded)

instance Default SendDataType
instance ToElement SendDataType where
   toElement x = toElement $ case x of
      SendAcrossNetwork -> True
      AssumeAvailable   -> False
      
data GridVolume = GridVolume 
   { gridVolumeFilename :: FilePath
   , gridVolumeSendData :: SendDataType
   , gridVolumeToWorld  :: Transform
   , gridVolumeMin      :: Point
   , gridVolumeMax      :: Point
   } deriving (Show, Eq, Read, Ord, Generic, Data, Typeable)
   
instance Default GridVolume
instance ToElement GridVolume

data VolCache = VolCache
   { volCacheBlockSize   :: Integer
   , volCacheVoxelWidth  :: Double
   , volCacheMemoryLimit :: Integer
   , volCacheToWorld     :: RegularTransform
   , volCacheChild       :: Child Volume
   } deriving (Show, Eq, Read, Ord, Generic, Data, Typeable)

instance Default VolCache
instance ToElement VolCache

data Volume
   = VConstvolume ConstVolume
   | VGridvolume  GridVolume
   | VVolcache    VolCache
   deriving (Show, Eq, Read, Ord, Generic, Data, Typeable)

instance Default Volume
instance ToElement Volume
   
data Heterogeneous = Heterogeneous
   { heterogeneousMethod      :: HeterogeneousSampling
   , heterogeneousDensity     :: Volume
   , heterogeneousAlbedo      :: Volume 
   , heterogeneousOrientation :: Volume
   , heterogeneousScale       :: Double
   , heterogeneousPhase       :: Child Phase
   } deriving (Show, Eq, Read, Ord, Generic, Data, Typeable)

instance Default Heterogeneous
instance ToElement Heterogeneous where
  toElement = hideChild "phase" . defaultGeneric 
   
data Medium 
   = MHomogeneous   Homogeneous 
   | MHeterogeneous Heterogeneous
   deriving (Show, Eq, Read, Ord, Generic, Data, Typeable)

instance Default Medium
instance ToElement Medium 

data MediumPair = MediumPair 
   { mediumPairInterior :: Child Medium
   , mediumPairExterior :: Child Medium
   } deriving(Eq, Show, Ord, Read, Data, Typeable, Generic)

instance Default MediumPair
instance Each MediumPair MediumPair (Child Medium) (Child Medium) where
   each f MediumPair {..}
       =  MediumPair 
      <$> f mediumPairInterior
      <*> f mediumPairExterior
   
instance (ToElement a, ToElement b) => ToElement (Either a b) where
  toElement = either toElement toElement
   
data OBJLeaf = OBJLeaf 
  { objLeafObj        :: OBJ
  , objLeafMaterial   :: Either (Child BSDF) (Map Text (Child BSDF))
  } deriving(Eq, Show, Ord, Read, Data, Typeable, Generic)


instance ToElement OBJLeaf where
  toElement OBJLeaf {..} 
    = appendChildren (tag "dummy") (toElement objLeafObj) 
    & elementChildrenL <>~ 
         (case objLeafMaterial of
            Left  x -> H.singleton "2" $ ChildItem (Nested E.Hidden) $ toElement x
            Right h -> 
                H.fromList
              $ map (\(n, y) -> (n, ChildItem (Nested E.Shown) $ toElement y)) 
              $ M.toList h
         )


instance ToShapeType OBJLeaf where
  toShapeType _ = "obj"

data SimpleShapeLeaf = SimpleShapeLeaf 
  { simpleShapeLeafType       :: ShapeType
  , simpleShapeLeafMaterial   :: Child BSDF
  } deriving(Eq, Show, Ord, Read, Data, Typeable, Generic)


instance Default SimpleShapeLeaf
instance ToElement SimpleShapeLeaf where
  toElement SimpleShapeLeaf {..}
    =  (tag "dummy" 
   .!> ("material", simpleShapeLeafMaterial))
    `appendChildren`
      simpleShapeLeafType
      
instance ToShapeType SimpleShapeLeaf where
  toShapeType = toShapeType . simpleShapeLeafType

data ShapeLeaf = ShapeLeaf 
   { shapeLeafLeaf       :: Either SimpleShapeLeaf OBJLeaf
   , shapeLeafToWorld    :: Transform
   , shapeLeafMediumPair :: Maybe MediumPair
   , shapeLeafEmitter    :: Maybe (Child Emitter)
   } deriving(Eq, Show, Ord, Read, Data, Typeable, Generic)


instance Default ShapeLeaf

instance (ToShapeType a, ToShapeType b) => ToShapeType (Either a b) where
  toShapeType = \case
    Left  x -> toShapeType x
    Right x -> toShapeType x

instance ToShapeType ShapeLeaf where
  toShapeType ShapeLeaf {..} = toShapeType shapeLeafLeaf

instance ToElement ShapeLeaf where
  toElement ShapeLeaf {..} = appendChildren 
       (tag "shape" # ("type", toShapeType shapeLeafLeaf)) shapeLeafLeaf 
       `addChildList`
       (  [toElement shapeLeafToWorld # ("name", "toWorld")]
       <> (fromMaybe [] $ do
             MediumPair x y <- shapeLeafMediumPair
             return $ 
                [ toElement x
                , toElement y
                ]
          )
       <> (maybeToList . fmap toElement) shapeLeafEmitter
       )

data Shape 
  = SShapeGroup [Shape]
  | SShapeLeaf  ShapeLeaf
    deriving(Eq, Show, Ord, Read, Data, Typeable, Generic)

instance ToElement Shape where
   toElement = \case
     SShapeGroup xs -> (tag "shape" # ("type", "shapegroup")) 
                    `addChildList` map toElement xs
     SShapeLeaf x -> toElement x

instance Default Shape

data Dipole = Dipole 
   { dipoleMaterialStyle :: MaterialStyle
   , dipoleScale         :: Double
   , dipoleIntIOR        :: Refraction
   , dipoleExtIOR        :: Refraction
   , dipoleIrrSamples    :: Integer
   } deriving (Show, Eq, Read, Ord, Generic, Data, Typeable)
   
instance Default Dipole
instance ToElement Dipole where
  toElement Dipole {..}
    =  (tag "dipole"
    .> ("scale", dipoleScale)
    .> ("intIOR", dipoleIntIOR)
    .> ("extIOR", dipoleExtIOR)
    .> ("irrSamples", dipoleIrrSamples))
    `appendChildren` dipoleMaterialStyle
   
data Subsurface = SDipole Dipole
   deriving (Show, Eq, Read, Ord, Generic, Data, Typeable)

instance ToElement Subsurface
instance Default Subsurface

data FOVType
   = FOVTX
   | FOVTY
   | FOVTDiagonal
   | FOVTSmaller
   | FOVTLarger
   deriving (Show, Eq, Read, Ord, Generic, Data, Typeable, Enum, Bounded)

instance Default FOVType
instance ToElement FOVType where
   toElement x = primitive "string" $ case x of
      FOVTX        -> "tx" :: Text
      FOVTY        -> "ty"
      FOVTDiagonal -> "diagonal"
      FOVTSmaller  -> "smaller"
      FOVTLarger   -> "larger"

           
data LensView 
  = LVFocalLength Double
  | LVFieldOfView Double
   deriving (Show, Eq, Read, Ord, Generic, Data, Typeable)
  
instance Default LensView
instance ToElement LensView where
  toElement x = ($ tag "dummy") $ case x of
     LVFocalLength d -> (.> ("focalLength", show d ++ "mm"))
     LVFieldOfView d -> (.> ("fov", d))

data Perspective = Perspective
   { perspectiveToWorld      :: Transform
   , perspectiveView         :: LensView
   , perspectiveFovAxis      :: FOVType
   , perspectiveShutterOpen  :: Double
   , perspectiveShutterClose :: Double
   , perspectiveNearClip     :: Double
   , perspectiveFarClip      :: Double
   } deriving (Show, Eq, Read, Ord, Generic, Data, Typeable)

instance Default Perspective
instance ToElement Perspective where
  toElement Perspective {..} 
     =  tag "perspective"
      `appendChildren`   perspectiveView
     .> ("toWorld"     , perspectiveToWorld)
     .> ("fovAxis"     , perspectiveFovAxis)
     .> ("shutterOpen" , perspectiveShutterOpen)
     .> ("shutterClose", perspectiveShutterClose)
     .> ("nearClip"    , perspectiveNearClip)
     .> ("farClip"     , perspectiveFarClip)
    

data Thinlens = Thinlens
   { thinlensToWorld         :: Transform
   , thinlensAperatureRadius :: Double
   , thinlensFocusDistance   :: Double
   , thinlensView            :: LensView
   , thinlensFOVAxis         :: FOVType
   , thinlensShutterOpen     :: Double
   , thinlensShutterClose    :: Double
   , thinlensNearClip        :: Double
   , thinlensFarClip         :: Double
   } deriving (Show, Eq, Read, Ord, Generic, Data, Typeable)

instance Default Thinlens
instance ToElement Thinlens where
  toElement Thinlens {..} 
    =  tag "thinlens" 
    `appendChildren` thinlensView
    .> ("toWorld", thinlensToWorld)
    .> ("aperatureRadius", thinlensAperatureRadius)
    .> ("focusDistance", thinlensFocusDistance)
    .> ("fovaxis", thinlensFOVAxis)
    .> ("shutterOpen", thinlensShutterOpen)
    .> ("shutterClose", thinlensShutterClose)
    .> ("nearClip", thinlensNearClip)
    .> ("farClip", thinlensFarClip)

data Orthographic = Orthographic
   { orthographicToWorld      :: Transform
   , orthographicShutterOpen  :: Double
   , orthographicShutterClose :: Double
   , orthographicNearClip     :: Double
   , orthographicFarClip      :: Double   
   } deriving (Show, Eq, Read, Ord, Generic, Data, Typeable)

instance Default Orthographic
instance ToElement Orthographic

data Telecentric = Telecentric
   { telecentricToWorld         :: Transform
   , telecentricAperatureRadius :: Double
   , telecentricFocusDistance   :: Double
   , telecentricShutterOpen     :: Double
   , telecentricShutterClose    :: Double
   , telecentricNearClip        :: Double
   , telecentricFarClip         :: Double
   } deriving (Show, Eq, Read, Ord, Generic, Data, Typeable)

instance Default Telecentric
instance ToElement Telecentric

data Spherical = Spherical 
   { sphericalToWorld      :: Transform
   , sphericalShutterOpen  :: Double
   , sphericalShutterClose :: Double
   } deriving (Show, Eq, Read, Ord, Generic, Data, Typeable)

instance Default Spherical
instance ToElement Spherical   
   
data IrradianceMeter = IrradianceMeter
   { irradianceMeterShutterOpen  :: Double
   , irradianceMeterShutterClose :: Double
   } deriving (Show, Eq, Read, Ord, Generic, Data, Typeable)

instance Default IrradianceMeter
instance ToElement IrradianceMeter   
   
data RadianceMeter = RadianceMeter
   { radianceMeterToWorld      :: Transform
   , radianceMeterShutterOpen  :: Double
   , radianceMeterShutterClose :: Double
   } deriving (Show, Eq, Read, Ord, Generic, Data, Typeable)

instance Default RadianceMeter
instance ToElement RadianceMeter

data FluenceMeter = FluenceMeter
   { fluenceMeterToWorld      :: Transform
   , fluenceMeterShutterOpen  :: Double
   , fluenceMeterShutterClose :: Double
   } deriving (Show, Eq, Read, Ord, Generic, Data, Typeable)

instance Default FluenceMeter
instance ToElement FluenceMeter

data PolyTwoAndFour = PolyTwoAndFour
   { polyTwoAndFourTwo  :: Double
   , polyTwoAndFourFour :: Double
   } deriving (Show, Eq, Read, Ord, Generic, Data, Typeable)
   
instance Default PolyTwoAndFour
instance Each PolyTwoAndFour PolyTwoAndFour Double Double where
   each f PolyTwoAndFour {..} 
       =  PolyTwoAndFour 
      <$> f polyTwoAndFourTwo
      <*> f polyTwoAndFourFour

instance ToElement PolyTwoAndFour where
   toElement 
      = primitive "string" 
      . T.pack 
      . intercalate "," 
      . map show 
      . toListOf each  

data PerspectiveRDist = PerspectiveRDist
   { perspectiveRDistToWorld      :: Transform
   , perspectiveRDistkc           :: PolyTwoAndFour
   , perspectiveRDistFocalLength  :: Double
   , perspectiveRDistFov          :: Double
   , perspectiveRDistFovAxis      :: FOVType
   , perspectiveRDistShutterOpen  :: Double
   , perspectiveRDistShutterClose :: Double
   , perspectiveRDistNearClip     :: Double
   , perspectiveRDistFarClip      :: Double
   } deriving (Show, Eq, Read, Ord, Generic, Data, Typeable)

instance Default PerspectiveRDist
instance ToElement PerspectiveRDist

data SensorType
   = STPerspective       Perspective
   | STThinlens          Thinlens
   | STOrthographic      Orthographic
   | STTelecentric       Telecentric
   | STSpherical         Spherical
   | STIrradiancemeter   IrradianceMeter
   | STRadiancemeter     RadianceMeter
   | STFluencemeter      FluenceMeter
   | STPerspective_rdist PerspectiveRDist
   deriving (Show, Eq, Read, Ord, Generic, Data, Typeable)

instance Default SensorType
instance ToElement SensorType where
  toElement = \case 
    STPerspective      x -> (tag "sensor" # ("type", "perspective"))
                         `appendChildren` x
    STThinlens         x -> (tag "sensor" # ("type", "thinlens"))
                         `appendChildren` x
    STOrthographic     x -> (tag "sensor" # ("type", "orthographic"))
                         `appendChildren` x
    STTelecentric      x -> (tag "sensor" # ("type", "telecentric"))
                         `appendChildren` x
    STSpherical        x -> (tag "sensor" # ("type", "spherical"))
                         `appendChildren` x
    STIrradiancemeter  x -> (tag "sensor" # ("type", "irradiancemeter"))
                         `appendChildren` x
    STRadiancemeter    x -> (tag "sensor" # ("type", "radiancemeter"))
                         `appendChildren` x
    STFluencemeter     x -> (tag "sensor" # ("type", "fluencemeter"))
                         `appendChildren` x
    STPerspective_rdist x -> (tag "sensor" # ("type", "perspective_rdist"))
                         `appendChildren` x

data Sensor = Sensor
  { sensorFilm    :: Maybe Film
  , sensorSampler :: Maybe Sampler
  , sensorType    :: SensorType
  } deriving (Show, Eq, Read, Ord, Generic, Data, Typeable)
   
instance Default Sensor
instance ToElement Sensor where
  toElement Sensor {..} 
    = toElement sensorType   
    `addChildList` ((maybeToList $ fmap toElement sensorFilm) 
                  ++ (maybeToList $ fmap toElement sensorSampler))
   
data AmbientOcclusion = AmbientOcclusion 
   { ambientOcclusionShadingSamples :: Integer
   , ambientOcclusionRayLength      :: Double
   } deriving (Show, Eq, Read, Ord, Generic, Data, Typeable)

instance Default AmbientOcclusion
instance ToElement AmbientOcclusion

data Visibility = Hidden | Visible
   deriving (Show, Eq, Read, Ord, Generic, Data, Typeable, Enum, Bounded)

instance Default Visibility
instance ToElement Visibility where
   toElement x = toElement $ case x of
      Hidden  -> False
      Visible -> True

data NormalStrictness = Strict | Loose
   deriving (Show, Eq, Read, Ord, Generic, Data, Typeable, Enum, Bounded)

instance Default NormalStrictness
instance ToElement NormalStrictness where
   toElement x = toElement $ case x of
         Strict -> True
         Loose  -> False

data Direct = Direct
   { directShadingSamples :: Integer
   , directEmitterSamples :: Integer
   , directBsdfSamples    :: Integer
   , directStrictNormals  :: NormalStrictness
   , directHideEmitters   :: Visibility
   } deriving (Show, Eq, Read, Ord, Generic, Data, Typeable)

instance Default Direct
instance ToElement Direct
   
data Path = Path
   { pathMaxDepth      :: Integer
   , pathRrDepth       :: Integer
   , pathStrictNormals :: NormalStrictness
   , pathHideEmitters  :: Visibility
   } deriving (Show, Eq, Read, Ord, Generic, Data, Typeable)

instance Default Path
instance ToElement Path

data VolPathSimple = VolPathSimple
   { volPathSimpleMaxDepth      :: Integer
   , volPathSimpleRrDepth       :: Integer
   , volPathSimpleStrictNormals :: NormalStrictness
   , volPathSimpleHideEmitters  :: Visibility
   } deriving (Show, Eq, Read, Ord, Generic, Data, Typeable)

instance Default VolPathSimple
instance ToElement VolPathSimple

data VolPath = VolPath
   { volPathMaxDepth      :: Integer
   , volPathRrDepth       :: Integer
   , volPathStrictNormals :: NormalStrictness
   , volPathHideEmitters  :: Visibility
   } deriving (Show, Eq, Read, Ord, Generic, Data, Typeable)

instance Default VolPath
instance ToElement VolPath

data PathConnectedness = ConnectToCamera | DontConnect
   deriving (Show, Eq, Read, Ord, Generic, Data, Typeable, Enum, Bounded)

instance Default PathConnectedness
instance ToElement PathConnectedness where
   toElement x = toElement $ case x of
         ConnectToCamera -> True
         DontConnect     -> False

data DirectSampling = DirectSampling | NoDirectSampling   
   deriving (Show, Eq, Read, Ord, Generic, Data, Typeable, Enum, Bounded)

instance Default DirectSampling
instance ToElement DirectSampling where
   toElement x = toElement $ case x of
         DirectSampling   -> True
         NoDirectSampling -> False
   
data BDPT = BDPT
   { bdptMaxDepth     :: Integer
   , bdptLightImage   :: PathConnectedness
   , bdptSampleDirect :: DirectSampling
   , bdptRrDepth      :: Integer
   } deriving (Show, Eq, Read, Ord, Generic, Data, Typeable)

instance Default BDPT
instance ToElement BDPT

data PhotonMapper = PhotonMapper
   { photonMapperDirectSamples       :: Integer
   , photonMapperGlossySamples       :: Integer
   , photonMapperMaxDepth            :: Integer
   , photonMapperGlobalPhotons       :: Integer
   , photonMapperCausticPhotons      :: Integer
   , photonMapperVolumePhotons       :: Integer
   , photonMapperGlobalLookupRadius  :: Double
   , photonMapperCausticLookupRadius :: Double
   , photonMapperLookupSize          :: Integer
   , photonMapperGranularity         :: Integer
   , photonMapperHideEmitters        :: Bool
   , photonMapperRrDepth             :: Integer
   } deriving (Show, Eq, Read, Ord, Generic, Data, Typeable)

instance Default PhotonMapper
instance ToElement PhotonMapper

data PPM = PPM 
   { ppmMaxDepth      :: Integer
   , ppmPhotonCount   :: Integer
   , ppmInitialRadius :: Double
   , ppmAlpha         :: Double
   , ppmGranularity   :: Integer
   , ppmRrDepth       :: Integer
   , ppmMaxPasses     :: Integer
   } deriving (Show, Eq, Read, Ord, Generic, Data, Typeable)

instance Default PPM
instance ToElement PPM

data SPPM = SPPM
   { sppmMaxDepth      :: Integer
   , sppmPhotonCount   :: Integer
   , sppmInitialRadius :: Double
   , sppmAlpha         :: Double
   , sppmGranularity   :: Integer
   , sppmRrDepth       :: Integer
   , sppmMaxPasses     :: Integer
   } deriving (Show, Eq, Read, Ord, Generic, Data, Typeable)

instance Default SPPM
instance ToElement SPPM

data PSSMLT = PSSMLT
   { pssmltBidirectional    :: Bool
   , pssmltMaxDepth         :: Integer
   , pssmltDirectSamples    :: Integer
   , pssmltRrDepth          :: Integer
   , pssmltLuminanceSamples :: Integer
   , pssmltTwoStage         :: Bool
   , pssmltPlarge           :: Double
   } deriving (Show, Eq, Read, Ord, Generic, Data, Typeable)

instance Default PSSMLT
instance ToElement PSSMLT

data MLT = MLT
   { mltMaxDepth               :: Integer
   , mltDirectSamples          :: Integer
   , mltLuminanceSamples       :: Integer
   , mltTwoStage               :: Bool
   , mltBidirectionalMutation  :: Bool
   , mltLensPerturbation       :: Bool
   , mltMultiChainPertubation  :: Bool
   , mltCausticPertubation     :: Bool
   , mltManifoldPertubation    :: Bool
   , mltLambda                 :: Double
   } deriving (Show, Eq, Read, Ord, Generic, Data, Typeable)

instance Default MLT
instance ToElement MLT

data ERPT = ERPT
   { erptMaxDepth               :: Integer
   , erptNumChains              :: Double
   , erptMaxChains              :: Double
   , erptChainLength            :: Integer
   , erptDirectSamples          :: Integer
   , erptLensPerturbation       :: Bool
   , erptMultiChainPerturbation :: Bool
   , erptCausticPerturbation    :: Bool
   , erptManifoldPerturbation   :: Bool
   , erptLambda                 :: Double
   } deriving (Show, Eq, Read, Ord, Generic, Data, Typeable)

instance Default ERPT
instance ToElement ERPT

data PTracer = PTracer
   { ptracerMaxDepth    :: Integer
   , ptracerRrDepth     :: Integer
   , ptracerGranularity :: Integer
   , ptracerBruteForce  :: Bool
   } deriving (Show, Eq, Read, Ord, Generic, Data, Typeable)

instance Default PTracer
instance ToElement PTracer

data Adaptive = Adaptive
   { adaptiveMaxError        :: Double
   , adaptivePValue          :: Double
   , adaptiveMaxSampleFactor :: Integer
   } deriving (Show, Eq, Read, Ord, Generic, Data, Typeable)

instance Default Adaptive
instance ToElement Adaptive

data VPL = VPL 
   { vplMaxDepth            :: Integer
   , vplShadowMapResolution :: Integer
   , vplClamping            :: Double
   } deriving (Show, Eq, Read, Ord, Generic, Data, Typeable)

instance Default VPL
instance ToElement VPL

data IRRCache = IRRCache 
   { irrCacheResolution        :: Integer
   , irrCacheQuality           :: Double
   , irrCacheGradients         :: Bool
   , irrCacheClampNeighbor      :: Bool
   , irrCacheClampScreen       :: Bool
   , irrCacheOverture          :: Bool
   , irrCacheQualityAdjustment :: Double
   , irrCacheIndirectOnly      :: Bool
   , irrCacheDebug             :: Bool
   } deriving (Show, Eq, Read, Ord, Generic, Data, Typeable)

instance Default IRRCache
instance ToElement IRRCache

data FieldType 
  = FTPosition
  | FTRelPosition
  | FTDistance
  | FTGeoNormal
  | FTShNormal
  | FTUV
  | FTAlbedo
  | FTShapeIndex
  | FTPrimIndex
   deriving (Show, Eq, Read, Ord, Generic, Data, Typeable, Enum, Bounded)
  
instance Default FieldType
instance ToElement FieldType where
  toElement x = toElement $ case x of
    FTPosition    -> "position"
    FTRelPosition -> "relPosition"
    FTDistance    -> "distance"
    FTGeoNormal   -> "geoNormal"
    FTShNormal    -> "shNormal"
    FTUV          -> "uv"
    FTAlbedo      -> "albedo"
    FTShapeIndex  -> "shapeIndex"
    FTPrimIndex   -> "primIndex"

data Field = Field
  { fieldField     :: FieldType
  , fieldUndefined :: Luminance
  } deriving (Show, Eq, Read, Ord, Generic, Data, Typeable)

instance Default Field
instance ToElement Field

data SamplingIntegrator 
  = SIField Field
  | SIAo     AmbientOcclusion
  | SIDirect Direct
  | SIPath Path
  | SIVolpath VolPath
  | SIVolpath_simple VolPathSimple
  | SIIrrcache IRRCache 
   deriving (Show, Eq, Read, Ord, Generic, Data, Typeable)
  
instance Default SamplingIntegrator
instance ToElement SamplingIntegrator where
  toElement = set elementTagL "integrator" . defaultGeneric

newtype MultiChannel 
  = MultiChannel { unMultiChannel :: [SamplingIntegrator] }
   deriving (Show, Eq, Read, Ord, Generic, Data, Typeable)

instance Default MultiChannel
instance ToElement MultiChannel where
  toElement (MultiChannel xs) = addChildList (tag "multichannel") xs

data Integrator 
   = IAo             AmbientOcclusion 
   | IDirect         Direct
   | IPath           Path
   | IVolpath_simple VolPathSimple
   | IVolpath        VolPath
   | IBdpt           BDPT
   | IPhotonmapper   PhotonMapper
   | IPpm            PPM
   | ISppm           SPPM
   | IPssmlt         PSSMLT
   | IMlt            MLT
   | IErpt           ERPT
   | IPtracer        PTracer
   | IAdaptive       Adaptive
   | IVpl            VPL
   | IIrrcache       IRRCache
   | IMultichannel   MultiChannel
   deriving (Show, Eq, Read, Ord, Generic, Data, Typeable)

instance Default Integrator
instance ToElement Integrator

data Independent = Independent
   { independentSampleCount :: Whole
   } deriving (Show, Eq, Read, Ord, Generic, Data, Typeable)

instance Default Independent
instance ToElement Independent

data Stratified = Stratified
   { stratifiedSampleCount :: Whole
   , stratifiedDimension   :: Whole
   } deriving (Show, Eq, Read, Ord, Generic, Data, Typeable)

instance Default Stratified
instance ToElement Stratified

data LDSampler = LDSampler
   { ldSamplerSampleCount :: Whole
   , ldSamplerDimension   :: Whole
   } deriving (Show, Eq, Read, Ord, Generic, Data, Typeable)

instance Default LDSampler
instance ToElement LDSampler

data Halton = Halton
   { haltonSampleCount :: Whole
   , haltonScramble    :: Whole
   } deriving (Show, Eq, Read, Ord, Generic, Data, Typeable)

instance Default Halton
instance ToElement Halton

data Hammersley = Hammersley
   { hammersleySampleCount :: Whole
   , hammersleyScramble    :: Whole
   } deriving (Show, Eq, Read, Ord, Generic, Data, Typeable)

instance Default Hammersley
instance ToElement Hammersley

data Sobol = Sobol 
   { sobolSampleCount :: Whole
   , sobolScramble    :: Whole
   } deriving (Show, Eq, Read, Ord, Generic, Data, Typeable)

instance Default Sobol
instance ToElement Sobol

data Sampler
   = SIndependent Independent
   | SStratified  Stratified
   | SLdsampler   LDSampler 
   | SHalton      Halton
   | SHammersley  Hammersley
   | SSobol       Sobol
   deriving (Show, Eq, Read, Ord, Generic, Data, Typeable)

instance Default Sampler
instance ToElement Sampler

data FileFormatType
   = Openexr
   | RGBE
   | PFM
   deriving (Show, Eq, Read, Ord, Generic, Data, Typeable, Enum, Bounded)

instance Default FileFormatType
instance ToElement FileFormatType where
   toElement x = primitive "string" $ case x of
      Openexr -> "openexr" :: Text
      RGBE    -> "rgbe"
      PFM     -> "pfm"

data PixelFormat
   = PFLuminance
   | PFLuminanceAlpha
   | PFRGB
   | PFRGBA
   | PFXYZ
   | PFXYZA
   | PFSpectrum
   | PFSpectrumAlpha
   deriving (Show, Eq, Read, Ord, Generic, Data, Typeable, Bounded, Enum)

instance Default PixelFormat
instance ToElement PixelFormat where
   toElement x = primitive "string" $ case x of
      PFLuminance      -> "luminance" :: Text
      PFLuminanceAlpha -> "luminancealpha"
      PFRGB            -> "rgb"
      PFRGBA           -> "rgba"
      PFXYZ            -> "xyz"
      PFXYZA           -> "xyza"
      PFSpectrum       -> "spectrum"
      PFSpectrumAlpha  -> "spectrumalpha"

data ComponentFormat 
   = Float16
   | Float32
   | UInt32
   deriving (Show, Eq, Read, Ord, Generic, Data, Typeable, Bounded, Enum)

instance Default ComponentFormat
instance ToElement ComponentFormat where
   toElement x = primitive "string" $ case x of
      Float16 -> "float16" :: Text
      Float32 -> "float32"
      UInt32  -> "uint32"

-- Add a type for the types of things you can refer to.
      
data Label = Label 
  { labelWidth  :: Integer
  , labelHeight :: Integer
  , labelValue  :: Text
  } deriving (Show, Eq, Read, Ord, Generic, Data, Typeable)
  
instance Default Label
instance ToElement Label where
  toElement Label {..} 
    = tag "label"
    .!> ("a", toElement labelValue 
    # ("name", format' "label[{}, {}]" (labelWidth, labelHeight))) 

data HDRfilm m = HDRfilm
   { hdrFilmWidth            :: Integer
   , hdrFilmHeight           :: Integer
   , hdrFilmFileFormat       :: FileFormatType
   , hdrFilmPixelFormat      :: PixelFormat
   , hdrFilmComponentFormat  :: ComponentFormat
   , hdrFilmCropOffsetX      :: Integer
   , hdrFilmCropOffsetY      :: Integer
   , hdrFilmCropWidth        :: Integer
   , hdrFilmCropHeight       :: Integer
   , hdrFilmAttachLog        :: Bool
   , hdrFilmBanner           :: Bool
   , hdrFilmHighQualityEdges :: Bool
   , hdrFilmRFilter          :: RFilter
   , hdrFilmMetaData         :: m 
   , hdrFilmLabels           :: Label
   } deriving (Show, Eq, Read, Ord, Generic, Data, Typeable)

makeMetaDataNames :: Element -> Element
makeMetaDataNames e 
  = over 
      elementChildrenL 
      ( H.fromList 
      . map (first (\x -> "metadata['" <> x <> "']")) 
      . H.toList
      )
      e

instance Default m => Default (HDRfilm m)  
instance ToElement m => ToElement (HDRfilm m) where
  toElement HDRfilm {..} = (tag "hdrfilm" 
    .> ("width"           , hdrFilmWidth)
    .> ("height"          , hdrFilmHeight)
    .> ("fileFormat"      , hdrFilmFileFormat)
    .> ("pixelFormat"     , hdrFilmPixelFormat)
    .> ("componentFormat" , hdrFilmComponentFormat)
    .> ("cropOffsetX"     , hdrFilmCropOffsetX)
    .> ("cropOffsetY"     , hdrFilmCropOffsetY)
    .> ("cropWidth"       , hdrFilmCropWidth)
    .> ("cropHeight"      , hdrFilmCropHeight)
    .> ("attachLog"       , hdrFilmAttachLog)
    .> ("banner"          , hdrFilmBanner)
    .> ("highQualityEdges", hdrFilmHighQualityEdges)
    .!> ("rfilter"         , hdrFilmRFilter))
    `appendChildren` (makeMetaDataNames $ toElement hdrFilmMetaData)
    `appendChildren` hdrFilmLabels
  
   
data Crop = Crop
   { cropCropOffsetX :: Integer
   , cropCropOffsetY :: Integer
   , cropCropWidth   :: Integer
   , cropCropHeight  :: Integer
   } deriving (Show, Eq, Read, Ord, Generic, Data, Typeable)

instance Default Crop
instance ToElement Crop

data TiledHDRFilm = TiledHDRFilm
   { tiledHDRFilmWidth           :: Integer
   , tiledHDRFilmHeight          :: Integer
   , tiledHDRFilmCrop            :: Maybe Crop
   , tiledHDRFilmPixelFormat     :: PixelFormat
   , tiledHDRFilmComponentFormat :: ComponentFormat
   , tiledHDRFilmRFilter         :: RFilter
   } deriving (Show, Eq, Read, Ord, Generic, Data, Typeable)

instance Default TiledHDRFilm
instance ToElement TiledHDRFilm where
   toElement TiledHDRFilm {..}
      = maybe (tag "tiledhdrfilm") 
          (\x -> tag "tiledhdrfilm" `appendChildren` x) 
          tiledHDRFilmCrop
      .> ("width"          , tiledHDRFilmWidth          )
      .> ("height"         , tiledHDRFilmHeight         )
      .> ("pixelFormat"    , tiledHDRFilmPixelFormat    )
      .> ("componentFormat", tiledHDRFilmComponentFormat)
      .!> ("rFilter"        , tiledHDRFilmRFilter        )
          

data GammaType
   = GTGammaCurve Double
   | GTSRGB
   deriving (Show, Eq, Read, Ord, Generic, Data, Typeable)

instance Default GammaType   
instance ToElement GammaType where
   toElement x = toElement $ case x of
      GTGammaCurve y -> y
      GTSRGB         -> -1

data LDRfilm
   = LDRFGammaFilm GammaFilm 
   | LDRFReinhard  ReinhardFilm
   deriving (Show, Eq, Read, Ord, Generic, Data, Typeable)
   
instance Default LDRfilm   
instance ToElement LDRfilm where
   toElement = forwardToElement

data LDRFileFormatType 
  = JPEG
  | PNG
  deriving (Show, Eq, Read, Ord, Generic, Data, Typeable, Enum, Bounded)

instance Default   LDRFileFormatType
instance ToElement LDRFileFormatType where
  toElement x = toElement $ case x of
     JPEG -> "jpeg"
     PNG  -> "png"

data GammaFilm = GammaFilm
   { ldrfilmWidth            :: Integer
   , ldrfilmHeight           :: Integer
   , ldrfilmFileFormat       :: LDRFileFormatType
   , ldrfilmPixelFormat      :: PixelFormat
   , ldrfilmGamma            :: GammaType
   , ldffilmExposure         :: Double
   , ldffilmBanner           :: Bool
   , ldffilmCrop             :: Maybe Crop
   , ldffilmHighQualityEdges :: Bool
   , ldffilmRFilter          :: RFilter
   } deriving (Show, Eq, Read, Ord, Generic, Data, Typeable)
   
instance Default GammaFilm
instance ToElement GammaFilm where
   toElement GammaFilm {..} 
      = maybe (tag "gammafilm") 
          (\x -> tag "gammafilm" `appendChildren` x) ldffilmCrop
     .>  ("width"                       , ldrfilmWidth            )
     .>  ("height"                      , ldrfilmHeight           )   
     .>  ("fileFormat"                  , ldrfilmFileFormat       )
     .>  ("pixelFormat"                 , ldrfilmPixelFormat      )
     .>  ("gamma"                       , ldrfilmGamma            )
     .>  ("exposure"                    , ldffilmExposure         )   
     .>  ("banner"                      , ldffilmBanner           )
     .>  ("highQualityEdges"            , ldffilmHighQualityEdges )
     .!> ("rFilter"                     , ldffilmRFilter          )
     .>  ("tonemapMethod", "gamma")
                                       
data ReinhardFilm = ReinhardFilm
   { reinhardFilmWidth            :: Integer
   , reinhardFilmHeight           :: Integer
   , reinhardFilmFileFormat       :: LDRFileFormatType
   , reinhardFilmPixelFormat      :: PixelFormat
   , reinhardFilmGamma            :: GammaType
   , reinhardFilmExposure         :: Double
   , reinhardFilmKey              :: Double
   , reinhardFilmBurn             :: Double
   , reinhardFilmBanner           :: Bool
   , reinhardFilmCrop             :: Maybe Crop
   , reinhardFilmHighQualityEdges :: Bool
   , reinhardFilmRFilter          :: RFilter
   } deriving (Show, Eq, Read, Ord, Generic, Data, Typeable)

instance Default ReinhardFilm
instance ToElement ReinhardFilm where
   toElement ReinhardFilm {..} 
     = maybe (tag "reinhardfilm") 
               (\x -> tag "reinhardfilm" `appendChildren` x) reinhardFilmCrop
     .> ("width"            , reinhardFilmWidth           )
     .> ("height"           , reinhardFilmHeight          )
     .> ("fileFormat"       , reinhardFilmFileFormat      )
     .> ("pixelFormat"      , reinhardFilmPixelFormat     )
     .> ("gamma"            , reinhardFilmGamma           )
     .> ("exposure"         , reinhardFilmExposure        )
     .> ("key"              , reinhardFilmKey             )
     .> ("burn"             , reinhardFilmBurn            )
     .> ("banner"           , reinhardFilmBanner          )
     .> ("highQualityEdges" , reinhardFilmHighQualityEdges)
     .!> ("rfilter"          , reinhardFilmRFilter        )
     .> ("tonemapMethod", "reinhard")

data MFilmFileFormat 
  = MFFFMatlab
  | MFFFMathematica
  | MFFFNumpy
  deriving (Show, Eq, Read, Ord, Generic, Data, Typeable, Enum, Bounded)
  
instance Default MFilmFileFormat
instance ToElement MFilmFileFormat where
  toElement x = toElement $ case x of
    MFFFMatlab      -> "matlab"
    MFFFMathematica -> "mathematica"
    MFFFNumpy       -> "numpy"

data MFilm = MFilm 
   { mfilmWidth            :: Integer
   , mfilmHeight           :: Integer
   , mfilmCrop             :: Maybe Crop
   , mfilmFileFormat       :: MFilmFileFormat
   , mfilmDigits           :: Integer
   , mfilmVariable         :: String
   , mfilmPixelFormat      :: PixelFormat
   , mfilmHighQualityEdges :: Bool
   , mfilmRFilter          :: RFilter
   } deriving (Show, Eq, Read, Ord, Generic, Data, Typeable)

instance Default MFilm
instance ToElement MFilm where
   toElement MFilm {..} 
       = maybe (tag "mfilm") (\x -> tag "mfilm" `appendChildren` x) mfilmCrop
      .> ("width"           , mfilmWidth )
      .> ("height"          , mfilmHeight)
      .> ("digits"          , mfilmDigits)
      .> ("variable"        , mfilmVariable)
      .> ("fileFormat"      , mfilmFileFormat)
      .> ("pixelFormat"     , mfilmPixelFormat)
      .> ("highQualityEdges", mfilmHighQualityEdges)
      .!> ("rfilter"         , mfilmRFilter)

instance ToElement () where
  toElement () = tag "unit"

-- TODO change () to Element when Element gets fixed to use an 
-- enum attribute
data Film 
   = FHdrfilm      (HDRfilm ())
   | FTiledhdrfilm TiledHDRFilm
   | FLdrfilm      LDRfilm
   | FMfilm        MFilm
   deriving (Show, Eq, Read, Ord, Generic, Data, Typeable)
   
instance Default Film
instance ToElement Film    
   
data RFilter
   = RFBox
   | RFTent
   | RFGaussian
   | RFMitchell   { mitchellB :: Double, mitchellC :: Double }
   | RFCatmullrom { catmullromB :: Double, catmullromC :: Double}
   | RFLanczos    { lanczosLobes :: Integer }
   deriving (Show, Eq, Read, Ord, Generic, Data, Typeable)

instance Default RFilter
instance ToElement RFilter where
   toElement x = case x of
      RFBox        -> tag "rfilter" # ("type", "box")
      RFTent       -> tag "rfilter" # ("type", "tent") 
      RFGaussian   -> tag "rfilter" # ("type", "gaussian") 
      RFMitchell   b c   -> (tag "rfilter" 
                          # ("type", "mitchell")) 
                         .> ("B", b)
                         .> ("C", c)
      RFCatmullrom b c   -> (tag "rfilter" 
                          # ("type", "catmullrom"))
                         .> ("B", b)
                         .> ("C", c)
      RFLanczos    lobes -> (tag "rfilter" 
                          # ("type", "lanczos"))
                         .> ("lobes", lobes)
      
      
data Include = Include 
   { includeFilename :: FilePath
   } deriving (Show, Eq, Read, Ord, Generic, Data, Typeable)

instance Default Include
instance ToElement Include where
  toElement = allAttribute . defaultGeneric

data AnyAlias = forall a. AnyAlias (Alias a)

instance Default AnyAlias where
  def = AnyAlias (Alias def def :: Alias ())

retagRef :: Ref a -> Ref b
retagRef = Ref . unRef

retagAlias :: Alias a -> Alias b
retagAlias (Alias x y) = Alias (retagRef x) y

deriving instance Show AnyAlias

instance Eq AnyAlias where
   AnyAlias x == AnyAlias y = retagAlias x == y

deriving instance Read AnyAlias

instance Ord AnyAlias where
   AnyAlias x `compare` AnyAlias y = retagAlias x `compare` y
   
   
deriving instance Typeable AnyAlias

instance ToElement AnyAlias where
   toElement (AnyAlias x) = toElement x
      
data SceneNodeData 
   = SNDShape      Shape
   | SNDBSDF       BSDF
   | SNDTexture    Texture
   | SNDSSS        Subsurface
   | SNDMedium     Medium  
   | SNDPhase      Phase
   | SNDVolume     Volume 
   | SNDEmitter    Emitter
   | SNDSensor     Sensor
   | SNDIntegrator Integrator
   | SNDSampler    Sampler
   | SNDInclude    Include
   | SNDAlias      AnyAlias
   deriving (Show, Eq, Read, Ord, Generic, Typeable)

instance Default SceneNodeData
instance ToElement SceneNodeData where
   toElement = forwardToElement

data SceneNode = SceneNode 
   { nodeData :: SceneNodeData
   , nodeId   :: Maybe String
   } deriving(Eq, Show, Ord, Read, Typeable, Generic)

instance Default SceneNode
instance ToElement SceneNode where
   toElement SceneNode {..} = case nodeId of
      Just i  -> toElement nodeData # ("id", i)
      Nothing -> toElement nodeData

data Scene = Scene 
   { nodes   :: [SceneNode]
   } deriving(Eq, Show, Ord, Read, Typeable, Generic)

instance Default Scene
instance ToElement Scene where
   toElement (Scene nodes) 
      = addChildList 
         (tag "scene" # ("version", "0.5.0"))
         nodes
            

concatMapM makePrisms  . mapMaybe dataDataName =<< declarations
concatMapM makeLensesL . mapMaybe dataName =<< declarations
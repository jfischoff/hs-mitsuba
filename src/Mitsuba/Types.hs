{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE StandaloneDeriving #-}
module Mitsuba.Types where
--import Mitsuba.Class

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
   { blackBodyTemperature :: Temperature
   , blackBodyScale       :: Double
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

format' x = TL.toStrict . format x

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

instance ToElement Spectrum where
   toElement = \case
      SWavelengths x -> primitive "spectrum" x
      SUniform     x -> primitive "spectrum" x
      SInternal    x -> primitive "spectrum" x
      SRGB         x -> primitive "rgb"      x
      SSRGB        x -> primitive "rgb"      x
      SFile        x -> tag "spectrum"  # ("file", x)
      SBlackbody   x -> tag "blackbody" 
                           # ("temperature", blackBodyTemperature x)
                           # ("scale"      , blackBodyScale       x)
   
newtype Ref a = Ref { unRef :: String }
   deriving(Show, Read, Data, Typeable, Generic)
   
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
   
instance ToElement a => ToElement (Child a) where
   toElement = forwardToElement

data Point = Point 
   { pointX :: Double
   , pointY :: Double
   , pointZ :: Double
   } deriving(Eq, Show, Ord, Read, Data, Typeable, Generic)
   
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
   
instance Each Vector Vector Double Double where
   each f Vector {..} = Vector <$> f vectorX <*> f vectorY <*> f vectorZ
   
instance ToElement Vector where
   toElement Vector {..}
      = tag "vector" 
         # ("x", vectorX)
         # ("y", vectorY)
         # ("z", vectorZ)

data Translate = Translate 
   { translateX :: Double
   , translateY :: Double
   , translateZ :: Double
   } deriving(Eq, Show, Ord, Read, Data, Typeable, Generic)

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
   { rotateX     :: Double
   , rotateY     :: Double
   , rotateZ     :: Double
   , rotateAngle :: Double
   } deriving(Eq, Show, Ord, Read, Data, Typeable, Generic)

instance ToElement Rotate where
   toElement Rotate {..} 
      = tag "rotate"
         # ("x"    , rotateX    )
         # ("y"    , rotateY    )
         # ("z"    , rotateZ    )
         # ("angle", rotateAngle)

data Scale 
   = SUniformScale Double
   | SScaleAxis    Vector
    deriving(Eq, Show, Ord, Read, Data, Typeable, Generic)

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

instance ToElement TransformCmd where
   toElement = forwardToElement

newtype RegularTransform = 
   RegularTransform { unRegularTransform :: [TransformCmd] }
    deriving(Eq, Show, Ord, Read, Data, Typeable, Generic)

instance ToElement RegularTransform where
   toElement (RegularTransform xs) = 
       tag "transform" `addChildList` xs

newtype Animation = Animation {unAnimation :: [(Double, RegularTransform)] }
    deriving(Eq, Show, Ord, Read, Data, Typeable, Generic)

instance ToElement Animation where
   toElement (Animation xs) 
      = let toKeyframe (k, trans) = toElement trans # ("time", k) 
      in tag "animation" `addChildList` map toKeyframe xs
data Transform 
   = TAnimated Animation
   | TRegular  RegularTransform
    deriving(Eq, Show, Ord, Read, Data, Typeable, Generic)
   
instance ToElement Transform where
   toElement = forwardToElement

tshow = T.pack . show

data Alias a = Alias
   { aliasId :: Ref a
   , aliasAs :: String
   } deriving(Show, Read, Data, Typeable, Generic)
   
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
   
instance ToElement WrapMode where
   toElement = defaultShowInstance
   
data FilterType
   = EWA
   | Trilinear
   | Nearest
   deriving(Eq, Show, Ord, Read, Data, Typeable, Generic, Bounded, Enum)
   
instance ToElement FilterType where
   toElement = defaultShowInstance

data CachePolicy = Cache | DontCache
   deriving(Eq, Show, Ord, Read, Data, Typeable, Generic, Bounded, Enum)
   
instance ToElement CachePolicy where
   toElement = defaultShowInstance

data Bitmap = Bitmap 
   { bitmapFilename      :: FilePath
   , bitmapWrapMode      :: WrapMode
   , bitmapWrapModeU     :: WrapMode
   , bitmapWrapModeV     :: WrapMode
   , bitmapGamma         :: Double
   , bitmapFilterType    :: FilterType
   , bitmapMaxAnisotropy :: Double
   , bitmapCache         :: CachePolicy
   , bitmapUOffset       :: Double
   , bitmapVOffset       :: Double
   , bitmapUScale        :: Double
   , bitmapVScale        :: Double
   , bitmapChannel       :: Channel 
   } deriving (Show, Eq, Read, Ord, Generic, Data, Typeable)

instance ToElement Bitmap   

data Checkerboard = Checkerboard
   { checkerboardColor0  :: Spectrum
   , checkerboardColor1  :: Spectrum
   , checkerboardUOffset :: Double
   , checkerboardVOffset :: Double
   , checkerboardUScale  :: Double
   , checkerboardVScale  :: Double
   } deriving (Show, Eq, Read, Ord, Generic, Data, Typeable)

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

instance ToElement GridTexture

data Color = CS Spectrum | CT Texture
   deriving(Eq, Show, Ord, Read, Data, Typeable, Generic)
   
instance ToElement Color where
   toElement = forwardToElement

data ScaleTexture = ScaleTexture
   { scaleTexture :: Color
   , scaleValue   :: Double
   } deriving (Show, Eq, Read, Ord, Generic, Data, Typeable)
   
instance ToElement ScaleTexture

data Wireframe = Wireframe
   { wireframeInteriorColor :: Spectrum
   , wireframeEdgeColor     :: Spectrum
   , wireframeLineWidth     :: Double
   , wireframeStepWidth     :: Double
   } deriving (Show, Eq, Read, Ord, Generic, Data, Typeable)

instance ToElement Wireframe

data CurvatureType = Mean | Gaussian   
   deriving (Show, Eq, Read, Ord, Generic, Data, Typeable)

instance ToElement CurvatureType where
   toElement = defaultShowInstance

data Curvature = Curvature
   { curvatureCurvature :: CurvatureType
   , curvatureScale     :: Double
   } deriving (Show, Eq, Read, Ord, Generic, Data, Typeable)

instance ToElement Curvature

data Texture 
   = TBitmap       Bitmap
   | TCheckerboard Checkerboard
   | TGridTexture  GridTexture
   | TScale        ScaleTexture
   | TVertexColor  
   | TWireframe    Wireframe
   | TCurvature    Curvature
   deriving (Show, Eq, Read, Ord, Generic, Data, Typeable)

instance ToElement Texture 

data Cube = Cube 
   { cubeToWorld     :: Transform
   , cubeFlipNormals :: Bool
   } deriving(Eq, Show, Ord, Read, Data, Typeable, Generic)
  
instance ToElement Cube  
   
data Sphere = Sphere 
   { sphereCenter      :: Point
   , sphereRadius      :: Double
   , sphereToWorld     :: Transform
   , sphereFlipNormals :: Bool
   } deriving(Eq, Show, Ord, Read, Data, Typeable, Generic) 
   
instance ToElement Sphere

data Cylinder = Cylinder 
   { cylinderP0          :: Point
   , cylinderP1          :: Point
   , cylinderRadius      :: Double
   , cylinderFlipNormals :: Bool
   , cylinderToWorld     :: Transform
   } deriving(Eq, Show, Ord, Read, Data, Typeable, Generic)

instance ToElement Cylinder
   
data Rectangle = Rectangle 
   { rectangleToWorld     :: Transform
   , rectangleFlipNormals :: Bool
   } deriving(Eq, Show, Ord, Read, Data, Typeable, Generic)
   
instance ToElement Rectangle
   
data Disk = Disk 
   { diskToWorld     :: Disk
   , diskFlipNormals :: Bool
   } deriving(Eq, Show, Ord, Read, Data, Typeable, Generic)
   
instance ToElement Disk

data OBJ = OBJ
   { objFileName       :: String
   , objFaceNormals    :: Bool
   , objMaxSmoothAngle :: Double
   , objFlipNormals    :: Bool
   , objFlipTexCoords  :: Bool
   , objToWorld        :: Transform
   } deriving(Eq, Show, Ord, Read, Data, Typeable, Generic)
   
instance ToElement OBJ

data Serialized = Serialized 
   { serializedFileName       :: String
   , serializedShapeIndex     :: Int
   , serializedFaceNormals    :: Bool
   , serializedMaxSmoothAngle :: Double
   , serializedFlipNormals    :: Bool
   , serializedToWorld        :: Transform
   } deriving(Eq, Show, Ord, Read, Data, Typeable, Generic)
   
instance ToElement Serialized

data PLY = PLY 
   { plyFileName       :: String
   , plyFaceNormals    :: Bool
   , plyMaxSmoothAngle :: Double
   , plyFlipNormals    :: Bool
   , plyToWorld        :: Transform
   , plySRGB           :: Bool
   } deriving(Eq, Show, Ord, Read, Data, Typeable, Generic)
   
instance ToElement PLY

data Hair = Hair 
   { hairFileName       :: String
   , hairRadius         :: Double
   , hairAngleThreshold :: Double
   , hairReduction      :: Double
   , hairToWorld        :: Transform
   , hairWidth          :: Int
   , hairHeight         :: Int
   , hairTexture        :: Texture
   } deriving(Eq, Show, Ord, Read, Data, Typeable, Generic)
   
instance ToElement Hair

data Diffuse = Diffuse 
   { diffuseReflectance :: Color
   } deriving(Eq, Show, Ord, Read, Data, Typeable, Generic)
   
instance ToElement Diffuse
   
data RoughDiffuse = RoughDiffuse 
   { roughDiffuseReflectance   :: Color
   , roughDiffuseAlpha         :: Color
   , roughDiffuseUseFastApprox :: Bool
   } deriving(Eq, Show, Ord, Read, Data, Typeable, Generic)

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

instance ToAttributeValue KnownMaterial where
   toAttributeValue = \case
      Vacuum              -> "vacuum"
      Helium              -> "helium"
      Hydrogen            -> "hydrogen"
      Air                 -> "air"
      CarbonDioxide       -> "carbonDioxide"
      Water               -> "water"
      Acetone             -> "acetone"
      Ethanol             -> "ethanol"
      CarbonTetrachloride -> "carbonTetrachloride"
      Glycerol            -> "glycerol"
      Benzene             -> "benzene"
      SiliconeOil         -> "siliconeOil"
      Bromine             -> "bromine"
      WaterIce            -> "waterIce"
      FusedQuartz         -> "fusedQuartz"
      Pyrex               -> "pyrex"
      AcrylicGlass        -> "acrylicGlass"
      Polypropylene       -> "polypropylene"
      Bk7                 -> "bk7"
      SodiumChloride      -> "sodiumChloride"
      Amber               -> "amber"
      Pet                 -> "pet"
      Diamond             -> "diamond"

instance ToElement KnownMaterial where
   toElement = toElement . toAttributeValue

data Refraction 
   = RKM KnownMaterial 
   | IOR Double 
   deriving(Eq, Show, Ord, Read, Data, Typeable, Generic)
   
instance ToElement Refraction where
   toElement = forwardToElement

data Dielectric = Dielectric 
   { dielectricIntIOR                :: Refraction
   , dielectricExtIOR                :: Refraction
   , dielectricSpecularReflectance   :: Color
   , dielectricSpecularTransmittance :: Color
   } deriving(Eq, Show, Ord, Read, Data, Typeable, Generic)

instance ToElement Dielectric

data ThinDielectric = ThinDielectric
   { thinDielectricIntIOR               :: Refraction
   , thinDielectricExtIOR               :: Refraction
   , thinDielectricSpecularReflectance  :: Color
   , thinDielectricSpecularTranmittance :: Color
   } deriving(Eq, Show, Ord, Read, Data, Typeable, Generic)

instance ToElement ThinDielectric
   
data Distribution 
   = Beckmann
   | GGX
   | Phong
    deriving(Eq, Show, Ord, Read, Data, Typeable, Generic, Enum, Bounded)

instance ToAttributeValue Distribution where
   toAttributeValue = \case
      Beckmann -> "beckmann"
      GGX      -> "ggx"
      Phong    -> "phong"
      
instance ToElement Distribution where
   toElement = toElement . toAttributeValue

data Luminance 
   = UniformLuminance Double
   | TextureLuminance Texture
   deriving (Eq, Show, Ord, Read, Data, Typeable, Generic)
   
instance ToElement Luminance where
   toElement = forwardToElement

data RoughDielectricRegular = RoughDielectricRegular
   { roughDielectricRegularDistribution          :: Distribution
   , roughDielectricRegularAlpha                 :: Luminance
   , roughDielectricRegularIntIOR                :: Refraction
   , roughDielectricRegularExtIOR                :: Refraction
   , roughDielectricRegularSpecularReflectance   :: Color
   , roughDielectricRegularSpecularTransmittance :: Color
   } deriving(Eq, Show, Ord, Read, Data, Typeable, Generic)
   
instance ToElement RoughDielectricRegular
   
data RoughDielectricAnistrophic = RoughDielectricAnistrophic
   { roughDielectricAnistrophicDistribution          :: Distribution
   , roughDielectricAnistrophicAlphaU                :: Luminance
   , roughDielectricAnistrophicAlphaV                :: Luminance
   , roughDielectricAnistrophicIntIOR                :: Refraction
   , roughDielectricAnistrophicExtIOR                :: Refraction
   , roughDielectricAnistrophicSpecularReflectance   :: Color
   , roughDielectricAnistrophicSpecularTransmittance :: Color
   } deriving(Eq, Show, Ord, Read, Data, Typeable, Generic)
   
instance ToElement RoughDielectricAnistrophic   
   
data RoughDielectric 
   = RDRoughDielectricRegular      RoughDielectricRegular
   | RDRoughDielectricAnisotrophic RoughDielectricAnistrophic
   deriving(Eq, Show, Ord, Read, Data, Typeable, Generic)
   
instance ToElement RoughDielectric where
   toElement = forwardToElement

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
   | Selenium
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
      Selenium                       -> "Se"
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
   
data SmoothConductor = SmoothConductor 
   { smoothConductorMaterial            :: ConductorType
   , smoothConductorEta                 :: Spectrum
   , smoothConductorK                   :: Spectrum
   , smoothConductorExtEta              :: Refraction
   , smoothConductorSpecularReflectance :: Color
   } deriving(Eq, Show, Ord, Read, Data, Typeable, Generic)
   
instance ToElement SmoothConductor

--TODO make luminance
-- Luminance
-- and a instance that forwards

data IndexOfRefraction 
   = IORNumeric       Double
   | IORKnownMaterial KnownMaterial
   deriving(Eq, Show, Ord, Read, Data, Typeable, Generic) 

instance ToElement IndexOfRefraction where
   toElement = forwardToElement

data RoughConductorRegular = RoughConductorRegular 
   { roughConductorRegularDistribution :: Distribution
   , roughConductorRegularAlpha        :: Luminance
   , roughConductorRegularMaterial     :: ConductorType
   , roughConductorRegularEta          :: Spectrum
   , roughConductorRegularK            :: Spectrum
   , roughConductorRegularExtEta       :: IndexOfRefraction
   , roughConductorSpecularReflectance :: Color
   } deriving(Eq, Show, Ord, Read, Data, Typeable, Generic)
   
instance ToElement RoughConductorRegular
   
data RoughConductorAnisotropic = RoughConductorAnisotropic
   { roughConductorAnisotropicAlphaU              :: Luminance
   , roughConductorAnisotropicAlphaV              :: Luminance
   , roughConductorAnisotropicMaterial            :: ConductorType
   , roughConductorAnisotropicEta                 :: Spectrum
   , roughConductorAnisotropicRegularK            :: Spectrum
   , roughConductorAnisotropicRegularExtEta       :: IndexOfRefraction
   , roughConductorAnisotropicSpecularReflectance :: Color
   } deriving(Eq, Show, Ord, Read, Data, Typeable, Generic)
   
instance ToElement RoughConductorAnisotropic

data RoughConductor 
   = RCRegular     RoughConductorRegular
   | RCAnisotropic RoughConductorAnisotropic
   deriving(Eq, Show, Ord, Read, Data, Typeable, Generic)
   
instance ToElement RoughConductor where
   toElement = forwardToElement

data Plastic = Plastic
   { plasticIntIOR              :: Refraction
   , plasticExtIOR              :: Refraction
   , plasticSpecularReflectance :: Color
   , plasticDiffuseReflectance  :: Color
   , plasticNonlinear           :: Bool
   } deriving(Eq, Show, Ord, Read, Data, Typeable, Generic)
   
instance ToElement Plastic

data RoughPlastic = RoughPlastic
   { roughPlasticDistribution        :: Distribution
   , roughPlasticAlpha               :: Luminance
   , roughPlasticIntIOR              :: Refraction
   , roughPlasticExtIOR              :: Refraction
   , roughPlasticSpecularReflectance :: Color
   , roughPlasticDiffuseReflectance  :: Color
   , roughPlasticNonlinear           :: Bool
   } deriving(Eq, Show, Ord, Read, Data, Typeable, Generic)
   
instance ToElement RoughPlastic

data SmoothDielectricCoating = SmoothDielectricCoating
   { smoothDielectricCoatingIntIOR             :: Refraction
   , smoothDielectricCoatingExtIOR             :: Refraction
   , smoothDielectricCoatingThickness          :: Double
   , smoothDielectricCoatingSigmaA             :: Color
   , smoothDielectricCoatingSpecularReflection :: Color
   , smoothDielectricCoatingChild              :: Child BSDF
   } deriving(Eq, Show, Ord, Read, Data, Typeable, Generic)
   
instance ToElement SmoothDielectricCoating   
   
-- TODO make a Coating a = Coating { coatingData :: a, child :: BSDF }   
data RoughDielectricCoating = RoughDielectricCoating
   { roughDielectricCoatingDistribution        :: Distribution
   , roughDielectricCoatingAlpha               :: Luminance
   , roughDielectricCoatingIntIOR              :: IndexOfRefraction
   , roughDielectricCoatingExtIOR              :: IndexOfRefraction
   , roughDielectricCoatingThickness           :: Double
   , roughDielectricCoatingSigmaA              :: Color
   , roughDielectricCoatingSpecularReflectance :: Color
   , roughDielectricCoatingChild               :: Child BSDF
   } deriving(Eq, Show, Ord, Read, Data, Typeable, Generic)

instance ToElement RoughDielectricCoating

data Bump = Bump 
   { bumpMap  :: Texture
   , bumpBSDF :: Child BSDF
   } deriving(Eq, Show, Ord, Read, Data, Typeable, Generic)

instance ToElement Bump

data ModifiedPhong = ModifiedPhong
   { modifiedPhongExponent            :: Luminance
   , modifiedPhongSpecularReflectance :: Color
   , modifiedPhongDiffuseReflectance  :: Color 
   } deriving(Eq, Show, Ord, Read, Data, Typeable, Generic)
   
instance ToElement ModifiedPhong
   
data WardType
   = WTWard
   | WTWardDuer
   | WTBalanced
   deriving(Eq, Show, Ord, Read, Data, Typeable, Generic, Bounded, Enum)
   
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
   
instance ToElement Ward
   
data MixtureBSDF = MixtureBSDF 
   { mixtureBSDFChildren :: [(Double, BSDF)]
   } deriving(Eq, Show, Ord, Read, Data, Typeable, Generic)

instance ToElement MixtureBSDF where
   toElement MixtureBSDF {..} 
      = let (weights, children) = unzip mixtureBSDFChildren
      in addChildList (tag "mixturebsdf")
         $ (primitive "string" (intercalate ", " $ map show weights) # 
            ("name", "weights" :: Text)) :
           map toElement children

data BlendBSDF = BlendBSDF
   { blendBSDFWeight :: Luminance
   , blendBSDFChild  :: Child BSDF
   } deriving(Eq, Show, Ord, Read, Data, Typeable, Generic)
   
instance ToElement BlendBSDF
   
data Mask = Mask 
   { maskOpacity :: Color
   , maskChild   :: Child BSDF
   } deriving(Eq, Show, Ord, Read, Data, Typeable, Generic)
   
instance ToElement Mask
   
data Twosided = Twosided 
   { twosidedChild :: Child BSDF
   } deriving(Eq, Show, Ord, Read, Data, Typeable, Generic)
   
instance ToElement Twosided

data Difftrans = Difftrans
   { diffTransTransmittance :: Color
   } deriving(Eq, Show, Ord, Read, Data, Typeable, Generic)
   
instance ToElement Difftrans
   
data HK = HK 
   { hkMaterial  :: KnownMaterial
   , hkSigmaS    :: Color
   , hkSigmaA    :: Color
   , hkSigmaT    :: Color
   , hkAlbedo    :: Color
   , hkThickness :: Double
   , hkChild     :: Child BSDF
   } deriving(Eq, Show, Ord, Read, Data, Typeable, Generic)
   
instance ToElement HK
   
data Irawan = Irawan
   { irawanFilename             :: FilePath
   , irawanRepeatU              :: Double
   , irawanRepeatV              :: Double
   , irawanAdditionalParameters :: [(String, Either Spectrum Double)]
   } deriving(Eq, Show, Ord, Read, Data, Typeable, Generic)

instance ToElement Irawan where
   toElement Irawan {..} 
      = foldl' (.>) (tag "irawan") 
      $ [ ("filename", toElement irawanFilename)
        , ("repeatU" , toElement irawanRepeatU)
        , ("repeatV" , toElement irawanRepeatV)
        ] ++ map (T.pack *** forwardToElement) irawanAdditionalParameters

data BSDF 
   = BSDFDiffuse                 Diffuse 
   | BSDFRoughDiffuse            RoughDiffuse
   | BSDFDielectric              Dielectric
   | BSDFThinDielectric          ThinDielectric
   | BSDFRoughDielectric         RoughDielectric
   | BSDFSmoothConductor         SmoothConductor
   | BSDFRoughConductor          RoughConductor
   | BSDFPlastic                 Plastic
   | BSDFRoughPlastic            RoughPlastic
   | BSDFSmoothDielectricCoating SmoothDielectricCoating
   | BSDFRoughDielectricCoating  RoughDielectricCoating
   | BSDFBump                    Bump
   | BSDFModifiedPhong           ModifiedPhong
   | BSDFWard                    Ward
   | BSDFMixtureBSDF             MixtureBSDF
   | BSDFBlendBSDF               BlendBSDF
   | BSDFMask                    Mask
   | BSDFTwosided                Twosided
   | BSDFDifftrans               Difftrans
   | BSDFHK                      HK
   | BSDFIrawan                  Irawan
   deriving(Eq, Show, Ord, Read, Data, Typeable, Generic)

instance ToElement BSDF

data ShapeType 
   = STCube       Cube
   | STSphere     Sphere
   | STRectangle  Rectangle
   | STDisk       Disk
   | STOBJ        OBJ
   | STPLY        PLY
   | STSerialized Serialized
   | STGroup      [ShapeType]
   | STInstance   String
   | STHair       Hair
    deriving(Eq, Show, Ord, Read, Data, Typeable, Generic)

instance ToElement a => ToElement [a] where
   toElement = addChildList (tag "list") 
    
instance ToElement ShapeType where
   
    
toShapeType :: ShapeType -> Text
toShapeType = \case
   STCube       {} -> "cube"
   STSphere     {} -> "sphere"
   STRectangle  {} -> "rectangle"
   STDisk       {} -> "disk"
   STOBJ        {} -> "obj"
   STPLY        {} -> "ply"
   STSerialized {} -> "serialized"
   STGroup      {} -> "group"
   STInstance   {} -> "instance"
   STHair       {} -> "hair"
   

data MicroFlake = MicroFlake
   { microFlakeStddev :: Double
   } deriving (Show, Eq, Read, Ord, Generic, Data, Typeable)

instance ToElement MicroFlake

-- Combine with other mixture phases
data MixturePhase = MixturePhase
   { mixturePhaseChildren :: [(Double, Child PhaseFunction)]
   } deriving (Show, Eq, Read, Ord, Generic, Data, Typeable)

instance ToElement MixturePhase where
   toElement MixturePhase {..} = 
      let (weights, phaseFunctions) = unzip mixturePhaseChildren
      in addChildList (tag "mixturephase") $
            ( primitive "weights" 
                $ T.intercalate ", " 
                $ map (T.pack . show) weights
            ) : map toElement phaseFunctions
         
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
   { sigmaASA :: Spectrum
   , sigmaASS :: Spectrum
   }
   deriving (Show, Eq, Read, Ord, Generic, Data, Typeable)

instance ToElement SigmaAS

data SigmaTAlbedo = SigmaTAlbedo 
   { sigmaTAlbedoT      :: Spectrum
   , sigmaTAlbedoAlbedo :: Spectrum
   } deriving (Show, Eq, Read, Ord, Generic, Data, Typeable)

instance ToElement SigmaTAlbedo

data MaterialStyle 
   = MSMaterial      SSSMaterial
   | MSSigmaAS       SigmaAS
   | MSSigmaTAlbedo  SigmaTAlbedo
   deriving (Show, Eq, Read, Ord, Generic, Data, Typeable)

instance ToElement MaterialStyle where
   toElement = forwardToElement

data PhaseFunction 
   = PFIsotropic 
   | PFHG        Double
   | PFRayleigh  
   | PFKKay
   | PFMicroflake   MicroFlake
   | PFMixturePhase MixturePhase
   deriving (Show, Eq, Read, Ord, Generic, Data, Typeable)

instance ToElement PhaseFunction

data Homogeneous = Homogeneous 
   { homogeneousMaterialStyle :: MaterialStyle
   , homogeneousScale         :: Double
   , homogeneousPhase         :: PhaseFunction
   } deriving (Show, Eq, Read, Ord, Generic, Data, Typeable)

instance ToElement Homogeneous
   
data HeterogeneousSampling 
   = Simpson
   | Woodcock
   deriving (Show, Eq, Read, Ord, Generic, Data, Typeable, Enum, Bounded)
   
instance ToElement HeterogeneousSampling where
   toElement x = toElement $ case x of
      Simpson  -> "simpson" :: Text
      Woodcock -> "woodcock"

data ConstVolume 
   = CVDouble   Double
   | CVSpectrum Spectrum
   | CVVector   Vector
   deriving (Show, Eq, Read, Ord, Generic, Data, Typeable)
   
instance ToElement ConstVolume where
   toElement = forwardToElement
   
data SendDataType = SendAcrossNetwork | AssumeAvailable
   deriving (Show, Eq, Read, Ord, Generic, Data, Typeable, Enum, Bounded)

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
   
instance ToElement GridVolume

data VolCache = VolCache
   { volCacheBlockSize   :: Integer
   , volCacheVoxelWidth  :: Double
   , volCacheMemoryLimit :: Integer
   , volCacheToWorld     :: RegularTransform
   , volCacheChild       :: Child VolumeDataSource
   } deriving (Show, Eq, Read, Ord, Generic, Data, Typeable)

instance ToElement VolCache

data VolumeDataSource
   = VDSConstVolume ConstVolume
   | VDSGridVolume  GridVolume
   | VDSVolCache    VolCache
   deriving (Show, Eq, Read, Ord, Generic, Data, Typeable)
   
instance ToElement VolumeDataSource where
   toElement = forwardToElement
   
data Heterogeneous = Heterogeneous
   { heterogeneousMethod      :: HeterogeneousSampling
   , heterogeneousDensity     :: VolumeDataSource
   , heterogeneousAlbedo      :: VolumeDataSource 
   , heterogeneousOrientation :: VolumeDataSource
   , heterogeneousScale       :: Double
   , heterogenousPhase        :: Child PhaseFunction
   } deriving (Show, Eq, Read, Ord, Generic, Data, Typeable)

instance ToElement Heterogeneous
   
data PartcipatingMedia 
   = PMHomogeneous   Homogeneous 
   | PMHeterogeneous Heterogeneous
   deriving (Show, Eq, Read, Ord, Generic, Data, Typeable)

instance ToElement PartcipatingMedia where
   toElement = forwardToElement

data MediumPair = MediumPair 
   { mediumPairInterior :: PartcipatingMedia
   , mediumPairExterior :: PartcipatingMedia
   } deriving(Eq, Show, Ord, Read, Data, Typeable, Generic)
   
instance Each MediumPair MediumPair PartcipatingMedia PartcipatingMedia where
   each f MediumPair {..}
       =  MediumPair 
      <$> f mediumPairInterior
      <*> f mediumPairExterior
   
data Shape = Shape 
   { shapeType       :: ShapeType
   , shapeMaterial   :: Maybe BSDF 
   , shapeMediumPair :: Maybe MediumPair
   } deriving(Eq, Show, Ord, Read, Data, Typeable, Generic)

instance ToElement Shape where
   toElement Shape {..} = 
     addChildList (tag "shape" # ("type", toShapeType shapeType))
          $ ( map childItemElement
            . filter (has (childItemTypeL . _Nested)) 
            . F.toList 
            . elementChildren 
            . toElement
            ) shapeType
         <> (maybeToList . fmap toElement) shapeMaterial
         <> (fromMaybe [] $ do
               MediumPair x y <- shapeMediumPair
               return $ 
                  [ toElement x
                  , toElement y
                  ]
            )

data Dipole = Dipole 
   { dipoleMaterialStyle :: MaterialStyle
   , dipoleScale         :: Double
   , dipoleIntIOR        :: Refraction
   , dipoleExtIOR        :: Refraction
   , dipoleIRRSamples    :: Integer
   } deriving (Show, Eq, Read, Ord, Generic, Data, Typeable)
   
instance ToElement Dipole   
   
data SSS 
   = SSSDipole Dipole
   deriving (Show, Eq, Read, Ord, Generic, Data, Typeable)

instance ToElement SSS

data PointLight = PointLight
   { pointLightToWorld        :: Transform
   , pointLightPosition       :: Point
   , pointLightIntensity      :: Spectrum
   , pointLightSamplingWeight :: Double
   } deriving (Show, Eq, Read, Ord, Generic, Data, Typeable)
   
instance ToElement PointLight     
   
data AreaLight = AreaLight 
   { areaLightRadiance       :: Spectrum
   , areaLightSamplingWeight :: Double
   } deriving (Show, Eq, Read, Ord, Generic, Data, Typeable)

instance ToElement AreaLight

data SpotLight = SpotLight
   { spotLightToWorld        :: Transform
   , spotLightIntensity      :: Spectrum
   , spotLightCutoffAngle    :: Double
   , spotLightBeamWidth      :: Double
   , spotLightTexture        :: Texture
   , spotLightSamplingWeight :: Double
   } deriving (Show, Eq, Read, Ord, Generic, Data, Typeable)

instance ToElement SpotLight

data DirectionalLight = DirectionalLight
   { directionalLightToWorld        :: Transform
   , directionalLightVector         :: Vector
   , directionalLightIrradiance     :: Spectrum
   , directionalLightSamplingWeight :: Double
   } deriving (Show, Eq, Read, Ord, Generic, Data, Typeable)

instance ToElement DirectionalLight

data Collimated = Collimated 
   { collimatedToWorld        :: Transform
   , collimatedPower          :: Spectrum
   , collimatedSamplingWeight :: Double
   } deriving (Show, Eq, Read, Ord, Generic, Data, Typeable)

instance ToElement Collimated

data Sky = Sky
   { skyTurbidity      :: Double
   , skyAlbedo         :: Spectrum
   , skyYear           :: Integer
   , skyMinute         :: Integer
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
   , sunLongitute    :: Double
   , sunTimezone     :: Double
   , sunResolution   :: Integer
   , sunScale        :: Double
   , sunRadiusScale  :: Double
   , sunSampleWeight :: Double
   } deriving (Show, Eq, Read, Ord, Generic, Data, Typeable)
   
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
   , sunskySunDirection   :: Double
   , sunskyStretch        :: Double
   , sunskyResolution     :: Integer
   , sunskySunScale       :: Double
   , sunskySkyScale       :: Double
   , sunskySunRadiusScale :: Double
   } deriving (Show, Eq, Read, Ord, Generic, Data, Typeable)
   
instance ToElement Sunsky   
   
data Envmap = Envmap
   { envmapFilename       :: FilePath
   , envmapScale          :: Double
   , envmapToWorld        :: Transform
   , envmapGamma          :: Double
   , envmapCache          :: Bool
   , envmapSamplingWeight :: Double
   } deriving (Show, Eq, Read, Ord, Generic, Data, Typeable)
   
instance ToElement Envmap  
   
data Constant = Constant
   { constantRadiance       :: Spectrum
   , constantSamplingWeight :: Double
   } deriving (Show, Eq, Read, Ord, Generic, Data, Typeable)

instance ToElement Constant

data Emitter
   = EPointLight       PointLight
   | EAreaLight        AreaLight
   | ESpotLight        SpotLight
   | EDirectionalLight DirectionalLight
   | ESky              Sky
   | ESun              Sun
   | ESunSky           Sunsky
   | EEnvmap           Envmap
   | EConstant         Constant
   deriving (Show, Eq, Read, Ord, Generic, Data, Typeable)

instance ToElement Emitter


data FOVType
   = FOVTX
   | FOVTY
   | FOVTDiagonal
   | FOVTSmaller
   | FOVTLarger
   deriving (Show, Eq, Read, Ord, Generic, Data, Typeable)
   
instance ToElement FOVType where
   toElement x = primitive "string" $ case x of
      FOVTX        -> "tx" :: Text
      FOVTY        -> "ty"
      FOVTDiagonal -> "diagonal"
      FOVTSmaller  -> "smaller"
      FOVTLarger   -> "larger"

data Perspective = Perspective
   { perspectiveToWorld      :: Transform
   , perspectiveFocalLength  :: Double
   , perspectiveFOV          :: Double
   , perspectiveFOVAxis      :: FOVType
   , perspectiveShutterOpen  :: Double
   , perspectiveShutterClose :: Double
   , perspectiveNearClip     :: Double
   , perspectiveFarClip      :: Double
   } deriving (Show, Eq, Read, Ord, Generic, Data, Typeable)

instance ToElement Perspective

data Thinlens = Thinlens
   { thinlensToWorld         :: Transform
   , thinlensAperatureRadius :: Double
   , thinlensFocusDistance   :: Double
   , thinlensFocalLength     :: Double
   , thinlensFOV             :: Double
   , thinlensFOVAxis         :: FOVType
   , thinlensShutterOpen     :: Double
   , thinlensShutterClose    :: Double
   , thinlensNearClip        :: Double
   , thinlensFarClip         :: Double
   } deriving (Show, Eq, Read, Ord, Generic, Data, Typeable)

instance ToElement Thinlens

data Orthographic = Orthographic
   { orthographicToWorld      :: Transform
   , orthographicShutterOpen  :: Double
   , orthographicShutterClose :: Double
   , orthographicNearClip     :: Double
   , orthographicFarClip      :: Double   
   } deriving (Show, Eq, Read, Ord, Generic, Data, Typeable)

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

instance ToElement Telecentric

data Spherical = Spherical 
   { sphericalToWorld      :: Transform
   , sphericalShutterOpen  :: Double
   , sphericalShutterClose :: Double
   } deriving (Show, Eq, Read, Ord, Generic, Data, Typeable)
   
instance ToElement Spherical   
   
data IrradianceMeter = IrradianceMeter
   { irradianceMeterShutterOpen  :: Double
   , irradianceMeterShutterClose :: Double
   } deriving (Show, Eq, Read, Ord, Generic, Data, Typeable)
   
instance ToElement IrradianceMeter   
   
data RadianceMeter = RadianceMeter
   { radianceToWorld      :: Transform
   , radianceShutterOpen  :: Double
   , radianceShutterClose :: Double
   } deriving (Show, Eq, Read, Ord, Generic, Data, Typeable)

instance ToElement RadianceMeter

data FluenceMeter = FluenceMeter
   { fluenceMeterToWorld      :: Transform
   , fluenceMeterShutterOpen  :: Double
   , fluenceMeterShutterClose :: Double
   } deriving (Show, Eq, Read, Ord, Generic, Data, Typeable)

instance ToElement FluenceMeter

data PolyTwoAndFour = PolyTwoAndFour
   { polyTwoAndFourTwo  :: Double
   , polyTwoAndFourFour :: Double
   } deriving (Show, Eq, Read, Ord, Generic, Data, Typeable)
   
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
   , perspectiveRDistKC           :: PolyTwoAndFour
   , perspectiveRDistFocalLength  :: Double
   , perspectiveRDistFOV          :: Double
   , perspectiveRDistFOVAxis      :: FOVType
   , perspectiveRDistShutterOpen  :: Double
   , perspectiveRDistShutterClose :: Double
   , perspectiveRDistNearClip     :: Double
   , perspectiveRDistFarClip      :: Double
   } deriving (Show, Eq, Read, Ord, Generic, Data, Typeable)

instance ToElement PerspectiveRDist

data Sensor
   = SPerspective      Perspective
   | SThinlens         Thinlens
   | SOrthographic     Orthographic
   | STelecentric      Telecentric
   | SSpherical        Spherical
   | SIrradianceMeter  IrradianceMeter
   | SRadianceMeter    RadianceMeter
   | SFluenceMeter     FluenceMeter
   | SPerspectiveRDist PerspectiveRDist
   deriving (Show, Eq, Read, Ord, Generic, Data, Typeable)
   
instance ToElement Sensor
   
data AmbientOcclusion = AmbientOcclusion 
   { ambientOcclusionShdingSamples :: Integer
   , ambientOcclusionRayLength     :: Double
   } deriving (Show, Eq, Read, Ord, Generic, Data, Typeable)

instance ToElement AmbientOcclusion

data Visibility = Hidden | Visible
   deriving (Show, Eq, Read, Ord, Generic, Data, Typeable, Enum, Bounded)

instance ToElement Visibility where
   toElement x = toElement $ case x of
      Hidden  -> False
      Visible -> True

data NormalStrictness = Strict | Loose
   deriving (Show, Eq, Read, Ord, Generic, Data, Typeable, Enum, Bounded)

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
   
instance ToElement Direct
   
data Path = Path
   { pathMaxDepth      :: Integer
   , pathRRDepth       :: Integer
   , pathStrictNormals :: NormalStrictness
   , pathHideEmitters  :: Visibility
   } deriving (Show, Eq, Read, Ord, Generic, Data, Typeable)

instance ToElement Path

data VolPathSimple = VolPathSimple
   { volPathSimpleMaxDepth      :: Integer
   , volPathSimpleRRDepth       :: Integer
   , volPathSimpleStrictNormals :: NormalStrictness
   , volPathSimpleHideEmitters  :: Visibility
   } deriving (Show, Eq, Read, Ord, Generic, Data, Typeable)

instance ToElement VolPathSimple

data VolPath = VolPath
   { volPathMaxDepth      :: Integer
   , volPathRRDepth       :: Integer
   , volPathStrictNormals :: NormalStrictness
   , volPathHideEmitters  :: Visibility
   } deriving (Show, Eq, Read, Ord, Generic, Data, Typeable)

instance ToElement VolPath

data PathConnectedness = ConnectToCamera | DontConnect
   deriving (Show, Eq, Read, Ord, Generic, Data, Typeable, Enum, Bounded)

instance ToElement PathConnectedness where
   toElement x = toElement $ case x of
         ConnectToCamera -> True
         DontConnect     -> False

data DirectSampling = DirectSampling | NoDirectSampling   
   deriving (Show, Eq, Read, Ord, Generic, Data, Typeable, Enum, Bounded)
   
instance ToElement DirectSampling where
   toElement x = toElement $ case x of
         DirectSampling   -> True
         NoDirectSampling -> False
   
data BDPT = BDPT
   { bdptMaxDepth     :: Integer
   , bdptLightImage   :: PathConnectedness
   , bdptSampleDirect :: DirectSampling
   , bdptRRDepth      :: Integer
   } deriving (Show, Eq, Read, Ord, Generic, Data, Typeable)

instance ToElement BDPT

data PhotonMapper = PhotonMapper
   { photonMapperDirectSamples       :: Integer
   , photonMapperGlossySamples       :: Integer
   , photonMapperMaxDepth            :: Integer
   , photonMapperGlobalPhotons       :: Integer
   , photonMapperCausticPhotons      :: Integer
   , photonMapperVolumenPhotons      :: Integer
   , photonMapperGlobalLookupRadius  :: Double
   , photonMapperCausticLookupRadius :: Double
   , photonMapperLookupSize          :: Integer
   , photonMapperGranularity         :: Integer
   , photonMapperHideEmitters        :: Bool
   , photonMapperRRDepth             :: Integer
   } deriving (Show, Eq, Read, Ord, Generic, Data, Typeable)

instance ToElement PhotonMapper

data PPM = PPM 
   { ppmMaxDepth      :: Integer
   , ppmPhotonCount   :: Integer
   , ppmInitialRadius :: Double
   , ppmAlpha         :: Double
   , ppmGranularity   :: Integer
   , ppmRRDepth       :: Integer
   , ppmMaxPasses     :: Integer
   } deriving (Show, Eq, Read, Ord, Generic, Data, Typeable)

instance ToElement PPM

data SPPM = SPPM
   { sppmMaxDepth      :: Integer
   , sppmphotonCount   :: Integer
   , sppmInitialRadius :: Double
   , sppmAlpha         :: Double
   , sppmGranularity   :: Integer
   , sppmRRDepth       :: Integer
   , sppmMaxPasses     :: Integer
   } deriving (Show, Eq, Read, Ord, Generic, Data, Typeable)

instance ToElement SPPM

data PSSMLT = PSSMLT
   { pssmltBidirectional    :: Bool
   , pssmltMaxDepth         :: Integer
   , pssmltDirectSamples    :: Integer
   , pssmltRRDepth          :: Integer
   , pssmltLuminanceSamples :: Integer
   , pssmltTwoStage         :: Bool
   , pssmltPLarge           :: Double
   } deriving (Show, Eq, Read, Ord, Generic, Data, Typeable)

instance ToElement PSSMLT

data MLT = MLT
   { mltMaxDepth               :: Integer
   , mltDirectSamples          :: Integer
   , mltLuminanceSamples       :: Integer
   , mltTwoStage               :: Bool
   , mltBidirectionalMutation  :: Bool
   , mltLensPerturbation       :: Bool
   , mltMultiChainLens         :: Bool
   , mltMultiChainMultiChain   :: Bool
   , mltMultiChainCaustic      :: Bool
   , mltMultiChainManifold     :: Bool
   , mltMultiChainPerturbation :: Bool
   } deriving (Show, Eq, Read, Ord, Generic, Data, Typeable)

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

instance ToElement ERPT

data PTracer = PTracer
   { ptracerMaxDepth    :: Integer
   , ptracerRRDepth     :: Integer
   , ptracerGranularity :: Integer
   , ptracerBruteForce  :: Bool
   } deriving (Show, Eq, Read, Ord, Generic, Data, Typeable)

instance ToElement PTracer

data Adaptive = Adaptive
   { adaptiveMaxError        :: Double
   , adaptivePValue          :: Double
   , adaptiveMaxSampleFactor :: Integer
   } deriving (Show, Eq, Read, Ord, Generic, Data, Typeable)

instance ToElement Adaptive

data VP1 = VP1 
   { vp1MaxDepth            :: Integer
   , vp1ShadowMapResolution :: Integer
   , vp1Clamping            :: Double
   } deriving (Show, Eq, Read, Ord, Generic, Data, Typeable)

instance ToElement VP1

data IRRCache = IRRCache 
   { irrCacheResolution        :: Integer
   , irrCacheQuality           :: Double
   , irrCacheGradients         :: Bool
   , irrCacheClampNeighor      :: Bool
   , irrCacheClampScreen       :: Bool
   , irrCacheOverture          :: Bool
   , irrCacheQualityAdjustment :: Double
   , irrCacheIndirectOnly      :: Bool
   , irrCacheDebug             :: Bool
   } deriving (Show, Eq, Read, Ord, Generic, Data, Typeable)

instance ToElement IRRCache

data Integrator 
   = IAmbientOcclusion AmbientOcclusion 
   | IDirect           Direct
   | IPath             Path
   | IVolPathSimple    VolPathSimple
   | IVolPath          VolPath
   | IBDPT             BDPT
   | IPhotonMapper     PhotonMapper
   | IPPM              PPM
   | ISPPM             SPPM
   | IPSSMLT           PSSMLT
   | IMLT              MLT
   | IERPT             ERPT
   | IPTracer          PTracer
   | IAdaptive         Adaptive
   | IVP1              VP1
   | IIRRCache         IRRCache
   deriving (Show, Eq, Read, Ord, Generic, Data, Typeable)

instance ToElement Integrator

data Independent = Independent
   { independentSampleCount :: Integer
   } deriving (Show, Eq, Read, Ord, Generic, Data, Typeable)

instance ToElement Independent

data Stratified = Stratified
   { stratifiedSampleCount :: Integer
   , stratifiedDimension   :: Integer
   } deriving (Show, Eq, Read, Ord, Generic, Data, Typeable)

instance ToElement Stratified

data LDSampler = LDSampler
   { ldSamplerSampleCount :: Integer
   , ldSamplerDimension   :: Integer
   } deriving (Show, Eq, Read, Ord, Generic, Data, Typeable)

instance ToElement LDSampler

data Halton = Halton
   { haltonSampleCount :: Integer
   , haltonScramble    :: Integer
   } deriving (Show, Eq, Read, Ord, Generic, Data, Typeable)

instance ToElement Halton

data Hammersley = Hammersley
   { hammersleySampleCount :: Integer
   , hammersleyScramble    :: Integer
   } deriving (Show, Eq, Read, Ord, Generic, Data, Typeable)

instance ToElement Hammersley

data Sobol = Sobol 
   { sobolSampleCount :: Integer
   , sobolScramble    :: Integer
   } deriving (Show, Eq, Read, Ord, Generic, Data, Typeable)

instance ToElement Sobol

data SampleGenerator
   = SIndependent Independent
   | SStratified  Stratified
   | SLDSampler   LDSampler 
   | SHalton      Halton
   | SHammersley  Hammersley
   | SSobol       Sobol
   deriving (Show, Eq, Read, Ord, Generic, Data, Typeable)

instance ToElement SampleGenerator

data FileFormatType
   = Openexr
   | RGBE
   | PFM
   deriving (Show, Eq, Read, Ord, Generic, Data, Typeable, Enum, Bounded)

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
   
instance ToElement ComponentFormat where
   toElement x = primitive "string" $ case x of
      Float16 -> "float16" :: Text
      Float32 -> "float32"
      UInt32  -> "uint32"

data HDRfilm = HDRfilm
   { hdrfilmWidth            :: Integer
   , hdrfilmHeight           :: Integer
   , hdrfilmFileFormat       :: FileFormatType
   , hdrfilmPixelFormat      :: PixelFormat
   , hdrfilmComponentFormat  :: ComponentFormat
   , hdrfilmCropOffsetX      :: Integer
   , hdrfilmCropOffsetY      :: Integer
   , hdrfilmCropWidth        :: Integer
   , hdrfilmCropHeight       :: Integer
   , hdrfilmAttachLog        :: Bool
   , hdrfilmBanner           :: Bool
   , hdrfilmHighQualityEdges :: Bool
   , hdrfilmRFilter          :: ReconstructionFilter
   } deriving (Show, Eq, Read, Ord, Generic, Data, Typeable)
   
instance ToElement HDRfilm
   
data Crop = Crop
   { cropCropOffsetX :: Integer
   , cropCropOffsetY :: Integer
   , cropCropWidth   :: Integer
   , cropCropHeight  :: Integer
   } deriving (Show, Eq, Read, Ord, Generic, Data, Typeable)

instance ToElement Crop

data TiledHDRFilm = TiledHDRFilm
   { tiledHDRFilmWidth           :: Integer
   , tiledHDRFilmHeight          :: Integer
   , tiledHDRFilmCrop            :: Maybe Crop
   , tiledHDRFilmPixelFormat     :: PixelFormat
   , tiledHDRFilmComponentFormat :: ComponentFormat
   , tiledHDRFilmRFilter         :: ReconstructionFilter
   } deriving (Show, Eq, Read, Ord, Generic, Data, Typeable)

instance ToElement TiledHDRFilm where
   toElement TiledHDRFilm {..}
      = maybe (tag "tiledhdrfilm") 
          (\x -> tag "tiledhdrfilm" .> ("crop", x)) tiledHDRFilmCrop
      .> ("width"          , tiledHDRFilmWidth          )
      .> ("height"         , tiledHDRFilmHeight         )
      .> ("pixelFormat"    , tiledHDRFilmPixelFormat    )
      .> ("componentFormat", tiledHDRFilmComponentFormat)
      .> ("rFilter"        , tiledHDRFilmRFilter        )
          

data GammaType
   = GTGammaCurve Double
   | GTSRGB
   deriving (Show, Eq, Read, Ord, Generic, Data, Typeable)
   
instance ToElement GammaType where
   toElement x = toElement $ case x of
      GTGammaCurve y -> y
      GTSRGB         -> -1

data LDRfilm
   = LDRFGammaFilm GammaFilm 
   | LDRFReinhard  ReinhardFilm
   deriving (Show, Eq, Read, Ord, Generic, Data, Typeable)
   
instance ToElement LDRfilm where
   toElement = forwardToElement
   
data GammaFilm = GammaFilm
   { ldrfilmWidth            :: Integer
   , ldrfilmHeight           :: Integer
   , ldrfilmFileFormat       :: FileFormatType
   , ldrfilmPixelFormat      :: PixelFormat
   , ldrfilmGamma            :: GammaType
   , ldffilmExposure         :: Double
   , ldffilmBanner           :: Bool
   , ldffilmCrop             :: Maybe Crop
   , ldffilmHighQualityEdges :: Bool
   , ldffilmRFilter          :: ReconstructionFilter
   } deriving (Show, Eq, Read, Ord, Generic, Data, Typeable)
   
instance ToElement GammaFilm where
   toElement GammaFilm {..} 
      = maybe (tag "gammafilm") 
          (\x -> tag "gammafilm" .> ("crop", x)) ldffilmCrop
     .> ("width"                       , ldrfilmWidth            )
     .> ("height"                      , ldrfilmHeight           )   
     .> ("fileFormat"                  , ldrfilmFileFormat       )
     .> ("pixelFormat"                 , ldrfilmPixelFormat      )
     .> ("gamma"                       , ldrfilmGamma            )
     .> ("exposure"                    , ldffilmExposure         )   
     .> ("banner"                      , ldffilmBanner           )
     .> ("highQualityEdges"            , ldffilmHighQualityEdges )
     .> ("rFilter"                     , ldffilmRFilter          )
                                       
data ReinhardFilm = ReinhardFilm
   { reinhardFilmWidth            :: Integer
   , reinhardFilmHeight           :: Integer
   , reinhardFilmPixelFormat      :: FileFormatType
   , reinhardFilmGamma            :: GammaType
   , reinhardFilmExposure         :: Double
   , reinhardFilmKey              :: Double
   , reinhardFilmBurn             :: Double
   , reinhardFilmBanner           :: Bool
   , reinhardFilmCrop             :: Maybe Crop
   , reinhardFilmHighQualityEdges :: Bool
   , reinhardFilmRFilter          :: ReconstructionFilter
   } deriving (Show, Eq, Read, Ord, Generic, Data, Typeable)

instance ToElement ReinhardFilm where
   toElement ReinhardFilm {..} 
     = maybe (tag "reinhardfilm") 
               (\x -> tag "reinhardfilm" .> ("crop", x)) reinhardFilmCrop
     .> ("width"            , reinhardFilmWidth           )
     .> ("height"           , reinhardFilmHeight          )
     .> ("pixelFormat"      , reinhardFilmPixelFormat     )
     .> ("gamma"            , reinhardFilmGamma           )
     .> ("exposure"         , reinhardFilmExposure        )
     .> ("key"              , reinhardFilmKey             )
     .> ("burn"             , reinhardFilmBurn            )
     .> ("banner"           , reinhardFilmBanner          )
     .> ("highQualityEdges" , reinhardFilmHighQualityEdges)
     .> ("rfilter"          , reinhardFilmRFilter         )

data MFilm = MFilm 
   { mfilmWidth            :: Integer
   , mfilmHeight           :: Integer
   , mfilmCrop             :: Maybe Crop
   , mfilmFileFormat       :: FileFormatType
   , mfilmDigits           :: Integer
   , mfilmVariable         :: String
   , mfilmPixelFormat      :: String
   , mfilmHighQualityEdges :: Bool
   , mfilmRFilter          :: ReconstructionFilter
   } deriving (Show, Eq, Read, Ord, Generic, Data, Typeable)

instance ToElement MFilm where
   toElement MFilm {..} 
       = maybe (tag "mfilm") (\x -> tag "mfilm" .> ("crop", x)) mfilmCrop
      .> ("width"           , mfilmWidth )
      .> ("height"          , mfilmHeight)
      .> ("digits"          , mfilmDigits)
      .> ("variable"        , mfilmVariable)
      .> ("pixelFormat"     , mfilmPixelFormat)
      .> ("highQualityEdges", mfilmHighQualityEdges)
      .> ("rfilter"         , mfilmRFilter)


data Film 
   = FHdrfilm      HDRfilm
   | FTiledHDRFilm TiledHDRFilm
   | FLdrfilm      LDRfilm
   | FMFilm        MFilm
   deriving (Show, Eq, Read, Ord, Generic, Data, Typeable)
   
instance ToElement Film where
   toElement = forwardToElement   
   
data ReconstructionFilter
   = RFBox
   | RFTent
   | RFGaussian
   | RFMitchell
   | RFCatmullrom
   | RFLanczos
   deriving (Show, Eq, Read, Ord, Generic, Data, Typeable, Enum, Bounded)

instance ToElement ReconstructionFilter where
   toElement x = primitive "string" $ case x of
      RFBox        -> "box" :: Text
      RFTent       -> "tent"
      RFGaussian   -> "gaussian"
      RFMitchell   -> "mitchell"
      RFCatmullrom -> "catmullrom"
      RFLanczos    -> "lanczos"
      
data Include = Include 
   { includeFilename :: FilePath
   } deriving (Show, Eq, Read, Ord, Generic, Data, Typeable)

instance ToElement Include

data AnyAlias = forall a. AnyAlias (Alias a)

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
   = SNShape             Shape
   | SNBSDF              BSDF
   | SNTexture           Texture
   | SNSSS               SSS
   | SNPartcipatingMedia PartcipatingMedia
   | SNPhaseFunction     PhaseFunction
   | SNVolumeDataSource  VolumeDataSource
   | SNEmitter           Emitter
   | SNSensor            Sensor
   | SNIntegrator        Integrator
   | SNSampleGenerator   SampleGenerator
   | SNFilms             Film
   | SInclude            Include
   | SAlias              AnyAlias
   deriving (Show, Eq, Read, Ord, Generic, Typeable)

instance ToElement SceneNodeData where
   toElement = forwardToElement

data SceneNode = SceneNode 
   { nodeData :: SceneNodeData
   , nodeId   :: Maybe String
   } deriving(Eq, Show, Ord, Read, Typeable, Generic)

instance ToElement SceneNode where
   toElement SceneNode {..} = case nodeId of
      Just i  -> toElement nodeData # ("id", i)
      Nothing -> toElement nodeData

data Scene = Scene 
   { version :: (Int, Int, Int)
   , nodes   :: [SceneNode]
   } deriving(Eq, Show, Ord, Read, Typeable, Generic)

instance ToElement Scene where
   toElement (Scene (x, y, z) nodes) 
      = addChildList 
         (tag "scene" # ("version", intercalate "." $ map show [x, y, z]))
         nodes
            

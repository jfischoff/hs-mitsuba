{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
module Mitsuba.Types where
import Mitsuba.Primitive
import Mitsuba.Class
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

newtype Ref a = Ref { unRef :: String }
   deriving(Eq, Show, Ord, Read, Data, Typeable, Generic)

instance ToAttributeValue (Ref a) where
   toAttributeValue = toAttributeValue . unRef

instance ToElement (Ref a) where
   toElement Ref {..} = T $ Tag "ref" Nothing Nothing $ H.fromList 
      [("id", (P $ Primitive (Named "id" Show) $ String unRef, Attribute))]

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
   toElement = allAttribute . defaultGeneric

data Vector = Vector 
   { vectorX :: Double
   , vectorY :: Double
   , vectorZ :: Double
   } deriving(Eq, Show, Ord, Read, Data, Typeable, Generic)
   
instance Each Vector Vector Double Double where
   each f Vector {..} = Vector <$> f vectorX <*> f vectorY <*> f vectorZ
   
instance ToElement Vector where
   toElement = allAttribute . defaultGeneric

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
   toElement = allAttribute . defaultGeneric

data Rotate = Rotate
   { rotateX     :: Double
   , rotateY     :: Double
   , rotateZ     :: Double
   , rotateAngle :: Double
   } deriving(Eq, Show, Ord, Read, Data, Typeable, Generic)

instance ToElement Rotate where
   toElement = allAttribute . defaultGeneric

data Scale 
   = SUniformScale UniformScale
   | SScaleAxis    ScaleAxis
    deriving(Eq, Show, Ord, Read, Data, Typeable, Generic)

data UniformScale = UniformScale
   { uniformScaleScale :: Double
   } deriving(Eq, Show, Ord, Read, Data, Typeable, Generic)
   
instance ToElement UniformScale where
   toElement = allAttribute . defaultGeneric
   
data ScaleAxis = ScaleAxis
   { scaleAxisX :: Double
   , scaleAxisY :: Double
   , scaleAxisZ :: Double
   } deriving(Eq, Show, Ord, Read, Data, Typeable, Generic)
   
instance ToElement ScaleAxis where
   toElement = allAttribute . defaultGeneric

instance ToElement Scale where
   toElement = forwardToElement

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
      in T $ tag "matrix" # ("value", values)

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

      in T $ tag "lookat"
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

toNestedChildren :: ToElement a => [a] -> Children
toNestedChildren
   = H.fromList 
   . map (\x -> (view (toNamed . name) x, (x, Nested))) 
   . map toElement

instance ToElement RegularTransform where
   toElement (RegularTransform xs) = 
     T $ tag "transform" & tagChildren .~ toNestedChildren xs

newtype Animation = Animation {unAnimation :: [(Double, RegularTransform)] }
    deriving(Eq, Show, Ord, Read, Data, Typeable, Generic)

instance ToElement Animation where
   toElement (Animation xs) 
      = let toKeyframe (k, trans) = toElement trans # ("time", k) 
      in T $ tag "animation"
       & tagChildren .~ (toNestedChildren . map toKeyframe) xs

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
   } deriving(Eq, Show, Ord, Read, Data, Typeable, Generic)
   
instance ToElement (Alias a) where
   toElement Alias {..} = T $
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
   
data Color = CS Spectrum | CT Texture
   deriving(Eq, Show, Ord, Read, Data, Typeable, Generic)
   
instance ToElement Color where
   toElement = forwardToElement

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

instance ToElement ShapeType

data MediumPair = MediumPair 
   { mediumPairInterior :: Medium
   , mediumPairExterior :: Medium
   } deriving(Eq, Show, Ord, Read, Data, Typeable, Generic)
   
instance Each MediumPair where
   each f MediumPair 
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
     let shapeTypeElement = toElement shapeType 
     tag "shape" & tagType .~ type="{toShapeTypeName shapeType}">
            {toXMLElements shapeType}
            {fmap toXML shapeMaterial}
            {toXMLElements shapeMediumPair}
        </shape>
     |]
   
{-

instance ToXMLChild Color where
   toXMLChild name = \case
      CS x -> toXMLChild name x
      CT x -> toXMLChild name x

data Diffuse = Diffuse 
   { diffuseReflectance :: Color
   } deriving(Eq, Show, Ord, Read, Data, Typeable, Generic)
   
instance ToElement Diffuse
   
data RoughDiffuse = RoughDiffuse 
   { roughDiffuseReflectance   :: Color
   , roughDiffuseAlpha         :: Color
   , roughDiffuseUseFastApprox :: Bool
   } deriving(Eq, Show, Ord, Read, Data, Typeable, Generic)

instance ToElement Diffuse
   
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
      
instance ToXMLChild KnownMaterial where
   toXMLChild = asPrimitive "string"

data Refraction 
   = RKM KnownMaterial 
   | IOR Double 
   deriving(Eq, Show, Ord, Read, Data, Typeable, Generic)
   
instance ToXMLChild Refraction where
   toXMLChild name = \case 
      RKM x -> toXMLChild name x
      IOR x -> toXMLChild name x
   
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
      
instance ToXMLChild Distribution where
   toXMLChild = asPrimitive "string"       

data RoughDielectricRegular = RoughDielectricRegular
   { roughDielectricRegularDistribution          :: Distribution
   , roughDielectricRegularAlpha                 :: Either Double Texture
   , roughDielectricRegularIntIOR                :: Refraction
   , roughDielectricRegularExtIOR                :: Refraction
   , roughDielectricRegularSpecularReflectance   :: Color
   , roughDielectricRegularSpecularTransmittance :: Color
   } deriving(Eq, Show, Ord, Read, Data, Typeable, Generic, Enum, Bounded)
   
instance ToElement RoughDielectricRegular
   
data RoughDielectricAnistrophic = RoughDielectricAnistrophic
   { roughDielectricAnistrophicDistribution          :: Distribution
   , roughDielectricAnistrophicAlphaU                :: Either Double Texture
   , roughDielectricAnistrophicAlphaV                :: Either Double Texture
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
   
deriveForwardInstance ''ToXMLElements ''RoughDielectric 

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
   | PolycrystThoriumFluoride
   | PolycrystallineTitaniumCarbide
   | TitaniumNitride
   | TetragonalTitanDioxide
   | Vanadium
   | VanadiumNitride
   | Tungsten
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
      Mirror                         -> "none"
   
instance ToXMLChild ConductorType where
   toXMLChild = asPrimitive "string"
   
data SmoothConductor = SmoothConductor 
   { smoothConductorMaterial            :: ConductorType
   , smoothConductorEta                 :: Spectrum
   , smoothConductorK                   :: Spectrum
   , smoothConductorExtEta              :: Refraction
   , smoothConductorSpecularReflectance :: Color
   } deriving(Eq, Show, Ord, Read, Data, Typeable, Generic)
   
instance ToElement SmoothConductor

data RoughConductorRegular = RoughConductorRegular 
   { roughConductorRegularDistribution :: Distribution
   , roughConductorRegularAlpha        :: Either Double Texture
   , roughConductorRegularMaterial     :: ConductorType
   , roughConductorRegularEta          :: Spectrum
   , roughConductorRegularK            :: Spectrum
   , roughConductorRegularExtEta       :: Either Double KnownMaterial
   , roughConductorSpecularReflectance :: Color
   } deriving(Eq, Show, Ord, Read, Data, Typeable, Generic)
   
instance ToElement RoughConductorRegular
   
data RoughConductorAnisotropic = RoughConductorAnisotropic
   { roughConductorAnisotropicAlphaU              :: Either Double Texture
   , roughConductorAnisotropicAlphaV              :: Either Double Texture
   , roughConductorAnisotropicMaterial            :: ConductorType
   , roughConductorAnisotropicEta                 :: Spectrum
   , roughConductorAnisotropicRegularK            :: Spectrum
   , roughConductorAnisotropicRegularExtEta       :: Either Double KnownMaterial
   , roughConductorAnisotropicSpecularReflectance :: Color
   } deriving(Eq, Show, Ord, Read, Data, Typeable, Generic)
   
instance ToElement RoughConductorAnisotropic

data RoughConductor 
   = RCRegular     RoughConductorRegular
   | RCAnisotropic RoughConductorAnisotropic
   deriving(Eq, Show, Ord, Read, Data, Typeable, Generic)
   
deriveForwardInstance ''ToXMLElements ''RoughConductor

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
   , roughPlasticAlpha               :: Either Double Texture
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
   
instance ToElement RoughDielectricCoating   
   
-- TODO make a Coating a = Coating { coatingData :: a, child :: BSDF }   
data RoughDielectricCoating = RoughDielectricCoating
   { roughDielectricCoatingDistribution        :: Distribution
   , roughDielectricCoatingAlpha               :: Either Double Texture
   , roughDielectricCoatingIntIOR              :: Either Double String
   , roughDielectricCoatingExtIOR              :: Either Double String
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
   { modifiedPhongExponent            :: Either Float Texture
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

instance ToXMLChild WardType where
   toXMLChild = asPrimitive "string"
   
data Ward = Ward 
   { wardVariant             :: WardType
   , wardAlphaU              :: Either Double Texture
   , wardAlphaV              :: Either Double Texture
   , wardSpecularReflectance :: Color
   , wardDiffuseReflectance  :: Color
   } deriving(Eq, Show, Ord, Read, Data, Typeable, Generic)
   
instance ToElement Ward
   
data MixtureBSDF = MixtureBSDF 
   { mixtureBSDFChildren :: [(Double, BSDF)]
   } deriving(Eq, Show, Ord, Read, Data, Typeable, Generic)
   
-- TODO make this instance

data BlendBSDF = BlendBSDF
   { blendBSDFWeight :: Either Double Texture
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

instance ToElement Irawan

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

instance ToTypeName BSDF where
   toTypeName = forwardLowerTypeName
   
instance ToXMLElements BSDF where
   toXMLElements = forwardToXMLElements

instance ToXML BSDF where
   toXML = defaultToXML


   

   
  
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
   
data SigmaAS = SigmaAS 
   { sigmaASA :: Spectrum
   , sigmaASS :: Spectrum
   }
   deriving (Show, Eq, Read, Ord, Generic, Data, Typeable)
   
data SigmaTAlbedo = SigmaTAlbedo 
   { sigmaTAlbedoT      :: Spectrum
   , sigmaTAlbedoAlbedo :: Spectrum
   } deriving (Show, Eq, Read, Ord, Generic, Data, Typeable)
   
data MaterialStyle 
   = MSMaterial      SSSMaterial
   | MSSigmaAS       SigmaAS
   | MSSigmaTAlbedo  SigmaTAlbedo
   deriving (Show, Eq, Read, Ord, Generic, Data, Typeable)
   
data Dipole = Dipole 
   { dipoleMaterialStyle :: MaterialStyle
   , dipoleScale         :: Double
   , dipoleIntIOR        :: Refraction
   , dipoleExtIOR        :: Refraction
   , dipoleIRRSamples    :: Integer
   } deriving (Show, Eq, Read, Ord, Generic, Data, Typeable)
   
data SSS 
   = SSSDipole Dipole
   deriving (Show, Eq, Read, Ord, Generic, Data, Typeable)
   
data Homogeneous = Homogeneous 
   { homogeneousMaterialStyle :: MaterialStyle
   , homogeneousScale         :: Double
   , homogeneousPhase         :: PhaseFunction
   } deriving (Show, Eq, Read, Ord, Generic, Data, Typeable)
   
data HeterogeneousSampling 
   = Simpson
   | Woodcock
   deriving (Show, Eq, Read, Ord, Generic, Data, Typeable, Enum, Bounded)
   
data Heterogeneous = Heterogeneous
   { heterogeneousMethod      :: HeterogeneousSampling
   , heterogeneousDensity     :: VolumeDataSource
   , heterogeneousAlbedo      :: VolumeDataSource 
   , heterogeneousOrientation :: VolumeDataSource
   , heterogeneousScale       :: Double
   , heterogenousPhase        :: Child PhaseFunction
   } deriving (Show, Eq, Read, Ord, Generic, Data, Typeable)
   
data PartcipatingMedia 
   = PMHomogeneous   Homogeneous 
   | PMHeterogeneous Heterogeneous
   deriving (Show, Eq, Read, Ord, Generic, Data, Typeable)

data HG = HG 
   { hgG :: Double
   } deriving (Show, Eq, Read, Ord, Generic, Data, Typeable)
   
data MicroFlake = MicroFlake
   { microFlakeStddev :: Double
   } deriving (Show, Eq, Read, Ord, Generic, Data, Typeable)

-- Combine with other mixture phases
data MixturePhase = MixturePhase
   { mixturePhaseChildren :: [(Double, Child PhaseFunction)]
   } deriving (Show, Eq, Read, Ord, Generic, Data, Typeable)

data PhaseFunction 
   = PFIsotropic 
   | PFHG        HG
   | PFRayleigh  
   | PFKKay
   | PFMicroflake   MicroFlake
   | PFMixturePhase MixturePhase
   deriving (Show, Eq, Read, Ord, Generic, Data, Typeable)
   
data ConstVolume 
   = CVDouble   Double
   | CVSpectrum Spectrum
   | CVVector   Vector
   deriving (Show, Eq, Read, Ord, Generic, Data, Typeable)

data SendDataType = SendAcrossNetwork | AssumeAvailable
   deriving (Show, Eq, Read, Ord, Generic, Data, Typeable, Enum, Bounded)

data GridVolume = GridVolume 
   { gridVolumeFilename :: FilePath
   , gridVolumeSendData :: SendDataType
   , gridVolumeToWorld  :: Transform
   , gridVolumeMin      :: Point
   , gridVolumeMax      :: Point
   } deriving (Show, Eq, Read, Ord, Generic, Data, Typeable)

data VolCache = VolCache
   { volCacheBlockSize   :: Integer
   , volCacheVoxelWidth  :: Double
   , volCacheMemoryLimit :: Integer
   , volCacheToWorld     :: RegularTransform
   , volCacheChild       :: Child VolumeDataSource
   } deriving (Show, Eq, Read, Ord, Generic, Data, Typeable)

data VolumeDataSource
   = VDSConstVolume ConstVolume
   | VDSGridVolume  GridVolume
   | VDSVolCache    VolCache
   deriving (Show, Eq, Read, Ord, Generic, Data, Typeable)

data PointLight = PointLight
   { pointLightToWorld        :: Transform
   , pointLightPosition       :: Point
   , pointLightIntensity      :: Spectrum
   , pointLightSamplingWeight :: Double
   } deriving (Show, Eq, Read, Ord, Generic, Data, Typeable)
   
data AreaLight = AreaLight 
   { areaLightRadiance       :: Spectrum
   , areaLightSamplingWeight :: Double
   } deriving (Show, Eq, Read, Ord, Generic, Data, Typeable)

data SpotLight = SpotLight
   { spotLightToWorld        :: Transform
   , spotLightIntensity      :: Spectrum
   , spotLightCutoffAngle    :: Double
   , spotLightBeamWidth      :: Double
   , spotLightTexture        :: Texture
   , spotLightSamplingWeight :: Double
   } deriving (Show, Eq, Read, Ord, Generic, Data, Typeable)

data DirectionalLight = DirectionalLight
   { directionalLightToWorld        :: Transform
   , directionalLightVector         :: Vector
   , directionalLightIrradiance     :: Spectrum
   , directionalLightSamplingWeight :: Double
   } deriving (Show, Eq, Read, Ord, Generic, Data, Typeable)
   
data Collimated = Collimated 
   { collimatedToWorld        :: Transform
   , collimatedPower          :: Spectrum
   , collimatedSamplingWeight :: Double
   } deriving (Show, Eq, Read, Ord, Generic, Data, Typeable)

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
   
data Envmap = Envmap
   { envmapFilename       :: FilePath
   , envmapScale          :: Double
   , envmapToWorld        :: Transform
   , envmapGamma          :: Double
   , envmapCache          :: Bool
   , envmapSamplingWeight :: Double
   } deriving (Show, Eq, Read, Ord, Generic, Data, Typeable)
   
data Constant = Constant
   { constantRadiance       :: Spectrum
   , constantSamplingWeight :: Double
   } deriving (Show, Eq, Read, Ord, Generic, Data, Typeable)

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

instance ToTypeName Emitter where
   toTypeName = forwardLowerTypeName

instance ToXMLElements Emitter where
   toXMLElements = forwardToXMLElements
   
instance ToXMLChild Emitter where
   toXMLChild = toXMLChildDefault
   
instance ToXML Emitter where
   toXML = toXMLDefault

data FOVType
   = FOVTX
   | FOVTY
   | FOVTDiagonal
   | FOVTSmaller
   | FOVTLarger
   deriving (Show, Eq, Read, Ord, Generic, Data, Typeable)

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

data Orthographic = Orthographic
   { orthographicToWorld      :: Transform
   , orthographicShutterOpen  :: Double
   , orthographicShutterClose :: Double
   , orthographicNearClip     :: Double
   , orthographicFarClip      :: Double   
   } deriving (Show, Eq, Read, Ord, Generic, Data, Typeable)

data Telecentric = Telecentric
   { telecentricToWorld         :: Transform
   , telecentricAperatureRadius :: Double
   , telecentricFocusDistance   :: Double
   , telecentricShutterOpen     :: Double
   , telecentricShutterClose    :: Double
   , telecentricNearClip        :: Double
   , telecentricFarClip         :: Double
   } deriving (Show, Eq, Read, Ord, Generic, Data, Typeable)

data Spherical = Spherical 
   { sphericalToWorld      :: Transform
   , sphericalShutterOpen  :: Double
   , sphericalShutterClose :: Double
   } deriving (Show, Eq, Read, Ord, Generic, Data, Typeable)
   
data IrradianceMeter = IrradianceMeter
   { irradianceMeterShutterOpen  :: Double
   , irradianceMeterShutterClose :: Double
   } deriving (Show, Eq, Read, Ord, Generic, Data, Typeable)
   
data RadianceMeter = RadianceMeter
   { radianceToWorld      :: Transform
   , radianceShutterOpen  :: Double
   , radianceShutterClose :: Double
   } deriving (Show, Eq, Read, Ord, Generic, Data, Typeable)

data FluenceMeter = FluenceMeter
   { fluenceMeterToWorld      :: Transform
   , fluenceMeterShutterOpen  :: Double
   , fluenceMeterShutterClose :: Double
   } deriving (Show, Eq, Read, Ord, Generic, Data, Typeable)

data PerspectiveRDist = PerspectiveRDist
   { perspectiveRDistToWorld      :: Transform
   , perspectiveRDistKC           :: (Double, Double)
   , perspectiveRDistFocalLength  :: Double
   , perspectiveRDistFOV          :: Double
   , perspectiveRDistFOVAxis      :: FOVType
   , perspectiveRDistShutterOpen  :: Double
   , perspectiveRDistShutterClose :: Double
   , perspectiveRDistNearClip     :: Double
   , perspectiveRDistFarClip      :: Double
   } deriving (Show, Eq, Read, Ord, Generic, Data, Typeable)

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
   
data AmbientOcclusion = AmbientOcclusion 
   { ambientOcclusionShdingSamples :: Integer
   , ambientOcclusionRayLength     :: Double
   } deriving (Show, Eq, Read, Ord, Generic, Data, Typeable)
   
data Visibility = Hide | Show
   deriving (Show, Eq, Read, Ord, Generic, Data, Typeable, Enum, Bounded)
data NormalStrictness = Strict | Loose
   deriving (Show, Eq, Read, Ord, Generic, Data, Typeable, Enum, Bounded)
   
data Direct = Direct
   { directShadingSamples :: Integer
   , directEmitterSamples :: Integer
   , directBsdfSamples    :: Integer
   , directStrictNormals  :: NormalStrictness
   , directHideEmitters   :: Visibility
   } deriving (Show, Eq, Read, Ord, Generic, Data, Typeable)
   
data Path = Path
   { pathMaxDepth      :: Integer
   , pathRRDepth       :: Integer
   , pathStrictNormals :: NormalStrictness
   , pathHideEmitters  :: Visibility
   } deriving (Show, Eq, Read, Ord, Generic, Data, Typeable)
   
data VolPathSimple = VolPathSimple
   { volPathSimpleMaxDepth      :: Integer
   , volPathSimpleRRDepth       :: Integer
   , volPathSimpleStrictNormals :: NormalStrictness
   , volPathSimpleHideEmitters  :: Visibility
   } deriving (Show, Eq, Read, Ord, Generic, Data, Typeable)
   
data VolPath = VolPath
   { volPathMaxDepth      :: Integer
   , volPathRRDepth       :: Integer
   , volPathStrictNormals :: NormalStrictness
   , volPathHideEmitters  :: Visibility
   } deriving (Show, Eq, Read, Ord, Generic, Data, Typeable)
   
data PathConnectedness = ConnectToCamera | DontConnect
   deriving (Show, Eq, Read, Ord, Generic, Data, Typeable, Enum, Bounded)
   
data DirectSampling = DirectSampling | NoDirectSampling   
   deriving (Show, Eq, Read, Ord, Generic, Data, Typeable, Enum, Bounded)
   
data BDPT = BDPT
   { bdptMaxDepth     :: Integer
   , bdptLightImage   :: PathConnectedness
   , bdptSampleDirect :: DirectSampling
   , bdptRRDepth      :: Integer
   } deriving (Show, Eq, Read, Ord, Generic, Data, Typeable)
   
data Photonmapper = Photonmapper
   { photonmapperDirectSamples       :: Integer
   , photonmapperGlossySamples       :: Integer
   , photonmapperMaxDepth            :: Integer
   , photonmapperGlobalPhotons       :: Integer
   , photonmapperCausticPhotons      :: Integer
   , photonmapperVolumenPhotons      :: Integer
   , photonmapperGlobalLookupRadius  :: Double
   , photonmapperCausticLookupRadius :: Double
   , photonmapperLookupSize          :: Integer
   , photonmapperGranularity         :: Integer
   , photonmapperHideEmitters        :: Bool
   , photonmapperRRDepth             :: Integer
   } deriving (Show, Eq, Read, Ord, Generic, Data, Typeable)
   
data PPM = PPM 
   { ppmMaxDepth      :: Integer
   , ppmPhotonCount   :: Integer
   , ppmInitialRadius :: Double
   , ppmAlpha         :: Double
   , ppmGranularity   :: Integer
   , ppmRRDepth       :: Integer
   , ppmMaxPasses     :: Integer
   } deriving (Show, Eq, Read, Ord, Generic, Data, Typeable)
   
data SPPM = SPPM
   { sppmMaxDepth      :: Integer
   , sppmphotonCount   :: Integer
   , sppmInitialRadius :: Double
   , sppmAlpha         :: Double
   , sppmGranularity   :: Integer
   , sppmRRDepth       :: Integer
   , sppmMaxPasses     :: Integer
   } deriving (Show, Eq, Read, Ord, Generic, Data, Typeable)
   
data PSSMLT = PSSMLT
   { pssmltBidirectional    :: Bool
   , pssmltMaxDepth         :: Integer
   , pssmltDirectSamples    :: Integer
   , pssmltRRDepth          :: Integer
   , pssmltLuminanceSamples :: Integer
   , pssmltTwoStage         :: Bool
   , pssmltPLarge           :: Double
   } deriving (Show, Eq, Read, Ord, Generic, Data, Typeable)
   
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
   
data PTracer = PTracer
   { ptracerMaxDepth    :: Integer
   , ptracerRRDepth     :: Integer
   , ptracerGranularity :: Integer
   , ptracerBruteForce  :: Bool
   } deriving (Show, Eq, Read, Ord, Generic, Data, Typeable)
   
data Adaptive = Adaptive
   { adaptiveMaxError        :: Double
   , adaptivePValue          :: Double
   , adaptiveMaxSampleFactor :: Integer
   } deriving (Show, Eq, Read, Ord, Generic, Data, Typeable)
   
data VP1 = VP1 
   { vp1MaxDepth            :: Integer
   , vp1ShadowMapResolution :: Integer
   , vp1Clamping            :: Double
   } deriving (Show, Eq, Read, Ord, Generic, Data, Typeable)
   
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
   
data Integrator 
   = IAmbientOcclusion AmbientOcclusion 
   | IDirect           Direct
   | IPath             Path
   | IVolPathSimple    VolPathSimple
   | IVolPath          VolPath
   | IBDPT             BDPT
   | IPhotonmapper     Photonmapper
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
   
data Independent = Independent
   { independentSampleCount :: Integer
   } deriving (Show, Eq, Read, Ord, Generic, Data, Typeable)
   
data Stratified = Stratified
   { stratifiedSampleCount :: Integer
   , stratifiedDimension   :: Integer
   } deriving (Show, Eq, Read, Ord, Generic, Data, Typeable)
   
data LDSampler = LDSampler
   { ldSamplerSampleCount :: Integer
   , ldSamplerDimension   :: Integer
   } deriving (Show, Eq, Read, Ord, Generic, Data, Typeable)
   
data Halton = Halton
   { haltonSampleCount :: Integer
   , haltonScramble    :: Integer
   } deriving (Show, Eq, Read, Ord, Generic, Data, Typeable)
   
data Hammersley = Hammersley
   { hammersleySampleCount :: Integer
   , hammersleyScramble    :: Integer
   } deriving (Show, Eq, Read, Ord, Generic, Data, Typeable)
   
data Sobol = Sobol 
   { sobolSampleCount :: Integer
   , sobolScramble    :: Integer
   } deriving (Show, Eq, Read, Ord, Generic, Data, Typeable)

data SampleGenerator
   = SIndependent Independent
   | SStratified  Stratified
   | SLDSampler   LDSampler 
   | SHalton      Halton
   | SHammersley  Hammersley
   | SSobol       Sobol
   deriving (Show, Eq, Read, Ord, Generic, Data, Typeable)

data FileFormatType
   = Openexr
   | RGBE
   | PFM
   deriving (Show, Eq, Read, Ord, Generic, Data, Typeable, Enum, Bounded)
   
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
   
data ComponentFormat 
   = Float16
   | Float32
   | UInt32
   deriving (Show, Eq, Read, Ord, Generic, Data, Typeable, Bounded, Enum)

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
   
data Crop = Crop
   { cropCropOffsetX :: Integer
   , cropCropOffsetY :: Integer
   , cropCropWidth   :: Integer
   , cropCropHeight  :: Integer
   } deriving (Show, Eq, Read, Ord, Generic, Data, Typeable)
   
data TiledHDRFilm = TiledHDRFilm
   { tiledHDRFilmWidth           :: Integer
   , tiledHDRFilmHeight          :: Integer
   , tiledHDRFilmCrop            :: Maybe Crop
   , tiledHDRFilmPixelFormat     :: PixelFormat
   , tiledHDRFilmComponentFormat :: ComponentFormat
   , tiledHDRFilmRFilter         :: ReconstructionFilter
   } deriving (Show, Eq, Read, Ord, Generic, Data, Typeable)
   
data GammaType
   = GTGammaCurve Double
   | GTSRGB
   deriving (Show, Eq, Read, Ord, Generic, Data, Typeable, Enum, Bounded)

data LDRfilm
   = LDRFGammaFilm GammaFilm 
   | LDRFReinhard  ReinhardFilm
   deriving (Show, Eq, Read, Ord, Generic, Data, Typeable)
   
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

data MFilm = MFilm 
   { mfilmWidth            :: Integer
   , mfilmhHeight          :: Integer
   , mfilmCrop             :: Maybe Crop
   , mfilmFileFormat       :: FileFormatType
   , mfilmDigits           :: Integer
   , mfilmVariable         :: String
   , mfilmPixelFormat      :: String
   , mfilmHighQualityEdges :: Bool
   , mfilmRFilter          :: ReconstructionFilter
   } deriving (Show, Eq, Read, Ord, Generic, Data, Typeable)

data Film 
   = FHdrfilm      HDRfilm
   | FTiledHDRFilm TiledHDRFilm
   | FLdrfilm      LDRfilm
   | FMFilm        MFilm
   deriving (Show, Eq, Read, Ord, Generic, Data, Typeable)
   
data ReconstructionFilter
   = RFBox
   | RFTent
   | RFGaussian
   | RFMitchell
   | RFCatmullrom
   | RFLanczos
   deriving (Show, Eq, Read, Ord, Generic, Data, Typeable, Enum, Bounded)

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
   | SAlias              Alias
   deriving (Show, Eq, Read, Ord, Generic, Data, Typeable)
   
instance ToXML SceneNodeData where
   toXML = \case
      SNShape             x -> toXML x
      SNBSDF              x -> toXML x
      SNTexture           x -> toXML x
      SNSSS               x -> toXML x
      SNPartcipatingMedia x -> toXML x
      SNPhaseFunction     x -> toXML x
      SNVolumeDataSource  x -> toXML x
      SNEmitter           x -> toXML x
      SNSensor            x -> toXML x
      SNIntegrator        x -> toXML x
      SNSampleGenerator   x -> toXML x
      SNFilms             x -> toXML x
      SInclude            x -> [xmlQQ| <include filename="{x}"/>|]
      SAlias              x -> toXML x


data SceneNode = SceneNode 
   { nodeData :: SceneNodeData
   , nodeId   :: Maybe String
   } deriving(Eq, Show, Ord, Read, Data, Typeable, Generic)

instance ToElement SceneNode where
   toElement SceneNode {..} = case nodeId of
      Just i  -> toElement nodeData # ("id", i)
      Nothing -> toElement nodeData

data Scene = Scene 
   { version :: (Int, Int, Int)
   , nodes   :: [SceneNode]
   } deriving(Eq, Show, Ord, Read, Data, Typeable, Generic)

instance ToXML Scene where
   toXML (Scene (x, y, z) nodes) 
      = t "scene" ! a "version" (intercalate "." $ map show [x, y, z])
            $ toXML nodes
            
-}
{-# LANGUAGE TemplateHaskell #-}
module Main where
import Mitsuba.Types
import Mitsuba.Element.Class
import Test.QuickCheck
import Test.QuickCheck.Monadic
import Data.Default.Generics
import System.Process
import System.IO.Temp
import System.Exit
import Text.Blaze.Renderer.Utf8
import Data.Monoid
import qualified Data.ByteString.Lazy as BSL
import Control.Applicative
import Control.Monad
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as M

--main = testScene' "testScene.xml" defaultVerifiedScene

main = quickCheck quickTestScene

--testScene :: Scene -> IO ExitCode
--testScene ident scene = withSystemTempDirectory "hs-mitsuba" $ \tempDirPath -> do
--    let path = tempDirPath ++ "/tempFile.xml"
--    BSL.writeFile path $ renderMarkup $ toXML scene
--    
--    system $ "mitsuba " ++ path
    
  
  
    
    
testScene' :: FilePath -> Scene -> IO ExitCode
testScene' filePath scene = do
      BSL.writeFile filePath $ renderMarkup $ toXML scene
      system $ "mitsuba " ++ filePath
      
quickTestScene :: Property
quickTestScene = monadicIO $ forAllM (genSceneWithShape testGenShape) $ \scene -> 
  forAllM (listOf1 $ elements ['a' .. 'z']) $ \filename -> do 
    ec <- run $ testScene' (filename ++ ".xml") scene
    unless (ec == ExitSuccess) $ do
      run $ print $ "failed at file: " ++ filename
      assert False
      
    

-- I need make all of the scenes of the xml test data
-- make a list of scenes of the swapping out the parts that are necessary
-- make special cases for things like include if it is easier.
-- I will need test versions of all the formats.

defaultVerifiedShapeNode :: SceneNodeData
defaultVerifiedShapeNode 
  = SNDShape 
  $ SShapeLeaf 
  $ ShapeLeaf
       { shapeLeafLeaf = Left 
                       $ SimpleShapeLeaf 
                            { simpleShapeLeafType 
                                = STCube
                                $ def
                            , simpleShapeLeafMaterial 
                                = CNested defaultBSDF'
                            }
       , shapeLeafToWorld    = def
       , shapeLeafMediumPair = Nothing
       , shapeLeafEmitter    = Nothing
       }

  
defaultBSDF :: SceneNodeData 
defaultBSDF = SNDBSDF defaultBSDF'

defaultBSDF' :: BSDF
defaultBSDF' 
  = BSDFDiffuse
  $ Diffuse
  $ CTexture defaultVerifiedTexture'
     

defaultVerifiedTexture :: SceneNodeData
defaultVerifiedTexture = SNDTexture defaultVerifiedTexture'
  
defaultVerifiedTexture'
  = TCheckerboard
  $ Checkerboard
      { checkerboardColor0  = SUniform 1
      , checkerboardColor1  = SUniform 0
      , checkerboardUoffset = 0
      , checkerboardVoffset = 0
      , checkerboardUscale  = 1
      , checkerboardVscale  = 1
      }      
  
defaultVerifiedSSS :: SceneNodeData
defaultVerifiedSSS 
  = SNDSSS
  $ SDipole
  $ Dipole 
      { dipoleMaterialStyle 
          = MSMaterial Cream
      , dipoleScale         = 1
      , dipoleIntIOR        = IOR $ RefractiveValue 1.0
      , dipoleExtIOR        = IOR $ RefractiveValue 1.0
      , dipoleIrrSamples    = 1
      }
      
defaultMedium :: SceneNodeData
defaultMedium 
  = SNDMedium
  $ MHomogeneous
  $ Homogeneous 
      { homogeneousMaterialStyle 
          = MSMaterial Cream
      , homogeneousScale = 1.0
      , homogeneousPhase = PIsotropic
      }

defaultPhase :: SceneNodeData 
defaultPhase 
  = SNDPhase PIsotropic

defaultVolume :: SceneNodeData
defaultVolume 
  = SNDVolume 
  $ VConstvolume
  $ CVDouble 1.0 
  
defaultEmitter :: SceneNodeData 
defaultEmitter 
  = SNDEmitter
  $ EPoint
      PointLight 
        { pointLightLocation       = Right $ Point (-10) (-10) (-10)
        , pointLightIntensity      = SBlackbody $ Blackbody 10000 1
        , pointLightSamplingWeight = def
        }

defaultSensor :: SceneNodeData
defaultSensor 
  = SNDSensor
  $ Sensor 
      { sensorFilm    = Just 
                      $ defaultFilm'
                       
      , sensorSampler = Just
                      $ defaultSampler'
                      
      , sensorType  
          = STPerspective
          $ Perspective
              { perspectiveToWorld      = TRegular
                                        $ translate 0 0 (-10) 
              , perspectiveView         = LVFocalLength 35
              , perspectiveFovAxis      = FOVTX
              , perspectiveShutterOpen  = 0
              , perspectiveShutterClose = 1
              , perspectiveNearClip     = 0.1
              , perspectiveFarClip      = 500
              }
 
      }

defaultIntegrator :: SceneNodeData 
defaultIntegrator 
  = SNDIntegrator
  $ IPath
  $ Path
     { pathMaxDepth      = 1
     , pathRrDepth       = 2
     , pathStrictNormals = Strict
     , pathHideEmitters  = Visible
     }

defaultSampler :: SceneNodeData
defaultSampler = SNDSampler defaultSampler'
  
defaultSampler' :: Sampler
defaultSampler' = SLdsampler $ LDSampler (Whole 1) (Whole 1)
      
defaultFilm' :: Film
defaultFilm' = 
    FLdrfilm
  $ LDRFGammaFilm
  $ GammaFilm
     { ldrfilmWidth            = 100
     , ldrfilmHeight           = 100
     , ldrfilmFileFormat       = JPEG
     , ldrfilmPixelFormat      = PFRGB
     , ldrfilmGamma            = GTGammaCurve 1.0
     , ldffilmExposure         = 1.0
     , ldffilmBanner           = False
     , ldffilmCrop             = Nothing
     , ldffilmHighQualityEdges = True
     , ldffilmRFilter          = RFGaussian
     }

toDefSceneNode x = SceneNode x Nothing

defaultVerifiedScene :: Scene
defaultVerifiedScene = Scene $ map toDefSceneNode
  [ defaultVerifiedShapeNode 
  , defaultSensor
  , defaultEmitter
  , defaultIntegrator
  ]
  
genSceneWithShape :: Gen Shape -> Gen Scene
genSceneWithShape gen = do
  shape <- gen
  return $ Scene $ map toDefSceneNode
    [ SNDShape shape 
    , defaultSensor
    , defaultEmitter
    , defaultIntegrator
    ]

instance Arbitrary RefractiveValue where
  arbitrary = RefractiveValue <$> choose (1.000100, 4.0)    
    
instance Arbitrary WavelengthStyle where
  arbitrary = WavelengthStyle <$> arbitrary
    
instance Arbitrary InternalSpectralFormat where
  arbitrary 
     =  InternalSpectralFormat
    <$> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary

instance Arbitrary RGBTriple where
  arbitrary = RGBTriple <$> arbitrary <*> arbitrary <*> arbitrary     
    
instance Arbitrary Hex where
  arbitrary = Hex <$> arbitrary <*> arbitrary <*> arbitrary    
    
instance Arbitrary RGBLike where
  arbitrary 
    = oneof 
        [ RGBLTriple <$> arbitrary
        , RGBLHex    <$> arbitrary
        ]
    
instance Arbitrary Temperature where
  arbitrary = Temperature <$> arbitrary    
    
instance Arbitrary Blackbody where
  arbitrary = Blackbody <$> arbitrary <*> arbitrary    
    
instance Arbitrary Spectrum where
  arbitrary = oneof 
    [ SWavelengths <$> arbitrary
    , SUniform     <$> arbitrary
--    , SInternal    <$> arbitrary
    , SRGB         <$> arbitrary
    , SSRGB        <$> arbitrary
--    , SFile        <$> arbitrary
    , SBlackbody   <$> arbitrary
    ] 
    
instance Arbitrary Color where
  arbitrary = CSpectrum <$> arbitrary
    
instance Arbitrary Diffuse where
  arbitrary = Diffuse <$> arbitrary
  
instance Arbitrary RoughDiffuse where
  arbitrary 
     =  RoughDiffuse 
    <$> arbitrary
    <*> arbitrary
    <*> arbitrary
    
instance Arbitrary KnownMaterial where
  arbitrary = elements [Vacuum .. Diamond]
    
instance Arbitrary Refraction where
  arbitrary 
    = oneof
        [ RKM <$> arbitrary
        , IOR <$> arbitrary
        ]
    
instance Arbitrary Dielectric where
  arbitrary 
     =  Dielectric
    <$> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary

instance Arbitrary ThinDielectric where
  arbitrary 
     =  ThinDielectric 
    <$> arbitrary 
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    
instance Arbitrary Distribution where
  arbitrary 
      = elements  
          [ Beckmann
          , GGX
          , PhongDistribution
          ]
        
instance Arbitrary Luminance where
  arbitrary 
      = oneof 
          [ UniformLuminance <$> arbitrary
--          , TextureLuminance <$> arbitrary
          ]
    
instance Arbitrary AnistrophicAlpha where
  arbitrary
     =  AnistrophicAlpha 
    <$> arbitrary 
    <*> arbitrary
    
instance Arbitrary UniformAlpha where
  arbitrary 
     =  UniformAlpha 
    <$> arbitrary
    <*> arbitrary    
    
instance Arbitrary AlphaDistribution where
  arbitrary 
    = oneof
        [ ADAnistrophicAlpha <$> arbitrary
        , ADUniformAlpha     <$> arbitrary
        ]
    
instance Arbitrary RoughDielectric where
  arbitrary 
     =  RoughDielectric 
    <$> arbitrary 
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary

instance Arbitrary NonTransmission where 
  arbitrary = oneof
    [ NTDiffuse         <$> arbitrary
    , NTRoughDiffuse    <$> arbitrary
    , NTDielectric      <$> arbitrary
    , NTThindielectric  <$> arbitrary
    , NTRoughdielectric <$> arbitrary
    , NTConductor       <$> arbitrary
    , NTRoughconductor  <$> arbitrary
    , NTPlastic         <$> arbitrary
    , NTRoughplastic    <$> arbitrary
    , NTCoating         <$> arbitrary
    , NTRoughcoating    <$> arbitrary
--    , NTBumpmap         <$> arbitrary
    , NTPhong           <$> arbitrary
    , NTWard            <$> arbitrary
    ]



instance Arbitrary ConductorType where
  arbitrary = elements [ AmorphousCarbon .. None ]
  
instance Arbitrary ManualConductance where
  arbitrary = ManualConductance <$> arbitrary <*> arbitrary
  
instance Arbitrary Conductance where
  arbitrary 
    = oneof 
        [ CConductorType <$> arbitrary
        , CManualConductance <$> arbitrary
        ]
instance Arbitrary Conductor where
  arbitrary 
       =  Conductor 
      <$> arbitrary
      <*> arbitrary 
      <*> arbitrary
        
instance Arbitrary RoughConductor where
  arbitrary 
     =  RoughConductor 
    <$> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    
instance Arbitrary Plastic where
  arbitrary
     =  Plastic 
    <$> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    
instance Arbitrary RoughPlastic where
  arbitrary 
     =  RoughPlastic 
    <$> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    
instance Arbitrary Coating where
  arbitrary 
     =  Coating
    <$> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary

instance Arbitrary a => Arbitrary (Child a) where
  arbitrary = CNested <$> arbitrary

instance Arbitrary RoughCoating where
  arbitrary 
     =  RoughCoating 
    <$> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    
--instance Arbitrary BumpMap where
--  arbitrary = 
  
instance Arbitrary Phong where
  arbitrary 
     =  Phong 
    <$> arbitrary 
    <*> arbitrary
    <*> arbitrary
    
instance Arbitrary WardType where
  arbitrary 
    = elements [WTWard, WTWardDuer, WTBalanced]
    
instance Arbitrary Ward where
  arbitrary 
     =  Ward
    <$> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
  
instance Arbitrary a => Arbitrary (NonEmpty a) where
  arbitrary = NonEmpty.fromList <$> listOf1 arbitrary  
    
instance Arbitrary MixtureBSDF where
  arbitrary = MixtureBSDF <$> arbitrary
  
instance Arbitrary BlendBSDF where
  arbitrary 
     =  BlendBSDF
    <$> arbitrary
    <*> arbitrary
    <*> arbitrary
    
instance Arbitrary Mask where
  arbitrary 
     =  Mask 
    <$> arbitrary 
    <*> arbitrary
    
instance Arbitrary Twosided where
  arbitrary 
     =  Twosided 
    <$> arbitrary
    
instance Arbitrary Difftrans where
  arbitrary = Difftrans <$> arbitrary
  
instance Arbitrary ScatteringMethod where
  arbitrary 
    = oneof 
        [ SMMaterial <$> arbitrary
        , SMSA <$> arbitrary <*> arbitrary
        , SMTA <$> arbitrary <*> arbitrary
        ]
  
instance Arbitrary HK where
  arbitrary 
     =  HK 
    <$> arbitrary
    <*> arbitrary
    <*> arbitrary
      
instance Arbitrary Irawan where
  arbitrary 
     =  Irawan 
    <$> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
  
instance Arbitrary BSDF where
  arbitrary = 
    oneof 
      [ BSDFDiffuse         <$> arbitrary
      , BSDFRoughdiffuse    <$> arbitrary
      , BSDFDielectric      <$> arbitrary
      , BSDFThindielectric  <$> arbitrary
      , BSDFRoughdielectric <$> arbitrary
      , BSDFConductor       <$> arbitrary
      , BSDFRoughconductor  <$> arbitrary
      , BSDFPlastic         <$> arbitrary
      , BSDFRoughplastic    <$> arbitrary
      , BSDFCoating         <$> arbitrary
      , BSDFRoughcoating    <$> arbitrary
--      , BSDFBumpmap         <$> arbitrary
      , BSDFPhong           <$> arbitrary
      , BSDFWard            <$> arbitrary
      , BSDFMixturebsdf     <$> arbitrary
      , BSDFBlendbsdf       <$> arbitrary
      , BSDFMask            <$> arbitrary
      , BSDFTwosided        <$> arbitrary
      , BSDFDifftrans       <$> arbitrary
      , BSDFHk              <$> arbitrary
--      , BSDFIrawan          <$> arbitrary
      ]
     
    
instance Arbitrary Point where
  arbitrary 
     =  Point
    <$> arbitrary
    <*> arbitrary
    <*> arbitrary

instance Arbitrary Cube where
  arbitrary = Cube <$> arbitrary
  
instance Arbitrary Sphere where
  arbitrary 
     =  Sphere 
    <$> arbitrary
    <*> arbitrary
    <*> arbitrary

instance Arbitrary Cylinder where
  arbitrary 
     =  do 
    p0 <- arbitrary 
    
    p1 <- suchThat arbitrary (p0 /=) 
    
    Cylinder
      <$> pure p0
      <*> pure p1
      <*> arbitrary
      <*> arbitrary
    
instance Arbitrary Rectangle where
  arbitrary 
     =  Rectangle 
    <$> arbitrary
  
instance Arbitrary Disk where
  arbitrary = Disk <$> arbitrary
  
instance Arbitrary PositiveDouble where
  arbitrary = PositiveDouble . getPositive <$> arbitrary

instance (Arbitrary k, Ord k, Eq k, Arbitrary v) => Arbitrary (AtLeastTwo k v) where
  arbitrary = do 
    (k, v) <- arbitrary
    (k', v') <- suchThat arbitrary (\(x, _) -> k /= x)
    rest <- arbitrary
    return $ AtLeastTwo $ M.fromList $ (k, v):(k', v'):rest
    

-- I think that I can at least start by generating a bunch of different ShapeNodes
-- except for the obj files, xy files
genSimpleSimpleShapeLeaf :: Gen SimpleShapeLeaf
genSimpleSimpleShapeLeaf = do
  st <- oneof 
    [ STCube      <$> arbitrary
    , STSphere    <$> arbitrary
    , STCylinder  <$> arbitrary
    , STRectangle <$> arbitrary
    , STDisk      <$> arbitrary
    ]
  
  mat <- arbitrary
  
  return $ SimpleShapeLeaf st $ CNested mat
  
testGenShape = genShape (genShapeLeaf (Left <$> genSimpleSimpleShapeLeaf))
          
genShapeLeaf :: Gen (Either SimpleShapeLeaf OBJLeaf) -> Gen ShapeLeaf
genShapeLeaf gen = do 
  leaf <- gen

  return $ ShapeLeaf leaf mempty Nothing Nothing
          
genShape :: Gen ShapeLeaf -> Gen Shape
genShape gen =
    oneof 
      [ SShapeLeaf  <$> gen
--      , SShapeGroup <$> Test.QuickCheck.listOf (genShape gen)
      ]
    

arbitraryUnitTestScene :: Gen Scene
arbitraryUnitTestScene = undefined

-- also want to check that all of the versions of different things
-- work, even if I don't test all combos
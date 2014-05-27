{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}
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
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as M

--main = testScene' "testScene.xml" defaultVerifiedScene

main = quickCheckWith stdArgs { maxSize = 20, numTests = 10000 } quickTestScene

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
    run $ putStrLn $ "running file " ++ (filename ++ ".xml")
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
  shrink = genericShrink
  arbitrary = RefractiveValue <$> choose (1.000100, 4.0)    
    
instance Arbitrary WavelengthStyle where
  shrink = genericShrink
  arbitrary = WavelengthStyle <$> arbitrary
    
instance Arbitrary InternalSpectralFormat where
  shrink = genericShrink
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
  shrink = genericShrink
  arbitrary = RGBTriple <$> arbitrary <*> arbitrary <*> arbitrary     
    
instance Arbitrary Hex where
  shrink = genericShrink  
  arbitrary = Hex <$> arbitrary <*> arbitrary <*> arbitrary    
    
instance Arbitrary RGBLike where
  shrink = genericShrink  
  arbitrary 
    = oneof 
        [ RGBLTriple <$> arbitrary
        , RGBLHex    <$> arbitrary
        ]
    
instance Arbitrary Temperature where
  shrink = genericShrink
  arbitrary = Temperature <$> arbitrary    
    
instance Arbitrary Blackbody where
  shrink = genericShrink
  arbitrary = Blackbody <$> arbitrary <*> arbitrary    
    
instance Arbitrary Spectrum where
  shrink = genericShrink
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
  shrink _ = []
  arbitrary = CSpectrum <$> arbitrary
    
instance Arbitrary Diffuse where
  shrink = genericShrink
  arbitrary = Diffuse <$> arbitrary
  
instance Arbitrary RoughDiffuse where
  shrink = genericShrink
  arbitrary 
     =  RoughDiffuse 
    <$> arbitrary
    <*> arbitrary
    <*> arbitrary
    
instance Arbitrary KnownMaterial where
  arbitrary = elements [Vacuum .. Diamond]
    
instance Arbitrary Refraction where
  shrink = genericShrink
  arbitrary 
    = oneof
        [ RKM <$> arbitrary
        , IOR <$> arbitrary
        ]
    
instance Arbitrary Dielectric where
  shrink = genericShrink
  arbitrary 
     =  Dielectric
    <$> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary

instance Arbitrary ThinDielectric where
  shrink = genericShrink
  arbitrary 
     =  ThinDielectric 
    <$> arbitrary 
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    
instance Arbitrary Distribution where
  shrink = genericShrink
  arbitrary 
      = elements  
          [ Beckmann
          , GGX
          , PhongDistribution
          ]
        
instance Arbitrary Luminance where
  shrink _ = []
  arbitrary 
      = oneof 
          [ UniformLuminance <$> arbitrary
--          , TextureLuminance <$> arbitrary
          ]
    
instance Arbitrary AnistrophicAlpha where
  shrink = genericShrink
  arbitrary
     =  AnistrophicAlpha 
    <$> arbitrary 
    <*> arbitrary
    
instance Arbitrary UniformAlpha where
  shrink = genericShrink
  arbitrary 
     =  UniformAlpha 
    <$> arbitrary
    <*> arbitrary    
    
instance Arbitrary AlphaDistribution where
  shrink = genericShrink
  arbitrary 
    = oneof
        [ ADAnistrophicAlpha <$> arbitrary
        , ADUniformAlpha     <$> arbitrary
        ]
    
instance Arbitrary RoughDielectric where
  shrink = genericShrink
  arbitrary 
     =  RoughDielectric 
    <$> arbitrary 
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary

instance Arbitrary NonTransmission where 
  shrink = genericShrink
  arbitrary = oneof
    [ NTDiffuse         <$> arbitrary
    , NTRoughDiffuse    <$> arbitrary
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
  shrink = genericShrink
  arbitrary = elements [ AmorphousCarbon .. None ]
  
instance Arbitrary ManualConductance where
  shrink = genericShrink
  arbitrary = ManualConductance <$> arbitrary <*> arbitrary
  
instance Arbitrary Conductance where
  shrink = genericShrink
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
  shrink = genericShrink
  arbitrary 
     =  RoughConductor 
    <$> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    
instance Arbitrary Plastic where
  shrink = genericShrink
  arbitrary
     =  Plastic 
    <$> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    
instance Arbitrary RoughPlastic where
  shrink = genericShrink
  arbitrary 
     =  RoughPlastic 
    <$> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    
instance Arbitrary Coating where
  shrink = genericShrink
  arbitrary 
     =  Coating
    <$> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary

instance Arbitrary a => Arbitrary (Child a) where
  shrink (CNested x) = CNested <$> shrink x
  arbitrary = CNested <$> arbitrary

instance Arbitrary RoughCoating where
  shrink = genericShrink
  arbitrary 
     =  RoughCoating 
    <$> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    
instance Arbitrary RefractionPair where
  arbitrary = do
    p <- arbitrary
    RefractionPair <$> pure (IOR p) <*> (IOR <$> suchThat arbitrary (p /=))
    
--instance Arbitrary BumpMap where
--  arbitrary = 
  
instance Arbitrary Phong where
  shrink = genericShrink
  arbitrary 
     =  Phong 
    <$> arbitrary 
    <*> arbitrary
    <*> arbitrary
    
instance Arbitrary WardType where
  shrink = genericShrink
  arbitrary 
    = elements [WTWard, WTWardDuer, WTBalanced]
    
instance Arbitrary Ward where
  shrink = genericShrink
  arbitrary 
     =  Ward
    <$> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
  
instance Arbitrary a => Arbitrary (NonEmpty a) where
  shrink = \case
     x :| []       -> []
     x :| (y:rest) -> fmap (x :|) (shrink rest) ++ fmap (y :|) (shrink rest)
  arbitrary = NonEmpty.fromList <$> listOf1 arbitrary  
    
instance Arbitrary MixtureBSDF where
  shrink = genericShrink
  arbitrary = MixtureBSDF <$> arbitrary
  
instance Arbitrary BlendBSDF where
  shrink = genericShrink
  arbitrary 
     =  BlendBSDF
    <$> arbitrary
    <*> arbitrary
    <*> arbitrary
    
instance Arbitrary Mask where
  shrink = genericShrink
  arbitrary 
     =  Mask 
    <$> arbitrary 
    <*> arbitrary
    
instance Arbitrary Twosided where
  shrink = genericShrink
  arbitrary 
     =  Twosided 
    <$> arbitrary
    
instance Arbitrary Difftrans where
  shrink = genericShrink
  arbitrary = Difftrans <$> arbitrary
  
instance Arbitrary ScatteringMethod where
  shrink = genericShrink
  arbitrary 
    = oneof 
        [ SMMaterial <$> arbitrary
        , SMSA <$> arbitrary <*> arbitrary
        , SMTA <$> arbitrary <*> arbitrary
        ]
  
instance Arbitrary HK where
  shrink = genericShrink
  arbitrary 
     =  HK 
    <$> arbitrary
    <*> arbitrary
    <*> arbitrary
      
instance Arbitrary Irawan where
  shrink = genericShrink
  arbitrary 
     =  Irawan 
    <$> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
  
instance Arbitrary BumpMap 
  
instance Arbitrary BSDF where
  shrink = genericShrink
  arbitrary = 
    oneof 
      [ BSDFDiffuse         <$> arbitrary
      , BSDFRoughdiffuse    <$> arbitrary
      , BSDFDielectric      <$> arbitrary
      , BSDFThindielectric  <$> arbitrary
      , BSDFRoughdielectric <$> arbitrary
      , BSDFConductor       <$> arbitrary
--      , BSDFRoughconductor  <$> arbitrary
      , BSDFPlastic         <$> arbitrary
--      , BSDFRoughplastic    <$> arbitrary
      , BSDFCoating         <$> arbitrary
--      , BSDFRoughcoating    <$> arbitrary
--      , BSDFBumpmap         <$> arbitrary

      , BSDFPhong           <$> arbitrary
      , BSDFWard            <$> arbitrary
      , BSDFMixturebsdf     <$> arbitrary
      , BSDFBlendbsdf       <$> arbitrary
{-      
      , BSDFMask            <$> arbitrary
      , BSDFTwosided        <$> arbitrary
      , BSDFDifftrans       <$> arbitrary
-}      

-- keeps crashing
--      , BSDFHk              <$> arbitrary
--      , BSDFIrawan          <$> arbitrary
      ]
     
    
instance Arbitrary Point where
  shrink = genericShrink
  arbitrary 
     =  Point
    <$> arbitrary
    <*> arbitrary
    <*> arbitrary

instance Arbitrary Cube where
  shrink = genericShrink  
  arbitrary = Cube <$> arbitrary
  
instance Arbitrary Sphere where
  shrink = genericShrink
  arbitrary 
     =  Sphere 
    <$> arbitrary
    <*> arbitrary
    <*> arbitrary

instance Arbitrary Cylinder where
  shrink = genericShrink
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
  shrink = genericShrink
  arbitrary 
     =  Rectangle 
    <$> arbitrary
  
instance Arbitrary Disk where
  shrink = genericShrink
  arbitrary = Disk <$> arbitrary
  
instance Arbitrary PositiveDouble where
  shrink = genericShrink
  arbitrary = PositiveDouble . getPositive <$> arbitrary

instance (Arbitrary k, Ord k, Eq k, Arbitrary v) => Arbitrary (AtLeastTwo k v) where
  shrink (AtLeastTwo xs) 
    | M.size xs <= 2 = []
    | otherwise = result where
        x:y:rest = M.toList xs
        result = map AtLeastTwo $ fmap (M.fromList . (\r -> x:y:r)) $ shrink rest 
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
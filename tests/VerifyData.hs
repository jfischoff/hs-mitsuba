module Main where
import Mitsuba.Types
import Mitsuba.Element.Class
import Test.QuickCheck
import Data.Default.Generics
import System.Process
import System.IO.Temp
import System.Exit
import Text.Blaze.Renderer.Utf8
import qualified Data.ByteString.Lazy as BSL

main = testScene' "testScene.xml" defaultVerifiedScene

testScene :: Scene -> IO ExitCode
testScene scene = withSystemTempDirectory "hs-mitsuba" $ \tempDirPath -> do
    let path = tempDirPath ++ "/tempFile.xml"
    BSL.writeFile path $ renderMarkup $ toXML scene
    
    system $ "mitsuba " ++ path
    
    
testScene' :: FilePath -> Scene -> IO ExitCode
testScene' filePath scene = do
      BSL.writeFile filePath $ renderMarkup $ toXML scene
      system $ "mitsuba " ++ filePath

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
                                = CNested 
                                $ BSDFDiffuse 
                                $ def
                            }
       , shapeLeafToWorld    = def
       , shapeLeafMediumPair = Nothing
       , shapeLeafEmitter    = Nothing
       }

  
defaultVerifiedBSDF :: SceneNodeData 
defaultVerifiedBSDF
  = SNDBSDF
  $ BSDFDiffuse
  $ def

defaultVerifiedTexture :: SceneNodeData
defaultVerifiedTexture
  = SNDTexture 
  $ TCheckerboard
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
      , dipoleIntIOR        = IOR 1.0
      , dipoleExtIOR        = IOR 1.0
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
        { pointLightToWorld        = def
        , pointLightPosition       = def
        , pointLightIntensity      = def
        , pointLightSamplingWeight = def
        }

defaultSensor :: SceneNodeData
defaultSensor 
  = SNDSensor
  $ Sensor 
      { sensorFilm    = Just 
                      $ defaultFilm'
                       
      , sensorSampler = Just
                      $ CNested
                      $ defaultSampler'
                      
      , sensorType  
          = STPerspective
          $ Perspective
              { perspectiveToWorld      = def
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
  $ IAo
  $ AmbientOcclusion 
     { ambientOcclusionShadingSamples = 1
     , ambientOcclusionRayLength      = 1
     }

defaultSampler :: SceneNodeData
defaultSampler = SNDSampler defaultSampler'
  
defaultSampler' :: Sampler
defaultSampler' = SLdsampler $ LDSampler 1 1
      
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
  ]

arbitraryUnitTestScene :: Gen Scene
arbitraryUnitTestScene = undefined

-- also want to check that all of the versions of different things
-- work, even if I don't test all combos
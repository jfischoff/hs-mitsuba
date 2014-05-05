{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Tests.Mitsuba.Types where
import Mitsuba.Types
import Mitsuba.Utils
import Mitsuba.Element
import Test.Tasty
import Test.Tasty.HUnit hiding (Label)
import Test.Tasty.TH
import Data.Text (Text)
import Test.HUnit hiding (Label)
import GHC.Generics
import Data.List
import Text.Blaze
--import Text.InterpolatedString.Perl6
import Text.XML.QQ
import Text.Blaze.Renderer.Pretty
import Data.Monoid
import qualified Text.XML.Light.Types as XML
import qualified Text.XML.Light.Output as XML
import Data.Maybe
import Control.Lens
import Data.Data
import Data.Default.Generics
default (Text, Integer, Double)

-- TODO pull of the xml into it's own file
-- make sure that the mitsubi can load it without errors

assertElement x y = assertEqual 
  ( unlines 
      [ "Actual:"
      , XML.showElement $ toXML x
      , "Expected:"
      , XML.showElement y
      , ""
      ]
  ) (toXML x) y

tests :: TestTree
tests = $(testGroupGenerator)

instance Ord XML.Content where
  compare a b = case (a, b) of
    (XML.Elem x, XML.Elem y) -> compare x y

instance Eq XML.Content where
  a == b = case (a, b) of
    (XML.Elem x, XML.Elem y) -> x == y
    (XML.Text x, XML.Text y) -> error "Text" --x == y
    (XML.CRef x, XML.CRef y) -> error "CRef " --x == y
    
instance Eq XML.Element where
  XML.Element name0 attrs0 contents0 line0 ==
    XML.Element name1 attrs1 contents1 line1
        = all id 
            [ name0          == name1
            , sort attrs0    == sort attrs1
            , sort contents0 == sort contents1
            , line0          == line1
            ]

instance Ord XML.Element where
  XML.Element name0 attrs0 contents0 line0 `compare`
    XML.Element name1 attrs1 contents1 line1 =
      flip foldr1 
         [ name0          `compare` name1
         , sort attrs0    `compare` sort attrs1
         , sort contents0 `compare` sort contents1
         , line0          `compare` line1
         ] $ \x acc -> case acc of
            LT -> LT
            GT -> GT
            EQ -> x

--toXML = renderMarkup . toXML

case_Ref_toXML = toXML (Ref "thing") @?= [xmlQQ|<ref id="thing" />=|]

-- __case_Child_toXML = 
   
case_WavelengthStyle_toAttributeValue = 
   toAttributeValue (WavelengthStyle 
                        [ (400, 0.56)
                        , (500, 0.18)
                        , (600, 0.58)
                        , (700, 0.24)
                        ]) @?= 
      "400:0.56, 500:0.18, 600:0.58, 700:0.24"

case_InternalSpectralFormat_toAttributeValue = 
  toAttributeValue (InternalSpectralFormat 0.2 0.2 0.8 0.4 0.6 0.5 0.1 0.9 0.4 0.2) @?= 
     ".2, .2, .8, .4, .6, .5, .1, .9, .4, .2"
     
case_RGBTriple_toAttributeValue = 
  toAttributeValue (RGBTriple 0.2 0.8 0.4) @?= "0.200, 0.800, 0.400"
  
case_Hex_toAttributeValue = 
  toAttributeValue (Hex 1 15 128) @?= "#010f80"
  
case_Temperature_toAttributeValue =
  toAttributeValue (Temperature 5000) @?= "5000k"

case_Blackbody_toXML = 
 toXML (SBlackbody $ Blackbody (Temperature 5000) 1.5) @?= 
    [xmlQQ|<blackbody scale="1.5" temperature="5000k" />|]


case_point_toXML = 
  toXML (Point 3 4 5) @?= [xmlQQ|<point z="5.0" x="3.0" y="4.0" />|]

case_vector_toXML = 
  toXML (Vector 3 4 5) @?= [xmlQQ|<vector z="5.0" x="3.0" y="4.0" /> |]

case_RegularTransform_toXML =
   toXML (translate (-1) 3 4
            <> rotateY 45
             ) @?= 
 [xmlQQ|
  <transform>
    <translate z="4.0" x="-1.0" y="3.0" />
    <rotate z="0.0" angle="45.0" x="0.0" y="1.0" />
  </transform>|]

-- This is not the right way to test this
-- I should be comparing elements to elements
-- I should compare XML to XML

case_Translate_toXML 
  = toXML (Translate (-1) 3 4) 
  @?= [xmlQQ|<translate z="4.0" x="-1.0" y="3.0" />|]

case_rotate_toXML 
  = toXML (Rotate (Vector 0.701 0.701 0.0) 180) 
  @?= [xmlQQ| <rotate x="0.701" y="0.701" z="0.0" angle="180.0"/> |]

case_uniform_scale_toXML 
  =   toXML (SUniformScale 5.0)
  @?= [xmlQQ| <scale value="5.0" /> |]

case_nonuniform_scale_toXML 
    = toXML (SScaleAxis $ Vector 2 1 (-1)) 
  @?= [xmlQQ|<scale x="2.0" y="1.0" z="-1.0"/>|]

case_matrix_toXML 
    = toXML (Matrix 0.0 (-0.53) 0.0 (-1.79) 0.92 0.0 0.0 8.03 0.0 0.0 0.53 0.0 0.0 0.0 0.0 1.0) 
  @?= [xmlQQ|<matrix value="0.0 -0.53 0.0 -1.79 0.92 0.0 0.0 8.03 0.0 0.0 0.53 0.0 0.0 0.0 0.0 1.0"/>|]

case_lookat_toXML 
    = toXML (Lookat (Point 10 50 (-800)) (Point 0.0 0.0 0.0) (Vector 0.0 1.0 0.0)) 
  @?= [xmlQQ|<lookat origin="10.0, 50.0, -800.0" target="0.0, 0.0, 0.0" up="0.0, 1.0, 0.0"/>|] 
  

case_animated_transformations
    = toXML (Animation 
      [ (0.0, translate (-1.0) (3.0) (4.0))
      , (1.0, translate (-1.0) (4.0) (4.0))
      ]) 
  @?= [xmlQQ| 
  <animation >
    <transform time="0.0">
      <translate z="4.0" x="-1.0" y="3.0" />
    </transform>
    <transform time="1.0">
      <translate z="4.0" x="-1.0" y="4.0" />
    </transform>
  </animation>
   |]

case_include_toXML 
  = toXML (Include "nested-scene.xml") @?= [xmlQQ|<include filename="nested-scene.xml"/>|]

case_alias_toXML 
    = toXML (Alias (Ref "myMaterial1") "myMaterial2")
  @?= [xmlQQ|<alias id="myMaterial1" as="myMaterial2"/>|]

cubeActual 
  = SShapeLeaf 
  $ ShapeLeaf
    ( Left 
    $ SimpleShapeLeaf 
        (STCube $ Cube True) 
        $ CNested $ BSDFDiffuse $ Diffuse 
        $ CTexture 
        $ TCheckerboard $ Checkerboard 
             { checkerboardColor0  = SRGB $ RGBLTriple $ RGBTriple 1.0 1.0 1.0
             , checkerboardColor1  = SRGB $ RGBLTriple $ RGBTriple 0 0.0 0.0
             , checkerboardUoffset = 0
             , checkerboardVoffset = 0
             , checkerboardUscale  = 6.0
             , checkerboardVscale  = 6.0
             }
    )
    (TRegular $ uniformScaleZ 2)
    Nothing
    Nothing
              
cubeExpected = [xmlQQ|
      <shape type="cube"> 
        <transform name="toWorld">
          <scale x="1.0" y="1.0" z="2.0"/>
        </transform>

        <boolean name="flipNormals" value="true" />

        <bsdf type="diffuse">
          <texture type="checkerboard" name="reflectance">
            <rgb name="color0" value="1.000, 1.000, 1.000" />
            <rgb name="color1" value="0.000, 0.000, 0.000" />

            <float name="uoffset" value="0.0"/>
            <float name="voffset" value="0.0"/>

            <float name="uscale" value="6.0"/>
            <float name="vscale" value="6.0"/>
          </texture>
        </bsdf> 
      </shape>
    |]


case_cube_toXML = cubeActual `assertElement` cubeExpected

actualSphere0 
  = SShapeLeaf 
  $ ShapeLeaf
    ( Left 
    $ SimpleShapeLeaf
      ( STSphere 
      $ Sphere 
        (Point 0 0 0)
        1.0
        True
      ) 
      $ CNested $ BSDFDiffuse $ Diffuse $ CSpectrum $ SUniform 1.0
    )
    ( TRegular 
    $  uniformScale 2
    <> translate 1 0 0
    )
    Nothing
    Nothing

case_sphere_0_toXML 
  = actualSphere0 `assertElement` [xmlQQ|
    <shape type="sphere">
      <bsdf type="diffuse">
        <spectrum name="reflectance" value="1.0" />
      </bsdf> 
      
      <float name="radius" value="1.0"/>
      <point name="center" z="0.0" x="0.0" y="0.0"/>
      <boolean name="flipNormals" value="true"/>
      
      <transform name="toWorld">
        <scale value="2.0" />
        <translate x="1.0" y="0.0" z="0.0" />
      </transform>
      
    </shape>
  |]


actualSphereBlackBody 
  = SShapeLeaf 
  $ ShapeLeaf
    ( Left 
    $ SimpleShapeLeaf 
      ( STSphere 
      $ Sphere 
        (Point 0 1 0)
        1.0
        True
      ) 
      (CNested diffuse)
    )
    mempty
    Nothing
    $ Just
    $ CNested
    $ EArea
    $ AreaLight
       ( SBlackbody
       $ Blackbody
          (Temperature 7000)
          1.0
       )
    $ 1.0

case_sphere_blackbody_toXML 
  = actualSphereBlackBody `assertElement` [xmlQQ|
    <shape type="sphere">
      <point name="center" x="0.0" y="1.0" z="0.0"/> 
      <float name="radius" value="1.0"/>
      
      <boolean name="flipNormals" value="true" />
      <transform name="toWorld" />
      
      <bsdf type="diffuse">
        <spectrum name="reflectance" value="1.0" />
      </bsdf>
      
      <emitter type="area">
        <blackbody name="radiance" scale="1.0" temperature="7000k"/>
        <float name="samplingWeight" value="1.0" />
      </emitter>
    </shape>
  |]

actualCylinder 
  = cylinder 0.3 
  & bsdf .~ twosided diffuse 
  
case_cylinder_toXML 
  = actualCylinder `assertElement` [xmlQQ|
    <shape type="cylinder">
      <float name="radius" value="0.3"/> 
       
      <boolean name="flipNormals" value="true" />
      <transform name="toWorld" />
      
      <point name="p0" z="0.0" x="0.0" y="0.0" />
      <point name="p1" z="0.0" x="0.0" y="1.0" />
      
      <bsdf type="twosided">
        <bsdf type="diffuse">
          <spectrum name="reflectance" value="1.0" />
        </bsdf>
      </bsdf> 
    </shape>
  |]


actualRectange_0 = square
    
case_rectangle_0_toXML 
  = actualRectange_0 `assertElement` [xmlQQ|
    <shape type="rectangle">
    
    <boolean name="flipNormals" value="true" />
    <transform name="toWorld" />
    
      <bsdf type="diffuse">
        <spectrum name="reflectance" value="1.0" />
      </bsdf>
    </shape>
  |]

actualRectangle1
   = square 
   & toWorld 
        .~ TRegular (rotateX   90)
        <> TRegular (scale     0.4 0.3 0.2)
        <> TRegular (translate 0.0 1.0 0.2)
   & _SShapeLeaf . shapeLeafEmitterL .~ Just (CNested $ EArea $ AreaLight (SUniform 3.0) 1.0)

-- Question can you have an emitter and a material? or is it one or another?
case_rectangle_1_toXML 
  = actualRectangle1 `assertElement` [xmlQQ|
  <shape type="rectangle"> 
    <transform name="toWorld">
      <rotate x="1.0" y="0.0" z="0.0" angle="90.0"/>
      <scale x="0.4" y="0.3" z="0.2"/>
      <translate x="0.0" y="1.0" z="0.2"/>
    </transform>
    
    <boolean name="flipNormals" value="true" />
    
    <bsdf type="diffuse">
      <spectrum name="reflectance" value="1.0" />
    </bsdf>
    
    <emitter type="area">
      <spectrum name="radiance" value="3.0"/> 
      <float name="samplingWeight" value="1.0" />
    </emitter>
  </shape>
  |]

actualDisk0 = disk 

case_disk_0_toXML 
  = toXML actualDisk0 @?= [xmlQQ|
    <shape type="disk">
      <boolean name="flipNormals" value="true" />
      <transform name="toWorld" />
      
      <bsdf type="diffuse">
        <spectrum name="reflectance" value="1.0" />
      </bsdf> 
    </shape>
  |]

actualDisk1 
  = disk 
  & toWorld .~ TRegular
      (  rotateX 90 
      <> uniformScale 0.3
      <> translate 0 1 0.3
      )
  & _SShapeLeaf . shapeLeafEmitterL .~ Just (CNested $ EArea $ AreaLight (SUniform 4.0) 1.0)

case_disk_1_toXML 
  = actualDisk1 `assertElement` [xmlQQ|
  <shape type="disk">
    <transform name="toWorld">
      <rotate x="1.0" y="0.0" z="0.0" angle="90.0"/> 
      <scale value="0.3"/> 
      <translate x="0.0" y="1.0" z="0.3"/>
    </transform>
    <emitter type="area">
      <spectrum name="radiance" value="4.0"/> 
      <float name="samplingWeight" value="1.0" />
    </emitter>
    
    <boolean name="flipNormals" value="true" />
    
    <bsdf type="diffuse">
      <spectrum name="reflectance" value="1.0" />
    </bsdf>
  </shape>
  |]

actualObj0 = obj "myShape.obj"



case_obj_0_toXML
  = actualObj0 `assertElement` [xmlQQ|
    <shape type="obj">
       <string name="filename" value="myShape.obj"/>
       
       <boolean name="flipNormals"     value="false" />
       <boolean name="flipTexCoords"   value="false" />
       <boolean name="faceNormals"     value="false" />
       <float name="maxSmoothAngle" value="0.0"   />
       
       <bsdf type="diffuse">
         <spectrum name="reflectance" value="1.0" />
       </bsdf>
       
       <transform name="toWorld" />
       
     </shape>
  |]


actualObj1 
  = obj "myShape.obj"
  & bsdf .~ (BSDFRoughplastic $ RoughPlastic 
    { roughPlasticAlpha               
      = ADUniformAlpha 
      $ UniformAlpha Beckmann 
      $ UniformLuminance 1.0
    , roughPlasticIntIOR              = RKM $ Vacuum
    , roughPlasticExtIOR              = RKM $ Water
    , roughPlasticSpecularReflectance = CSpectrum $ SUniform 2
    , roughPlasticDiffuseReflectance  = CSpectrum 
                                      $ SRGB 
                                      $ RGBLTriple 
                                      $ RGBTriple 0.2 0.6 0.3
    , roughPlasticNonlinear           = True
    })

case_obj_1_toXML 
  = actualObj1 `assertElement` [xmlQQ|
  <shape type="obj">
    <bsdf type="roughplastic">
      <string name="intIOR" value="vacuum"/>
      <string name="distribution" value="beckmann"/>
      <string name="extIOR" value="water"/>
      <spectrum name="specularReflectance" value="2.0"/>
      <float name="alpha" value="1.0"/>
      <boolean name="nonlinear" value="true"/>
      <rgb name="diffuseReflectance" value="0.200, 0.600, 0.300"/>
    </bsdf>
    <boolean name="faceNormals" value="false"/>
    <transform name="toWorld"/>
    <boolean name="flipTexCoords" value="false"/>
    <float name="maxSmoothAngle" value="0.0"/>
    <boolean name="flipNormals" value="false"/>
    <string name="filename" value="myShape.obj"/>
  </shape>
  |]

actualObj2 
  = objMultiMaterial "myShape.obj"
  & objMaterialMap . at "Glass" .~ Just (CNested $ dielectric 1.5 1.5)
  & objMaterialMap . at "Water" .~ Just (CNested $ dielectric 1.333 1.333)

-- Third
-- This is different
-- all of sudden 
case_obj_2_toXML 
  = actualObj2 `assertElement` [xmlQQ|
  <shape type="obj">
    <transform name="toWorld"/>
    <boolean name="faceNormals" value="false"/>
    <boolean name="flipTexCoords" value="false"/>
    <float name="maxSmoothAngle" value="0.0"/>
    <string name="filename" value="myShape.obj"/> 
    
    <bsdf name="Glass" type="dielectric">
      <float name="intIOR" value="1.5"/> 
      <float name="extIOR" value="1.5"/>
      <texture name="specularReflectance" type="vertexcolors" />
      <texture name="specularTransmittance" type="vertexcolors" />
    </bsdf>
    
    <bsdf name="Water" type="dielectric">
      <float name="intIOR" value="1.333"/>
      <float name="extIOR" value="1.333"/>
      <texture name="specularReflectance" type="vertexcolors" />
      <texture name="specularTransmittance" type="vertexcolors" />
    </bsdf> 
    
    <boolean name="flipNormals" value="false"/>
  </shape>
  
  
  |]

-- TODO PLY
actualPLY = ply "myShape.ply"

case_PLY_toXML 
  = actualPLY `assertElement` [xmlQQ|
      <shape type="ply">
        <transform name="toWorld"/>
        
        <bsdf type="diffuse">
          <spectrum name="reflectance" value="1.0" />
        </bsdf>
        
        <string name="filename" value="myShape.ply"/> 
        
        <float name="maxSmoothAngle" value="0.0"/>
        <boolean name="srgb" value="false"/>
        <boolean name="flipNormals" value="false"/>
        <boolean name="faceNormals" value="true"/>
      </shape>

  |]

actualSerialized = serialized "myShape.serialized"

case_Serialized_toXML 
  = actualSerialized `assertElement` [xmlQQ|
    <shape type="serialized">
      <transform name="toWorld"/>
      
      <bsdf type="diffuse">
        <spectrum name="reflectance" value="1.0"/>
      </bsdf>
      
      <string name="filename" value="myShape.serialized"/>
      <boolean name="faceNormals" value="true" />
      <boolean name="flipNormals" value="false" />
      <float name="maxSmoothAngle" value="0.0"/>
      <integer name="shapeIndex" value="0" />
    </shape>
  
  
  |]

actualNested = SShapeGroup [cube, cube]

case_shapegroup_nested_toXML 
  = actualNested `assertElement` [xmlQQ|
  <shape type="shapegroup"> 
    <shape type="cube">
      <transform name="toWorld"/>
      <bsdf type="diffuse">
        <spectrum name="reflectance" value="1.0"/>
      </bsdf>
      <boolean name="flipNormals" value="false"/>
    </shape>
    <shape type="cube">
      <transform name="toWorld"/>
      <bsdf type="diffuse">
        <spectrum name="reflectance" value="1.0"/>
      </bsdf>
      <boolean name="flipNormals" value="false"/>
    </shape>
  </shape>

  |]

actualInstance = instanceShape (Ref "myShapeGroup")

case_instance_0_toXML 
  = actualInstance `assertElement` [xmlQQ|
  <shape type="instance">
    <transform name="toWorld"/>
    <bsdf type="diffuse">
      <spectrum name="reflectance" value="1.0"/>
    </bsdf>
    <ref id="myShapeGroup"/>
  </shape>
  |]


actualInstance1 
  = instanceShape (Ref "myShapeGroup") 
  & toWorld .~ TRegular
      (  rotateX 45
      <> uniformScale 1.5
      <> translate 0 0 10
      )
      
case_instance_1_toXML 
  = actualInstance1 `assertElement` [xmlQQ|
  <shape type="instance">
    <ref id="myShapeGroup"/> 
    <transform name="toWorld">
      <rotate x="1.0" y="0.0" z="0.0" angle="45.0"/>
      <scale value="1.5"/>
      <translate x="0.0" y="0.0" z="10.0"/>
    </transform>
    
    <bsdf type="diffuse">
      <spectrum name="reflectance" value="1.0"/>
    </bsdf>
  </shape>  
  |]
  
actualHair = hair "myHair.hair"

case_hair_toXML 
  = actualHair `assertElement` [xmlQQ|
     <shape type="hair">
          <string name="filename" value="myHair.hair" />
          <float name="radius"         value="1.0" /> 
          <float name="angleThreshold" value="0.0" /> 
          <float name="reduction" value="0.0" /> 
          <integer name="width" value="1" /> 
          <integer name="height" value="1" /> 
          <texture type="checkerboard" name="texture">
            <blackbody name="color1" scale="0.0" temperature="0k"/>
            <blackbody name="color0" scale="0.0" temperature="0k"/>

            <float name="uoffset" value="0.0"/>
            <float name="voffset" value="0.0"/>

            <float name="uscale" value="0.0"/>
            <float name="vscale" value="0.0"/>
          </texture>
          
            <transform name="toWorld"/>
          
          <bsdf type="diffuse">
            <spectrum name="reflectance" value="1.0"/>
          </bsdf>

     </shape>
  |]

actualHeightField = heightField "mountain_profile.png"



case_heightfield_toXML
  = actualHeightField `assertElement` [xmlQQ|
    <shape type="heightfield"> 
        <texture type="checkerboard" name="texture">
          <blackbody name="color1" scale="0.0" temperature="0k"/>
          <blackbody name="color0" scale="0.0" temperature="0k"/>

          <float name="uoffset" value="0.0"/>
          <float name="voffset" value="0.0"/>

          <float name="uscale" value="0.0"/>
          <float name="vscale" value="0.0"/>
        </texture>
        
        <boolean name="shadingNormals" value="true" />
        <boolean name="flipNormals"    value="false" />
        
        <transform name="toWorld"/>
        
        <bsdf type="diffuse">
          <spectrum name="reflectance" value="1.0"/>
        </bsdf>
        
        <integer name="width" value="1" />
        <integer name="height" value="1"/>
        <float name="scale" value="1.0" />
        <string name="filename" value="mountain_profile.png" />
    </shape>
  |]

actualDiffuse0 
  = BSDFDiffuse 
  $ Diffuse 
  $ CSpectrum
  $ SSRGB
  $ RGBLHex
  $ Hex 0x6d 0x71 0x85


case_diffuse_0_toXML 
  = actualDiffuse0 `assertElement` [xmlQQ|
    <bsdf type="diffuse">
      <srgb name="reflectance" value="#6d7185"/>
    </bsdf>
  |]

actualDiffuse1 
  = BSDFDiffuse 
  $ Diffuse 
  $ CTexture
  $ TBitmap
  $ Bitmap
     { bitmapFilename      = "wood.jpg"
     , bitmapWrapMode      = Left Repeat
     , bitmapGamma         = Nothing
     , bitmapFilterType    = Trilinear
     , bitmapMaxAnisotropy = 0.0
     , bitmapCache         = False
     , bitmapUoffset       = 0.5
     , bitmapVoffset       = 1.0
     , bitmapUscale        = 1.5
     , bitmapVscale        = 2.0
     , bitmapChannel       = R
     }

case_diffuse_1_toXML 
  = actualDiffuse1 `assertElement` [xmlQQ|
  <bsdf type="diffuse">
    <texture type="bitmap" name="reflectance">
      <string name="filename" value="wood.jpg"/>
      <string name="wrapMode" value="repeat" />
      <string name="filterType" value="trilinear" />
      <float  name="maxAnisotropy" value="0.0" />
      <boolean   name="cache" value="false" />
      <float  name="uoffset" value="0.5" />
      <float  name="voffset" value="1.0" />  

      <float name="uscale" value="1.5" />
      <float name="vscale" value="2.0" />  

      <string name="channel" value="r" />
    </texture> 
  </bsdf>
  |]

actualRoughDiffuse 
  = BSDFRoughdiffuse
  $ RoughDiffuse 
      ( CSpectrum 
      $ SRGB
      $ RGBLTriple
      $ RGBTriple 1.0 0.5 0.0
      )
      ( CSpectrum 
      $ SRGB
      $ RGBLTriple
      $ RGBTriple 0.1 0.05 0.0
      )
      True

case_roughdiffuse_toXML 
  = actualRoughDiffuse `assertElement` [xmlQQ|
      <bsdf type="roughdiffuse">
        <rgb name="reflectance" value="1.000, 0.500, 0.000" />
        <rgb name="alpha"       value="0.100, 0.050, 0.000" />
        <boolean name="useFastApprox" value="true" />
      </bsdf>
  |]


actualDielectric 
  = BSDFDielectric
  $ Dielectric 
      { dielectricIntIOR                = RKM Water
      , dielectricExtIOR                = RKM Air
      , dielectricSpecularReflectance   = CSpectrum $ SUniform 1.0
      , dielectricSpecularTransmittance = CSpectrum $ SUniform 1.0
      }

case_dielectric_toXML 
  = actualDielectric `assertElement` [xmlQQ|
    <bsdf type="dielectric">
      <string name="intIOR" value="water"/>
      <string name="extIOR" value="air"/> 
      <spectrum name="specularReflectance" value="1.0" />
      <spectrum name="specularTransmittance" value="1.0" />
    </bsdf>
  |]


-- TODO thin dielectric
actualThinDielectric 
  = BSDFThindielectric
  $ ThinDielectric
    { thinDielectricIntIOR                = RKM Water
    , thinDielectricExtIOR                = RKM Air
    , thinDielectricSpecularReflectance   = CSpectrum $ SUniform 1.0
    , thinDielectricSpecularTransmittance = CSpectrum $ SUniform 1.0
    }

case_thindielectric_toXML 
    = actualThinDielectric `assertElement` [xmlQQ|
      <bsdf type="thindielectric">
        <string name="intIOR" value="water"/>
        <string name="extIOR" value="air"/> 
        <spectrum name="specularReflectance" value="1.0" />
        <spectrum name="specularTransmittance" value="1.0" />
      </bsdf>
    |]


actualRoughDielectric0 
  = BSDFRoughdielectric
  $ RoughDielectric 
      { roughDielectricAlpha                 = ADUniformAlpha
                                             $ UniformAlpha GGX
                                             $ UniformLuminance 0.304
      , roughDielectricIntIOR                = RKM Bk7
      , roughDielectricExtIOR                = RKM Air
      , roughDielectricSpecularReflectance   = CSpectrum $ SUniform 1.0
      , roughDielectricSpecularTransmittance = CSpectrum $ SUniform 1.0
      }

case_roughdielectric_0_toXML 
  = actualRoughDielectric0 `assertElement` [xmlQQ|
  <bsdf type="roughdielectric">
    <string name="distribution" value="ggx"/>
    <float name="alpha" value="0.304"/> 
    <string name="extIOR" value="air"/>
    <string name="intIOR" value="bk7"/> 
    <spectrum name="specularReflectance" value="1.0" />
    <spectrum name="specularTransmittance" value="1.0" />
  </bsdf>  
  |]

actualRoughDielectric1 
  = BSDFRoughdielectric
  $ RoughDielectric 
      { roughDielectricAlpha                 
          = ADUniformAlpha 
          $ UniformAlpha Beckmann
          $ TextureLuminance 
          $ TBitmap
          $ Bitmap
             { bitmapFilename      = "roughness.exr"
             , bitmapWrapMode      = Left Repeat
             , bitmapGamma         = Nothing
             , bitmapFilterType    = Trilinear
             , bitmapMaxAnisotropy = 0.0
             , bitmapCache         = False
             , bitmapUoffset       = 0.5
             , bitmapVoffset       = 1.0
             , bitmapUscale        = 1.5
             , bitmapVscale        = 2.0
             , bitmapChannel       = R
             }
                  
      , roughDielectricIntIOR                = IOR 1.5046
      , roughDielectricExtIOR                = IOR 1.0
      , roughDielectricSpecularReflectance   = CSpectrum $ SUniform 1.0
      , roughDielectricSpecularTransmittance = CSpectrum $ SUniform 1.0
      }

case_roughdielectric_1_toXML 
  = actualRoughDielectric1 `assertElement` [xmlQQ|
  <bsdf type="roughdielectric">
    <string name="distribution" value="beckmann"/> 
    <float name="intIOR" value="1.5046"/>
    <float name="extIOR" value="1.0"/>
    
    <spectrum name="specularReflectance" value="1.0" />
    <spectrum name="specularTransmittance" value="1.0" />
    
    <texture name="alpha" type="bitmap">
      <string name="filename" value="roughness.exr"/>
      
      <string name="wrapMode" value="repeat" />
      <string name="filterType" value="trilinear" />
      <float  name="maxAnisotropy" value="0.0" />
      <boolean   name="cache" value="false" />
      <float  name="uoffset" value="0.5" />
      <float  name="voffset" value="1.0" />  

      <float name="uscale" value="1.5" />
      <float name="vscale" value="2.0" />  

      <string name="channel" value="r" />
    </texture> 
  </bsdf>
  
  |]

actualConductor 
  = BSDFConductor
  $ Conductor
      { conductorConductance         = CConductorType Gold
      , conductorExtEta              = RKM Bk7
      , conductorSpecularReflectance = CSpectrum $ SUniform 1.0
      }

case_conductor_0_toXML 
  = actualConductor `assertElement` [xmlQQ|
    <bsdf type="conductor">
      <string name="material" value="Au"/>
      <string name="extEta" value="bk7"/> 
      <spectrum name="specularReflectance" value="1.0" />
    </bsdf>
  |]


actualConductor1 
  = BSDFConductor
  $ Conductor
      { conductorConductance         
          = CManualConductance 
          $ ManualConductance
              { manualConductanceEta   = SFile "conductorIOR.eta.spd"
              , manualConductanceK = SFile "conductorIOR.k.spd"
              }
                
      , conductorExtEta              = RKM Bk7
      , conductorSpecularReflectance = CSpectrum $ SUniform 1.0
      }

case_conductor_1_toXML 
  = actualConductor1 `assertElement` [xmlQQ|
  <bsdf type="conductor">
    <spectrum name="eta" filename="conductorIOR.eta.spd"/> 
    <spectrum name="k" filename="conductorIOR.k.spd"/>
    <string name="extEta" value="bk7"/> 
    <spectrum name="specularReflectance" value="1.0" />
  </bsdf>
  |]


actualRoughConductor 
  = BSDFRoughconductor
  $ RoughConductor
      { roughConductorAlpha        
          = ADAnistrophicAlpha
          $ AnistrophicAlpha
            { anistrophicAlphaU = UniformLuminance 0.05
            , anistrophicAlphaV = UniformLuminance 0.3
            }
                                   
      , roughConductorConductance  = CConductorType Aluminium
      , roughConductorExtEta       = def
      , roughConductorSpecularReflectance = def
      }

case_roughconductor_toXML 
  = actualRoughConductor `assertElement` [xmlQQ|
  <bsdf type="roughconductor">
    <string name="material" value="Al"/> 
    <string name="distribution" value="as"/> 
    <float name="alphaU" value="5.0e-2"/> 
    <float name="alphaV" value="0.3"/>
    <string name="extEta" value="vacuum" />
    <texture name="specularReflectance" type="vertexcolors" />
  </bsdf>
  |]

actualPlastic 
  = BSDFPlastic
  $ Plastic
     { plasticIntIOR              = IOR 1.9
     , plasticExtIOR              = IOR 1.0
     , plasticSpecularReflectance = def
     , plasticDiffuseReflectance  
         = CSpectrum 
         $ SSRGB
         $ RGBLHex
         $ Hex 0x18 0x45 0x5c
     , plasticNonlinear           = def
     }

case_plastic_toXML 
  = actualPlastic `assertElement` [xmlQQ|
  <bsdf type="plastic">
    <srgb name="diffuseReflectance" value="#18455c"/> 
    <float name="intIOR" value="1.9"/>
    <float name="extIOR" value="1.0"/>
    <boolean name="nonlinear" value="false" />
    <texture name="specularReflectance" type="vertexcolors" />
  </bsdf>
  |]

actualRoughPlastic :: BSDF
actualRoughPlastic 
  = BSDFRoughplastic
  $ RoughPlastic 
      { roughPlasticAlpha               
          = ADUniformAlpha 
          $ UniformAlpha Beckmann 
          $ TextureLuminance 
          $ TScale 
          $ ScaleTexture 
              ( CTexture 
              $ TBitmap
              $ Bitmap
                 { bitmapFilename      = "bump.png"
                 , bitmapWrapMode      = Left Repeat
                 , bitmapGamma         = Nothing
                 , bitmapFilterType    = Trilinear
                 , bitmapMaxAnisotropy = 0.0
                 , bitmapCache         = False
                 , bitmapUoffset       = 0.5
                 , bitmapVoffset       = 1.0
                 , bitmapUscale        = 1.5
                 , bitmapVscale        = 2.0
                 , bitmapChannel       = R
                 }
              )
              0.6
             
      , roughPlasticIntIOR              = IOR 1.61
      , roughPlasticExtIOR              = IOR 1.0
      , roughPlasticSpecularReflectance = def
      , roughPlasticDiffuseReflectance  = CSpectrum $ SUniform 0
      , roughPlasticNonlinear           = def
      }

case_roughplastic_toXML 
  = actualRoughPlastic `assertElement` [xmlQQ|
  <bsdf type="roughplastic">
    <string name="distribution" value="beckmann"/>
    <float name="intIOR" value="1.61"/>
    <float name="extIOR" value="1.0"/>
    <texture name="specularReflectance" type="vertexcolors" />
    <spectrum name="diffuseReflectance" value="0.0"/>
    <texture type="scale" name="alpha">
      <texture type="bitmap">
        <string name="filename" value="bump.png"/>
        <string name="wrapMode" value="repeat" />
        <string name="filterType" value="trilinear" />
        <float  name="maxAnisotropy" value="0.0" />
        <boolean   name="cache" value="false" />
        <float  name="uoffset" value="0.5" />
        <float  name="voffset" value="1.0" />  
        <float name="uscale" value="1.5" />
        <float name="vscale" value="2.0" />  
        <string name="channel" value="r" />
      </texture>
      <float name="scale" value="0.6"/> 
    </texture>
    <boolean name="nonlinear" value="false" />
  </bsdf>
  |]

actualCoating :: BSDF
actualCoating 
  = BSDFCoating
  $ Coating
    { coatingIntIOR             = IOR 1.7
    , coatingExtIOR             = IOR 1.0
    , coatingThickness          = 1.0
    , coatingSigmaA             = CSpectrum $ SUniform 0.0
    , coatingSpecularReflection = CSpectrum $ SUniform 1.0
    , coatingChild              
        = CNested 
        $ BSDFRoughconductor
        $ RoughConductor
            { roughConductorAlpha        
                = ADUniformAlpha
                $ UniformAlpha Beckmann $ UniformLuminance 0.1
            , roughConductorConductance  = CConductorType Copper
            , roughConductorExtEta       = def
            , roughConductorSpecularReflectance = def
            }
    }
    
case_coating_toXML 
  = actualCoating `assertElement` [xmlQQ|
    <bsdf type="coating">
      <float name="intIOR" value="1.7"/>
      <float name="extIOR" value="1.0"/>
      <bsdf type="roughconductor">
        <string name="distribution" value="beckmann"/>
        <string name="material" value="Cu"/>
        <float name="alpha" value="0.1"/>
        <string name="extEta" value="vacuum"/>
        <texture name="specularReflectance" type="vertexcolors" />
      </bsdf>
      
      <spectrum name="specularReflection" value="1.0"/>
      <spectrum name="sigmaA" value="0.0"/>
      <float name="thickness" value="1.0"/>
    </bsdf>  
  |]

actualBump 
  = BSDFBumpmap
  $ BumpMap
    { bumpMapMap  = TCheckerboard $ Checkerboard
        { checkerboardColor0  = SUniform 0
        , checkerboardColor1  = SUniform 1
        , checkerboardUoffset = 1
        , checkerboardVoffset = 2
        , checkerboardUscale  = 3
        , checkerboardVscale  = 4
        }
    , bumpMapBSDF = CRef $ Ref "childBSDF"
    }

case_bump_toXML 
  = actualBump `assertElement` [xmlQQ|
  <bsdf type="bumpmap">
    <texture type="checkerboard">
      <float name="uscale" value="3.0"/>
      <float name="voffset" value="2.0"/>
      <spectrum name="color1" value="1.0"/>
      <spectrum name="color0" value="0.0"/>
      <float name="uoffset" value="1.0"/>
      <float name="vscale" value="4.0"/>
    </texture>
    <ref id="childBSDF"/>
  </bsdf>
  |]

actualPhong 
  = BSDFPhong
  $ Phong
      { phongExponent            = UniformLuminance 1.0
      , phongSpecularReflectance = CSpectrum $ SUniform 2.0
      , phongDiffuseReflectance  = CSpectrum $ SUniform 3.0
      }
      
case_phong_toXML 
  = actualPhong `assertElement` [xmlQQ|
    <bsdf type="phong">
      <float name="exponent" value="1.0" />
      <spectrum name="specularReflectance" value="2.0" />
      <spectrum name="diffuseReflectance"   value="3.0" />      
    </bsdf>
  
  |]
-- TODO ward

actualWard 
  = BSDFWard
  $ Ward 
      { wardVariant             = WTWard
      , wardAlphaU              = UniformLuminance 1.0
      , wardAlphaV              = UniformLuminance 2.0
      , wardSpecularReflectance = CSpectrum $ SUniform 3.0
      , wardDiffuseReflectance  = CSpectrum $ SUniform 4.0
      }

case_ward_toXML 
  = actualWard `assertElement` [xmlQQ|
    <bsdf type="ward">
      <string name="variant" value="ward" />
      <float name="alphaU" value="1.0" />
      <float name="alphaV" value="2.0" />
      <spectrum name="specularReflectance" value="3.0" />
      <spectrum name="diffuseReflectance"   value="4.0" />      
    </bsdf>

  |]

actualMixtureBSDF 
  = BSDFMixturebsdf 
  $ MixtureBSDF
      [ (0.7, CRef $ Ref "child0")
      , (0.3, CRef $ Ref "child1")
      ]

case_mixturebsdf_toXML 
  = actualMixtureBSDF `assertElement` [xmlQQ|
  <bsdf type="mixturebsdf">
    <string name="weights" value="0.7, 0.3"/>
    
    <ref id="child0" />
    <ref id="child1" />
  
  </bsdf>
  |]

actualBlendBSDF 
  = BSDFBlendbsdf
  $ BlendBSDF
      { blendBSDFWeight = UniformLuminance 0.5
      , blendBSDFChild0  = CRef $ Ref "child0"
      , blendBSDFChild1  = CRef $ Ref "child1"
      }

case_blendbsdf_toXML 
  = actualBlendBSDF `assertElement` [xmlQQ|
  <bsdf type="blendbsdf">
    <float name="weight" value="0.5" />
    
    <ref id="child0" />
    <ref id="child1" />
    
  </bsdf>
  |]

actualMask 
  = BSDFMask
  $ Mask
      (CSpectrum $ SUniform 1.0)
      $ CRef $ Ref "child"

  

case_mask_toXML 
  = actualMask `assertElement` [xmlQQ|
  <bsdf type="mask">
    <ref id="child" />
    <spectrum name="opacity" value="1.0" />
  </bsdf>
  |]

actualTwosided 
  = BSDFTwosided
  $ Twosided
  $ CRef
  $ Ref "child"    

case_twosided_toXML
  = actualTwosided `assertElement` [xmlQQ|
    <bsdf type="twosided"> 
      <ref id="child" />
    </bsdf>
  |]

--TODO difftrans

actualDifftrans 
  = BSDFDifftrans
  $ Difftrans 
  $ CSpectrum
  $ SUniform 0.5
  
case_difftrans_toXML 
  = actualDifftrans `assertElement` [xmlQQ|
     <bsdf type="difftrans">
        <spectrum name="transmittance" value="0.5" />
     </bsdf>
  |]

actualHK 
  = BSDFHk
  $ HK 
    { hkMaterial  = Hydrogen
    , hkSigmaS    = CSpectrum $ SUniform 1.0
    , hkSigmaA    = CSpectrum $ SUniform 2.0
    , hkSigmaT    = CSpectrum $ SUniform 3.0
    , hkAlbedo    = CSpectrum $ SUniform 4.0
    , hkThickness = 1.0
    , hkChild     = CRef $ Ref "child"
    }

case_hk_toXML 
  = actualHK `assertElement` [xmlQQ|
  <bsdf type="hk">
    <spectrum name="sigmaS" value="1.0"/>
    <spectrum name="sigmaT" value="3.0"/>
    <string name="material" value="hydrogen"/>
    <spectrum name="sigmaA" value="2.0"/>
    <float name="thickness" value="1.0"/>
    <spectrum name="albedo" value="4.0"/>
    <ref id="child"/>  
  </bsdf>
  |]
  
actualIrawan 
  = BSDFIrawan
  $ Irawan 
      { irawanFilename             = "irawan.path"
      , irawanRepeatU              = 1.0
      , irawanRepeatV              = 2.0
      , irawanAdditionalParameters = [("param", Right 3.0)]
      }

case_irawan_toXML 
  = actualIrawan `assertElement` [xmlQQ|
      <bsdf type="irawan">
        <string name="filename" value="irawan.path" />
        <float name="repeatU" value="1.0" />
        <float name="repeatV" value="2.0" />
        <float name="param" value="3.0" />
      </bsdf>
  |]

actualCheckerboard 
  = TCheckerboard
  $ Checkerboard
      { checkerboardColor0  = SUniform 0.0
      , checkerboardColor1  = SUniform 0.5
      , checkerboardUoffset = 1.0
      , checkerboardVoffset = 2.0
      , checkerboardUscale  = 3.0
      , checkerboardVscale  = 4.0
      }
      
      
    
case_checkerboard_toXML 
  = actualCheckerboard `assertElement` [xmlQQ|  
      <texture type="checkerboard">
        <float name="uscale" value="3.0"/>
        <float name="voffset" value="2.0"/>
        <spectrum name="color1" value="0.5"/>
        <spectrum name="color0" value="0.0"/>
        <float name="uoffset" value="1.0"/>
        <float name="vscale" value="4.0"/>
      </texture>
  |]

-- TODO gridtexture

actualGridtexture 
  = TGridtexture
  $ GridTexture
     { gridTextureColor0    = SUniform 0.0
     , gridTextureColor1    = SUniform 0.5
     , gridTextrueLineWidth = 1.0
     , gridTextureUScale    = 2.0
     , gridTextureVScale    = 3.0
     , gridTextureUOffset   = 4.0
     , gridTextureVOffset   = 5.0
     }

case_gridtexture_toXML 
  = actualGridtexture `assertElement` [xmlQQ|
    <texture type="gridtexture">
      <float name="uScale" value="2.0" />
      <float name="lineWidth" value="1.0" />
      <float name="vOffset" value="5.0" />
      <spectrum name="color1" value="0.5" />
      <spectrum name="color0" value="0.0" />
      <float name="uOffset" value="4.0" />
      <float name="vScale" value="3.0" />
    </texture>
  |]

actualScale 
  = TScale
  $ ScaleTexture (CTexture actualGridtexture) 0.5

case_scale_toXML 
  = actualScale `assertElement` [xmlQQ|
  <texture type="scale">
    <float name="scale" value="0.5"/>
    <texture type="gridtexture">
      <float name="uScale" value="2.0" />
      <float name="lineWidth" value="1.0" />
      <float name="vOffset" value="5.0" />
      <spectrum name="color1" value="0.5" />
      <spectrum name="color0" value="0.0" />
      <float name="uOffset" value="4.0" />
      <float name="vScale" value="3.0" />
    </texture>
  </texture>
  |]
  
actualVertexColors = TVertexcolors 
  
case_vertexcolors_toXML 
  = actualVertexColors `assertElement` [xmlQQ|
      <texture type="vertexcolors"/>
  |]

actualWireframe 
  = TWireframe
  $ Wireframe
     { wireframeInteriorColor = SUniform 0.0
     , wireframeEdgeColor     = SUniform 0.5
     , wireframeLineWidth     = 1.0
     , wireframeStepWidth     = 2.0
     }

case_wireframe_toXML 
  = actualWireframe `assertElement` [xmlQQ|
      <texture type="wireframe">
        <spectrum name="interiorColor" value="0.0" />
        <spectrum name="edgeColor" value="0.5" />
        <float name="lineWidth" value="1.0" />
        <float name="stepWidth" value="2.0" />        
      </texture>
  |]

actualBitmap 
  = TBitmap
  $ Bitmap
      { bitmapFilename      = "bitmap.bmp"
      , bitmapWrapMode      = Left Repeat
      , bitmapGamma         = Just 1.0
      , bitmapFilterType    = EWA
      , bitmapMaxAnisotropy = 2.0
      , bitmapCache         = True
      , bitmapUoffset       = 3.0
      , bitmapVoffset       = 4.0
      , bitmapUscale        = 5.0
      , bitmapVscale        = 6.0
      , bitmapChannel       = R 
      }

case_bitmap_toXML 
  = actualBitmap `assertElement` [xmlQQ|
        <texture type="bitmap">
          <float name="gamma" value="1.0"/>
          <float name="uscale" value="5.0"/>
          <string name="filterType" value="ewa"/>
          <string name="channel" value="r"/>
          <float name="voffset" value="4.0"/>
          <float name="maxAnisotropy" value="2.0"/>
          <boolean name="cache" value="true"/>
          <string name="wrapMode" value="repeat"/>
          <float name="uoffset" value="3.0"/>
          <float name="vscale" value="6.0"/>
          <string name="filename" value="bitmap.bmp"/>
        </texture>  
  |]

actualCurvature 
  = TCurvature
  $ Curvature
      { curvatureCurvature = Mean
      , curvatureScale     = 1.0
      }

case_curvature_toXML 
  = actualCurvature `assertElement` [xmlQQ|
      <texture type="curvature">
        <string name="curvature" value="mean"/>
        <float name="scale" value="1.0"/>
      </texture>
  |]


actualDipole = SDipole $ Dipole 
  { dipoleMaterialStyle 
      = MSSigmaAS 
      $ SigmaAS 
          (SRGB $ RGBLTriple $ RGBTriple 1.04 5.6 11.6)
          (SRGB $ RGBLTriple $ RGBTriple 87.2 127.2 143.2) 
  , dipoleScale         = 1.0
  , dipoleIntIOR        = RKM Water
  , dipoleExtIOR        = RKM Air
  , dipoleIrrSamples    = 64
  }

case_dipole_toXML 
  = actualDipole `assertElement` [xmlQQ|
  <subsurface type="dipole">
    <string name="intIOR" value="water"/>
    <string name="extIOR" value="air"/>
    <float name="scale" value="1.0" />
    <rgb name="sigmaS" value="87.200, 127.200, 143.200"/>
    <rgb name="sigmaA" value="1.040, 5.600, 11.600"/>
    <integer name="irrSamples" value="64"/>
  </subsurface>
  |]

actualHomogeneous 
  = MHomogeneous 
  $ Homogeneous 
     { homogeneousMaterialStyle 
          = MSSigmaAS 
          $ SigmaAS
              (SUniform 0.05)
              (SUniform 1)
     , homogeneousScale = 1.0
     , homogeneousPhase = PHg $ HG 0.7
     }

case_homogeneous_toXML 
  = actualHomogeneous `assertElement` [xmlQQ|
  <medium type="homogeneous"> 
    <spectrum name="sigmaS" value="1.0"/> 
    <spectrum name="sigmaA" value="5.0e-2"/>
    <float name="scale" value="1.0" />
    
    <phase type="hg">
      <float name="g" value="0.7"/>
    </phase> 
  </medium>
  |]

actualHeterogeneous 
  = MHeterogeneous
  $ Heterogeneous
      { heterogeneousMethod      = Simpson
      , heterogeneousDensity     
          = VGridvolume
          $ GridVolume 
              { gridVolumeFilename = "frame_0150.vol"
              , gridVolumeSendData = SendAcrossNetwork
              , gridVolumeToWorld  = mempty
              , gridVolumeMin      = Point 0 0 0
              , gridVolumeMax      = Point 0 0 0
              }
      , heterogeneousAlbedo      = VConstvolume  
                                 $ CVSpectrum
                                 $ SUniform 0.9
      , heterogeneousOrientation 
          = VVolcache
          $ VolCache
              { volCacheBlockSize   = 1
              , volCacheVoxelWidth  = 2.0
              , volCacheMemoryLimit = 3
              , volCacheToWorld     = mempty
              , volCacheChild       = CRef $ Ref "volCacheChild"
              }
             
      , heterogeneousScale       = 200
      , heterogeneousPhase        = CNested $ PIsotropic
      }

case_heterogeneous_toXML 
  = actualHeterogeneous `assertElement` [xmlQQ|
<medium type="heterogeneous">
  <phase type="isotropic"/>
  <float name="scale" value="200.0"/>
  <string name="method" value="simpson"/>
  <volume name="density" type="gridvolume">
    <point name="max" z="0.0" x="0.0" y="0.0"/>
    <boolean name="sendData" value="true"/>
    <point name="min" z="0.0" x="0.0" y="0.0"/>
    <string name="filename" value="frame_0150.vol"/>
    <transform name="toWorld"/>
  </volume>
  <volume name="orientation" type="volcache">
    <integer name="blockSize" value="1"/>
    <integer name="memoryLimit" value="3"/>
    <ref name="child" id="volCacheChild"/>
    <float name="voxelWidth" value="2.0"/>
    <transform name="toWorld"/>
  </volume>
  <volume name="albedo" type="constvolume">
    <spectrum name="value" value="0.9"/>
  </volume>
</medium>

  |]

case_isotropic_toXML 
  = PIsotropic `assertElement` [xmlQQ|<phase type="isotropic"/>|]

case_hg_toXML 
  = PHg (HG 0.5) `assertElement` [xmlQQ|
    <phase type="hg"> 
       <float name="g" value="0.5" /> 
    </phase>
  |]

case_raleigh_toXML
  = PRayleigh `assertElement` [xmlQQ|<phase type="rayleigh"/>|]

case_kkay_toXML
  = PKay `assertElement` [xmlQQ|<phase type="kay"/>|]

case_microflake_toXML
  = PMicroflake (MicroFlake 1.0) `assertElement` [xmlQQ|
    <phase type="microflake">
      <float name="stddev" value="1.0" />
    </phase>
  |]

actualMixturephase 
  = PMixturephase
  $ MixturePhase
      [ (0.7, CNested PRayleigh)
      , (0.3, CNested PKay)
      ]

case_mixturephase_toXML
  = actualMixturephase `assertElement` [xmlQQ|
    <phase type="mixturephase">
        <string name="weights" value="0.7, 0.3"/>
        
        <phase type="kay"/>
        <phase type="rayleigh"/>
    </phase>
  |]

actualConstVolume0 
  = VConstvolume  
  $ CVDouble 1.0

case_constvolume_0_toXML 
  = actualConstVolume0 `assertElement` [xmlQQ|
  <volume type="constvolume" >
    <float name="value" value="1.0"/>
  </volume>
  |]

actualConstVolume1 
  = VConstvolume  
  $ CVSpectrum
  $ SRGB
  $ RGBLTriple
  $ RGBTriple 0.9 0.9 0.7

case_constvolume_1_toXML 
  = actualConstVolume1 `assertElement` [xmlQQ|
  <volume type="constvolume" >
    <rgb name="value" value="0.900, 0.900, 0.700"/>
  </volume>
  |]

actualConstVolume2
  = VConstvolume  
  $ CVVector
  $ Vector 0 1 0

case_constvolume_2_toXML 
  = actualConstVolume2 `assertElement` [xmlQQ|
  <volume type="constvolume" >
    <vector name="value" x="0.0" y="1.0" z="0.0"/> 
  </volume>
  |]

-- TODO gridvolume

actualGridVolume 
  = VGridvolume
  $ GridVolume 
    { gridVolumeFilename = "frame_0150.vol"
    , gridVolumeSendData = SendAcrossNetwork
    , gridVolumeToWorld  = mempty
    , gridVolumeMin      = Point 0 0 0
    , gridVolumeMax      = Point 0 0 0
    }

case_gridvolume_toXML 
  = actualGridVolume `assertElement` [xmlQQ|
      <volume type="gridvolume">
        <point name="max" z="0.0" x="0.0" y="0.0"/>
        <boolean name="sendData" value="true"/>
        <point name="min" z="0.0" x="0.0" y="0.0"/>
        <string name="filename" value="frame_0150.vol"/>
        <transform name="toWorld"/>
      </volume>
  |]

actualVolCache 
  = VVolcache
  $ VolCache
      { volCacheBlockSize   = 1
      , volCacheVoxelWidth  = 2.0
      , volCacheMemoryLimit = 3
      , volCacheToWorld     = mempty
      , volCacheChild       = CRef $ Ref "volCacheChild"
      }
    
case_volcache_toXML 
  = actualVolCache `assertElement` [xmlQQ|
    <volume type="volcache">
      <integer name="blockSize" value="1"/>
      <integer name="memoryLimit" value="3"/>
      <ref name="child" id="volCacheChild"/>
      <float name="voxelWidth" value="2.0"/>
      <transform name="toWorld"/>
    </volume>
  |]

actualPointLight 
  = EPoint
  $ PointLight
     { pointLightToWorld        = mempty
     , pointLightPosition       = Point 1 2 3
     , pointLightIntensity      = SUniform 1.0
     , pointLightSamplingWeight = 2.0
     }
    
case_pointlight_toXML 
  = actualPointLight `assertElement` [xmlQQ|
    <emitter type="point">
      <spectrum name="intensity" value="1.0"/>
      <float name="samplingWeight" value="2.0"/>
      <transform name="toWorld"/>
      <point name="position" z="3.0" x="1.0" y="2.0"/>
    </emitter>
  |]

actualAreaLight
  = SShapeLeaf 
  $ ShapeLeaf 
     { shapeLeafLeaf       
         = Left 
         $ SimpleShapeLeaf
            (STCube def)
            (CRef $ Ref "material")
     , shapeLeafToWorld    = mempty
     , shapeLeafMediumPair = Nothing
     , shapeLeafEmitter    
        = Just 
        $ CNested 
        $ EArea
        $ AreaLight 
            { areaLightRadiance       = SUniform 1.0
            , areaLightSamplingWeight = 2.0
            }
      }

case_area_toXML
  = actualAreaLight `assertElement` [xmlQQ|
    <shape type="cube"> 
      <transform name="toWorld"/>
      <ref id="material"/>
      <emitter type="area">
        <spectrum name="radiance" value="1.0"/>
        <float name="samplingWeight" value="2.0"/>
      </emitter>
      <boolean name="flipNormals" value="false"/>
    </shape>
  |]

actualSpot 
  = ESpot
  $ SpotLight
       { spotLightToWorld        
            = TRegular
            $ RegularTransform 
                [ TCLookat 
                    $ Lookat 
                        { origin = Point 1 1 1
                        , target = Point 1 2 1
                        , up     = Vector 1 0 0
                        }
                ]
       , spotLightIntensity      = SUniform 1.0
       , spotLightCutoffAngle    = 2.0
       , spotLightBeamWidth      = 3.0
       , spotLightTexture        = TVertexcolors
       , spotLightSamplingWeight = 4.0
       }

case_spot_toXML 
  = actualSpot `assertElement` [xmlQQ|
  <emitter type="spot">
    <transform name="toWorld">
      <lookat origin="1.0, 1.0, 1.0" up="1.0, 0.0, 0.0" target="1.0, 2.0, 1.0"/> 
    </transform>
    
    <float name="cutoffAngle" value="2.0"/>
    <texture name="texture" type="vertexcolors"/>
    <spectrum name="intensity" value="1.0"/>
    <float name="beamWidth" value="3.0"/>
    <float name="samplingWeight" value="4.0"/>
  </emitter>
  |]

actualDirectionLight 
  = EDirectional
  $ DirectionalLight
     { directionalLightToWorld        = mempty
     , directionalLightDirection      = Vector 1 0 0
     , directionalLightIrradiance     = SUniform 1.0
     , directionalLightSamplingWeight = 2.0
     }

case_directional_toXML 
  = actualDirectionLight `assertElement` [xmlQQ|
      <emitter type="directional">
          <transform name="toWorld" />
          <vector name="direction" x="1.0" y="0.0" z="0.0" />
          <spectrum name="irradiance" value="1.0" />
          <float name="samplingWeight" value="2.0" />
      </emitter>
  |]

actualCollimated 
  = ECollimated
  $ Collimated 
      { collimatedToWorld = mempty
      , collimatedPower   = SUniform 1.0
      , collimatedSamplingWeight = 0.0
      }

case_collimated_toXML 
  = actualCollimated `assertElement` [xmlQQ|
      <emitter type="collimated">
          <transform name="toWorld" />
          <spectrum name="power" value="1.0" />
          <float name="samplingWeight" value="0.0" />
      </emitter>
      
  |]

actualSky 
  = ESky
  $ Sky 
      { skyTurbidity = 1.0
      , skyAlbedo    = SUniform 2.0
      , skyYear      = 3
      , skyMonth     = 4
      , skyDay       = 5
      , skyHour      = 6
      , skyMinute    = 7
      , skySecond    = 8
      , skyLatitude  = 9.0
      , skyLongitude = 10.0
      , skyTimezone  = 11.0
      , skySunDirection = Vector 
            { vectorX = 12.0
            , vectorY = 13.0
            , vectorZ = 14.0
            }
      , skyStretch        = 15.0
      , skyResolution     = 16
      , skyScale          = 17.0
      , skySamplingWeight = 18.0
      , skyToWorld        = mempty
      }

case_sky_toXML 
  = actualSky `assertElement` [xmlQQ|
<emitter type="sky">
  <float name="turbidity" value="1.0"/>
  <spectrum name="albedo" value="2.0"/>
  <integer name="year" value="3"/>
  <integer name="month" value="4"/>
  <integer name="day" value="5"/>
  <float name="hour" value="6.0"/>
  <float name="minute" value="7.0"/>
  <float name="second" value="8.0"/>
  <float name="latitude" value="9.0"/>
  <float name="longitude" value="10.0"/>
  <float name="timezone" value="11.0"/>
  <vector name="sunDirection" x="12.0" y="13.0" z="14.0"/>
  <float name="stretch" value="15.0"/>
  <integer name="resolution" value="16"/>
  <float name="scale" value="17.0"/>
  <float name="samplingWeight" value="18.0"/>
  <transform name="toWorld" />
</emitter>
  |]

actualSun 
  = ESun 
  $ Sun
       { sunTurbidity    = 1.0
       , sunYear         = 2
       , sunMonth        = 3
       , sunDay          = 4
       , sunHour         = 5
       , sunMinute       = 6
       , sunSecond       = 7
       , sunLatitude     = 8
       , sunLongitude    = 9
       , sunTimezone     = 10
       , sunResolution   = 11
       , sunScale        = 12
       , sunRadiusScale  = 13
       , sunSamplingWeight = 14
       }

case_sun_toXML 
  = actualSun `assertElement` [xmlQQ|
    <emitter type="sun">
      <float name="turbidity" value="1.0"/>
      <integer name="year" value="2"/>
      <integer name="month" value="3"/>
      <integer name="day" value="4"/>
      <float name="hour" value="5.0"/>
      <float name="minute" value="6.0"/>
      <float name="second" value="7.0"/>
      <float name="latitude" value="8.0"/>
      <float name="longitude" value="9.0"/>
      <float name="timezone" value="10.0"/>
      <integer name="resolution" value="11"/>
      <float name="scale" value="12.0"/>
      <float name="radiusScale" value="13.0"/>
      <float name="samplingWeight" value="14.0"/>
    </emitter>
  |]
  
actualSunSky 
  = ESunsky
  $ Sunsky 
      { sunskyTurbidity       = 1.0
       , sunskyAlbedo         = SUniform 2.0
       , sunskyYear           = 3
       , sunskyMonth          = 4
       , sunskyDay            = 5
       , sunskyHour           = 6
       , sunskyMinute         = 7
       , sunskySecond         = 8
       , sunskyLatitude       = 9
       , sunskyLongitude      = 10
       , sunskyTimezone       = 11
       , sunskySunDirection   = Vector 
             { vectorX = 12.0
             , vectorY = 13.0
             , vectorZ = 14.0
             }
       , sunskyStretch        = 15
       , sunskyResolution     = 16
       , sunskySunScale       = 17
       , sunskySkyScale       = 18
       , sunskySunRadiusScale = 19
       }
       
case_sunsky_toXML 
  = actualSunSky `assertElement` [xmlQQ|
      <emitter type="sunsky">
        <float name="turbidity" value="1.0"/>
        <spectrum name="albedo" value="2.0"/>
        <integer name="year" value="3"/>
        <integer name="month" value="4"/>
        <integer name="day" value="5"/>
        <float name="hour" value="6.0"/>
        <float name="minute" value="7.0"/>
        <float name="second" value="8.0"/>
        <float name="latitude" value="9.0"/>
        <float name="longitude" value="10.0"/>
        <float name="timezone" value="11.0"/>
        <vector name="sunDirection" x="12.0" y="13.0" z="14.0"/>
        <float name="stretch" value="15.0"/>
        <integer name="resolution" value="16"/>
        <float name="sunScale" value="17.0"/>
        <float name="skyScale" value="18.0"/>
        <float name="sunRadiusScale" value="19.0"/>
      </emitter>
  |]

actualEnvmap 
  = EEnvmap
  $ Envmap
     { envmapFilename       = "filepath"
     , envmapScale          = 1
     , envmapToWorld        = mempty
     , envmapGamma          = 2
     , envmapCache          = True
     , envmapSamplingWeight = 3
     }

case_envmap_toXML 
  = actualEnvmap `assertElement` [xmlQQ|
    <emitter type="envmap">
      <string name="filename" value="filepath" />
      <float name="scale" value="1.0" />
      <transform name="toWorld" />
      <float name="gamma" value="2.0" />
      <boolean name="cache" value="true" />
      <float name="samplingWeight" value="3.0" />
    </emitter>
  |]

actualConstantEmitter 
  = EConstant
  $ Constant 
       { constantRadiance       = SUniform 1.0
       , constantSamplingWeight = 2.0
       }

case_constant_toXML 
  = actualConstantEmitter `assertElement` [xmlQQ|
      <emitter type="constant">
        <spectrum name="radiance" value="1.0" />
        <float name="samplingWeight" value="2.0" />
      </emitter>
  |]

actualPerspective 
  = Sensor Nothing Nothing 
  $ STPerspective
  $ Perspective
       { perspectiveToWorld      = mempty
       , perspectiveFocalLength  = 1
       , perspectiveFov          = 2
       , perspectiveFovAxis      = FOVTX
       , perspectiveShutterOpen  = 3
       , perspectiveShutterClose = 4
       , perspectiveNearClip     = 5
       , perspectiveFarClip      = 6
       }

case_perspective_toXML 
  = actualPerspective `assertElement` [xmlQQ|
      <sensor type="perspective" >
          <transform name="toWorld" />
          <float name="focalLength" value="1.0" />
          <float name="fov" value="2.0" />
          <string name="fovAxis" value="tx" />
          <float name="shutterOpen" value="3.0" />
          <float name="shutterClose" value="4.0" />
          <float name="nearClip" value="5.0" />
          <float name="farClip" value="6.0" />
      </sensor>
  |]

actualThinLens 
  = Sensor Nothing Nothing 
  $ STThinlens
  $ Thinlens
      { thinlensToWorld         = mempty
      , thinlensAperatureRadius = 1
      , thinlensFocusDistance   = 2
      , thinlensFocalLength     = 3
      , thinlensFOV             = 4
      , thinlensFOVAxis         = FOVTY
      , thinlensShutterOpen     = 5
      , thinlensShutterClose    = 6
      , thinlensNearClip        = 7
      , thinlensFarClip         = 8
      }

case_thinlens_toXML 
  = actualThinLens `assertElement` [xmlQQ|
    <sensor type="thinlens">
      <transform name="toWorld" />
      <float name="aperatureRadius" value="1.0" />
      <float name="focusDistance" value="2.0" />
      <string name="focalLength" value="3.0mm" />
      <float name="fov" value="4.0" />
      <string name="fovaxis" value="ty" />
      <float name="shutterOpen" value="5.0" />
      <float name="shutterClose" value="6.0" />
      <float name="nearClip" value="7.0" />
      <float name="farClip" value="8.0" />
    </sensor>
  |]

actualOrthographic 
  = Sensor Nothing Nothing 
  $ STOrthographic
  $ Orthographic
      { orthographicToWorld      = mempty
      , orthographicShutterOpen  = 1.0
      , orthographicShutterClose = 2.0
      , orthographicNearClip     = 3.0
      , orthographicFarClip      = 4.0   
      }

case_orthographic_toXML 
  = actualOrthographic `assertElement` [xmlQQ|
      <sensor type="orthographic">
          <transform name="toWorld" />
          <float name="shutterOpen" value="1.0" />
          <float name="shutterClose" value="2.0" />
          <float name="nearClip" value="3.0" />
          <float name="farClip" value="4.0" />
      </sensor>
  |]

actualTelecentric 
  = Sensor Nothing Nothing 
  $ STTelecentric
  $ Telecentric
     { telecentricToWorld         = mempty
     , telecentricAperatureRadius = 1
     , telecentricFocusDistance   = 2
     , telecentricShutterOpen     = 3
     , telecentricShutterClose    = 4
     , telecentricNearClip        = 5
     , telecentricFarClip         = 6
     }

case_telecentric_toXML 
  = actualTelecentric `assertElement` [xmlQQ|
      <sensor type="telecentric">
          <transform name="toWorld" />
          <float name="aperatureRadius" value="1.0" />
          <float name="focusDistance" value="2.0" />
          <float name="shutterOpen" value="3.0" />
          <float name="shutterClose" value="4.0" />
          <float name="nearClip" value="5.0" />
          <float name="farClip" value="6.0" />
      </sensor>
  |]

actualSpherical 
  = Sensor Nothing Nothing 
  $ STSpherical
  $ Spherical
      { sphericalToWorld      = mempty
      , sphericalShutterOpen  = 1.0
      , sphericalShutterClose = 2.0
      }

case_spherical_toXML 
  = actualSpherical `assertElement` [xmlQQ|
    <sensor type="spherical">
        <transform name="toWorld" />
        <float name="shutterOpen" value="1.0" />
        <float name="shutterClose" value="2.0" />
    </sensor>
  |]

actualIrradianceMeter 
  = Sensor 
      { sensorFilm = Just 
          ( FMfilm 
            ( MFilm 
              { mfilmWidth = 0
              , mfilmHeight = 0
              , mfilmCrop = Nothing
              , mfilmFileFormat = MFFFMatlab
              , mfilmDigits = 0
              , mfilmVariable = ""
              , mfilmPixelFormat = PFLuminance
              , mfilmHighQualityEdges = False
              , mfilmRFilter = RFBox
              }
            )
          )
        , sensorSampler = Just 
            ( SSobol 
              ( Sobol 
                  { sobolSampleCount = 0
                  , sobolScramble = 0
                  }
              )
            )
        , sensorType = STIrradiancemeter 
          ( IrradianceMeter 
              { irradianceMeterShutterOpen  = 1.0
              , irradianceMeterShutterClose = 2.0
              }
          )
        }
    
case_irradiancemeter_toXML 
  = actualIrradianceMeter `assertElement` [xmlQQ|
      <sensor type="irradiancemeter">
        <film type="mfilm">
          <integer name="digits" value="0"/>
          <integer name="height" value="0"/>
          <rfilter type="box"/>
          <integer name="width" value="0"/>
          <string name="variable" value=""/>
          <boolean name="highQualityEdges" value="false"/>
          <string name="fileFormat" value="matlab" />
          <string name="pixelFormat" value="luminance"/>
        </film>
        <sampler type="sobol">
          <integer name="sampleCount" value="0"/>
          <integer name="scramble" value="0"/>
        </sampler>
        <float name="shutterOpen" value="1.0"/>
        <float name="shutterClose" value="2.0"/>
      </sensor>
  |]
  


actualRadianceMeter
  = Sensor Nothing Nothing
  $ STRadiancemeter 
  $ RadianceMeter
     { radianceMeterToWorld      = mempty
     , radianceMeterShutterOpen  = 1
     , radianceMeterShutterClose = 2
     }
  
case_radiancemeter_toXML 
  = actualRadianceMeter `assertElement` [xmlQQ|
  <sensor type="radiancemeter">
    <transform name="toWorld" />
    <float name="shutterOpen"  value="1.0" />
    <float name="shutterClose" value="2.0" />
  </sensor>
  |]

actualFluenceMeter 
  = Sensor Nothing Nothing
  $ STFluencemeter
  $ FluenceMeter
       { fluenceMeterToWorld      = mempty
       , fluenceMeterShutterOpen  = 1
       , fluenceMeterShutterClose = 2
       }

case_fluencemeter_toXML 
  = actualFluenceMeter `assertElement` [xmlQQ|
  <sensor type="fluencemeter">
    <transform name="toWorld" />
    <float name="shutterOpen"  value="1.0" />
    <float name="shutterClose" value="2.0" />
  </sensor>
  |]
  
actualPerspectiveDist 
  = Sensor Nothing Nothing
  $ STPerspective_rdist
  $ PerspectiveRDist
     { perspectiveRDistToWorld      = mempty
     , perspectiveRDistkc           = PolyTwoAndFour 1 2
     , perspectiveRDistFocalLength  = 3
     , perspectiveRDistFov          = 4
     , perspectiveRDistFovAxis      = FOVTDiagonal
     , perspectiveRDistShutterOpen  = 5
     , perspectiveRDistShutterClose = 6
     , perspectiveRDistNearClip     = 7
     , perspectiveRDistFarClip      = 8
     }

case_perspective_rdist_toXML 
  = actualPerspectiveDist `assertElement` [xmlQQ|
  <sensor type="perspective_rdist">
    <transform name="toWorld" />
    <string name="kc" value="1.0,2.0" />
    <float name="focalLength" value="3.0" />
    <float name="fov" value="4.0" />
    <string name="fovAxis" value="diagonal" />
    <float name="shutterOpen" value="5.0" />
    <float name="shutterClose" value="6.0" />
    <float name="nearClip" value="7.0" />
    <float name="farClip" value="8.0" />
  </sensor>
  |]

actualAmbientOcclusion 
  = IAo
  $ AmbientOcclusion 
    { ambientOcclusionShadingSamples = 1
    , ambientOcclusionRayLength     = 2
    }
    
case_ao_toXML 
  = actualAmbientOcclusion `assertElement` [xmlQQ|
      <integrator type="ao">
        <integer name="shadingSamples" value="1" />
        <float name="rayLength" value="2.0" />
      </integrator>
  |]

actualDirect 
  = IDirect
  $ Direct
     { directShadingSamples = 1
     , directEmitterSamples = 2
     , directBsdfSamples    = 3
     , directStrictNormals  = Strict
     , directHideEmitters   = Visible
     }

case_direct_toXML
  = actualDirect `assertElement` [xmlQQ|
    <integrator type="direct">
      <integer name="shadingSamples" value="1" />
      <integer name="emitterSamples" value="2" />
      <integer name="bsdfSamples" value="3" />
      <boolean name="strictNormals" value="true" />
      <boolean name="hideEmitters" value="true" />
    </integrator>
  
  |]


actualPath 
  = IPath
  $ Path
      { pathMaxDepth      = 1
      , pathRrDepth       = 2
      , pathStrictNormals = Strict
      , pathHideEmitters  = Visible
      }

case_path_toXML 
  = actualPath `assertElement` [xmlQQ|
    <integrator type="path">
      <integer name="maxDepth" value="1" />
      <integer name="rrDepth" value="2" />
      <boolean name="strictNormals" value="true" />
      <boolean name="hideEmitters" value="true" />
    </integrator>
  
  |]

actualVolPathSimple 
  = IVolpath_simple
  $ VolPathSimple
     { volPathSimpleMaxDepth      = 1
     , volPathSimpleRrDepth       = 2
     , volPathSimpleStrictNormals = Strict
     , volPathSimpleHideEmitters  = Visible
     }

case_volpath_simple_toXML 
  = actualVolPathSimple `assertElement` [xmlQQ|
  <integrator type="volpath_simple">
    <integer name="maxDepth" value="1" />
    <integer name="rrDepth" value="2" />
    <boolean name="strictNormals" value="true" />
    <boolean name="hideEmitters" value="true" />
  </integrator>
  
  |]

actualVolPath
  = IVolpath
  $ VolPath
     { volPathMaxDepth      = 1
     , volPathRrDepth       = 2
     , volPathStrictNormals = Strict
     , volPathHideEmitters  = Visible
     }

case_volpath_toXML
  = actualVolPath `assertElement` [xmlQQ|
    <integrator type="volpath">
      <integer name="maxDepth" value="1" />
      <integer name="rrDepth" value="2" />
      <boolean name="strictNormals" value="true" />
      <boolean name="hideEmitters" value="true" />
    </integrator>
  
  |]

actualBdpt 
  = IBdpt
  $ BDPT
      { bdptMaxDepth     = 0
      , bdptLightImage   = ConnectToCamera
      , bdptSampleDirect = DirectSampling
      , bdptRrDepth      = 1
      }

case_bdpt_toXML
  = actualBdpt `assertElement` [xmlQQ|
    <integrator type="bdpt">
      <integer name="maxDepth" value="0" />
      <boolean name="lightImage" value="true" />
      <boolean name="sampleDirect" value="true" />
      <integer name="rrDepth" value="1" />
    </integrator>

  |]
  
actualPhotonMapper 
  = IPhotonmapper
  $ PhotonMapper
     { photonMapperDirectSamples       = 1
     , photonMapperGlossySamples       = 2
     , photonMapperMaxDepth            = 3
     , photonMapperGlobalPhotons       = 4
     , photonMapperCausticPhotons      = 5
     , photonMapperVolumePhotons      = 6
     , photonMapperGlobalLookupRadius  = 7
     , photonMapperCausticLookupRadius = 8
     , photonMapperLookupSize          = 9
     , photonMapperGranularity         = 10
     , photonMapperHideEmitters        = False
     , photonMapperRrDepth             = 11
     }
  
case_photonmapper_toXML 
  = actualPhotonMapper `assertElement` [xmlQQ|
    <integrator type="photonmapper">
      <integer name="directSamples"  value="1"      />
      <integer name="glossySamples"  value="2"      />
      <integer name="maxDepth"       value="3"      />
      <integer name="globalPhotons"  value="4"      />
      <integer name="causticPhotons" value="5"      />
      <integer name="volumePhotons"  value="6"      />
      <float   name="globalLookupRadius"   value="7.0"    />
      <float   name="causticLookupRadius"  value="8.0"    />
      <integer name="lookupSize"     value="9"      />
      <integer name="granularity"    value="10"     />
      <boolean name="hideEmitters"   value="false"  />
      <integer name="rrDepth"        value="11"     />
    </integrator>
  
  |]
  
actualPpm 
  = IPpm 
  $ PPM 
      { ppmMaxDepth      = 1
      , ppmPhotonCount   = 2
      , ppmInitialRadius = 3
      , ppmAlpha         = 4
      , ppmGranularity   = 5
      , ppmRrDepth       = 6
      , ppmMaxPasses     = 7
      }


case_ppm_toXML
  = actualPpm `assertElement` [xmlQQ|
    <integrator type="ppm">
      <integer name="maxDepth" value="1" />
      <integer name="photonCount" value="2" />
      <float name="initialRadius" value="3.0" />
      <float name="alpha" value="4.0" />
      <integer name="granularity" value="5" />
      <integer name="rrDepth" value="6" />
      <integer name="maxPasses" value="7" />
    </integrator>

  |]
  
actualSppm 
  = ISppm
  $ SPPM
      { sppmMaxDepth      = 1
      , sppmPhotonCount   = 2
      , sppmInitialRadius = 3
      , sppmAlpha         = 4
      , sppmGranularity   = 5
      , sppmRrDepth       = 6
      , sppmMaxPasses     = 7
      }

case_sppm_toXML
  = actualSppm `assertElement` [xmlQQ|
    <integrator type="sppm">
      <integer name="maxDepth" value="1" />
      <integer name="photonCount" value="2" />
      <float name="initialRadius" value="3.0" />
      <float name="alpha" value="4.0" />
      <integer name="granularity" value="5" />
      <integer name="rrDepth" value="6" />
      <integer name="maxPasses" value="7" />
    </integrator>

  |]
  
actualPssmlt 
  = IPssmlt
  $ PSSMLT
     { pssmltBidirectional    = True
     , pssmltMaxDepth         = 1
     , pssmltDirectSamples    = 2
     , pssmltRrDepth          = 3
     , pssmltLuminanceSamples = 4
     , pssmltTwoStage         = True
     , pssmltPlarge           = 5
     }

case_pssmlt_toXML
   = actualPssmlt `assertElement` [xmlQQ|
     <integrator type="pssmlt">
       <boolean name="bidirectional"    value="true" />
       <integer name="maxDepth"         value="1" />
       <integer name="directSamples"    value="2" />
       <integer name="rrDepth"          value="3" />
       <integer name="luminanceSamples" value="4" />
       <boolean name="twoStage"         value="true" />
       <float   name="plarge"           value="5.0" />
     </integrator>
   |]
   
actualMlt 
  = IMlt
  $ MLT
      { mltMaxDepth               = 1
      , mltDirectSamples          = 2
      , mltLuminanceSamples       = 3
      , mltTwoStage               = True
      , mltBidirectionalMutation  = False
      , mltLensPerturbation       = True
      , mltMultiChainPertubation  = False
      , mltCausticPertubation     = True
      , mltManifoldPertubation    = False
      , mltLambda                 = 4
      }

case_mlt_toXML
   = actualMlt `assertElement` [xmlQQ|
     <integrator type="mlt">
       <integer name="maxDepth"              value="1" />
       <integer name="directSamples"         value="2" />
       <integer name="luminanceSamples"      value="3" />
       <boolean name="twoStage"              value="true" />
       <boolean name="bidirectionalMutation" value="false" />
       <boolean name="lensPerturbation"      value="true" />
       <boolean name="multiChainPertubation" value="false" />
       <boolean name="causticPertubation"    value="true" />
       <boolean name="manifoldPertubation"   value="false" />
       <float   name="lambda"                value="4.0" />
     </integrator>
   |]

actualErpt
  = IErpt
  $ ERPT
      { erptMaxDepth               = 1
      , erptNumChains              = 2
      , erptMaxChains              = 3
      , erptChainLength            = 4
      , erptDirectSamples          = 5
      , erptLensPerturbation       = True
      , erptMultiChainPerturbation = False
      , erptCausticPerturbation    = True
      , erptManifoldPerturbation   = False
      , erptLambda                 = 6
      }

case_erpt_toXML
   = actualErpt `assertElement` [xmlQQ|
     <integrator type="erpt">
       <integer name="maxDepth"               value="1"     />
       <float   name="numChains"              value="2.0"   />
       <float   name="maxChains"              value="3.0"   />
       <integer name="chainLength"            value="4"     />
       <integer name="directSamples"          value="5"     />
       <boolean name="lensPerturbation"       value="true"  />
       <boolean name="multiChainPerturbation" value="false" />
       <boolean name="causticPerturbation"    value="true"  />
       <boolean name="manifoldPerturbation"   value="false" />
       <float   name="lambda"                 value="6.0"   />
     </integrator>
   |]

actualPtracer 
  = IPtracer
  $ PTracer
      { ptracerMaxDepth    = 1
      , ptracerRrDepth     = 2
      , ptracerGranularity = 3
      , ptracerBruteForce  = False
      }

case_ptracer_toXML 
  = actualPtracer `assertElement` [xmlQQ|
     <integrator type="ptracer">
       <integer name="maxDepth"    value="1"     />
       <integer name="rrDepth"     value="2"     />
       <integer name="granularity" value="3"     />
       <boolean name="bruteForce"  value="false" />
     </integrator>
   |]

actualAdaptive
  = IAdaptive
  $ Adaptive
      { adaptiveMaxError        = 1
      , adaptivePValue          = 2
      , adaptiveMaxSampleFactor = 3
      }

case_adaptive_toXML 
  = actualAdaptive `assertElement` [xmlQQ|
     <integrator type="adaptive">
       <float   name="maxError"        value="1.0"     />
       <float   name="pValue"          value="2.0"     />
       <integer name="maxSampleFactor" value="3"     />
     </integrator>
   |]

actualVpl 
  = IVpl
  $ VPL
      { vplMaxDepth            = 1
      , vplShadowMapResolution = 2
      , vplClamping            = 3
      }

case_vpl_toXML 
  = actualVpl `assertElement` [xmlQQ|
     <integrator type="vpl">
       <integer name="maxDepth"            value="1"   />
       <integer name="shadowMapResolution" value="2"   />
       <float   name="clamping"            value="3.0" />
     </integrator>
   |]

actualIRRCache
  = IIrrcache
  $ IRRCache 
     { irrCacheResolution        = 1
     , irrCacheQuality           = 2
     , irrCacheGradients         = True
     , irrCacheClampNeighbor      = False
     , irrCacheClampScreen       = True
     , irrCacheOverture          = False
     , irrCacheQualityAdjustment = 3
     , irrCacheIndirectOnly      = False
     , irrCacheDebug             = True
     }

case_irrcache_toXML 
  = actualIRRCache `assertElement` [xmlQQ|
     <integrator type="irrcache"> 
        <integer name="resolution"         value="1"     />
        <float   name="quality"            value="2.0"   />
        <boolean name="gradients"          value="true"  />
        <boolean name="clampNeighbor"      value="false" />
        <boolean name="clampScreen"        value="true"  />
        <boolean name="overture"           value="false" />
        <float   name="qualityAdjustment"  value="3.0"   />
        <boolean name="indirectOnly"       value="false" />
        <boolean name="debug"              value="true"  />
     </integrator>
  |]

actualField = SIField $ Field FTPosition $ UniformLuminance 1

case_field_toXML 
  = actualField `assertElement` [xmlQQ|
    <integrator type="field">
      <string name="field"    value="position"/>
      <float name="undefined" value="1.0"     />
    </integrator>
  |]

actualMultichannel 
  = IMultichannel
  $ MultiChannel [SIField $ Field FTPosition $ UniformLuminance 1]
  
  
case_multichannel_toXML
  = actualMultichannel `assertElement` [xmlQQ|
    <integrator type="multichannel">
      <integrator type="field">
        <string name="field" value="position"/> 
        <float name="undefined" value="1.0"     />
      </integrator>
    </integrator>
  |]

actualIndependent = SIndependent $ Independent 
  { independentSampleCount = 1
  }

case_independent 
  = actualIndependent `assertElement` [xmlQQ|
      <sampler type="independent">
        <integer name="sampleCount" value="1" />
      </sampler>
  |]

actualStratified 
  = SStratified
  $ Stratified
      { stratifiedSampleCount = 1
      , stratifiedDimension   = 2
      }

case_stratified 
  = actualStratified `assertElement` [xmlQQ|
      <sampler type="stratified">
        <integer name="sampleCount" value="1" />
        <integer name="dimension"   value="2" />
      </sampler>
  |]

actualLdsampler 
  = SLdsampler
  $ LDSampler
     { ldSamplerSampleCount = 1
     , ldSamplerDimension   = 2
     }

case_ldsampler
   = actualLdsampler `assertElement` [xmlQQ|
       <sampler type="ldsampler">
         <integer name="sampleCount" value="1" />
         <integer name="dimension"   value="2" />
       </sampler>
   |]

actualHalton 
  = SHalton
  $ Halton
     { haltonSampleCount = 1
     , haltonScramble    = 2
     }

case_halton 
  = actualHalton `assertElement` [xmlQQ|
    <sampler type="halton">
      <integer name="sampleCount" value="1" />
      <integer name="scramble"    value="2" />
    </sampler>
  |]
  
actualHammersley 
  = SHammersley
  $ Hammersley
     { hammersleySampleCount = 1
     , hammersleyScramble    = 2
     }

case_Hammersley 
   = actualHammersley `assertElement` [xmlQQ|
     <sampler type="hammersley">
       <integer name="sampleCount" value="1" />
       <integer name="scramble"    value="2" />
     </sampler>
   |]
   
actualSobol
   = SSobol
   $ Sobol
      { sobolSampleCount = 1
      , sobolScramble    = 2
      }

case_Sobol
    = actualSobol `assertElement` [xmlQQ|
      <sampler type="sobol">
        <integer name="sampleCount" value="1" />
        <integer name="scramble"    value="2" />
      </sampler>
    |]
    
data TestMetaData = TestMetaData { testMetaDataX :: Int }
  deriving (Show, Eq, Ord, Read, Typeable, Data, Generic)
  
instance ToElement TestMetaData

actualHdrFilm 
  = FHdrfilm
  $ HDRfilm
      { hdrFilmWidth            = 1
      , hdrFilmHeight           = 2
      , hdrFilmFileFormat       = Openexr
      , hdrFilmPixelFormat      = PFLuminance
      , hdrFilmComponentFormat  = Float16
      , hdrFilmCropOffsetX      = 3
      , hdrFilmCropOffsetY      = 4
      , hdrFilmCropWidth        = 5
      , hdrFilmCropHeight       = 6
      , hdrFilmAttachLog        = True
      , hdrFilmBanner           = False
      , hdrFilmHighQualityEdges = True
      , hdrFilmRFilter          = RFBox
      , hdrFilmMetaData         = ()
      , hdrFilmLabels           = Label 50 80 "hey"
      }


case_HdrFilm_toXML 
  = actualHdrFilm `assertElement` [xmlQQ|
     <film type="hdrfilm">
       <integer name="width"            value="1" />
       <integer name="height"           value="2" />
       <integer name="cropOffsetX"      value="3" />
       <integer name="cropOffsetY"      value="4" />
       <integer name="cropWidth"        value="5" />
       <integer name="cropHeight"       value="6" />
       <string  name="fileFormat"       value="openexr" />
       <string  name="pixelFormat"      value="luminance" />
       <string  name="componentFormat"  value="float16"  />
       <boolean name="attachLog"        value="true" />
       <boolean name="banner"           value="false" />
       <boolean name="highQualityEdges" value="true" />
       <rfilter type="box" />
       <string name="label[50, 80]"    value="hey" />
     </film>
  |]


actualTiledHDRFilm 
  = FTiledhdrfilm
  $ TiledHDRFilm 
      { tiledHDRFilmWidth           = 1
      , tiledHDRFilmHeight          = 2
      , tiledHDRFilmCrop            = Just $ Crop 
          { cropCropOffsetX = 3
          , cropCropOffsetY = 4
          , cropCropWidth   = 5
          , cropCropHeight  = 6
          }
      , tiledHDRFilmPixelFormat     = PFLuminance
      , tiledHDRFilmComponentFormat = Float16
      , tiledHDRFilmRFilter         = RFBox
      }

case_TiledHdrFilm_toXML 
  = actualTiledHDRFilm `assertElement` [xmlQQ|
     <film type="tiledhdrfilm">
       <integer name="width"            value="1" />
       <integer name="height"           value="2" />
       <integer name="cropOffsetX"      value="3" />
       <integer name="cropOffsetY"      value="4" />
       <integer name="cropWidth"        value="5" />
       <integer name="cropHeight"       value="6" />
       <string  name="pixelFormat"      value="luminance" />
       <string  name="componentFormat"  value="float16"  />
       <rfilter type="box" />
     </film>
  |]

actualGammaLDRFilm 
  = FLdrfilm
  $ LDRFGammaFilm
  $ GammaFilm
      { ldrfilmWidth            = 1
      , ldrfilmHeight           = 2
      , ldrfilmFileFormat       = Openexr
      , ldrfilmPixelFormat      = PFLuminance
      , ldrfilmGamma            = GTGammaCurve 3.0
      , ldffilmExposure         = 4
      , ldffilmBanner           = True
      , ldffilmCrop             = Just $ Crop 
            { cropCropOffsetX = 5
            , cropCropOffsetY = 6
            , cropCropWidth   = 7
            , cropCropHeight  = 8
            }
      , ldffilmHighQualityEdges = False
      , ldffilmRFilter          = RFBox
      }

case_GammaLdrFilm_toXML 
  = actualGammaLDRFilm `assertElement` [xmlQQ|
     <film type="ldrfilm">
       <integer name="width"            value="1" />
       <integer name="height"           value="2" />
       <string  name="fileFormat"       value="openexr" />
       <string  name="pixelFormat"      value="luminance" />
       <string  name="tonemapMethod"    value="gamma" />
       <float   name="gamma"            value="3.0" />
       <float   name="exposure"         value="4.0" />
       <boolean name="banner"           value="true"/>
       <integer name="cropOffsetX"      value="5" />
       <integer name="cropOffsetY"      value="6" />
       <integer name="cropWidth"        value="7" />
       <integer name="cropHeight"       value="8" />
       <boolean name="highQualityEdges" value="false" />
       <rfilter type="box" />
     </film>
  |]


actualReinhardLDRFilm 
  = FLdrfilm
  $ LDRFReinhard
  $  ReinhardFilm
     { reinhardFilmWidth            = 1
     , reinhardFilmHeight           = 2
     , reinhardFilmFileFormat       = Openexr
     , reinhardFilmPixelFormat      = PFLuminance
     , reinhardFilmGamma            = GTGammaCurve 3.0
     , reinhardFilmExposure         = 4
     , reinhardFilmKey              = 5
     , reinhardFilmBurn             = 6
     , reinhardFilmBanner           = True
     , reinhardFilmCrop             = Just $ Crop 
           { cropCropOffsetX = 7
           , cropCropOffsetY = 8
           , cropCropWidth   = 9
           , cropCropHeight  = 10
           }
     , reinhardFilmHighQualityEdges = False
     , reinhardFilmRFilter          = RFBox
     }


case_ReinhardLdrFilm_toXML 
   = actualReinhardLDRFilm `assertElement` [xmlQQ|
      <film type="ldrfilm">
        <integer name="width"            value="1" />
        <integer name="height"           value="2" />
        <string  name="fileFormat"       value="openexr" />
        <string  name="pixelFormat"      value="luminance" />
        <string  name="tonemapMethod"    value="reinhard" />
        <float   name="gamma"            value="3.0" />
        <float   name="exposure"         value="4.0" />
        <float   name="key"              value="5.0" />
        <float   name="burn"             value="6.0" />        
        <boolean name="banner"           value="true"/>
        <integer name="cropOffsetX"      value="7" />
        <integer name="cropOffsetY"      value="8" />
        <integer name="cropWidth"        value="9" />
        <integer name="cropHeight"       value="10" />
        <boolean name="highQualityEdges" value="false" />
        <rfilter type="box" />
      </film>
   |]

actualMFilm 
  = FMfilm
  $ MFilm 
     { mfilmWidth            = 1
     , mfilmHeight           = 2
     , mfilmCrop             = Just $ Crop 
            { cropCropOffsetX = 3
            , cropCropOffsetY = 4
            , cropCropWidth   = 5
            , cropCropHeight  = 6
            }
     , mfilmFileFormat       = MFFFMatlab
     , mfilmDigits           = 7
     , mfilmVariable         = "data"
     , mfilmPixelFormat      = PFLuminance
     , mfilmHighQualityEdges = True
     , mfilmRFilter          = RFBox
     }

case_MFilm_toXML
  = actualMFilm `assertElement` [xmlQQ|
      <film type="mfilm">
        <integer name="width"            value="1" />
        <integer name="height"           value="2" />
        <integer name="cropOffsetX"      value="3" />
        <integer name="cropOffsetY"      value="4" />
        <integer name="cropWidth"        value="5" />
        <integer name="cropHeight"       value="6" />
        <string  name="fileFormat"       value="matlab" />
        <integer name="digits"           value="7" />
        <string  name="variable"         value="data" />
        <string  name="pixelFormat"      value="luminance" />
        <boolean name="highQualityEdges" value="true" />
        <rfilter type="box" />
      </film>
   |]

case_box_filter_toXML 
  = RFBox `assertElement` [xmlQQ|
    <rfilter type="box" />
  |]

case_tent_filter_toXML
  = RFTent `assertElement` [xmlQQ|
    <rfilter type="tent" />
  |]
  
case_gaussian_toXML 
  = RFGaussian `assertElement` [xmlQQ|
    <rfilter type="gaussian" />
  |]

actualMitchell = RFMitchell 1 2

case_mitchell_toXML 
  = actualMitchell `assertElement` [xmlQQ|
    <rfilter type="mitchell" >
      <float name="B" value="1.0" />
      <float name="C" value="2.0" />
    </rfilter>
  |]

actualCatmullrom = RFCatmullrom 1 2

case_catmullrom_toXML 
  = actualCatmullrom `assertElement` [xmlQQ|
    <rfilter type="catmullrom" >
      <float name="B" value="1.0" />
      <float name="C" value="2.0" />
    </rfilter>
  |]

actualLanczos = RFLanczos 2

case_lanczos_toXML 
  = actualLanczos `assertElement` [xmlQQ|
    <rfilter type="lanczos" >
      <integer name="lobes" value="2" />
    </rfilter>
  |]

actualSceneNode = SceneNode 
      { nodeData = SNDInclude $ Include "file.txt"
      , nodeId   = Just "id"
      }

case_SceneNode_toXML 
  = actualSceneNode `assertElement` [xmlQQ|
        <include id="id" filename="file.txt" />
      |]

-- SceneNode test
-- Scene Test

case_Scene_toXML 
  = Scene [actualSceneNode]
  `assertElement` [xmlQQ|
    <scene version="0.5.0" >
       <include id="id" filename="file.txt" />
    </scene>
  |]









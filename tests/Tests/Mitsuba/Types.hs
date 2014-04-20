{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE QuasiQuotes #-}
module Tests.Mitsuba.Types where
import Mitsuba.Types
import Mitsuba.Utils
import Mitsuba.Element
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.TH
import Data.Text (Text)
import Test.HUnit
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
import Data.Default.Generics
default (Text, Integer, Double)

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

        <bool name="flipNormals" value="true" />

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
      <bool name="flipNormals" value="true"/>
      
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
      
      <bool name="flipNormals" value="true" />
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
       
      <bool name="flipNormals" value="true" />
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
    
    <bool name="flipNormals" value="true" />
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
    
    <bool name="flipNormals" value="true" />
    
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
      <bool name="flipNormals" value="true" />
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
    
    <bool name="flipNormals" value="true" />
    
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
       
       <bool name="flipNormals"     value="false" />
       <bool name="flipTexCoords"   value="false" />
       <bool name="faceNormals"     value="false" />
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
      <bool name="nonlinear" value="true"/>
      <rgb name="diffuseReflectance" value="0.200, 0.600, 0.300"/>
    </bsdf>
    <bool name="faceNormals" value="false"/>
    <transform name="toWorld"/>
    <bool name="flipTexCoords" value="false"/>
    <float name="maxSmoothAngle" value="0.0"/>
    <bool name="flipNormals" value="false"/>
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
    <bool name="faceNormals" value="false"/>
    <bool name="flipTexCoords" value="false"/>
    <float name="maxSmoothAngle" value="0.0"/>
    <string name="filename" value="myShape.obj"/> 
    
    <bsdf name="Glass" type="dielectric">
      <float name="intIOR" value="1.5"/> 
      <float name="extIOR" value="1.5"/>
      <texture name="specularReflectance"/>
      <texture name="specularTransmittance"/>
    </bsdf>
    
    <bsdf name="Water" type="dielectric">
      <float name="intIOR" value="1.333"/>
      <float name="extIOR" value="1.333"/>
      <texture name="specularReflectance"/>
      <texture name="specularTransmittance"/>
    </bsdf> 
    
    <bool name="flipNormals" value="false"/>
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
        <bool name="srgb" value="false"/>
        <bool name="flipNormals" value="false"/>
        <bool name="faceNormals" value="true"/>
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
      <bool name="faceNormals" value="true" />
      <bool name="flipNormals" value="false" />
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
      <bool name="flipNormals" value="false"/>
    </shape>
    <shape type="cube">
      <transform name="toWorld"/>
      <bsdf type="diffuse">
        <spectrum name="reflectance" value="1.0"/>
      </bsdf>
      <bool name="flipNormals" value="false"/>
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
        
        <bool name="shadingNormals" value="true" />
        <bool name="flipNormals"    value="false" />
        
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
      <bool   name="cache" value="false" />
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
        <bool name="useFastApprox" value="true" />
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
      <bool   name="cache" value="false" />
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
    <texture name="specularReflectance" />
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
    <bool name="nonlinear" value="false" />
    <texture name="specularReflectance" />
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
    <texture name="specularReflectance"/>
    <spectrum name="diffuseReflectance" value="0.0"/>
    <texture type="scale" name="alpha">
      <texture name="texture" type="bitmap">
        <string name="filename" value="bump.png"/>
        <string name="wrapMode" value="repeat" />
        <string name="filterType" value="trilinear" />
        <float  name="maxAnisotropy" value="0.0" />
        <bool   name="cache" value="false" />
        <float  name="uoffset" value="0.5" />
        <float  name="voffset" value="1.0" />  
        <float name="uscale" value="1.5" />
        <float name="vscale" value="2.0" />  
        <string name="channel" value="r" />
      </texture>
      <float name="scale" value="0.6"/> 
    </texture>
    <bool name="nonlinear" value="false" />
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
        <texture name="specularReflectance"/>
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

{-

-- TODO  checkerboard
-- TODO gridtexture

_case_scale_toXML 
  = () `assertElement` [xmlQQ|
  <texture type="scale">
    <float name="scale" value="0.5"/>
    <texture type="bitmap">
      <string name="filename" value="wood.jpg"/>
    </texture>
  </texture>
  |]
  
_case_vertexcolors_toXML 
  = () `assertElement` [xmlQQ|
  <shape type="ply">
    <string name="filename" value="mesh.ply"/>
    <bsdf type="diffuse">
      <texture type="vertexcolors" name="reflectance"/>
    </bsdf>
  </shape>
  |]

-- TODO wireframe

-- TODO curvature

_case_dipole_toXML 
  = () `assertElement` [xmlQQ|
  <subsurface type="dipole">
    <string name="intIOR" value="water"/>
    <string name="extIOR" value="air"/>
    <rgb name="sigmaS" value="87.2, 127.2, 143.2"/>
    <rgb name="sigmaA" value="1.04, 5.6, 11.6"/>
    <integer name="irrSamples" value="64"/>
  </subsurface>
  |]

_case_homogeneous_toXML 
  = () `assertElement` [xmlQQ|
  <medium id="myMedium" type="homogeneous"> 
    <spectrum name="sigmaS" value="1"/> 
    <spectrum name="sigmaA" value="0.05"/>
    
    <phase type="hg">
      <float name="g" value="0.7"/>
    </phase> 
  </medium>
  |]
  
_case_heterogeneous_toXML 
  = () `assertElement` [xmlQQ|
  <medium type="heterogeneous" id="smoke">
    <string name="method" value="simpson"/>

    <volume name="density" type="gridvolume">
      <string name="filename" value="frame_0150.vol"/>
    </volume>

    <volume name="albedo" type="constvolume"> 
      <spectrum name="value" value="0.9"/>
    </volume>

    <phase type="isotropic"/>
    
    <float name="scale" value="200"/> 
  </medium>
  |]

_case_isotropic_toXML 
  = () `assertElement` [xmlQQ|<phase type="isotropic"/>|]


--TODO hg phase function, rayleigh, kkay, microflake, mixturephase

_case_constvolume_0_toXML 
  = () `assertElement` [xmlQQ|
  <volume type="constvolume" >
    <float name="value" value="1"/>
  </volume>
  |]

_case_constvolume_1_toXML 
  = () `assertElement` [xmlQQ|
  <volume type="constvolume" >
    <rgb name="value" value="0.9 0.9 0.7"/>
  </volume>
  |]

_case_constvolume_2_toXML 
  = () `assertElement` [xmlQQ|
  <volume type="constvolume" >
    <vector name="value" x="0" y="1" z="0"/> 
  </volume>
  |]

--TODO  make grid format binary format
-- TODO gridformat

-- TODO  volcache
-- TODO point light

_case_area_toXML
  = () `assertElement` [xmlQQ|
    <shape type="sphere"> 
      <emitter type="area">
        <spectrum name="radiance" value="1"/>
      </emitter>
    </shape>
  |]

_case_spot_toXML 
  = () `assertElement` [xmlQQ|
  <emitter type="spot">
    <transform name="toWorld">
      <lookat origin="1, 1, 1" target="1, 2, 1"/> 
    </transform>
  </emitter>
  |]
  
-- TODO 
--__case_directional_toXML 
-- TODo
-- __case_collimated_toXML 

-- TODO make another with more params
_case_sky_toXML_0
  = () `assertElement` [xmlQQ|
    <emitter type="sky"> 
      <transform name="toWorld">
        <rotate x="1" angle="90"/>
      </transform>
    </emitter>
  |]

-- TODO sun
-- TODO sunsky
-- TODO envmap
-- TODO constant

-- TODO more perspective tests
_case_perspective_toXML 
  = () `assertElement` [xmlQQ|
  <sensor type="perspective">
    <transform name="toWorld">
      <lookat origin="1, 1, 1" target="1, 2, 1" up="0, 0, 1"/>
    </transform>
  </sensor>
  |]
  
_case_thinlens_toXML 
  = () `assertElement` [xmlQQ|
  <sensor type="thinlens">
    <transform name="toWorld">
      <lookat origin="1, 1, 1" target="1, 2, 1" up="0, 0, 1"/>
    </transform>
    
    <float name="focusDistance" value="1"/>
    <float name="apertureRadius" value="0.1"/> 
  </sensor>
  
  |]

_case_orthographic_toXML 
  = () `assertElement` [xmlQQ|
  <sensor type="orthographic"> 
    <transform name="toWorld">
      <scale x="10" y="10"/>
      <lookat origin="1, 1, 1" target="1, 2, 1" up="0, 0, 1"/> 
    </transform>
  </sensor>
  |]

--TODO telecentric
--TODO spherical

_case_irradiancemeter_toXML 
  = () `assertElement` [xmlQQ|
<sensor type="irradiancemeter">
  <film type="mfilm"/>
  <sampler type="independent">
    <integer name="sampleCount" value="1024"/>
  </sampler>
</sensor>

  |]
  
_case_radiancemeter_toXML 
  = () `assertElement` [xmlQQ|
<sensor type="radiancemeter">
  <transform name="toWorld">
    <lookat origin="1,2,3" target="0,0,0"/>
  </transform>
  <film type="mfilm"/>
  <sampler type="independent">
    <integer name="sampleCount" value="1024"/>
  </sampler>
</sensor>
  
  |]
  
_case_fluencemeter_toXML
  = () `assertElement` [xmlQQ|
<sensor type="fluencemeter">
  <transform name="toWorld">
    <translate x="1" y="2" z="3"/>
  </transform>

  <film type="mfilm"/>

  <sampler type="independent">
    <integer name="sampleCount" value="1024"/>
  </sampler>
</sensor>

  |]
  
-- TODO perspective_rdist

-- TODO ao


--shadingSamples integer Specifies the number of shading samples that should be com- puted per primary ray (Default: 1)
--rayLength      float Specifies the world-space length of the ambient occlusion raysthatwillbecast. (Default:-1,i.e.automatic)

-- TODO direct

--shadingSamples integer This convenience parameter can be used to set both emitterSamples and bsdfSamples at the same time.
--emitterSamples integer Optional more fine-grained parameter: specifies the num- ber of samples that should be generated using the direct il-
--lumination strategies implemented by the scene’s emitters (Default: set to the value of shadingSamples)
--bsdfSamples integer Optional more fine-grained parameter: specifies the num- ber of samples that should be generated using the BSDF
--sampling strategies implemented by the scene’s surfaces (De- fault: set to the value of shadingSamples)
--strictNormals boolean Be strict about potential inconsistencies involving shading normals?Seepage151fordetails. (Default:no,i.e.false)
--hideEmitters boolean Hidedirectlyvisibleemitters?Seepage147fordetails. (De- fault: no, i.e. false)


--TODO Path
--maxDepth integer Specifies the longest path depth in the generated output im- age (where -1 corresponds to ∞). A value of 1 will only
--render directly visible light sources. 2 will lead to single- bounce(direct-only)illumination,andsoon. (Default:-1)
--rrDepth integer Specifies the minimum path depth, after which the imple- mentation will start to use the “russian roulette” path termi-
--nationcriterion. (Default:5)
--strictNormals boolean Be strict about potential inconsistencies involving shading normals? See the description below for details. (Default: no,
--i.e. false)
--hideEmitters boolean Hidedirectlyvisibleemitters?Seepage147fordetails. (De- fault: no, i.e. false)

--TODO volpath_simple
--maxDepth integer Specifies the longest path depth in the generated output im- age (where -1 corresponds to ∞). A value of 1 will only
--render directly visible light sources. 2 will lead to single- bounce(direct-only)illumination,andsoon. (Default:-1)
--rrDepth integer Specifies the minimum path depth, after which the imple- mentation will start to use the “russian roulette” path termi-
--nationcriterion. (Default:5)
--strictNormals boolean Be strict about potential inconsistencies involving shading normals?Seepage151fordetails. (Default:no,i.e.false)
--hideEmitters boolean Hidedirectlyvisibleemitters?Seepage147fordetails. (De- fault: no, i.e. false)

--TODO volpath
--maxDepth integer Specifies the longest path depth in the generated output im- age (where -1 corresponds to ∞). A value of 1 will only
--render directly visible light sources. 2 will lead to single- bounce(direct-only)illumination,andsoon. (Default:-1)
--rrDepth integer Specifies the minimum path depth, after which the imple- mentation will start to use the “russian roulette” path termi-
--nationcriterion. (Default:5)
--strictNormals boolean Be strict about potential inconsistencies involving shading normals?Seepage151fordetails. (Default:no,i.e.false)
--hideEmitters boolean Hidedirectlyvisibleemitters?Seepage147fordetails. (De- fault: no, i.e. false)
--

--TODO bdpt
--maxDepth integer Specifies the longest path depth in the generated output im- age (where -1 corresponds to ∞). A value of 1 will only
--render directly visible light sources. 2 will lead to single- bounce(direct-only)illumination,andsoon. (Default:-1)
--lightImage
--boolean Include sampling strategies that connect paths traced from emitters directly to the camera? (i.e. what ptracer does) This improves the effectiveness of bidirectional path tracing but severely increases the local and remote communication
--overhead, since large light images must be transferred be- tween threads or over the network. See the text below for amoredetailedexplanation. (Default:includethesestrate- gies, i.e. true)
--sampleDirect boolean Enable direct sampling strategies? This is a generalization of direct illumination sampling that works with both emit-
--ters and sensors. Usually a good idea. (Default: use direct sampling, i.e. true)
--rrDepth integer Specifies the minimum path depth, after which the imple- mentation will start to use the “russian roulette” path termi-
--nationcriterion. (Default:5)

--TODO ppm
--maxDepth integer Specifies the longest path depth in the generated output im- age (where -1 corresponds to ∞). A value of 1 will only
--render directly visible light sources. 2 will lead to single- bounce(direct-only)illumination,andsoon. (Default:-1)
--photonCount integer Number of photons to be shot per iteration (Default: 250000)
--initialRadius float Initialradiusofgatherpointsinworldspaceunits. (Default: 0, i.e. decide automatically)
--alpha float Radius reduction parameter alpha from the paper (Default: 0.7)
--granularity integer Granularity of photon tracing work units for the purpose of parallelization (in # of shot particles) (Default: 0, i.e. decide
--automatically)
--rrDepth integer Specifies the minimum path depth, after which the imple- mentation will start to use the “russian roulette” path termi-
--nationcriterion. (Default:5)
--maxPasses integer Maximum number of passes to render (where -1 corre- spondstorenderinguntilstoppedmanually). (Default:-1)

--TODO  sppm
--maxDepth integer Specifies the longest path depth in the generated output im- age (where -1 corresponds to ∞). A value of 1 will only
--render directly visible light sources. 2 will lead to single- bounce(direct-only)illumination,andsoon. (Default:-1)
--photonCount integer Number of photons to be shot per iteration (Default: 250000)
--initialRadius float Initialradiusofgatherpointsinworldspaceunits. (Default: 0, i.e. decide automatically)
--alpha float Radius reduction parameter alpha from the paper (Default: 0.7)
--granularity integer Granularity of photon tracing work units for the purpose of parallelization (in # of shot particles) (Default: 0, i.e. decide
--automatically)
--rrDepth integer Specifies the minimum path depth, after which the imple- mentation will start to use the “russian roulette” path termi-
--nationcriterion. (Default:5)
--maxPasses integer Maximum number of passes to render (where -1 corre- spondstorenderinguntilstoppedmanually). (Default:-1)

--TODO pssmlt
-- bidirectional
-- boolean PSSMLT works in conjunction with another rendering tech- nique that is endowed with Markov Chain-based sample
-- generation. Two choices are available (Default: true):
-- • true: Operate on top of a fully-fleged bidirectional
-- path tracer with multiple importance sampling.
-- • false: Rely on a unidirectional volumetric path tracer (i.e. volpath)
-- maxDepth integer Specifies the longest path depth in the generated output im- age (where -1 corresponds to ∞). A value of 1 will only
-- render directly visible light sources. 2 will lead to single- bounce(direct-only)illumination,andsoon. (Default:-1)
-- directSamples
-- integer By default, this plugin renders the direct illumination com- ponent separately using an optimized direct illumination
-- sampling strategy that uses low-discrepancy number se- quences for superior performance (in other words, it is not rendered by PSSMLT). This parameter specifies the number of samples allocated to that method. To force PSSMLT to be responsible for the direct illumination component as well, setthisparameterto-1. (Default:16)
-- rrDepth integer Specifies the minimum path depth, after which the imple- mentation will start to use the “russian roulette” path termi-
-- nationcriterion. (Default:5)
-- luminanceSamples
-- integer MLT-type algorithms create output images that are only rel- ative. The algorithm can e.g. determine that a certain pixel
-- is approximately twice as bright as another one, but the ab- solute scale is unknown. To recover it, this plugin computes the average luminance arriving at the sensor by generating anumberofsamples. (Default:100000samples)
-- twoStage boolean Usetwo-stageMLT?Seebelowfordetails. (Default:false)
-- pLarge float Rate at which the implementation tries to replace the cur- rent path with a completely new one. Usually, there is little
-- needtochangethis. (Default:0.3)

-- TODO mlt
-- maxDepth integer Specifies the longest path depth in the generated output im- age (where -1 corresponds to ∞). A value of 1 will only
-- render directly visible light sources. 2 will lead to single- bounce(direct-only)illumination,andsoon. (Default:-1)
-- directSamples
-- integer By default, the implementation renders direct illumina- tion component separately using the direct plugin, which
-- uses low-discrepancy number sequences for superior per- formance (in other words, it is not handled by MLT). This parameter specifies the number of samples allocated to that method. To force MLT to be responsible for the direct illu- minationcomponentaswell,setthisto-1. (Default:16)
-- luminanceSamples
-- integer MLT-type algorithms create output images that are only rel- ative. The algorithm can e.g. determine that a certain pixel
-- is approximately twice as bright as another one, but the ab- solute scale is unknown. To recover it, this plugin computes the average luminance arriving at the sensor by generating anumberofsamples. (Default:100000samples)
-- twoStage boolean Use two-stage MLT? See pssmlt for details. (Default: false)
-- bidirectional⤦ boolean These parameters can be used to pick the individual muta- Mutation, tion and perturbation strategies that will be used to explore
-- [lens,multiChain, caustic,manifold]⤦ Perturbation
-- path space. By default, the original set by Veach and Guibas is enabled (i.e. everything except the manifold perturba- tion). It is possible to extend this integrator with additional custom perturbations strategies if needed.
-- lambda float Jump size of the manifold perturbation (Default: 50)

-- TODO erpt
-- maxDepth integer Specifies the longest path depth in the generated output im- age (where -1 corresponds to ∞). A value of 1 will only
-- render directly visible light sources. 2 will lead to single- bounce(direct-only)illumination,andsoon. (Default:-1)
-- numChains float On average, how many Markov Chains should be started perpixel? (Default:1)
-- maxChains float How many Markov Chains should be started at most (per pixel) (Default: 0, i.e. this feature is not used)
-- chainLength integer Specifies the number of perturbation steps that are executed per Markov Chain (Default: 1).
-- directSamples
-- integer By default, the implementation renders direct illumina- tion component separately using the direct plugin, which
-- uses low-discrepancy number sequences for superior per- formance (in other words, it is not handled by ERPT). This parameter specifies the number of samples allocated to that method. To force MLT to be responsible for the direct illu- minationcomponentaswell,setthisto-1. (Default:16)
-- [lens,multiChain, boolean caustic,manifold]⤦ Perturbation
-- These parameters can be used to pick the individual pertur- bation strategies that will be used to explore path space. By default, the original set by Veach and Guibas is enabled (i.e. everything except the manifold perturbation).
-- lambda float Jump size of the manifold perturbation (Default: 50)

-- TODO ptracer
-- maxDepth integer Specifies the longest path depth in the generated output im- age (where -1 corresponds to ∞). A value of 1 will only
-- render directly visible light sources. 2 will lead to single- bounce(direct-only)illumination,andsoon. (Default:-1)
-- rrDepth integer Specifies the minimum path depth, after which the imple- mentation will start to use the “russian roulette” path termi-
-- nationcriterion. (Default:5)
-- granularity integer Specifies the work unit granularity used to parallize the the particle tracing task. This should be set high enough so that accumulating partially exposed images (and potentially
-- sendingthemoverthenetwork)isnotthebottleneck. (De- fault: 200K particles per work unit, i.e. 200000)
-- bruteForce boolean If set to true, the integrator does not attempt to create con- nections to the sensor and purely relies on hitting it via ray tracing. This is mainly intended for debugging purposes.
-- (Default: false)

--TODO adaptive
--maxError float Maximum relative error threshold (Default: 0.05)
--pValue float Required p-value to accept a sample (Default: 0.05)
--maxSampleFactor
--integer Maximum number of samples to be generated relative to the number of configured pixel samples. The adaptive integra-
--tor will stop after this many samples, regardless of whether or not the error criterion was satisfied. A negative value will be interpreted as ∞. (Default: 32—for instance, when 64 pixel samples are configured in the sampler, this means that the adaptive integrator will give up after 32*64=2048 samples)

-- TODO vpl
-- maxDepth integer Specifies the longest path depth in the generated output im- age (where -1 corresponds to ∞). A value of 2 will lead to
-- direct-onlyillumination. (Default:5)
-- shadowMap⤦ integer Resolution of the shadow maps that are used to compute the Resolution point-to-point visibility (Default: 512)
-- clamping float A relative clamping factor between [0, 1] that is used to con- troltherenderingartifactdiscussedbelow. (Default:0.1)

-- TODO -irrache

--Parameter Type Description
--resolution integer Elevational resolution of the stratified final gather hemi- sphere. The azimuthal resolution is two times this value.
--(Default: 14, i.e. 2 ⋅ 142 =392 samples in total)
--quality float Quality factor (the κ parameter of Tabellion et al. [43]) (De- fault: 1.0, which is adequate for most cases)
--gradients boolean Use irradiance gradients [49]? (Default: true)
--clampNeighbor boolean Use neighbor clamping [29]? (Default: true)
--clampScreen boolean Use a screen-space clamping criterion [43]? (Default: true)
--overture boolean Do an overture pass before starting the main rendering pro- cess? Usually a good idea. (Default: true)
--quality⤦ float When an overture pass is used, Mitsuba subsequently re- Adjustment duces the quality parameter by this amount to interpolate amongst more samples, creating a visually smoother result.
--(Default: 0.5)
--indirectOnly boolean Only show the indirect illumination? This can be useful to checktheinterpolationquality. (Default:false)
--debug boolean Visualizethesampleplacement? (Default:false)

-- TODO independent
-- sampleCount integer Number of samples per pixel (Default: 4)

-- TODO stratified 
-- sampleCount
-- dimension
-- Type
-- integer
-- integer
-- Description
-- Number of samples per pixel; should be a perfect square (e.g. 1, 4, 9, 16, 25, etc.), or it will be rounded up to the next one (Default: 4)
-- Effective dimension, up to which stratified samples are pro- vided. The number here is to be interpreted as the number of subsequent 1D or 2D sample requests that can be satis- fied using “good” samples. Higher high values increase both storageandcomputationalcosts. (Default:4)


8.11.3. Lowdiscrepancysampler(ldsampler)
Parameter
sampleCount
dimension
Type
integer
integer
Description
Number of samples per pixel; should be a power of two (e.g. 1, 2, 4, 8, 16, etc.), or it will be rounded up to the next one (Default: 4)
Effective dimension, up to which low discrepancy samples are provided. The number here is to be interpreted as the number of subsequent 1D or 2D sample requests that can be satisfied using “good” samples. Higher high values increase bothstorageandcomputationalcosts. (Default:4)


8.11.4. HaltonQMCsampler(halton)
Parameter
sampleCount
scramble
Type
integer
integer
Description
Number of samples per pixel (Default: 4)
This plugin can operate in one of three scrambling modes:
(i) When set to 0, the implementation will provide the standard Halton sequence.
(ii) When set to -1, the implementation will compute a scrambled variant of the Halton sequence based on permutations by Faure [10], which has better equidis- tribution properties in high dimensions.
(iii) Whensettoavaluegreaterthanone,arandompermu- tation is chosen based on this number. This is useful to break up temporally coherent noise when render- ing the frames of an animation — in this case, simply set the parameter to the current frame index.
Default: -1, i.e. use the Faure permutations. Note that per- mutations rely on a precomputed table that consumes ap- proximately 7 MiB of additional memory at run time.
1.0
0.8
0.6


8.11.5. HammersleyQMCsampler(hammersley)
Parameter
sampleCount
scramble
Type
integer
integer
Description
Number of samples per pixel (Default: 4)
This plugin can operate in one of three scrambling modes:
(i) When set to 0, the implementation will provide the standard Hammersley sequence.
(ii) When set to -1, the implementation will compute a scrambled variant of the Hammersley sequence based on permutations by Faure [10], which has bet- ter equidistribution properties in high dimensions.
(iii) Whensettoavaluegreaterthanone,arandompermu- tation is chosen based on this number. This is useful to break up temporally coherent noise when render- ing the frames of an animation — in this case, simply set the parameter to the current frame index.
Default: -1, i.e. use the Faure permutations. Note that per- mutations rely on a precomputed table that consumes ap- proximately 7 MiB of additional memory at run time.


8.11.6. SobolQMCsampler(sobol)
Parameter
sampleCount
scramble
Type
integer
integer
Description
Number of samples per pixel (Default: 4)
This parameter can be used to set a scramble value to break up temporally coherent noise patterns. For stills, this is ir- relevant. When rendering an animation, simply set it to the currentframeindex. (Default:0)


_case_hdrfilm_0_toXML
  = () `assertElement` [xmlQQ|
<film type="hdrfilm">
  <string name="pixelFormat" value="rgba"/>
  <integer name="width" value="1920"/>
  <integer name="height" value="1080"/>
  <boolean name="banner" value="false"/>
</film>
|]

_case_hdrfilm_1_toXML
  = () `assertElement` [xmlQQ|
<film type="hdrfilm">
  <string name="metadata['key_name']" value="Hello!"/>
  <string name="label[50, 80]" value="Hello!"/>
</film>
|]


-- test the label ability
--￼<string name="label[10, 10]" value="Integrator: $integrator['type'], $film['width']x$film['height'], $sampler['sampleCount'] spp, render time: $scene['renderTime'], memory: $scene['memUsage']"/>


-- add this functionality


$scene[’renderTime’]
$scene[’memUsage’]
$scene[’coreCount’]
$scene[’blockSize’]
$scene[’sourceFile’]
$scene[’destFile’]
$integrator[’..’]
$sensor[’..’]
$sampler[’..’]
$film[’..’]
Image render time, use renderTimePrecise for more digits. Mitsuba memory usage19. Use memUsagePrecise for more digits. Number of local and remote cores working on the rendering job Block size used to parallelize up the rendering workload
Source file name
Destination file name
Copy a named integrator parameter
Copy a named sensor parameter
Copy a named sampler parameter
Copy a named film parameter


8.12.2. Tiledhighdynamicrangefilm(tiledhdrfilm)
Parameter Type Description
width, height integer Width and height of the camera sensor in pixels (Default: 768, 576)
cropOffsetX,
cropOffsetY,
cropWidth,
cropHeight
integer
These parameters can optionally be provided to select a sub- rectangle of the output. In this case, Mitsuba will only ren- dertherequestedregions. (Default:Unused)
pixelFormat
string Specifies the desired pixel format for OpenEXR output im- ages. The options are luminance, luminanceAlpha, rgb, rgba, xyz, xyza, spectrum, and spectrumAlpha. In the
latter two cases, the number of written channels depends on the value assigned to SPECTRUM_SAMPLES during compila- tion (see Section 4 section for details) (Default: rgb)
componentFormat string Specifies the desired floating point component format used for the output. The options are float16, float32, or
uint32 (Default: float16)
(Nested plugin) rfilter Reconstruction filter that should be used by the film. (De- fault: gaussian, a windowed Gaussian filter)






8.12.3. Lowdynamicrangefilm(ldrfilm)
Parameter Type Description
width, height integer Camera sensor resolution in pixels (Default: 768, 576)
fileFormat integer Thedesiredoutputfileformat:pngorjpeg. (Default:png)
pixelFormat string Specifies the pixel format of the generated image. The op- tions are luminance, luminanceAlpha, rgb or rgba for
PNG output and rgb or luminance for JPEG output.
tonemapMethod string Method used to tonemap recorded radiance values
(i) gamma: Exposure and gamma correction (default)
(ii) reinhard: Apply the the tonemapping technique by Reinhard et al. [39] followd by gamma correction.
gamma float The gamma curve applied to correct the output image, where the special value -1 indicates sRGB. (Default: -1)
exposure float When gamma tonemapping is active, this parameter speci- fies an exposure factor in f-stops that is applied to the im-
age before gamma correction (scaling the radiance values by2exposure). (Default:0,i.e.donotchangetheexposure)
key float When reinhard tonemapping is active, this parameter in (0, 1] specifies whether a low-key or high-key image is de-
sired. (Default:0.18,correspondingtoamiddle-grey)
burn float When reinhard tonemapping is active, this parameter in [0,1]specifieshowmuchhighlightscanburnout. (Default:
0, i.e. map all luminance values into the displayable range)
banner boolean Include a banner in the output image? (Default: true)
cropOffsetX,
cropOffsetY,
cropWidth,
cropHeight
integer
These parameters can optionally be provided to select a sub- rectangle of the output. In this case, Mitsuba will only ren- dertherequestedregions. (Default:Unused)
highQualityEdges boolean If set to true, regions slightly outside of the film plane will also be sampled. This may improve image quality at the
edges,butisnotneededingeneral. (Default:false)
(Nested plugin) rfilter Reconstruction filter that should be used by the film. (De- fault: gaussian, a windowed Gaussian filter)



8.12.4. MATLAB/Mathematicafilm(mfilm)
Parameter Type Description
width, height integer Width and height of the sensor in pixels (Default: 1, 1)
cropOffsetX,
cropOffsetY,
cropWidth,
cropHeight
integer
These parameters can optionally be provided to select a sub- rectangle of the output. In this case, Mitsuba will only ren- dertherequestedregions. (Default:Unused)
fileFormat string Specifies the desired output format; must be one of matlab ormathematica. (Default:matlab)
digits integer Number of significant digits to be written (Default: 4)
variable string Name of the target variable (Default: "data")
pixelFormat
string Specifies the desired pixel format of the generated image. The options are luminance, luminanceAlpha, rgb, rgba, spectrum, and spectrumAlpha. In the latter two cases, the number of written channels depends on the value as- signed to SPECTRUM_SAMPLES during compilation (see Sec-
tion 4 section for details) (Default: luminance)
highQualityEdges boolean If set to true, regions slightly outside of the film plane will also be sampled. This may improve the image quality at the
edges, especially when using very large reconstruction fil- ters. In general (and particularly using the default box filter), thisisnotneededthough. (Default:false,i.e.disabled)
(Nested plugin) rfilter Reconstruction filter that should be used by the film. (De- fault: box, a simple box filter)




_case_rfilter_toXML 
  = () `assertElement` [xmlQQ|
<rfilter type="lanczos">
  <integer name="lobes" value="2"/>
</rfilter>
  |]

-}



















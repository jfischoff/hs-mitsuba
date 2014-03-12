{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE QuasiQuotes #-}
module Tests.Mitsuba.Types where
import Mitsuba.Types
import Mitsuba.Element
import Mitsuba.XML
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.TH
import Data.Text (Text)
import Test.HUnit
import GHC.Generics
import Data.List
import Text.Blaze
import Text.InterpolatedString.Perl6
import Text.Blaze.Renderer.Pretty
default (Text, Integer, Double)

tests :: TestTree
tests = $(testGroupGenerator)

renderXML = renderMarkup . toXML []

case_Ref_toXML = renderXML (Ref "thing") @?= [q|<ref id="thing" />
|]

-- _case_Child_toXML = 
   
case_WavelengthStyle_toPrimitiveValue = 
   toPrimitiveValue (WavelengthStyle 
                        [ (400, 0.56)
                        , (500, 0.18)
                        , (600, 0.58)
                        , (700, 0.24)
                        ]) @?= 
      "400:0.56, 500:0.18, 600:0.58, 700:0.24"

case_InternalSpectralFormat_toPrimitiveValue = 
   toPrimitiveValue (InternalSpectralFormat 0.2 0.2 0.8 0.4 0.6 0.5 0.1 0.9 0.4 0.2) @?= 
      ".2, .2, .8, .4, .6, .5, .1, .9, .4, .2"
      
case_RGBTriple_toPrimitiveValue = 
   toPrimitiveValue (RGBTriple 0.2 0.8 0.4) @?= "0.200, 0.800, 0.400"
   
case_Hex_toPrimitiveValue = 
   toPrimitiveValue (Hex 1 15 128) @?= "#010f80"
   
case_Temperature_toPrimitiveValue =
   toPrimitiveValue (Temperature 5000) @?= "5000k"

case_Blackbody_toXML = 
   renderXML (Blackbody (Temperature 5000) 1.5) @?= 
 [q|<blackbody temperature="5000k" scale="1.5" />
|]

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE QuasiQuotes #-}
module Tests.Mitsuba.Class where
import Mitsuba.Class
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.TH
import Data.Text (Text)
import Test.HUnit
import GHC.Generics
import Data.List
import Text.Blaze
import Mitsuba.XML
import Text.InterpolatedString.Perl6
import Text.Blaze.Renderer.Pretty
default (Text, Integer, Double)

tests :: TestTree
tests = $(testGroupGenerator)

case_conNameToType_1 = conNameToType "Foo" "FBar" @?= "bar"
case_conNameToType_2 = conNameToType "FooBar" "FBBar" @?= "bar"

case_selNameToType_1 = selNameToType "Foo" "fooThing" @?= "thing"
case_selNameToType_2 = selNameToType "FooType" "fooTypeThing" @?= "thing"

data Foo 
   = FType1 Type1
   | FType2 Type2
   deriving (Show, Eq, Generic)
   
data FooRecord = FooRecord 
   { fooRecordThing1 :: Type1
   , fooRecordThing2 :: Type2
   } deriving (Show, Eq, Generic)

instance ToXML FooRecord   
   
data Forwardable 
   = Foo1 Foo
   | Foo2 FooRecord
   deriving (Show, Eq, Generic)
   
instance ToXML Forwardable where
   toXML = forwardToXML
   
data Type1 = Type1 deriving (Show, Eq, Generic)
data Type2 = Type2 deriving (Show, Eq, Generic)
   
instance ToXML Type1 where
   toXML xs Type1 = foldl' (!) (ct "type1") xs
   
instance ToXML Type2 where
   toXML xs Type2 = foldl' (!) (ct "type2") xs
   
instance ToXML Foo

case_Type1_toXML 
   = renderMarkup (toXML [a "value" "hey"] Type1) @?= [q|<type1 value="hey" />
|]
case_Type2_toXML 
   = renderMarkup (toXML [a "value" "hey"] Type2) @?= [q|<type2 value="hey" />
|]

case_Foo_XML
   = renderMarkup (toXML [] $ FType1 Type1) @?= 
 [q|<foo type="type1">
    <type1 />
</foo>
|]

case_FooRecord_XML 
   = renderMarkup (toXML [] $ FooRecord Type1 Type2) @?= 
 [q|<foorecord>
    <type1 name="thing1" />
    <type2 name="thing2" />
</foorecord>
|]

case_forwardToXML_0 = 
   renderMarkup (toXML [] $ Foo1 $ FType1 Type1) @?= 
 [q|<foo type="type1">
    <type1 />
</foo>
|]

case_forwardToXML_1 = 
   renderMarkup (toXML [] $ Foo2 $ FooRecord Type1 Type2) @?= 
 [q|<foorecord>
    <type1 name="thing1" />
    <type2 name="thing2" />
</foorecord>
|]


case_Double_toXML = renderMarkup (toXML [] (1.0 :: Double)) @?= [q|<float value="1.0" />
|]









 
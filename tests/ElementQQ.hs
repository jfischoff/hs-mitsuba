{-# LANGUAGE RecordWildCards #-}
module ElementQQ where
import Text.XML.QQ
import qualified Text.XML.Light as XML
import Mitsuba.Element

xmlToElement :: XML.Element -> Element
xmlToElement XML.Element {..} 
   = Element (qName elName) 
   $  map attrToChild    elAttribs
   <> map contentToChild elContent

attrToChild :: XML.Attr -> ChildItem
attrToChild Attr {..} = ChildItem (Attribute 

contentToChild :: XML.Content -> ChildItem
contentToChild = undefined
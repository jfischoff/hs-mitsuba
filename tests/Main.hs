module Main where
import qualified Tests.Mitsuba.Types as Types
import qualified Tests.Mitsuba.Class as Class
import Test.Tasty


main = defaultMain $ testGroup 
   "main" 
      [ Types.tests
      , Class.tests
      ]
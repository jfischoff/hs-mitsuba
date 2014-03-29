module Mitsuba.Element.Diff where
import Mitsuba.Element.Types
import qualified Data.Algorithm.Patience as P

--data SumDiff a 
--    = Eq a
--    | Changed a a


class Diffable a d | a -> d, d -> a where
  diff  :: a -> a -> d
  patch :: d -> a -> a

data PrimitiveDiff a = PrimitiveDiff 
  { oldValue :: a 
  , newValue :: a
  }
  
instance Int (PrimitiveDiff Int)
  diff  = uncurry PrimitiveDiff
  patch x = newValue x

instance Diffable t d => [t] [Item t] where
  diff = P.diff
  patch = undefined

instance Diffable t d 

data DictionaryDiff a b = DictionaryDiff 
  { inserted :: [(a, b)]
  , deleted  :: [(a, b)]
  , modified :: [(a, Diff b)]
  }

data ChildItem = ChildItem
  { childItemType    :: Diff ChildType
  , childItemElement :: Diff Element
  }

data ElementDiff = ElementDiff
  { elementNameDiff :: Diff Name
  , elementChildren :: Diff Children
  }
  

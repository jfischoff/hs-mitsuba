{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
module Mitsuba.GenericLenses where
import GHC.Generics hiding (from)
import Control.Lens
import Control.Lens.Iso

par1 :: Iso' (Par1 p) p
par1 = iso unPar1 Par1

rec1 :: Iso' (Rec1 f p) (f p)
rec1 = iso unRec1 Rec1

k1 :: Iso' (K1 i c p) c
k1 = iso unK1 K1

m1 :: Iso' (M1 i c f p) (f p)
m1 = iso unM1 M1

_L1 :: Prism' ((f :+: g) p) (f p)
_L1 = prism L1 bk where
   bk = \case
      L1 x -> Right x
      R1 x -> Left $ R1 x
      
_R1 :: Prism' ((f :+: g) p) (g p)
_R1 = prism R1 bk where
   bk = \case
      L1 x -> Left $ L1 x
      R1 x -> Right x

times :: Iso' ((f :*: g) p) (f p, g p)
times = iso fw bk where
   fw (f :*: g) = (f, g)
   bk (f, g) = (f :*: g)

comp1 :: Iso' ((f :.: g) p) (f (g p))
comp1 = iso unComp1 Comp1

-- all the newtype iso
idI :: Iso' a a
idI = iso id id

plusZero :: Iso' ((V1 :+: f) p) (f p)
plusZero = iso (\(R1 x) -> x) R1

commutePlus :: Iso' ((a :+: b) p) ((b :+: a) p)
commutePlus = iso invo invo where
   invo = \case
      L1 x -> R1 x
      R1 x -> L1 x
      
assocPlus :: Iso' ((a :+: (b :+: c)) p) (((a :+: b) :+: c) p)
assocPlus = iso assocPlusLeft assocPlusRight where
   
   assocPlusLeft :: ((a :+: (b :+: c)) p) -> (((a :+: b) :+: c) p)
   assocPlusLeft = \case
      L1 x      -> L1 $ L1 x
      R1 (L1 x) -> L1 $ R1 x
      R1 (R1 x) -> R1 x

   assocPlusRight :: (((a :+: b) :+: c) p) -> ((a :+: (b :+: c)) p) 
   assocPlusRight = \case
      L1 (L1 x) -> L1 x
      L1 (R1 x) -> R1 $ L1 x
      R1     x  -> R1 $ R1 x
      
timesOne :: Iso' ((U1 :*: f) p) (f p)
timesOne = iso (\(U1 :*: x) -> x) ((:*:) U1) 

commuteTimes :: Iso' ((a :*: b) p) ((b :*: a) p)
commuteTimes = iso invo invo where
   invo (x :*: y) = y :*: x

assocTimes :: Iso' ((a :*: (b :*: c)) p) (((a :*: b) :*: c) p)
assocTimes = iso assocTimesLeft assocTimesRight where
   
   assocTimesLeft :: (a :*: (b :*: c)) p -> ((a :*: b) :*: c) p
   assocTimesLeft (a :*: (b :*: c)) = (a :*: b) :*: c
   
   assocTimesRight :: ((a :*: b) :*: c) p -> (a :*: (b :*: c)) p
   assocTimesRight ((a :*: b) :*: c) = a :*: (b :*: c)
   
distribute :: Iso' ((a :*: (b :+: c)) p) (((a :*: b) :+: (a :*: c)) p)
distribute = iso dist factor where
   dist (a :*: e) = case e of
      L1 b -> L1 $ a :*: b
      R1 c -> R1 $ a :*: c

   factor = \case
      L1 (a :*: b) -> a :*: L1 b
      R1 (a :*: c) -> a :*: R1 c




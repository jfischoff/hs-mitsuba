{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
module Mitsuba.Generic where
import GHC.Generics
import Data.Proxy

class GFold f c b where
   genericFold :: p c -> (forall e. c e => e -> b) -> f a -> b 

instance GFold a c d => GFold (M1 x y a) c d where
   genericFold p f (M1 x) = genericFold p f x

instance ( GFold a c d
         , GFold b c d
         ) => GFold (a :+: b) c d where
   genericFold p f = \case
       L1 x -> genericFold (Proxy :: Proxy c) f x
       R1 x -> genericFold (Proxy :: Proxy c) f x

instance c a => GFold (K1 i a) c d where
   genericFold p f (K1 x) = f x

gfold :: forall a c d p. (Generic a, GFold (Rep a) c d) 
      => p c -> (forall e. c e => e -> d) -> a -> d
gfold p h x = genericFold p h $ from x

-- I would like to generalize this to abitrary functions
-- forall e. c e => e -> .. -> e -> d 
-- foreach var either apply to all forwarded spots. case if it is e.
-- 

--class GGFold 
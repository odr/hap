{-# LANGUAGE ExistentialQuantification, RankNTypes, ScopedTypeVariables, FlexibleInstances, RecordWildCards, LambdaCase #-}
module Test where

-- class E
class FF e t where ff :: [e] -> t -> Maybe t

data EF e t

data D e = forall t. FF e t => D {unD :: EF e t}

data E e

f :: Functor f => EF e t -> (t->f t) -> E e -> f (E e)
f = undefined

g :: D e -> E e -> Maybe (E e)
g (D (ef :: EF e t)) = f ef (ff ([]::[e]))

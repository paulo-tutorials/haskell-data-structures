{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
module Set.Set where

class Set c e | c -> e where
    empty :: c
    insert :: e -> c -> c
    member :: e -> c -> Bool
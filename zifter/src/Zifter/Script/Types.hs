{-# LANGUAGE DeriveGeneric #-}

module Zifter.Script.Types where

import Introduction

import Zifter.Setup.Types

newtype ZiftScript a = ZiftScript
    { renderZiftScript :: IO (a, ZiftSetup)
    } deriving (Generic)

instance Functor ZiftScript where
    fmap f (ZiftScript func) =
        ZiftScript $ do
            (a, zs) <- func
            pure $ (f a, zs)

instance Applicative ZiftScript where
    pure a = ZiftScript $ pure (a, mempty)
    (ZiftScript funcf) <*> (ZiftScript funca) =
        ZiftScript $ do
            (f, z1) <- funcf
            (a, z2) <- funca
            pure $ (f a, z1 `mappend` z2)

instance Monad ZiftScript where
    (ZiftScript afunc) >>= func =
        ZiftScript $ do
            (a, z1) <- afunc
            let (ZiftScript bfunc) = func a
            (b, z2) <- bfunc
            pure $ (b, z1 `mappend` z2)

{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}

module Zifter.Zift.Types where

import Prelude

import Control.Concurrent.STM
import Control.Monad.Catch (MonadThrow(..))
import Control.Monad.Fail as Fail
import Control.Monad.IO.Class
import Data.Validity
import Data.Validity.Path ()
import GHC.Generics
import Path
import System.Console.ANSI (SGR)

import Zifter.OptParse.Types

data ZiftToken =
    ZiftToken [LR]
              (Maybe ZiftOutput)
    deriving (Show, Eq, Generic)

data ZiftOutput = ZiftOutput
    { outputColors :: [SGR]
    , outputMessage :: String
    } deriving (Show, Eq, Generic)

data ZiftContext = ZiftContext
    { rootdir :: Path Abs Dir
    , tmpdir :: Path Abs Dir
    , settings :: Settings
    , printChan :: TChan ZiftToken
    , recursionList :: [LR] -- In reverse order
    } deriving (Generic)

data LR
    = L
    | R
    deriving (Show, Eq, Generic)

instance Validity ZiftContext where
#if MIN_VERSION_validity(0,4,0)
    validate zc = annotate (rootdir zc) "rootdir"
#else
#if MIN_VERSION_validity(0,4,0)
#else
    isValid = isValid . rootdir
#endif
#endif
data Zift a where
    ZiftPure :: a -> Zift a
    ZiftCtx :: Zift ZiftContext
    ZiftPrint :: ZiftOutput -> Zift ()
    ZiftFail :: String -> Zift a
    ZiftIO :: IO a -> Zift a
    ZiftFmap :: (a -> b) -> Zift a -> Zift b
    ZiftApp :: Zift (a -> b) -> Zift a -> Zift b
    ZiftBind :: Zift a -> (a -> Zift b) -> Zift b

instance Monoid a => Monoid (Zift a) where
    mempty = ZiftPure mempty
    mappend z1 z2 = mappend <$> z1 <*> z2

instance Functor Zift where
    fmap = ZiftFmap

-- | 'Zift' actions can be sequenced.
--
-- The implementation automatically parallelises the arguments of the
-- @(<*>)@ function. If any of the actions fails, the other is cancelled
-- and the result fails.
instance Applicative Zift where
    pure = ZiftPure
    (<*>) = ZiftApp

-- | 'Zift' actions can be composed.
instance Monad Zift where
    (>>=) = ZiftBind
    fail = Fail.fail

-- | A 'Zift' action can fail.
--
-- To make a Zift action fail, you can use the @fail :: String -> Zift a@
-- function.
--
-- The implementation uses the given string as the message that is shown at
-- the very end of the run.
instance MonadFail Zift where
    fail = ZiftFail
    -- fail s = Zift $ \_ -> pure $ ZiftFailed s

-- | Any IO action can be part of a 'Zift' action.
--
-- This is the most important instance for the end user.
--
-- > liftIO :: IO a -> Zift a
-- allows embedding arbitrary IO actions inside a 'Zift' action.
--
-- The implementation also ensures that exceptions are caught.
instance MonadIO Zift where
    liftIO = ZiftIO

instance MonadThrow Zift where
    throwM = ZiftIO . throwM

data ZiftResult a
    = ZiftSuccess a
    | ZiftFailed String
    deriving (Show, Eq, Generic)

instance Validity a => Validity (ZiftResult a)

instance Monoid a => Monoid (ZiftResult a) where
    mempty = ZiftSuccess mempty
    mappend z1 z2 = mappend <$> z1 <*> z2

instance Functor ZiftResult where
    fmap f (ZiftSuccess a) = ZiftSuccess $ f a
    fmap _ (ZiftFailed s) = ZiftFailed s

instance Applicative ZiftResult where
    pure = ZiftSuccess
    (ZiftSuccess f) <*> (ZiftSuccess a) = ZiftSuccess $ f a
    (ZiftFailed e) <*> (ZiftSuccess _) = ZiftFailed e
    (ZiftSuccess _) <*> (ZiftFailed e) = ZiftFailed e
    (ZiftFailed e1) <*> (ZiftFailed e2) = ZiftFailed $ unwords [e1, e2]

instance Monad ZiftResult where
    (ZiftSuccess a) >>= fb = fb a
    (ZiftFailed e) >>= _ = ZiftFailed e

instance MonadFail ZiftResult where
    fail = ZiftFailed
-- -- | Internal: do not use yourself.
-- tryFlushZiftBuffer :: ZiftContext -> ZiftState -> IO ZiftState
-- tryFlushZiftBuffer ctx st =
--     if flushable $ recursionList ctx
--         then do
--             let zos = reverse $ bufferedOutput st
--                 st' = st {bufferedOutput = []}
--             atomically $ mapM_ (writeTChan $ printChan ctx) zos
--             pure st'
--         else pure st
--
-- -- The buffer is flushable when it's guaranteed to be the first in the in-order
-- -- of the evaluation tree.
-- flushable :: [LR] -> Bool
-- flushable = all (== L)

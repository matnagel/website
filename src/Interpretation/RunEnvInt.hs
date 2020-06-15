{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}


module Interpretation.RunEnvInt (
    RunEnvInt, execWithEnvironment
)
where

import qualified Control.Monad.State.Lazy as SM

import Interpretation.Environment

import qualified HtmlInterface as HI
import HtmlInterface(HasMenu(..))
import MarkLightParser

newtype RunEnvInt a = MkEnvI (Environment -> IO a)

instance Functor RunEnvInt where
    fmap f (MkEnvI x) = MkEnvI $ \env -> f <$> x env

instance Applicative RunEnvInt where
    pure x = MkEnvI $ \env -> return x
    (<*>) m1 m2 = m1 >>= (\x1 -> m2 >>= (\x2 -> return (x1 x2)))

instance Monad RunEnvInt where
    (>>=) (MkEnvI x) f = MkEnvI $ (\env -> do
        (MkEnvI res) <- f <$> (x env)
        res env)

instance MonadFail RunEnvInt where
    fail str = MkEnvI $ \env -> fail str

execWithEnvironment :: Environment -> RunEnvInt a -> IO a
execWithEnvironment env (MkEnvI x) = x env

instance HasMenu RunEnvInt where
    getMenu = MkEnvI $ \env -> return $ (HI.menuBlockFromList . getMenuEntries) env
    registerMenu mentry = return ()

instance ReadLocal RunEnvInt where
    readResource (MkLocalPath pth) =  MkEnvI $ const (readFile pth)

instance WriteLocal RunEnvInt where
    writeResource (MkLocalPath pth) cont = MkEnvI $ const $ (writeFile pth cont)

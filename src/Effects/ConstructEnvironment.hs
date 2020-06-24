{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}


module Effects.ConstructEnvironment (
    computeEnvironment, emptyEnvironment
)
where

import qualified Control.Monad.State.Lazy as SM

import Effects.Environment

import qualified HtmlInterface as HI
import HtmlInterface(HasMenu(..))
import MarkLightParser

newtype ConstructEnvironment a = MkEnvI (SM.StateT Environment IO a) deriving (Functor, Applicative, Monad)

computeEnvironment :: Environment -> ConstructEnvironment a -> IO Environment
computeEnvironment env (MkEnvI x) = SM.execStateT x env

instance (SM.MonadState Environment) ConstructEnvironment  where
    get = MkEnvI $ SM.get
    put s = MkEnvI $ SM.put s

instance HasMenu ConstructEnvironment where
    getMenu = return mempty -- (HI.menuBlockFromList . getMenuEntries) <$> SM.get
    registerMenu mentry = SM.modify (appendMenuEntry mentry)

instance ReadLocal ConstructEnvironment where
    readResource (MkLocalPath pth) =  MkEnvI $ SM.lift $ readFile pth

instance WriteLocal ConstructEnvironment where
    writeResource (MkLocalPath pth) cont = return () -- MkEnvI $ SM.lift $ writeFile pth cont

instance MonadFail ConstructEnvironment where
    fail str = MkEnvI $ fail str

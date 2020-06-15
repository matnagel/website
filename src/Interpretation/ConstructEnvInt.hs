{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}


module Interpretation.ConstructEnvInt (
    computeEnvironment, emptyEnvironment
)
where

import qualified Control.Monad.State.Lazy as SM

import Interpretation.Environment

import qualified HtmlInterface as HI
import HtmlInterface(HasMenu(..))
import MarkLightParser

newtype ConstEnvInt a = MkEnvI (SM.StateT Environment IO a) deriving (Functor, Applicative, Monad)

computeEnvironment :: Environment -> ConstEnvInt a -> IO Environment
computeEnvironment env (MkEnvI x) = SM.execStateT x env


instance (SM.MonadState Environment) ConstEnvInt  where
    get = MkEnvI (SM.get :: SM.StateT Environment IO Environment)
    put s = MkEnvI (SM.put s)

instance HasMenu ConstEnvInt where
    getMenu = (HI.menuBlockFromList . getMenuEntries) <$> SM.get
    registerMenu mentry = SM.modify (appendMenuEntry mentry)

instance ReadLocal ConstEnvInt where
    readResource (MkLocalPath pth) =  MkEnvI $ SM.lift (readFile pth)

instance WriteLocal ConstEnvInt where
    writeResource (MkLocalPath pth) cont = MkEnvI $ SM.lift (writeFile pth cont)

instance MonadFail ConstEnvInt where
    fail str = MkEnvI $ fail str

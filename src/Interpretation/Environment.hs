{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}


module Interpretation.Environment (
    appendMenuEntry, Environment (..), emptyEnvironment
)
where

import qualified Control.Monad.State.Lazy as SM

import qualified HtmlInterface as HI
import MarkLightParser

data Environment = MkEnvironment { getMenuEntries :: [HI.MenuEntry] }

appendMenuEntry :: HI.MenuEntry -> Environment -> Environment
appendMenuEntry entry env = MkEnvironment (entry : (getMenuEntries env))

emptyEnvironment = MkEnvironment []

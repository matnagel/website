module Effects.Environment (
    appendMenuEntry, Environment (..), emptyEnvironment
)
where

import qualified HtmlInterface as HI

data Environment = MkEnvironment { getMenuEntries :: [HI.MenuEntry] }

appendMenuEntry :: HI.MenuEntry -> Environment -> Environment
appendMenuEntry entry env = MkEnvironment (entry : (getMenuEntries env))

emptyEnvironment :: Environment
emptyEnvironment = MkEnvironment []

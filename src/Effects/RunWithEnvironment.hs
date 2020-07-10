module Effects.RunWithEnvironment (
    RunWithEnvironment, execWithEnvironment
)
where

import Effects.Environment
import qualified HtmlInterface as HI
import MarkLightParser

newtype RunWithEnvironment a = MkEnvI (Environment -> IO a)

instance Functor RunWithEnvironment where
    fmap f (MkEnvI x) = MkEnvI $ \env -> f <$> x env

instance Applicative RunWithEnvironment where
    pure x = MkEnvI $ const $ return x
    (<*>) m1 m2 = m1 >>= (\x1 -> m2 >>= (\x2 -> return (x1 x2)))

instance Monad RunWithEnvironment where
    (>>=) (MkEnvI x) f = MkEnvI $ (\env -> do
        (MkEnvI res) <- f <$> (x env)
        res env)

instance MonadFail RunWithEnvironment where
    fail str = MkEnvI $ const $ fail str

execWithEnvironment :: Environment -> RunWithEnvironment a -> IO a
execWithEnvironment env (MkEnvI x) = x env

instance HI.HasMenu RunWithEnvironment where
    getMenu = MkEnvI $ \env -> return $ (HI.menuBlockFromList . reverse . getMenuEntries) env
    registerMenu _ = return ()

instance ReadLocal RunWithEnvironment where
    readResource (MkLocalPath pth) =  MkEnvI $ const (readFile pth)

instance WriteLocal RunWithEnvironment where
    writeResource (MkLocalPath pth) cont = MkEnvI $ const $ (writeFile pth cont)

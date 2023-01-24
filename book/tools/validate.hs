#!/usr/bin/env stack
-- stack --resolver lts-20.8 script --package conduit --package xml-conduit --package filepath --package executable-path
{-# LANGUAGE OverloadedStrings #-}
import System.FilePath (takeDirectory, (</>), takeFileName)
import Conduit
import qualified Text.XML
import System.Environment.Executable (getScriptPath, ScriptPath (RunGHC))
import Control.Monad (void)

main :: IO ()
main = do
    RunGHC hs <- getScriptPath
    runResourceT
        $ sourceDirectory (takeDirectory hs </> ".." </> "generated-xml")
       $$ mapM_C (\fp -> liftIO $ do
            putStrLn $ "Validating: " ++ takeFileName fp
            void $ Text.XML.readFile Text.XML.def fp)

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
module Main where
import Shelly
import Prelude hiding(FilePath,unwords)
import System.Environment
import Data.Text.Lazy as LT
import Data.String(IsString,fromString)
default (LT.Text)
  
type MyResult a = Either Text a

infix 3 `orDie`
orDie :: Sh Bool -> Text -> Sh ()
c `orDie` msg = unlessM c (errorExit msg)

die :: Text -> Sh a
die t = errorExit t >> return undefined

requireDir :: FilePath -> Sh ()
requireDir d = test_d d `orDie` 
               LT.unwords ["Required dir ",toTextIgnore d, " does not exist"]
main = do
  args <- getArgs
  case args of
    [projectDir, testDir] -> tester (fromString projectDir) (fromString testDir)
    _ -> print "Usage: tester projectDir testDir"
    

tester :: FilePath -> FilePath -> IO () 
tester projectDir testDir = shelly $ verbosely $ do
  test_d projectDir `orDie` "No project directory"
  requireDir testDir 
  let goodDir = testDir </> "good"
  requireDir goodDir
  let badDir = testDir </> "bad"
  requireDir badDir
  cd projectDir
  (opts, archive) <- findArchive 
  inspect archive
  tar opts archive
  run_ "make" []
  test_f "latc" `orDie` "latc executable not found"
  b <- testGoodOne $ goodDir </> "core001.lat"
  if b then echo "Good tests passed" else echo "Good tests failed"
  return ()
  
testGoodOne :: FilePath -> Sh Bool
testGoodOne fp = do
  ft <- toTextWarn fp
  silently $ run_ "./latc" [ft]
  trace "stderr:"
  lastStderr >>= trace
  lastStderrHeadIs "OK"
  
lastStderrHead :: Sh (MyResult Text)
lastStderrHead = do
  text <- lastStderr
  return $ case LT.lines text of
    [] -> Left "empty stderr"
    (l:_) -> Right l
    
lastStderrHeadIs :: Text -> Sh Bool
lastStderrHeadIs expected = do
  text <- lastStderr
  case LT.lines text of
    [] -> echo "empty stderr" >> return False
    (got:_) | got == expected -> return True
            | otherwise -> do
                echo $ unwords ["Expected",expected,"got",got]
                return False
                
findArchive :: Sh (Text,FilePath)
findArchive = do
  allFiles <- ls "."
  echo "All project files:"
  inspect allFiles
  let archives = [(opts, s) | 
                    (opts, ext) <- [("zxf", ".tar.gz"), ("zxf", ".tgz"), ("jxf", ".tar.bz2"), ("jxf", ".tar.bzip2"), ("xf", ".tar")],
                    s <- Prelude.filter (isSuffixOfTFP ext) allFiles]
  echo "Archives:"
  inspect archives
  case archives of
    [a] -> return a
    [] -> die $ "No archive found"
    _ -> die "Multiple archives found"
    
        
isSuffixOfTFP :: Text -> FilePath -> Bool
isSuffixOfTFP t fp = LT.isSuffixOf t (toTextIgnore fp)

tar :: Text -> FilePath -> Sh ()
tar opts archive = do
  a <- toTextWarn archive
  run_ (fromText "tar") [opts,a]
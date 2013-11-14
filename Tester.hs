{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
module Main where
import Shelly
import Prelude hiding(FilePath,unwords)
import System.Environment
import qualified Data.Text as LT
import Data.Text(Text)
import Data.String(IsString,fromString)
import Control.Monad(forM)

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
  requireDir (testDir </> "good") 
  let newTestDir = projectDir </> "testerTests"
  cp_r testDir newTestDir
  cd projectDir
  (opts, archive) <- findArchive 
  inspect archive
  tar opts archive
  run_ "make" []
  test_f "latc" `orDie` "latc executable not found"
  let relTestDir = "testerTests"
  testFrontend relTestDir
  return ()
  
testFrontend :: FilePath -> Sh ()
testFrontend newTestDir = do
  let goodDir = newTestDir </> "good"
  requireDir goodDir
  let badDir = newTestDir </> "bad"
  requireDir badDir
  goodFiles <- (ls goodDir >>= return . havingExt "lat")
  results <- forM goodFiles testGoodOne
  if (and results) then echo "Good tests passed" 
                   else echo "Good tests failed"  

  badFiles <- (ls badDir >>= return . havingExt "lat")
  results <-   errExit False (forM badFiles testBadOne)
  if (and results) then echo "Bad tests passed" 
                   else echo "Bad tests failed"

testGoodOne :: FilePath -> Sh Bool
testGoodOne fp = do
  latc <- absPath "latc"
  ft <- toTextWarn fp
  cmd latc ft
  trace "stderr:"
  lastStderr >>= trace
  lastStderrHeadIs "OK"

testBadOne :: FilePath -> Sh Bool
testBadOne fp = do
  latc <- absPath "latc"
  ft <- toTextWarn fp
  -- echo "Expect ERROR"
  cmd latc ft
  trace "stderr:"
  lastStderr >>= trace
  lastStderrHeadIs "ERROR"

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
                echo $ LT.unwords ["Expected",expected,"got",got]
                return False
                
findArchive :: Sh (Text,FilePath)
findArchive = do
  allFiles <- ls "."
  echo "All project files:"
  inspect allFiles
  let archives = [(opts, s) | 
                    (opts, ext) <- [("xf", ".tar.gz"), ("xf", ".tgz"), ("xf", ".tar.bz2"), ("xf", ".tar.bzip2"), ("xf", ".tbz"), ("xf", ".tar")],
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
  cmd "tar" opts a
  
havingExt :: Text -> [FilePath] -> [FilePath]
havingExt ext = Prelude.filter (hasExt ext)
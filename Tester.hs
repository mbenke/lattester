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
  testGoodOne $ goodDir </> "core001.lat"
  
testGoodOne :: FilePath -> Sh ()
testGoodOne fp = do
  ft <- toTextWarn fp
  run_ "./latc" [ft]
  
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
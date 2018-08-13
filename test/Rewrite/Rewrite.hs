module Rewrite.Rewrite (testRewrite) where

import System.Process
import System.Directory
import Data.List
import System.IO (hIsEOF, hGetLine, Handle)
import Control.Monad (mapM_)

testRewrite :: [String] -> IO ()
testRewrite l = listDirectory prefix >>= mapM_ (runTest l) . filter (\x -> "sh." `isPrefixOf` reverse x)

runTest :: [String] -> FilePath -> IO ()
runTest l f = do
  (_, Just hout, _, p) <-
    createProcess_ f (proc "ghc" $ l ++ ["-O","-ddump-rule-firings","-fforce-recomp",f']){ std_out = CreatePipe }
  _ <- waitForProcess p
  lst <- hGetLines hout
  required <- takeWhile (not . isPrefixOf "-}") . tail . lines <$> readFile f'
  let was = map (\x -> any (isPrefixOf x . drop (length prefixRule)) lst) required
  case elemIndex False was of
    Nothing -> return ()
    Just b  -> error $ unlines
      [ ""
      , "Error"
      , "  TestFile: "++ f
      , "  The rule: "++ (required !! b)
      , "not fired"
      ]
  where
    f' = "./test/Rewrite/Test/" ++ f

prefix :: String
prefix = "test/Rewrite/Test/"

prefixRule :: String
prefixRule = "Rule fired: "

hGetLines :: Handle -> IO [String]
hGetLines h = do
  eof <- hIsEOF h
  if eof
     then return []
     else do
       line <- hGetLine h
       lines <- hGetLines h
       return (line:lines)

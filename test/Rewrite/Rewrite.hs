module Rewrite.Rewrite (testRewrite) where

import System.Process
import System.Directory
import Data.List
import System.IO (hIsEOF, hGetLine, Handle)
import Control.Monad (mapM_, forM_, when)

testRewrite :: [String] -> IO ()
testRewrite l = listDirectory prefix >>= mapM_ (runTest l) . filter (\x -> "sh." `isPrefixOf` reverse x)

runTest :: [String] -> FilePath -> IO ()
runTest l f = do
  (_, Just hout, _, p) <-
    createProcess_ f (proc "ghc" $ l ++ ["-O","-ddump-rule-firings","-fforce-recomp","-no-keep-hi-files","-no-keep-o-files",f']){ std_out = CreatePipe }
  _ <- waitForProcess p
  lst <- hGetLines hout

  let out = take (length f' - 3) f'
  isOutProduced <- doesPathExist out
  when isOutProduced $ removeFile out

  required <- takeWhile (not . isPrefixOf "-}") . tail . lines <$> readFile f'
  let was = map (\x -> (any (isPrefixOf x . drop (length prefixRule)) lst,x)) required
  putStrLn $ "\nTESTFILE: " ++ f ++ "\n"
  forM_ was $ \(b,n) ->
    if b
       then putStrLn $ unwords ["  Rule",n,"fired correctly"]
       else error $ unlines
        [ ""
        , "Error"
        , "  The rule: "++ n
        , "did not fire"
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

{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}
module Runner where

import System.IO
import System.IO.Temp
import System.Process
import Data.Text
import NeatInterpolation (text)

src :: Text -> Text -> Text
src z d= [text|
  #include <stdio.h>
  int main() {
    printf("Hello ${d} from generated ${z}");
  }
|]

compile :: FilePath -> FilePath -> IO ()
compile dir filePath = do
  (_, _, _, p) <- createProcess (proc "gcc" [filePath]) { cwd = Just dir, close_fds = True }
  _ <- waitForProcess p
  return ()

run :: FilePath -> IO ()
run dir = do
   withCreateProcess (shell "./a.out") { std_out = CreatePipe, cwd = Just dir } $ \_ (Just outputH) _ _ -> do
     outputText <- hGetContents outputH
     putStr outputText

compileAndRun :: IO ()
compileAndRun = do
  withSystemTempDirectory "wam-llvm-gen" $ \dir ->
    withTempFile dir "main.c" $ \filePath handle -> do
       hPutStr handle (unpack $ src "generated" "source")
       hSeek handle AbsoluteSeek 0
       compile dir filePath
       run dir

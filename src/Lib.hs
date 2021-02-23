module Lib where

import           Control.Concurrent.Async (forConcurrently)

import qualified Data.ByteString          as BS
import qualified Data.List                as List
import qualified Xeno.DOM                 as Xeno

run :: IO ()
run = do
  contents <- BS.readFile "example2.xml"
  let total = BS.length contents
  resps <- forConcurrently [1 .. total] $ \count -> do
    newContents <- BS.take (total - count) <$> BS.readFile "example2.xml"
    pure (Xeno.parse newContents)
  let (ok, errors) = List.foldl' go ([], []) resps
  putStrLn $ "errors: " <> show (length errors)
  putStrLn $ "good: " <> show (length ok)
  where
    go (ok, errors) resp = case resp of
      Left e  -> (ok, e : errors)
      Right r -> (r : ok, errors)

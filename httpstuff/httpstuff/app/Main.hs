module Main where

import Data.ByteString.Lazy
import Network.Wreq

urls :: [String]
urls = [ "http://httpbin.com/ip"
       , "http://httpbin.org/bytes/5"
       ]

mappingGet :: [IO (Response ByteString)]
mappingGet = map get urls

traversedUrls :: IO [Response ByteString]
mappingGet = map get urls

main :: IO ()
main = someFunc

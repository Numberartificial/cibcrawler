#!/usr/bin/env stack
-- stack --install-ghc --resolver lts-5.13 runghc --package http-conduit

  {- stack --resolver lts-8.8
   runghc
   -- package wreq
-}
{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( someFunc
    ) where


import Control.Lens
import Network.Wreq
import Data.Aeson
import Data.Aeson.Lens
import Data.Map as Map
import Data.Time.Clock

import qualified Data.Text.Lazy.IO as L
import Data.Text.Lazy.Encoding
import qualified Network.Wreq.Session as S

someFunc :: IO ()
someFunc = S.withSession $ \sess -> do
  -- First request: tell the server to set a cookie
  S.get sess "http://httpbin.org/cookies/set?name=hi"

  -- Second request: the cookie should still be set afterwards.
  r <- S.post sess "http://httpbin.org/post" ["a" := (3 :: Int)]
  print $ r ^. responseCookie "name" . cookieValue


loginCibhz :: IO ()
loginCibhz = S.withSession $ \session -> do
  -- S.get session "http://168.35.6.12:9999/zjcredit/login"
  r <- S.post session "http://168.35.6.12:9999/zjcredit/login" ["loginname" := ("cibhzgl2" :: String), "password" := ( "abcd1234" ::String)]
  print $ r
  r1 <- S.post session "http://168.35.6.12:9999/zjcredit/enterprise-search" ["q" := ("杭州" :: String)]
  L.putStr $ decodeUtf8 $ r1 ^. responseBody
    -- ^. responseCookie "name" . cookieValue

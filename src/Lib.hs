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
import qualified Network.Wreq.Session as S

someFunc :: IO ()
someFunc = S.withSession $ \sess -> do
  -- First request: tell the server to set a cookie
  S.get sess "http://httpbin.org/cookies/set?name=hi"

  -- Second request: the cookie should still be set afterwards.
  r <- S.post sess "http://httpbin.org/post" ["a" := (3 :: Int)]
  print $ r ^. responseCookie "name" . cookieValue

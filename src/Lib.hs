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
import qualified Data.Text.IO as Text
import qualified Data.Text as Text

import qualified Data.Text.Lazy.IO as L
import qualified Data.Text.Lazy as Lazy
import Data.Text.Lazy.Encoding
import qualified Network.Wreq.Session as S
import Text.HTML.DOM
import Text.XML.Cursor (Cursor, attributeIs, content, element, fromDocument, child,
                        ($//), (&|), (&//), (>=>))

import           Data.ByteString.Builder
import qualified Data.ByteString.Lazy as LazyB
import           Data.Monoid
import           Data.Foldable                        (foldMap)
import           Data.List                            (intersperse)

data Cell = StringC String
          | IntC Int
          deriving( Eq, Ord, Show )

type Row   = [Cell]
type Table = [Row]

encodeUtf8CSV :: Table -> LazyB.ByteString
encodeUtf8CSV = toLazyByteString . renderTable

renderTable :: Table -> Builder
renderTable rs = mconcat [renderRow r <> charUtf8 '\n' | r <- rs]

renderRow :: Row -> Builder
renderRow []     = mempty
renderRow (c:cs) =
    renderCell c <> mconcat [ charUtf8 ',' <> renderCell c' | c' <- cs ]

renderCell :: Cell -> Builder
renderCell (StringC cs) = renderString cs
renderCell (IntC i)     = intDec i

renderString :: String -> Builder
renderString cs = charUtf8 '"' <> foldMap escape cs <> charUtf8 '"'
  where
    escape '\\' = charUtf8 '\\' <> charUtf8 '\\'
    escape '\"' = charUtf8 '\\' <> charUtf8 '\"'
    escape c    = charUtf8 c

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
  r1 <- S.post session "http://168.35.6.12:9999/zjcredit/enterprise-search" ["q" := ("浙江朝日气动管业有限公司" :: String)]
  L.putStr $ decodeUtf8 $ r1 ^. responseBody
  putStrLn "json:"
  Text.putStr $ r1 ^. responseBody . nth 0 . key "id" . _String
  r2 <- S.get session $ "http://168.35.6.12:9999/zjcredit/enterprise_credit_detail" ++ "/" ++ Text.unpack (r1 ^. responseBody . nth 0 . key "id" . _String)
  L.putStr $ decodeUtf8 $ r2 ^. responseBody
      -- ^. responseCookie "name" . cookieValue

resPage url =
   S.withSession $ \session -> do
  -- S.get session "http://168.35.6.12:9999/zjcredit/login"
   r <- S.post session "http://168.35.6.12:9999/zjcredit/login" ["loginname" := ("cibhzgl2" :: String), "password" := ( "abcd1234" ::String)]
   S.get session "http://168.35.6.12:9999/zjcredit/enterprise_credit_detail/3300004000001382"
   >>= \r2 ->
   return $ decodeUtf8 $ r2 ^. responseBody
   -- L.putStr $ decodeUtf8 $ r2 ^. responseBody

page = L.putStr =<< resPage ""

googlePage =
  -- get "http://168.35.6.12:9999/zjcredit/login"
  get "http://localhost:8000"
  >>= return . (^. responseBody)

axeses =
  (Text.XML.Cursor.element "html" &// Text.XML.Cursor.element "h3")
  
parseGoogle =
  googlePage >>=
    return . axeses . fromDocument . parseLBS

-- parse HTML

-- Text.e URL we're going to search
url = "http://www.bing.com/search?q=school+of+haskell"

-- Text.e data we're going to search for
findNodes :: Cursor -> [Cursor]
findNodes = Text.XML.Cursor.element "h3"
  -- >=> attributeIs "id" "count" >=> child

-- Extract the data from each node in turn
extractData = Text.concat . content

-- Process the list of data elements
processData =  Text.putStr . Text.concat

cursorFor :: String -> IO Cursor
cursorFor u = do
     page <- resPage u
     return $ fromDocument $ parseLT page

-- test
parseDemo = do
     cursor <- cursorFor url
     processData $ cursor $// findNodes &| extractData


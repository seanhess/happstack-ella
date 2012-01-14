{-# LANGUAGE ScopedTypeVariables #-}
-- Ella, a wrapper around Happstack lite to make it more like sinatra 
-- For noobs, by noobs


module Happstack.Ella (get, post, route, routes, cap, mount) where

import Happstack.Lite (ServerPart, Response, Method(..), method, dir, nullDir, path, ok, toResponse)
import Control.Monad.Writer (tell, Writer, execWriter)
import Data.List.Split (splitOn)

-- import Control.Monad.IO.Class (liftIO)

type Path = String
type Handler = ServerPart Response

data PathSegment = PathName String |
                   Capture String

-- routes, route, let you convert a Writer Handler () block into Handler
routes, route :: Writer Handler () -> Handler
routes = execWriter
route = execWriter

-- so the interface to the functions that get passed in could be better, but this works
get :: Path -> ([String] -> Handler) -> Writer Handler ()
get = addRoute GET

post :: Path -> ([String] -> Handler) -> Writer Handler ()
post = addRoute POST

-- so now h is a function that first takes a string
-- can I make it return a function instead of actually calling it? 
-- yes, you just take off the h part
addRoute :: Method -> Path -> ([String] -> Handler) -> Writer Handler ()
addRoute m p h = tell $ do
    method m
    generate h ps []
    where ps = parsePath p

generate :: ([String] -> Handler) -> [PathSegment] -> [String] -> Handler
generate h ((PathName p):ps) cs = dir p (generate h ps cs)
generate h ((Capture c):ps) cs = path $ \(v :: String) -> generate h ps (v:cs)
generate h [] cs = h cs -- can't it just RETURN a function like this?

parsePath :: Path -> [PathSegment]
parsePath p = map parseSegment $ filter (not.null) $ splitOn "/" p

parseSegment :: String -> PathSegment
parseSegment (':':xs) = Capture xs
parseSegment xs = PathName xs



cap :: (String -> Handler) -> ([String] -> Handler) 
cap h caps = h (head caps)

-- right now only supports 1 level
mount :: Handler -> Writer Handler ()
mount = tell

mountAt :: String -> Handler -> Writer Handler()
mountAt = error "Todo: mountAt should mount another set of handlers at the specified urls"


{-

[√] /bob should not match /bob/woot
[√] /users/:id
[√] /users/:id/name
[ ] serveDirectory working

Later
[ ] Bring back sub-routers VS /bob/woot VS /bob
[ ] Better interface for captures

-}



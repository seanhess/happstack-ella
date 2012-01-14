{-# LANGUAGE ScopedTypeVariables, OverloadedStrings #-}
-- Ella, a wrapper around Happstack lite to make it more like sinatra 
-- For noobs, by noobs

module Happstack.Ella (get, post, route, routes, mount, Req(..), cap) where
import Happstack.Lite

import Happstack.Server.Types (RqBody(..), takeRequestBody)
import Happstack.Server (askRq)

import Control.Monad.Writer (tell, Writer, execWriter)
import Data.List.Split (splitOn)

import qualified Data.ByteString.Lazy.Char8 as L
import Control.Monad.IO.Class (liftIO)

type Path = String
type Handler = ServerPart Response

data PathSegment = PathName String |
                   Capture String

data Req = Req { captures :: [String], body :: L.ByteString }

-- returns the first capture
cap :: Req -> String
cap r = head (captures r)

-- routes, route, let you convert a Writer Handler () block into Handler
routes, route :: Writer Handler () -> Handler
routes = execWriter
route = execWriter

-- so the interface to the functions that get passed in could be better, but this works
get :: Path -> (Req -> Handler) -> Writer Handler ()
get = addRoute GET

post :: Path -> (Req -> Handler) -> Writer Handler ()
post = addRoute POST

-- so now h is a function that first takes a string
-- can I make it return a function instead of actually calling it? 
-- yes, you just take off the h part
addRoute :: Method -> Path -> (Req -> Handler) -> Writer Handler ()
addRoute m p h = tell $ do
    method m
    b <- getBody 
    generate ps [] $ \r -> 
        h $ r { body = b } 
    where ps = parsePath p

-- I feel like I should have to pass the "h" around everywhere in this
generate :: [PathSegment] -> [String] -> (Req -> Handler) -> Handler
generate ((PathName p):ps) cs h = dir p (generate ps cs h)
generate ((Capture c):ps) cs h = path $ \(v :: String) -> generate ps (v:cs) h
generate [] cs h = h (Req cs "")

parsePath :: Path -> [PathSegment]
parsePath p = map parseSegment $ filter (not.null) $ splitOn "/" p

parseSegment :: String -> PathSegment
parseSegment (':':xs) = Capture xs
parseSegment xs = PathName xs

-- returns the body text
getBody :: ServerPart L.ByteString
getBody = do
    req  <- askRq 
    body <- liftIO $ takeRequestBody req 
    case body of 
        Just rqbody -> return . unBody $ rqbody 
        Nothing     -> return "" 




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



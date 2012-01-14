-- Ella, a wrapper around Happstack lite to make it more like sinatra 
-- For the noobs, by a noob


module Happstack.Ella where

import Happstack.Lite (ServerPart, Response, Method(..), method, dir)
import Control.Monad.Writer (tell, Writer, execWriter)
import Data.List.Split (splitOn)

type Path = String
type Handler = ServerPart Response
type Route = Writer Handler ()

-- routes, route, let you convert a Writer Handler () block into Handler
routes, route :: Route -> Handler
routes = execWriter
route = execWriter

get :: Path -> Handler -> Route
get = addRoute GET

post :: Path -> Handler -> Route
post = addRoute POST

addRoute :: Method -> Path -> Handler -> Route
addRoute m p r = tell $ do
    method m
    let dirs = splitOn "/" p
    foldr combineDirs r dirs
    where combineDirs p r = dir p r





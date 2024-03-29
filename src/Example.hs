{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Happstack.Ella
import Happstack.Lite

import Control.Monad.IO.Class (liftIO)
import Data.ByteString.Lazy.Char8 as L

main :: IO ()
main = serve Nothing app

-- so, start at the top with the resource groups. You can put them in any order you wish
-- serve definitely wants a ServerPart Response
app :: ServerPart Response
app = routes $ do
    get "/hello/:name/loud" helloLoud -- means to call helloLoud with one argument
    get "/hello/:name" hello
    post "/hello" postName
    mount $ serveDirectory DisableBrowsing [] "./public"

root :: ServerPart Response
root = ok $ toResponse "root"

-- this is, of course, completely annoying
hello :: Req -> ServerPart Response
hello r = ok $ toResponse ("Hello " ++ (cap r))

helloLoud :: Req -> ServerPart Response
helloLoud r = ok $ toResponse ("HELLO " ++ (cap r))

postName :: Req -> ServerPart Response
postName r = do
    let bodyText = L.unpack (body r)
    ok $ toResponse ("WOOT " ++ bodyText)
   
    

happstackTests :: ServerPart Response
happstackTests = msum [ dir "asdf" $ nullDir >> asdf
                      ]

asdf :: ServerPart Response
asdf = ok $ toResponse "asdf2"

userById :: String -> ServerPart Response
userById userId = ok $ toResponse ("UserId: " ++ userId)


-- COMPANIES RESOURCE GROUP --
-- /companies/:companyId/people
-- /companies/:companyId
-- /companies/all

companies :: ServerPart Response
companies = do method GET -- guard against method GET here, because they all share it
               msum [routeAll, routeOne] -- companyById shares the same id-generating thing, no? So I would nest those
    where routeAll = do dir "all" getAllCompanies
          routeOne = do -- work with a specific company. Needs "do" to work (it doesn't something strange if you take it off)
            path $ \(companyId :: String) -> msum [
                        dir "people" companyPeople,
                        company
                    ] 

-- Controller in Companies.hs file?
getAllCompanies :: ServerPart Response
getAllCompanies = ok $ toResponse "All Companies"

company :: ServerPart Response
company = ok $ toResponse "Company"

companyPeople :: ServerPart Response
companyPeople = ok $ toResponse "Company People"


{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Happstack.Ella (routes, get)
import Happstack.Lite

main :: IO ()
main = serve Nothing app

-- so, start at the top with the resource groups. You can put them in any order you wish
app :: ServerPart Response
app = routes $ do
    get "hello" hello

    -- [
    -- -- , dir "companies" $ dir "all" $ allCompanies
    -- , dir "companies" $ companies
    -- , dir "users" $ users
    -- , method GET  >> nullDir >>      root
    -- , serveDirectory DisableBrowsing [] "./public"
    -- ]


root :: ServerPart Response
root = ok $ toResponse "root"

hello :: ServerPart Response
hello = ok $ toResponse "Hello"


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



-- to test my get DSL
-- [ ] Monad
-- [ ] Captures
-- users :: ServerPart Response
-- users = msum [ dir "fake" $ ok $ toResponse "fake"
--              , dir "fake2" $ ok $ toResponse "fake2"
--              -- , get "henry" company
--              -- , get "henry/name" companyPeople
--              -- , get "asdf" asdf
--              , dir "example" exampleRouteBlock
--              ]




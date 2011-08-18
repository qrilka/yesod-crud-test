{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Controller
    ( withYesod2
    , withDevelApp
    ) where

import Yesod2
import Settings
import Yesod.Helpers.Static
import Yesod.Helpers.Auth
import Yesod.Helpers.Crud
import Database.Persist.GenericSql
import Data.ByteString (ByteString)
import Data.Dynamic (Dynamic, toDyn)

-- Import all relevant handler modules here.
import Handler.Root
import Handler.Products

productCrud :: Yesod2 -> Crud Yesod2 Product
productCrud = defaultCrud

-- This line actually creates our YesodSite instance. It is the second half
-- of the call to mkYesodData which occurs in Yesod2.hs. Please see
-- the comments there for more details.
mkYesodDispatch "Yesod2" resourcesYesod2

-- Some default handlers that ship with the Yesod site template. You will
-- very rarely need to modify this.
getFaviconR :: Handler ()
getFaviconR = sendFile "image/x-icon" "config/favicon.ico"

getRobotsR :: Handler RepPlain
getRobotsR = return $ RepPlain $ toContent ("User-agent: *" :: ByteString)

-- This function allocates resources (such as a database connection pool),
-- performs initialization and creates a WAI application. This is also the
-- place to put your migrate statements to have automatic database
-- migrations handled by Yesod.
withYesod2 :: (Application -> IO a) -> IO a
withYesod2 f = Settings.withConnectionPool $ \p -> do
    runConnectionPool (runMigration migrateAll) p
    let h = Yesod2 s p
    toWaiApp h >>= f
  where
    s = static Settings.staticdir

withDevelApp :: Dynamic
withDevelApp = toDyn (withYesod2 :: (Application -> IO ()) -> IO ())

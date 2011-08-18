{-# LANGUAGE TemplateHaskell, QuasiQuotes, OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Handler.Products where

import Yesod2
import Control.Applicative
import Yesod.Form.Nic

instance ToForm Product Yesod2 where
  toForm p = fieldsToDivs $ Product
    <$> stringField "Code" (fmap productCode p)
    <*> nicHtmlField "Description"
            { ffsId = Just "desc"} (fmap productDescription p)

productFormlet :: Formlet s Yesod2 Product
productFormlet p = fieldsToDivs $ Product
    <$> stringField "Code" (fmap productCode p)
    <*> nicHtmlField "Description"
            { ffsId = Just "desc"} (fmap productDescription p)

getProductsR :: Handler RepHtml
getProductsR = do
    products <- runDB $ selectList [] [] 0 0
    (_, form, _) <- runFormGet $ productFormlet Nothing
    defaultLayout $ do
        setTitle "Products"
        addCassius $(cassiusFile "products")
        addWidget $(widgetFile "products")

postProductsR :: Handler RepHtml
postProductsR = do
    (res, form, _) <- runFormPostNoNonce $ productFormlet Nothing
    case res of
        FormSuccess prod -> do
            runDB $ insert prod
            setMessage "Product posted"
            redirect RedirectTemporary ProductsR
        _ -> return ()
    
    products <- runDB $ selectList [] [] 0 0
    defaultLayout $ do
        setTitle "Products"
        addCassius $(cassiusFile "products")
        addWidget $(widgetFile "products")

postProductDeleteR :: ProductId -> Handler ()
postProductDeleteR prid = do
    pr <- runDB $ get404 prid
    runDB $ delete prid
    setMessage "Product deleted"
    redirect RedirectTemporary ProductsR
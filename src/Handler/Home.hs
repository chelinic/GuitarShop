{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies, DeriveGeneric #-}


module Handler.Home where

import Import
import Network.HTTP.Types.Status
import Database.Persist.Postgresql
import Data.Time.Clock
import Data.Time.Calendar

footer :: WidgetT App IO ()  
footer = $(whamletFile "templates/Footer.hamlet")

home :: WidgetT App IO ()  
home = $(whamletFile "templates/Home.hamlet")

prods :: WidgetT App IO ()
prods = $(whamletFile "templates/Produto.hamlet")




getHomeR :: Handler Html
getHomeR = do
    clientelog <- lookupSession "_ID"
    defaultLayout $ do
        [whamlet|
            $maybe cli <- clientelog
                <h1> _{MsgBemvindo} - #{Cliente}
            $nothing
                <h1> _{MsgBemvindo} - _{MsgVisita}
            <ul>
                <li> <a href=@{CadastroCliR}> Cadastro de Cliente
                $maybe cli <- clientelog
                    <form action=@{CliLogoutR} method=post>
                        <input type="submit" value="Logout">
                    -- se tiver logado, faz o logout
                $nothing
                    <li> <a href=@{LoginCliR}> Login
            ^{home}
            ^{footer}
        |]


getRecemChegadosR :: Handler Html
getRecemChegadosR = do
    now <- getCurrentTime
    let (year, month, day) = toGregorian $ utctDay now
    defaultLayout $ do
    --	addStylesheet $ (StaticR css_recemchegados_css)
        [whamlet|
            ^{prods}
            ^{footer}
        |]
        
getGuitarrasR :: Handler Html
getGuitarrasR = do
    defaultLayout $ do
     --   addStylesheet $ (StaticR css_guitarras_css)
        
        [whamlet|
            ^{prods}
            ^{footer}
        |]
	    
getBaixosR :: Handler Html
getBaixosR = do
    defaultLayout $ do
       -- addStylesheet $ (StaticR css_baixos_css)
        [whamlet|
            ^{prods}
            ^{footer}
        |]        

getPedaisR :: Handler Html
getPedaisR = do
    defaultLayout $ do
        --addStylesheet $ (StaticR css_home_css)
        [whamlet|
            ^{prods}
            ^{footer}
        |]        

getAmplificadoresR :: Handler Html
getAmplificadoresR = do
    defaultLayout $ do
    	--addStylesheet $ (StaticR css_amplificadores_css)
    	[whamlet|
            ^{prods}
            ^{footer}
        |]

getAcessoriosR :: Handler Html
getAcessoriosR = do
    defaultLayout $ do
        --addStylesheet $ (StaticR css_home_css)
        [whamlet|
            ^{prods}
            ^{footer}
        |]	    
	    
getVioloesR :: Handler Html
getVioloesR = do
    defaultLayout $ do
        --addStylesheet $ (StaticR css_home_css)
        [whamlet|
            ^{prods}
            ^{footer}
        |]	    
	 
getVintageR :: Handler Html
getVintageR = do
    defaultLayout $ do
        --addStylesheet $ (StaticR css_home_css)
        [whamlet|
            ^{prods}
            ^{footer}
        |]	    
	    
getNovosR :: Handler Html
getNovosR = do
    defaultLayout $ do
        --addStylesheet $ (StaticR css_home_css)
        [whamlet|
            ^{prods}
            ^{footer}
        |]	    
	    
getUsadosR :: Handler Html
getUsadosR = do
    defaultLayout $ do
        --addStylesheet $ (StaticR css_home_css)
        [whamlet|
            ^{prods}
            ^{footer}
        |]	 
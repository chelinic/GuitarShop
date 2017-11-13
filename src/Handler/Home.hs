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

getHomeR :: Handler Html
getHomeR = do 
	defaultLayout $ do
	    addStylesheet $ (StaticR css_home_css)
	    addStylesheet $ (StaticR css_bootstrap_css)
	    $(whamletFile "templates/Home.hamlet")
    
getRecemChegadosR :: Handler Html
getRecemChegadosR = do
    defaultLayout $ do
        toWidget $ [lucius|
            h1 {
                color: blue;
            }
        |]
        [whamlet|
            <h1> Guitarras
        |]
        
getGuitarrasR :: Handler Html
getGuitarrasR = do
    defaultLayout $ do
        toWidget $ [lucius|
            h1 {
                color: blue;
            }
        |]
        [whamlet|
            <h1> Guitarras
        |]
        

getBaixosR :: Handler Html
getBaixosR = do
    defaultLayout $ do
        toWidget $ [lucius|
            h1{
                color: blue;
            }
        |]
        [whamlet|
            <h1> Baixos
        |]

getPedaisR :: Handler Html
getPedaisR = do
    defaultLayout $ do
        toWidget $ [lucius|
            h1{
                color: blue;
            }
        |]
        [whamlet|
            <h1> Pedais
        |]

getAmplificadoresR :: Handler Html
getAmplificadoresR = do
    defaultLayout $ do
        toWidget $ [lucius|
            h1{
                color: blue;
            }
        |]
        [whamlet|
            <h1> AmplificadoresR
        |]

getAcessoriosR :: Handler Html
getAcessoriosR = do
    defaultLayout $ do
        toWidget $ [lucius|
            h1{
                color: blue;
            }
        |]
        [whamlet|
            <h1> Acessorios
        |]

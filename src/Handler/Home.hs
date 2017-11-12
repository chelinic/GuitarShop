{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Home where

import Import
import Network.HTTP.Types.Status
import Database.Persist.Postgresql

getHomeR :: Handler Html
getHomeR = do
        defaultLayout $ do
        $ (whamletFile "templates/home.whamlet")

getRecemChegadosR :: Handler Html
getRecemChegadosR = do
    defaultLayout $ do
        toWidget $ [lucius|
            h1{
                color:pink;
            }
        |]
        [whamlet|
            <h1> Recem Chegados
            <a href=@{HomeR}> Voltar
    |]


getGuitarrasR :: Handler Html
getGuitarrasR = do
    defaultLayout $ do
        toWidget $ [lucius|
            h1{
                color:pink;
            }
        |]
        [whamlet|
            <h1> Guitarras
            <a href=@{HomeR}> Voltar
    |]

getBaixosR :: Handler Html
getBaixosR = do
    defaultLayout $ do
        toWidget $ [lucius|
            h1{
                color:pink;
            }
        |]
        [whamlet|
            <h1> Baixos
            <a href=@{HomeR}> Voltar
        |]

getPedaisR :: Handler Html
getPedaisR = do
    defaultLayout $ do
        toWidget $ [lucius|
            h1{
                color:pink;
            }
        |]
        [whamlet|
            <h1> Pedais
            <a href=@{HomeR}> Voltar
        |]
        
getAcessoriossR :: Handler Html
getAcessoriossR = do
    defaultLayout $ do
        toWidget $ [lucius|
            h1{
                color:pink;
            }
        |]
        [whamlet|
            <h1> Pedais
            <a href=@{HomeR}> Voltar
        |]


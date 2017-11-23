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
    clientelog <- lookupSession "_ID"
    defaultLayout $ do
       --addStylesheet $(StaticR css_home_css)
	    $(whamletFile "templates/Home.whamlet")
	     [whamlet|
            $maybe aluno <- alunologado
                <h1> _{MsgBemvindo} - #{aluno}
            $nothing
                <h1> _{MsgBemvindo} - _{MsgVisita}
            <ul>
                <li> <a href=@{AlunoR}> Cadastro de Usuario
                $maybe cli <- clientelog
                    <form action=@{LogoutCliR} method=post>
                        <input type="submit" value="Logout">
                    -- se tiver logado, faz o logout
                $nothing
                    <li> <a href=@{LoginCliR}> Login
        |]

getRecemChegadosR :: Handler Html
getRecemChegadosR = do
    defaultLayout $ do
    --	addStylesheet $ (StaticR css_recemchegados_css)
    	$(whamletFile "templates/RecemChegados.whamlet")
        
getGuitarrasR :: Handler Html
getGuitarrasR = do
    defaultLayout $ do
     --   addStylesheet $ (StaticR css_guitarras_css)
        $(whamletFile "templates/Guitarras.whamlet")
	    
getBaixosR :: Handler Html
getBaixosR = do
    defaultLayout $ do
       -- addStylesheet $ (StaticR css_baixos_css)
        $(whamletFile "templates/Baixos.whamlet")

getPedaisR :: Handler Html
getPedaisR = do
    defaultLayout $ do
        --addStylesheet $ (StaticR css_home_css)
        $(whamletFile "templates/Pedais.whamlet")

getAmplificadoresR :: Handler Html
getAmplificadoresR = do
    defaultLayout $ do
    	--addStylesheet $ (StaticR css_amplificadores_css)
    	$(whamletFile "templates/Amplificadores.whamlet")

getAcessoriosR :: Handler Html
getAcessoriosR = do
    defaultLayout $ do
        --addStylesheet $ (StaticR css_home_css)
	    $(whamletFile "templates/Acessorios.whamlet")
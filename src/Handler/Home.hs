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
       --addStylesheet $(StaticR css_home_css)
	    $(whamletFile "templates/Home.whamlet")

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
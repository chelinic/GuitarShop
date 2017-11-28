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
import Database.Esqueleto.PostgreSQL
import qualified Database.Esqueleto      as E
import Database.Esqueleto   ((^.))

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
    hoje <- getCurrentTime 
    --let (year, month, day) = toGregorian $ utctDay now
    defaultLayout $ do
    --	addStylesheet $ (StaticR css_recemchegados_css)
        [whamlet|
            ^{prods}
            listaprods <- runDB $ selectList [ProdutoDtChegada <=. (hoje-7)] []
            <div>
                <table>
                    <thead>
                        <tr>
                            <td> Id
                            <td> Nome 
                            <td> Estoqueminimo
                            <td> Estoqueatual
                            <td> Produto
                            <td>  
                
                    <tbody>
                        $forall (Entity idproduto produto) <- listaprods
                            <tr> 
                                <td> #{fromSqlKey idproduto}
                                <td> #{produtoNome produto}
                                <td> #{produtoEstoqueatual produto}
                                <td> #{produtoEstoqueminimo produto}
                                <td> 
       
            ^{footer}
        |]
        
{--
Query Join

getListaProdFR :: ClienteId -> Handler TypedContent
getListaProdFR cid = do 
    lista' <- runDB $ selectList [CompraCliid ==. cid] []
    lista <- return $ fmap (\(Entity _ comp) -> comp) lista'
    prodsIds <- return $ fmap compraProid lista
    produtos <- sequence $ fmap (\pid -> runDB $ get404 pid) prodsIds
    sendStatusJSON ok200 (object ["resp" .= (toJSON produtos)])
    
--}

getGuitarrasR :: Handler Html
getGuitarrasR = do
    let listaprods = runDB $ E.select
       $ E.from $ \(Produto `E.InnerJoin` TipoProd) -> do
            E.on $ Produto ^. ProdutotipoprodID E.==. TipoProd ^. TipoProdId
                E.where_ $
                    E. TipoProdnome = val "Guitarra"
                return
                    ( Produto ^. ProdutoId
                    , Produto ^. ProdutoNome
                    , Produto ^. ProdutoEstoqueAtual
                    , EstadoUso ^. EstadoUsonome
                    
                    )
    defaultLayout $ do
     --   addStylesheet $ (StaticR css_guitarras_css)
        
        [whamlet|
            ^{prods}
            ^{footer}
        |]
	    
getBaixosR :: Handler Html
getBaixosR = do
    let listaprods = runDB $ E.select
       $ E.from $ \(Produto `E.InnerJoin` TipoProd) -> do
            E.on $ Produto ^. ProdutotipoprodID E.==. TipoProd ^. TipoProdId
                E.where_ $
                    E. TipoProdnome = val "Baixo"
                return
                    ( Produto ^. ProdutoId
                    , Produto ^. ProdutoNome
                    , Produto ^. ProdutoEstoqueAtual
                    , EstadoUso ^. EstadoUsonome
                    
                    )
    defaultLayout $ do
       -- addStylesheet $ (StaticR css_baixos_css)
        [whamlet|
            ^{prods}
            ^{footer}
        |]        

getPedaisR :: Handler Html
getPedaisR = do
    let listaprods = runDB $ E.select
       $ E.from $ \(Produto `E.InnerJoin` TipoProd) -> do
            E.on $ Produto ^. ProdutotipoprodID E.==. TipoProd ^. TipoProdId
                E.where_ $
                    E. TipoProdnome = val "Pedal"
                return
                    ( Produto ^. ProdutoId
                    , Produto ^. ProdutoNome
                    , Produto ^. ProdutoEstoqueAtual
                    , EstadoUso ^. EstadoUsonome
                    
                    )
    defaultLayout $ do
        --addStylesheet $ (StaticR css_home_css)
        [whamlet|
            ^{prods}
            ^{footer}
        |]        

getAmplificadoresR :: Handler Html
getAmplificadoresR = do
    let listaprods = runDB $ E.select
       $ E.from $ \(Produto `E.InnerJoin` TipoProd) -> do
            E.on $ Produto ^. ProdutotipoprodID E.==. TipoProd ^. TipoProdId
                E.where_ $
                    E. TipoProdnome = val "Amplificador"
                return
                    ( Produto ^. ProdutoId
                    , Produto ^. ProdutoNome
                    , Produto ^. ProdutoEstoqueAtual
                    , EstadoUso ^. EstadoUsonome
                    
                    )
    defaultLayout $ do
    	--addStylesheet $ (StaticR css_amplificadores_css)
    	[whamlet|
            ^{prods}
            ^{footer}
        |]

getAcessoriosR :: Handler Html
getAcessoriosR = do
    let listaprods = runDB $ E.select
        $ E.from $ \(Produto `E.InnerJoin` TipoProd) -> do
            E.on $ Produto ^. ProdutotipoprodID E.==. TipoProd ^. TipoProdId
                E.where_ $
                    E. TipoProdnome = val "Acessorio"
                return
                    ( Produto ^. ProdutoId
                    , Produto ^. ProdutoNome
                    , Produto ^. ProdutoEstoqueAtual
                    , EstadoUso ^. EstadoUsonome
                    
                    )
    defaultLayout $ do
        --addStylesheet $ (StaticR css_home_css)
        [whamlet|
            ^{prods}
            ^{footer}
        |]	    
	    
getVioloesR :: Handler Html
getVioloesR = do
    let listaprods = runDB $ E.select
        $ E.from $ \(Produto `E.InnerJoin` TipoProd) -> do
            E.on $ Produto ^. ProdutotipoprodID E.==. TipoProd ^. TipoProdId
                E.where_ $
                    E. TipoProdnome = val "Violão"
                return
                    ( Produto ^. ProdutoId
                    , Produto ^. ProdutoNome
                    , Produto ^. ProdutoEstoqueAtual
                    , EstadoUso ^. EstadoUsonome
                    
                    )
    defaultLayout $ do
        --addStylesheet $ (StaticR css_home_css)
        [whamlet|
            ^{prods}
            ^{footer}
        |]	    
	 
getVintageR :: Handler Html
getVintageR = do
    let listaprods = runDB $ E.select
        $ E.from $ \(Produto `E.InnerJoin` EstadoUso) -> do
            E.on $ Produto ^. ProdutotipoprodID E.==. TipoProd ^. TipoProdId
                E.where_ $
                    E. EstadoUsonome = val "Novo"
                return
                    ( Produto ^. ProdutoId
                    , Produto ^. ProdutoNome
                    , Produto ^. ProdutoEstoqueAtual
                    , EstadoUso ^. EstadoUsonome
                    
                    )
    defaultLayout $ do
        --addStylesheet $ (StaticR css_home_css)
        [whamlet|
            ^{prods}
            ^{footer}
        |]	    
	    
getNovosR :: Handler Html
getNovosR = do
    let listaprods = runDB $ E.select
        $ E.from $ \(Produto `E.InnerJoin` EstadoUso) -> do
            E.on $ Produto ^. ProdutotipoprodID E.==. TipoProd ^. TipoProdId
                E.where_ $
                    E. EstadoUsonome = val "Novo"
                return
                    ( Produto ^. ProdutoId
                    , Produto ^. ProdutoNome
                    , Produto ^. ProdutoEstoqueAtual
                    , EstadoUso ^. EstadoUsonome
                    
                    )
    defaultLayout $ do
        --addStylesheet $ (StaticR css_home_css)
        [whamlet|
            ^{prods}
            ^{footer}
        |]	    
	    
getUsadosR :: Handler Html
getUsadosR = do
    let listaprods = runDB $ E.select
       $ E.from $ \(Produto `E.InnerJoin` EstadoUso) -> do
            E.on $ Produto ^. ProdutotipoprodID E.==. TipoProd ^. TipoProdId
                E.where_ $
                    E. EstadoUsonome = val "Usado"
                return
                    ( Produto ^. ProdutoId
                    , Produto ^. ProdutoNome
                    , Produto ^. ProdutoEstoqueAtual
                    , EstadoUso ^. EstadoUsonome
                    
                    )
    defaultLayout $ do
        --addStylesheet $ (StaticR css_home_css)
        [whamlet|
            ^{prods}
            ^{footer}
        |]	 
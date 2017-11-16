{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies, DeriveGeneric #-}
{-# LANGUAGE QuasiQuotes #-}

module Handler.Produto where

import Import
import Network.HTTP.Types.Status
import Database.Persist.Postgresql
import GHC.Generics
	
data Nome = Nome {nome :: Text} deriving Generic
instance ToJSON Nome where
instance FromJSON Nome where

formProduto :: Form Produto
formProduto = renderBootstrap $ Produto 
    <$> areq textField "Nome:" Nothing
    <*> areq doubleField "Valor:" Nothing
    <*> areq dayField "Data de chegada:" Nothing
    <*> areq intField "Ano:" Nothing
    <*> areq textField "Observacoes:" Nothing
    <*> areq intField "Estoque minimo:" Nothing
    <*> areq intField "Estoque atual:" Nothing

    
postApagarProdR :: ProdutoId -> Handler Html
postApagarProdR pid = do
    runDB $ delete pid
    redirect TodosProdR

getTodosProdR :: Handler Html
getTodosProdR = do 
    produtos <- runDB $ selectList [] [Asc ProdutoNome]
    defaultLayout $ do 
        addStylesheet $ (StaticR css_bootstrap_css)
        [whamlet|
            <table>
                <thead>
                    <tr>
                        <td> Id
                        <td> Nome 
                        <td> Valor 
                        <td> Produto
                        <td> 
                
                <tbody>
                    $forall (Entity pid produto) <- produtos
                        <tr> 
                            <td> #{fromSqlKey pid}
                            <td> #{produtoNome produto}
                            <td> #{produtoValor produto}
                            <td> #{produtoEstoqueatual produto}
                            <td> 
                                <form action=@{ApagarProdR pid} method=post>
                                    <input type="submit" value="Deletar">
                            
        |]


getBuscaProdR :: ProdutoId -> Handler Value
getBuscaProdR pid = do
	produto <- runDB $ get404 pid
	sendStatusJSON ok200 (object ["resp" .= toJSON produto])
{--
	[whamlet|
	    <ul>
	        $forall prods <- produto
	        <li> #{pack prods}
	 |]
--}
    		
-- unpack retorna string 
-- return para colocar algo puro dentro da mônada
-- $ para monada Maybe	

putAlteraProdR :: ProdutoId -> Handler Value
putAlteraProdR pid = do
	_ <- runDB $ get404 pid
	novoProd <- requireJsonBody :: Handler Produto
	runDB $ replace pid novoProd
	sendStatusJSON noContent204 (object ["resp" .=("Updated" ++ show (fromSqlKey pid))])
	
getVerProdR :: ProdutoId -> Handler Html
getVerProdR pid = do
    produto <- runDB $ get404 pid
    defaultLayout $
        [whamlet| 
            <div>
            <ul>
                <li><strong> Nome: #{produtoNome produto}
                <li><strong> Preco: #{produtoValor produto}
                <li><strong> Ano: #{produtoAno produto}
                <li><strong> Estoque: #{produtoEstoqueatual produto}
                <li><strong> Observacoes: #{produtoObservacoes produto}
        |]

postCadastroProdR :: Handler Html
postCadastroProdR = do
    ((result,_),_) <- runFormPost formProduto
    case result of
        FormSuccess produto -> do
            runDB $ insert produto
            redirect CadastroProdR
        _ -> redirect HomeR
        -- em caso de erro
        
getCadastroProdR :: Handler Html
getCadastroProdR = do
    (widget, enctype) <- generateFormPost formProduto
    defaultLayout $ do
        addStylesheet $ (StaticR css_bootstrap_css)
        [whamlet|
            <form action=@{CadastroProdR} method=post anctype=#{enctype}>
            ^{widget}
            <input type="submit" value="Cadastrar">
        |]
    {--
        o widget é o formulário
        o action é o mesmo, porém no post
    
    --}
    
getDetalheProdR :: ProdutoId -> Handler Html
getDetalheProdR pid = do
    produto <- runDB $ get404 pid
    defaultLayout $
        [whamlet| 
            <div>
            <ul>
                <li><strong> Nome: #{produtoNome produto}
                <li><strong> Preco: #{produtoValor produto}
                <li><strong> Estoque: #{produtoEstoqueatual produto}
        |]
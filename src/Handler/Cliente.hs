{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}

module Handler.Cliente where

import Import
import Database.Persist.Postgresql
import GHC.Generics

formCliente :: Form Cliente
formCliente = renderBootstrap $ Cliente
    <$> areq textField "Nome:" Nothing
    <*> areq textField "CPF:" Nothing
    <*> areq textField "Telefone:" Nothing
    <*> areq textField "Email:" Nothing
    <*> areq textField "Senha:" Nothing
    
    
getLoginCliR :: Text -> Text -> Handler TypedContent
getLoginCliR = undefined


postCadastroCliR :: Handler Html
postCadastroCliR = do
    ((result,_),_) <- runFormPost formCliente
    case result of
        FormSuccess cliente -> do
            runDB $ insert cliente
            redirect CadastroCliR
                -- redirect para o página do cliente ou pra home pra ele comprar
        _ -> redirect HomeR
            --redirect pro cadastro msm pra ele refazer
        -- em caso de erro
        
getCadastroCliR :: Handler Html
getCadastroCliR = do
    (widget, enctype) <- generateFormPost formCliente
    defaultLayout $ do
        addStylesheet $ (StaticR css_bootstrap_css)
        [whamlet|
            <form action=@{CadastroCliR} method=post anctype=#{enctype}>
            ^{widget}
            <input type="submit" value="Cadastrar">
        |]
    {--
        o widget é o formulário
        o action é o mesmo, porém no post
    
    --}

putAlterarDadosCliR :: ClienteId ->Handler Value
putAlterarDadosCliR cid = do
	_ <- runDB $ get404 cid
	novoCli <- requireJsonBody :: Handler Cliente
	runDB $ replace cid novoCli
	sendStatusJSON noContent204 (object ["resp" .=("Updated" ++ show (fromSqlKey cid))])


getCarrinhoComprasR :: Handler TypedContent
getCarrinhoComprasR = undefined

postFinalizarCompraR :: Handler TypedContent
postFinalizarCompraR = undefined

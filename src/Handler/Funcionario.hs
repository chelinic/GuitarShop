{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}

module Handler.Funcionario where

import Import
import Network.HTTP.Types.Status
import Database.Persist.Postgresql
import GHC.Generics

{--
data Estoque = Estoque {estoque :: Int} deriving Generic
instance ToJSON Estoque where
instance FromJSON Estoque where
--}	

formFuncionario :: Form Funcionario
formFuncionario = renderBootstrap $ Funcionario
    <$> areq textField "Nome:" Nothing
    <*> areq textField "Email:" Nothing
    <*> areq textField "CPF:" Nothing
    <*> areq textField "Senha:" Nothing
    <*> areq textField "Telefone:" Nothing
    

postCadastroFuncR :: Handler Html
postCadastroFuncR = do
    ((result,_),_) <- runFormPost formFuncionario
    case result of
        FormSuccess funcionario -> do
            runDB $ insert funcionario
            redirect CadastroFuncR
                -- redirect para o página do cliente ou pra home pra ele comprar
        _ -> redirect HomeR
            --redirect pro cadastro msm pra ele refazer
        -- em caso de erro
        
getCadastroFuncR :: Handler Html
getCadastroFuncR = do
    (widget, enctype) <- generateFormPost formFuncionario
    defaultLayout $ do
        addStylesheet $ (StaticR css_bootstrap_css)
        [whamlet|
            <form action=@{CadastroFuncR} method=post anctype=#{enctype}>
            ^{widget}
            <input type="submit" value="Cadastrar">
        |]
    {--
        o widget é o formulário
        o action é o mesmo, porém no post
    
    --}


getLoginFuncR :: Text -> Text -> Handler Html
getLoginFuncR email senha = undefined


patchRepEstoqueR :: ProdutoId -> Handler Text
patchRepEstoqueR = undefined
{--   	
patchRepEstoqueR pid = do
	_ <- runDB $ get404 pid		
	novoEstoq <- requireJsonBody :: Handler Produto
	runDB $ update pid [ProdutoEstoque =. (estoque novoEstoq)]
	sendStatusJSON noContent204 (object ["resp" .=("Updated" ++ show (fromSqlKey pid))])

--}


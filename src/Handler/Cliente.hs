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

formCadCliente :: Form Cliente
formCadCliente = renderBootstrap $ Cliente
    <$> areq textField "Nome:" Nothing
    <*> areq textField "CPF:" Nothing
    <*> areq textField "Telefone:" Nothing
    <*> areq textField "Email:" Nothing
    <*> areq textField "Senha:" Nothing
    
formLoginCli :: Form (Text, Text)
-- Form com dois campos de texto
formLoginCli = renderDivs $ (,)
    -- (,) 5 7 = (5,7)
    <$> areq textField "Email: " Nothing
    <*> areq passwordField "Senha: " Nothing   

autenticar :: Text -> Text -> HandlerT App IO (Maybe (Entity Cliente))
    -- HandlerT é monad transform
    -- App é o site
    -- o objetivo é transformar IO em um Handler
    -- retorna dentro do handler um Nothing ou um Just Aluno
    -- 2 Mônadas: Handler e Maybe
    -- Entity é TypeFamilies: transforma o Maybe a em Maybe Aluno
autenticar email senha = runDB $ selectFirst [ClienteEmailcli ==. email,
                                              ClienteSenha ==. senha][]
                                            -- a ","" é o AND
                                            -- primeira lista é condição WHERE

getLoginCliR :: Handler Html
getLoginCliR = do
    (widget, enctype) <- generateFormPost formLoginCli
    msg <- getMessage
    defaultLayout $ do
        [whamlet|
            $maybe mensa <- msg
                <h1> Usuario Invalido
            <form action=@{LoginCliR} method=post>
                ^{widget}
                <input type="submit" value="Login">
        |]

postLoginCliR :: Handler Html
postLoginCliR = do
    ((res,_),_) <- runFormPost formLoginCli
    -- res é o resultado
    case res of
        FormSuccess (email,senha) -> do
        -- se deu certo vai retornar um aluno
            cli <- autenticar email senha
            case cli of
                Nothing -> do
                    setMessage [shamlet| Usuario ou senha invalido|]
                    -- hamlet sem w
                    redirect LoginCliR
                Just (Entity clienteid cli) -> do
                    setSession "_ID" (clienteNome cli)
                    redirect LoginCliR
        _ -> redirect HomeR


postCadastroCliR :: Handler Html
postCadastroCliR = do
    ((result,_),_) <- runFormPost formCadCliente
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
    (widget, enctype) <- generateFormPost formCadCliente
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


postCliLogoutR :: Handler Html
postCliLogoutR = do 
    deleteSession "_ID"
    redirect HomeR
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
    
formAtCadCliente :: Form Cliente
formAtCadCliente = renderBootstrap $ Cliente
    <$> areq textField "Nome:" ClienteNome
    <*> areq textField "CPF:" ClienteCpf
    <*> areq textField "Telefone:" ClienteTelefone
    <*> areq textField "Senha:" ClienteSenha
    
formLoginCli :: Form (Text, Text)
-- Form com dois campos de texto
formLoginCli = renderDivs $ (,)
    -- (,) 5 7 = (5,7)
    <$> areq textField "Email: " Nothing
    <*> areq passwordField "Senha: " Nothing   

autenticar :: Text -> Text -> HandlerT App IO (Maybe (Entity Cliente))
autenticar email senha = runDB $ selectFirst [ClienteEmailcli ==. email,
                                              ClienteSenha ==. senha][]
                                            -- a ","" é o AND
                                            -- primeira lista é condição WHERE
                                            
    -- HandlerT é monad transform
    -- App é o site
    -- o objetivo é transformar IO em um Handler
    -- retorna dentro do handler um Nothing ou um Just Aluno
    -- 2 Mônadas: Handler e Maybe
    -- Entity é TypeFamilies: transforma o Maybe a em Maybe Aluno                                    
    
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
                    redirect HomeR
                Just (Entity clienteid cli) -> do
                    setSession "_ID" (clienteNome cli)
                    redirect HomeR
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

getAlterarDadosCLiR :: ClienteId -> Handler Html
getAlterarDadosCLiR cid = do
    clidados <- runDB $ E.select
       $ E.from $ \(Cliente) -> do
            E.where_ $
                E. ClienteId = val "cid"
            return
                ( Cliente ^. ClienteNome
                , Cliente ^. ClienteSenha
                , Cliente ^. ClienteTelefone
                , Cliente ^. ClienteCpf
                )
    (widget, enctype) <- generateFormPost formAtCadCliente
    defaultLayout $ do
        addStylesheet $ (StaticR css_bootstrap_css)
        [whamlet|
            ^{topo}
            <form action=@{AlterarDadosCliR} method=post anctype=#{enctype}>
            ^{widget}
            <input type="submit" value="Atualizar">
            ^{footer}
        |]
--talvez dê erro por não puxar o email            

postAlterarDadosCliR :: ClienteId ->Handler Html
postAlterarDadosCliR cid = do
	((result,_),_) <- runFormPost formAtCadCliente
    case result of
        FormSuccess cliente -> do
            runDB $ replace cid cliente
            redirect HomeR
        _ -> redirect HomeR


postFinalizarCompraR :: ProdutoId -> ClienteId -> Int -> Handler Html
postFinalizarCompraR pid cid qtd= do
    hoje <- getCurrentTime
    compracli <- insert $ Venda cid  pid hoje qtd
--parei aqui
    -- alteração de dados do cliente com esqueleto
    -- terminar finalizar compras

postCliLogoutR :: Handler Html
postCliLogoutR = do 
    deleteSession "_ID"
    redirect HomeR
    

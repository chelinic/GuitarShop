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

getFuncR :: Handler Html
getFuncR = do
	funclog <- lookupSession "_ID"

formLoginFunc :: Form (Text, Text)
-- Form com dois campos de texto
formLoginFunc = renderDivs $ (,)
    -- (,) 5 7 = (5,7)
    <$> areq textField "Email: " Nothing
    <*> areq passwordField "Senha: " Nothing

autenticar :: Text -> Text -> HandlerT App IO (Maybe (Entity Funcionario))
    -- HandlerT é monad transform
    -- App é o site
    -- o objetivo é transformar IO em um Handler
    -- retorna dentro do handler um Nothing ou um Just Aluno
    -- 2 Mônadas: Handler e Maybe
    -- Entity é TypeFamilies: transforma o Maybe a em Maybe Aluno
autenticar email senha = runDB $ selectFirst [FuncionarioEmailfunc ==. email,
                                              FuncionarioSenha ==. senha][]
                                            -- a ","" é o AND
                                            -- primeira lista é condição WHERE

getAreaFuncR :: Handler Html
getAreaFuncR = undefined
    -- addStylesheet $ (StaticR css_areafunc_css)


getLoginFuncR :: Handler Html
getLoginFuncR = do
    (widget, enctype) <- generateFormPost formLoginFunc
    msg <- getMessage
    defaultLayout $ do 
       [whamlet|
            $maybe mensa <- msg 
                <h1> Usuario Invalido
            <form action=@{LoginFuncR} method=post>
                ^{widget}
                <input type="submit" value="Login">  
        |]

postLoginFuncR :: Handler Html
postLoginFuncR = do
    ((res,_),_) <- runFormPost formLoginFunc
    -- res é o resultado
    case res of
        FormSuccess (email,senha) -> do
        -- se deu certo vai retornar um aluno
            func <- autenticar email senha
            case func of
                Nothing -> do
                    setMessage [shamlet| Usuario ou senha invalido|]
                    -- hamlet sem w
                    redirect LoginFuncR

                Just (Entity funcionarioid func) -> do
                    setSession "_ID" (funcionarioNome func)
                    redirect LoginR
        _ -> redirect HomeR
        
patchRepEstoqueR :: ProdutoId -> Handler Text
patchRepEstoqueR = undefined
{--   	
patchRepEstoqueR pid = do
	_ <- runDB $ get404 pid		
	novoEstoq <- requireJsonBody :: Handler Produto
	runDB $ update pid [ProdutoEstoque =. (estoque novoEstoq)]
	sendStatusJSON noContent204 (object ["resp" .=("Updated" ++ show (fromSqlKey pid))])

--}

postCadastroFuncR :: Handler Html
postCadastroFuncR = do
    ((result,_),_) <- runFormPost formFuncionario
    case result of
        FormSuccess funcionario -> do
            runDB $ insert funcionario
            redirect FuncR
                -- redirect para o página do cliente ou pra home pra ele comprar
        _ -> redirect FuncR
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
    
postFuncLogoutR :: Handler Html
postFuncLogoutR = do 
    deleteSession "_ID"
    redirect HomeR
    
getListarVendas :: Handler Html
getListarVendas = undefined
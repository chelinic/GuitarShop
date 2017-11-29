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

footer :: WidgetT App IO ()  
footer = $(whamletFile "templates/Footer.hamlet")

topo :: WidgetT App IO ()
topo = $(whamletFile "templates/Funcionario.hamlet")

formFuncionario :: Form Funcionario
formFuncionario = renderBootstrap $ Funcionario
    <$> areq textField "Nome:" Nothing
    <*> areq textField "Email:" Nothing
    <*> areq textField "CPF:" Nothing
    <*> areq textField "Senha:" Nothing
    <*> areq textField "Telefone:" Nothing
    
getFuncR :: Handler Html
getFuncR = undefined
--	funclog <- lookupSession "_ID"

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
                    redirect LoginFuncR
        _ -> redirect HomeR
        
getRepEstoqueR :: Handler Html
getRepEstoqueR = do
    let listaprods = runDB $ E.select
           $ E.from $ \(Produto `E.InnerJoin` EstadoUso) -> do
                E.on $ Produto ^. ProdutoEstadouso E.==. EstadoUso ^. EstadoUsoId
                E.where_ $
                    E. EstadoUsonome != val "Vintage",
                    E. ProdutoEstoqueAtual <= ProdutoEstoqueminimo
                return
                    ( Produto ^. ProdutoId
                    , Produto ^. ProdutoNome
                    , Produto ^. ProdutoEstoqueAtual
                    , EstadoUso ^. EstadoUsonome
                    
                    )
    defaultLayout $ do 
        addStylesheet $ (StaticR css_bootstrap_css)
        [whamlet|
            ^{topo}
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
        

getRepEstoqueProdR :: ProdutoId -> Handler Html
getRepEstoqueProdR pid = do
    let prod = runDB $ E.select
       $ E.from $ \(Produto `E.InnerJoin` EstadoUso) -> do
           E.where_ $
               E. ProdutoId != val pid
                return
                    ( Produto ^. ProdutoId
                    , Produto ^. ProdutoNome
                    , Produto ^. ProdutoMarca
                    , Produto ^. ProdutoEstoqueAtual
                    , EstadoUso ^. EstadoUsonome
                    )
     [whamlet|
        ^{topo}
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
                $forall (Entity idproduto produto) <- prod
                    <tr> 
                        <td> #{fromSqlKey idproduto}
                        <td> #{produtoNome produto}
                        <td> #{produtoEstoqueatual produto}
                        <td> #{produtoEstoqueminimo produto}
                        <td> 
                 <form action=@{RepEstoqueR pid} method=post>
                    <input type="text" value="Quantidade recebida">
                        <input type="submit" value="Registrar">
            ^{footer}
        
     |]               

postRepEstoqueProdR :: ProdutoId -> Handler Html
postRepEstoqueProdR = do
    runDB $ replace pid produto
    redirect RepEstoqueR

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
            ^{topo}
            <form action=@{CadastroFuncR} method=post anctype=#{enctype}>
            ^{widget}
            <input type="submit" value="Cadastrar">
            ^{footer}
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
getListarVendas = do
    let vendas = runDB $ E.select
       $ E.from $ \(Cliente `E.InnerJoin` Venda `E.InnerJoin` Produto) -> do
            E.on $ Venda ^. VendaClienteId E.==. Cliente ^. ClienteId
            E.on $ Venda ^. VendaProdutoId E.==. Produto ^. ProdutoId
               return
                   ( Cliente ^. ClienteNome
                   , Produto ^. ProdutoNome
                   , Produto ^. ProdutoMarca
                   , Venda ^. Quantidade
                   , Venda ^. FormaPgtoId
                   )
    [whamlet|
        ^{topo}
        <table>
            <thead>
                <tr>
                    <td> ClienteNome
                    <td> ProdutoNome
                    <td> ProdutoMarca
                    <td> VendaQuantidade
                    <td> VendaFormapgtoId
                    <td>  
            
            <tbody>
                $forall (Entity idvenda venda) <- venda
                    <tr> 
                        <td> #{fromSqlKey idvenda}
                        <td> #{clienteNome cliente}
                        <td> #{produtoNome produto}
                        <td> #{produtoMarca produto}
                        <td> #{vendQuantidade venda}
                        <td> #{vendFormapgtoID formapgto}
                        <td> 
                 <form action=@{RepEstoqueR pid} method=post>
                    <input type="text" value="Quantidade recebida">
                        <input type="submit" value="Registrar">
            ^{footer}
        
     |]               
    


--https://hackage.haskell.org/package/esqueleto-2.5.3/docs/Database-Esqueleto.html
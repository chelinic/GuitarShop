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
    <$> areq textField "Marca:" Nothing
    <*> areq doubleField "Valor:" Nothing
    <*> areq dayField "Data de chegada:" Nothing
    <*> areq intField "Ano:" Nothing
    <*> areq textField "Observacoes:" Nothing
    <*> areq intField "Estoque minimo:" Nothing
    <*> areq intField "Estoque atual:" Nothing
    <*> areq radioField "Novo" (Just someDefault)
    <*> areq radioField "Usado" (Just someDefault)
    <*> areq radioField "Vintage" (Just someDefault)
    
formArquivo :: Form FileInfo
formArquivo = renderDivs $ areq fileField "Imagem do produto: " Nothing

postApagarProdR :: ProdutoId -> Handler Html
postApagarProdR pid = do
    runDB $ delete pid
    redirect TodosProdR

-- unpack retorna string 
-- return para colocar algo puro dentro da mônada
-- $ para monada Maybe	

footer :: WidgetT App IO ()  
footer = $(whamletFile "templates/Footer.hamlet")

prods :: WidgetT App IO ()
prods = $(whamletFile "templates/Produto.hamlet")

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
            ^{prods}
            <div>
            <ul>
                <li><strong> Nome: #{produtoNome produto}
                <li><strong> Marca: #{produtoMarca produto}
                <li><strong> Preco: #{produtoValor produto}
                <li><strong> Ano: #{produtoAno produto}
                <li><strong> Estoque: #{produtoEstoqueatual produto}
                <li><strong> Observacoes: #{produtoObservacoes produto}
            ^{footer}
        |]

postCadastroProdR :: Handler Html
postCadastroProdR = do
    ((result,_),_) <- runFormPost formProduto
    case result of
        FormSuccess produto -> do
            ((res,_),_) <- runFormPost formArquivo
            case res of 
                FormSuccess arq -> do 
                liftIO $ fileMove arq ("static/" ++ (unpack $ fileName arq))
                runDB $ insert produto
                redirect CadastroProdR
        _ -> redirect HomeR
        -- em caso de erro
        
getCadastroProdR :: Handler Html
getCadastroProdR = do
    (widget, enctype) <- generateFormPost formProduto
    (widget, enctype) <- generateFormPost formArquivo
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


getBuscaProdR :: Text -> Handler Html
getBuscaProdR termo = do 
    listaprod <- runDB $ rawSql
        ("SELECT ?? \
        \FROM produto \
        \WHERE produto.produtoNome LIKE " <> (termo))
        [] :: Handler [(Entity Produto)]    
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
                    $forall (Entity idproduto produto) <- listaprod
                        <tr> 
                            <td> #{fromSqlKey idproduto}
                            <td> #{produtoNome produto}
                            <td> #{produtoValor produto}
                            <td> 
                                
        |]
    

        
        
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

postFuncR :: Handler TypedContent
postFuncR = do
	func <- requireJsonBody :: Handler Funcionario
	funcid <- runDB $  insert func
	sendStatusJSON created201 (object ["resp" .=(fromSqlKey funcid)])

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

postCadastroFuncR :: Handler TypedContent
postCadastroFuncR = undefined
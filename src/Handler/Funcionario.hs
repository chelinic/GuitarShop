{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.Funcionario where

import Import
import Network.HTTP.Types.Status
import Database.Persist.Postgresql
import GHC.Generics

postFuncR :: Handler TypedContent
postFuncR = postFuncR = do
		func <- requireJsonBody :: Handler Funcionario
		funcid <- runDB $  insert func
    	sendStatusJSON created201 (object ["resp" .=(fromSqlKey funcid)])
    	
-- essa tá de boa
patchRepEstoqueR :: ProdutoId -> Handler Text
patchAlteraNomeR pid = do
			_ <- runDB $ get404 pid		
			novoNome <- requireJsonBody :: Handler Produto
			runBD $ update pid [ProdutoNome =. (nome novoNome)]
			sendStatusJSON noContent204 (object ["resp" .=("Updated" ++ show (fromSqlKey pid))])

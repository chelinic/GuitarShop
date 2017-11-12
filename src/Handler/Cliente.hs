{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.Cliente where

import Import
import Network.HTTP.Types.Status
import Database.Persist.Postgresql
import GHC.Generics


getLoginCliR = Handler TypedContent
getLoginCliR = undefined


postCadastroCLiR = Handler TypedContent
postCadastroCLiR = undefined

putAlterarDadosCliR = ClienteId -> Handler Value
putAlterarDadosCliR cid = do
        	_ <- runDB $ get404 cid
			novoProd <- requireJsonBody :: Handler Cliente
			runDB $ replace cid novoCli
			sendStatusJSON noContent204 (object ["resp" .=("Updated" ++ show (fromSqlKey cid))])

getCarrinhoComprasR = Handler TypeFamilies
getCarrinhoComprasR = undefined


postFinalizarCompraR = Handler TypedContent
postFinalizarCompraR = undefined

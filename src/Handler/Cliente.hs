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


getLoginCliR :: Text -> Text -> Handler TypedContent
getLoginCliR = undefined


postCadastroCliR :: Handler TypedContent
postCadastroCliR = undefined

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

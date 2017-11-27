{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module Foundation where

import Import.NoFoundation
import Database.Persist.Sql (ConnectionPool, runSqlPool)
import Yesod.Core.Types     (Logger)

data App = App
    { appSettings    :: AppSettings
    , appStatic      :: Static 
    , appConnPool    :: ConnectionPool 
    , appHttpManager :: Manager
    , appLogger      :: Logger
    }

mkYesodData "App" $(parseRoutesFile "config/routes")

mkMessage "App" "messages" "pt-BR"


type Form a = Html -> MForm Handler (FormResult a, Widget)

instance Yesod App where
    makeLogger = return . appLogger
--    authRoute Clientelogin Clientesenha = Just $ LoginCliR
--    authRoute _ = Just $ LoginFuncR
    isAuthorized HomeR _ = return Authorized
    isAuthorized FuncLogoutR _ = return Authorized
    isAuthorized CliLogoutR _ = return Authorized
    isAuthorized FuncR _ = ehFunc
    isAuthorized _ _ = ehCliente

ehFunc :: Handler AuthResult
ehFunc = do
    sessao <- lookupSession "_ID"
    case sessao of 
        Nothing -> return AuthenticationRequired
        (Just "func") -> return Authorized
        (Just _ ) -> return $ Unauthorized "Acesso restrito"
    
ehCliente :: Handler AuthResult
ehCliente = do
    sessao <- lookupSession "_ID"
    case sessao of 
        Nothing -> return AuthenticationRequired
        (Just "func") -> return $ Unauthorized "Faça login na área de funcionários" 
        (Just _) -> return Authorized

instance YesodPersist App where
    type YesodPersistBackend App = SqlBackend
    runDB action = do
        master <- getYesod
        runSqlPool action $ appConnPool master

instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage

instance HasHttpManager App where
    getHttpManager = appHttpManager

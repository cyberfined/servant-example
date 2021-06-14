{-# LANGUAGE ScopedTypeVariables #-}

module Backend
    ( runBackend
    , backendApp
    , apiProxy

    , Environment(..)
    , envSubConfig

    , module Backend.AppM
    ) where

import Control.Exception                (SomeException)
import Control.Monad.IO.Class
import Data.ByteString                  (ByteString)
import Data.Proxy
import Network.HTTP.Types.Status
import Network.Socket
import Network.Wai
import Network.Wai.Handler.Warp
import Numeric
import Servant.API
import Servant.Server
import Servant.Server.Experimental.Auth

import Backend.Auth
import Backend.AppM
import Backend.Config
import Backend.Response
import Backend.User.Model
import Backend.User.Handler

import qualified Data.ByteString.Char8 as BS

type Api = "api" :>
    (    AuthApi
    :<|> UserApi
    )

servantContext :: Proxy '[ErrorFormatters, AuthHandler Request (AuthResult User)]
servantContext = Proxy

apiProxy :: Proxy Api
apiProxy = Proxy

apiServer :: AppM m => ServerT Api m
apiServer = authHandlers :<|> userHandlers

warpSettings :: Maybe LoggerContext -> Port -> Settings
warpSettings loggerCtx port = setPort port
                            $ setOnException exceptionLogger
                            $ setLogger requestLogger defaultSettings
  where exceptionLogger :: Maybe Request -> SomeException -> IO ()
        exceptionLogger _ e = writeLogIO loggerCtx $ "Exception: " <> BS.pack (show e)

        requestLogger :: Request -> Status -> Maybe Integer -> IO ()
        requestLogger req status _ =  writeLogIO loggerCtx
                                   $  requestMethod req
                                   <> " "
                                   <> rawPathInfo req
                                   <> " "
                                   <> BS.pack (show $ statusCode status)
                                   <> " "
                                   <> statusMessage status
                                   <> " from "
                                   <> showRemoteHost (remoteHost req)

        showRemoteHost :: SockAddr -> ByteString
        showRemoteHost = \case
            SockAddrInet clPort clHost -> strHost <> ":" <> strPort
              where (a, b, c, d) = hostAddressToTuple clHost
                    strHost = BS.intercalate "." $ map (BS.pack . show) [a,b,c,d]
                    strPort = BS.pack (show clPort)
            SockAddrInet6 clPort _ clHost _ -> strHost <> ":" <> strPort
              where (a,b,c,d,e,f,g,h) = hostAddress6ToTuple clHost
                    strHost = BS.intercalate "." $
                        map (BS.pack . flip showHex "") [a,b,c,d,e,f,g,h]
                    strPort = BS.pack (show clPort)
            SockAddrUnix clHost -> BS.pack clHost

runBackend :: AppM m => FilePath -> Environment -> Proxy m -> IO ()
runBackend cfgPath env prx = do
    (app, runApp) <- backendApp cfgPath env prx
    runApp app

backendApp :: forall m. AppM m
           => FilePath
           -> Environment
           -> Proxy m
           -> IO (Application, Application -> IO ())
backendApp cfgPath env monadPrx = do
    AppConfig{..} <- loadAppConfig cfgPath env
    loggerCtx <- initLoggerContext cfgLog
    let appCtx = AppContext { appCtxPoolSetings = cfgDbPoolSettings
                            , appCtxAuthCtx     = cfgAuthCtx
                            , appCtxLoggerCtx   = loggerCtx
                            }
        app = serveWithContext apiProxy
            (errorFormatters :.  genAuthServerContext monadPrx appCtx) $
            hoistServerWithContext apiProxy servantContext
                (runAppM @m appCtx) apiServer
    pure (app, runSettings (warpSettings loggerCtx cfgPort))

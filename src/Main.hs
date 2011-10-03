
module Main where

import Control.Concurrent (forkIO, killThread)
import Control.Monad.Error
import Data.Monoid
import Happstack.Server.FileServe
import Happstack.Server.Internal.Monads
import Happstack.Server.SimpleHTTP
import Happstack.State.Control
import Network.Socket
import P117.MainPage as MainPage
import System.Posix.Syslog

main :: IO ()
main = do
    let httpConf = Conf 9000 Nothing Nothing 60
    sock <- socket AF_INET Stream defaultProtocol
    setSocketOption sock ReuseAddr 1
    loopbackIp <- inet_addr "127.0.0.1"
    bindSocket sock $
        SockAddrInet 9000 loopbackIp
    listen sock (max 1024 maxListenQueue)

    httpTid <- forkIO $ simpleHTTPWithSocket' unpackErrorT
                                              sock
                                              httpConf
                                              $ decodeBody (defaultBodyPolicy "/tmp/" 4096 20000 40000 )
                                              >> control
    waitForTermination
    syslog Notice "Shutting down..."
    killThread httpTid
    syslog Notice "Shutdown complete"

unpackErrorT :: (Monad m) => UnWebT (ErrorT String m) a -> UnWebT m a
unpackErrorT handler = do
    resE <- runErrorT handler
    case resE of
        Left err -> return $ Just (Left $ toResponse err, Set mempty)
        Right x -> return x

control :: ServerPartT (ErrorT String IO) Response
control = msum [ nullDir >> seeOther "/mainpage" (toResponse "")
               , dir "mainpage" $ MainPage.pageHandler
               , dir "static" $ serveDirectory EnableBrowsing [] "static"
               ]


module Main where

import Control.Concurrent (forkIO, killThread)
import Control.Monad.Error
import Data.Monoid
import Happstack.Server.FileServe
import Happstack.Server.Internal.Monads
import Happstack.Server.SimpleHTTP
import Network.Socket
import P117.MainPage as MainPage
import System.Console.GetOpt (getOpt, ArgOrder(..), OptDescr(..), ArgDescr(..), usageInfo)
import System.Environment (getArgs, getProgName)
import System.Exit (exitWith, ExitCode(..))
import System.Posix.Syslog
import System.IO (hPutStrLn, stderr)
import Text.JSON

main :: IO ()
main = do
    let httpConf = Conf 9000 Nothing Nothing 60 Nothing
    appCfg <- parseArgs =<< getArgs
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
                                                >> control appCfg
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

control :: Options -> ServerPartT (ErrorT String IO) Response
control opts = do
  rq <- askRq
  void $ when (optReadOnly opts && rqMethod rq /= GET) $
    throwError $ encode "Writes are disallowed in read-only mode!"
  msum [ nullDir >> seeOther "/mainpage" (toResponse "")
       , dir "mainpage" $ MainPage.pageHandler
       , dir "static" $ serveDirectory EnableBrowsing [] "static"
       ]



data Options = Options  { optReadOnly :: Bool }
               deriving (Show)

startOptions :: Options
startOptions = Options  { optReadOnly = False }

options :: [ OptDescr (Options -> IO Options) ]
options =
    [ Option "R" ["read-only"]
        (NoArg
            (\opt -> return opt { optReadOnly = True }))
        "Read-only mode"
     , Option "h" ["help"]
        (NoArg
            (\_ -> do
                prg <- getProgName
                hPutStrLn stderr (usageInfo prg options)
                exitWith ExitSuccess))
        "Show help"
    ]


parseArgs :: [String] -> IO Options
parseArgs args = do
  let (actions, _, _) = getOpt RequireOrder options args
  opts <- foldl (>>=) (return startOptions) actions
  return opts


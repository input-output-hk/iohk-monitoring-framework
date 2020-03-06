{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

import           Control.Applicative ((<|>))
import           Control.Concurrent (threadDelay)
import           Control.Exception (IOException, catch, throwIO)
import           Control.Monad (forever)
import           Control.Tracer (Tracer, contramap, traceWith)
import           Data.Semigroup (First(..), getFirst)
import           Data.Maybe (isJust)
import           Data.Semigroup ((<>))
import           Data.Text (Text, pack)
import           Cardano.BM.Backend.TraceAcceptor
import           Cardano.BM.Data.Configuration
import           Cardano.BM.Configuration
import qualified Cardano.BM.Configuration as Config
import qualified Cardano.BM.Configuration.Model as Config
import           Cardano.BM.Plugin (loadPlugin)
import qualified Cardano.BM.Setup as Setup
import           Cardano.BM.Trace
import qualified Cardano.BM.Tracing as Trace
import qualified Options.Applicative as Opt


data CLI = CLI
  { cConfig :: FilePath
  }

cliParser :: Opt.Parser CLI
cliParser = CLI
  <$> parseFilePath "config" "Config file"
 where
   parseFilePath :: String -> String -> Opt.Parser FilePath
   parseFilePath optname desc =
     Opt.strOption $ Opt.long optname <> Opt.metavar "FILEPATH" <> Opt.help desc

   parseRemoteAddr :: String -> String -> Opt.Parser RemoteAddr
   parseRemoteAddr optname desc =
     RemotePipe <$>
       (Opt.strOption $ Opt.long optname <> Opt.metavar "FILEPATH" <> Opt.help desc)

main :: IO ()
main = do
  cli <- Opt.customExecParser pref opts
  config <- readConfig (cConfig cli)
  acceptAt <- Config.getAcceptAt config
  case acceptAt of
    Just ra -> do
      (tr :: Trace IO Text, sb) <- Setup.setupTrace_ config "cardano"
      Cardano.BM.Backend.TraceAcceptor.plugin config tr sb
        >>= loadPlugin sb
      forever $ threadDelay 1000000
    Nothing ->
      putStrLn $ "Trace acceptor not enabled in config: " <> cConfig cli
 where
   pref :: Opt.ParserPrefs
   pref = Opt.prefs Opt.showHelpOnEmpty

   opts :: Opt.ParserInfo CLI
   opts =
     Opt.info (cliParser Opt.<**> Opt.helper)
       ( Opt.fullDesc
         <> Opt.header
         "example-acceptor - utility to support a variety of key\
         \ operations (genesis generation, migration,\
         \ pretty-printing..) for different system generations."
       )

   readConfig :: FilePath -> IO Configuration
   readConfig fp =
     catch (Config.setup fp) $ \(e :: IOException) -> do
       putStrLn $ "Exception while reading configuration from: " <> fp
       throwIO e

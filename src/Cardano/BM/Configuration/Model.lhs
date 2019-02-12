
\subsection{Cardano.BM.Configuration.Model}
\label{module:Cardano.BM.Configuration.Model}

%if style == newcode
\begin{code}
{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}

module Cardano.BM.Configuration.Model
    ( Configuration (..)
    , ConfigurationInternal (..)
    , setup
    , setupFromRepresentation
    , empty
    , minSeverity
    , setMinSeverity
    , inspectSeverity
    , setSeverity
#ifdef MemoizeSeverity
    , getSeverity
    , getCachedSeverity
#endif
    , getBackends
    , setBackends
    , getDefaultBackends
    , setDefaultBackends
    , setSetupBackends
    , getSetupBackends
    , getScribes
    , setScribes
    , getCachedScribes
    , setCachedScribes
    , setDefaultScribes
    , setSetupScribes
    , getSetupScribes
    , getAggregatedKind
    , setDefaultAggregatedKind
    , setAggregatedKind
    , getOption
    , findSubTrace
    , setSubTrace
    , getMonitors
    , getEKGport
    , setEKGport
    , getGUIport
    , setGUIport
    ) where

import           Control.Concurrent.MVar (MVar, newMVar, readMVar,
                     modifyMVar_)
import           Control.Monad (when)
import qualified Data.HashMap.Strict as HM
import           Data.Maybe (catMaybes)
import           Data.Text (Text, breakOnEnd, dropWhileEnd, pack, unpack)
import qualified Data.Vector as Vector
import           Data.Yaml as Y

import           Cardano.BM.Data.AggregatedKind (AggregatedKind(..))
import           Cardano.BM.Data.BackendKind
import qualified Cardano.BM.Data.Configuration as R
import           Cardano.BM.Data.LogItem (LoggerName)
import           Cardano.BM.Data.MonitoringEval (MEvAction, MEvExpr)
import qualified Cardano.BM.Data.MonitoringEval as MEv
import           Cardano.BM.Data.Observable
import           Cardano.BM.Data.Output (ScribeDefinition (..), ScribeId,
                     ScribeKind (..))
import           Cardano.BM.Data.Rotation (RotationParameters (..))
import           Cardano.BM.Data.Severity
import           Cardano.BM.Data.SubTrace

\end{code}
%endif

\subsubsection{Configuration.Model}\label{code:Configuration}
\begin{figure}[htp]
\centering{
  \includegraphics[scale=0.54]{ConfigurationModel.pdf}
}
\caption{Configuration model}\label{fig:configuration}
\end{figure}

\begin{code}
type ConfigurationMVar = MVar ConfigurationInternal
newtype Configuration = Configuration
    { getCG :: ConfigurationMVar }

-- Our internal state; see {-"\nameref{fig:configuration}"-}
data ConfigurationInternal = ConfigurationInternal
    { cgMinSeverity       :: Severity
    -- minimum severity level of every object that will be output
    , cgMapSeverity       :: HM.HashMap LoggerName Severity
    -- severity filter per loggername
    , cgMapSubtrace       :: HM.HashMap LoggerName SubTrace
#ifdef MemoizeSeverity
    , cgMapSeverityCache  :: HM.HashMap LoggerName Severity
    -- map to cache info of the cgMapScribe
#endif
    -- type of trace per loggername
    , cgOptions           :: HM.HashMap Text Object
    -- options needed for tracing, logging and monitoring
    , cgMapBackend        :: HM.HashMap LoggerName [BackendKind]
    -- backends that will be used for the specific loggername
    , cgDefBackendKs      :: [BackendKind]
    -- backends that will be used if a set of backends for the
    -- specific loggername is not set
    , cgSetupBackends     :: [BackendKind]
    -- backends to setup; every backend to be used must have
    -- been declared here
    , cgMapScribe         :: HM.HashMap LoggerName [ScribeId]
    -- katip scribes that will be used for the specific loggername
    , cgMapScribeCache    :: HM.HashMap LoggerName [ScribeId]
    -- map to cache info of the cgMapScribe
    , cgDefScribes        :: [ScribeId]
    -- katip scribes that will be used if a set of scribes for the
    -- specific loggername is not set
    , cgSetupScribes      :: [ScribeDefinition]
    -- katip scribes to setup; every scribe to be used must have
    -- been declared here
    , cgMapAggregatedKind :: HM.HashMap LoggerName AggregatedKind
    -- kind of Aggregated that will be used for the specific loggername
    , cgDefAggregatedKind :: AggregatedKind
    -- kind of Aggregated that will be used if a set of scribes for the
    -- specific loggername is not set
    , cgMonitors          :: HM.HashMap LoggerName (MEvExpr, [MEvAction])
    , cgPortEKG           :: Int
    -- port for EKG server
    , cgPortGUI           :: Int
    -- port for changes at runtime (NOT IMPLEMENTED YET)
    } deriving (Show, Eq)

\end{code}

\subsubsection{Backends configured in the |Switchboard|}
For a given context name return the list of backends configured,
or, in case no such configuration exists, return the default backends.
\begin{code}
getBackends :: Configuration -> LoggerName -> IO [BackendKind]
getBackends configuration name = do
    cg <- readMVar $ getCG configuration
    let outs = HM.lookup name (cgMapBackend cg)
    case outs of
        Nothing -> return (cgDefBackendKs cg)
        Just os -> return os

getDefaultBackends :: Configuration -> IO [BackendKind]
getDefaultBackends configuration =
    cgDefBackendKs <$> (readMVar $ getCG configuration)

setDefaultBackends :: Configuration -> [BackendKind] -> IO ()
setDefaultBackends configuration bes =
    modifyMVar_ (getCG configuration) $ \cg ->
        return cg { cgDefBackendKs = bes }

setBackends :: Configuration -> LoggerName -> Maybe [BackendKind] -> IO ()
setBackends configuration name be =
    modifyMVar_ (getCG configuration) $ \cg ->
        return cg { cgMapBackend = HM.alter (\_ -> be) name (cgMapBackend cg) }

\end{code}

\subsubsection{Backends to be setup by the |Switchboard|}
Defines the list of |Backend|s that need to be setup by the |Switchboard|.
\begin{code}
setSetupBackends :: Configuration -> [BackendKind] -> IO ()
setSetupBackends configuration bes =
    modifyMVar_ (getCG configuration) $ \cg ->
        return cg { cgSetupBackends = bes }

getSetupBackends :: Configuration -> IO [BackendKind]
getSetupBackends configuration =
    cgSetupBackends <$> (readMVar $ getCG configuration)

\end{code}

\subsubsection{Scribes configured in the |Log| backend}
For a given context name return the list of scribes to output to,
or, in case no such configuration exists, return the default scribes to use.
\begin{code}
getScribes :: Configuration -> LoggerName -> IO [ScribeId]
getScribes configuration name = do
    cg <- readMVar (getCG configuration)
    (updateCache, scribes) <- do
        let defs = cgDefScribes cg
        let mapscribes = cgMapScribe cg
        let find_s lname = case HM.lookup lname mapscribes of
                Nothing ->
                    case dropToDot lname of
                        Nothing     -> defs
                        Just lname' -> find_s lname'
                Just os -> os
        let outs = HM.lookup name (cgMapScribeCache cg)
        -- look if scribes are already cached
        return $ case outs of
            -- if no cached scribes found; search the appropriate scribes that
            -- they must inherit and update the cached map
            Nothing -> (True, find_s name)
            Just os -> (False, os)

    when updateCache $ setCachedScribes configuration name $ Just scribes
    return scribes

dropToDot :: Text -> Maybe Text
dropToDot ts = dropToDot' (breakOnEnd "." ts)
  where
    dropToDot' (_,"")    = Nothing
    dropToDot' (name',_) = Just $ dropWhileEnd (=='.') name'

getCachedScribes :: Configuration -> LoggerName -> IO (Maybe [ScribeId])
getCachedScribes configuration name = do
    cg <- readMVar $ getCG configuration
    return $ HM.lookup name $ cgMapScribeCache cg

setScribes :: Configuration -> LoggerName -> Maybe [ScribeId] -> IO ()
setScribes configuration name scribes = do
    modifyMVar_ (getCG configuration) $ \cg ->
        return cg { cgMapScribe = HM.alter (\_ -> scribes) name (cgMapScribe cg) }
#ifdef MemoizeSeverity
    -- delete cached scribes
    setCachedScribes configuration name Nothing
#endif

setCachedScribes :: Configuration -> LoggerName -> Maybe [ScribeId] -> IO ()
setCachedScribes configuration name scribes =
    modifyMVar_ (getCG configuration) $ \cg ->
        return cg { cgMapScribeCache = HM.alter (\_ -> scribes) name (cgMapScribeCache cg) }

setDefaultScribes :: Configuration -> [ScribeId] -> IO ()
setDefaultScribes configuration scs =
    modifyMVar_ (getCG configuration) $ \cg ->
        return cg { cgDefScribes = scs }

\end{code}

\subsubsection{Scribes to be setup in the |Log| backend}
Defines the list of |Scribe|s that need to be setup in the |Log| backend.
\begin{code}
setSetupScribes :: Configuration -> [ScribeDefinition] -> IO ()
setSetupScribes configuration sds =
    modifyMVar_ (getCG configuration) $ \cg ->
        return cg { cgSetupScribes = sds }

getSetupScribes :: Configuration -> IO [ScribeDefinition]
getSetupScribes configuration =
    cgSetupScribes <$> readMVar (getCG configuration)

\end{code}

\subsubsection{|AggregatedKind| to define the type of measurement}
For a given context name return its |AggregatedKind| or in case no
such configuration exists, return the default |AggregatedKind| to use.
\begin{code}
getAggregatedKind :: Configuration -> LoggerName -> IO AggregatedKind
getAggregatedKind configuration name = do
    cg <- readMVar $ getCG configuration
    let outs = HM.lookup name (cgMapAggregatedKind cg)
    case outs of
        Nothing -> return $ cgDefAggregatedKind cg
        Just os -> return $ os

setDefaultAggregatedKind :: Configuration -> AggregatedKind -> IO ()
setDefaultAggregatedKind configuration defAK =
    modifyMVar_ (getCG configuration) $ \cg ->
        return cg { cgDefAggregatedKind = defAK }

setAggregatedKind :: Configuration -> LoggerName -> Maybe AggregatedKind -> IO ()
setAggregatedKind configuration name ak =
    modifyMVar_ (getCG configuration) $ \cg ->
        return cg { cgMapAggregatedKind = HM.alter (\_ -> ak) name (cgMapAggregatedKind cg) }

\end{code}

\subsubsection{Access port numbers of EKG, GUI}
\begin{code}
getEKGport :: Configuration -> IO Int
getEKGport configuration =
    cgPortEKG <$> (readMVar $ getCG configuration)

setEKGport :: Configuration -> Int -> IO ()
setEKGport configuration port =
    modifyMVar_ (getCG configuration) $ \cg ->
        return cg { cgPortEKG = port }

getGUIport :: Configuration -> IO Int
getGUIport configuration =
    cgPortGUI <$> (readMVar $ getCG configuration)

setGUIport :: Configuration -> Int -> IO ()
setGUIport configuration port =
    modifyMVar_ (getCG configuration) $ \cg ->
        return cg { cgPortGUI = port }

\end{code}

\subsubsection{Options}
\begin{code}
getOption :: Configuration -> Text -> IO (Maybe Text)
getOption configuration name = do
    cg <- readMVar $ getCG configuration
    case HM.lookup name (cgOptions cg) of
        Nothing -> return Nothing
        Just o  -> return $ Just $ pack $ show o

\end{code}

\subsubsection{Global setting of minimum severity}
\begin{code}
minSeverity :: Configuration -> IO Severity
minSeverity configuration =
    cgMinSeverity <$> (readMVar $ getCG configuration)

setMinSeverity :: Configuration -> Severity -> IO ()
setMinSeverity configuration sev =
    modifyMVar_ (getCG configuration) $ \cg ->
        return cg { cgMinSeverity = sev
#ifdef MemoizeSeverity
                  -- delete cached severities
                  , cgMapSeverityCache = HM.empty
#endif
                  }

\end{code}

\subsubsection{Relation of context name to minimum severity}
\begin{code}
inspectSeverity :: Configuration -> Text -> IO (Maybe Severity)
inspectSeverity configuration name = do
    cg <- readMVar $ getCG configuration
    return $ HM.lookup name (cgMapSeverity cg)

#ifdef MemoizeSeverity
getSeverity :: Configuration -> Severity -> LoggerName -> IO Severity
getSeverity configuration minTraceSeverity name = do
    cg <- readMVar (getCG configuration)
    (updateCache, sev) <- do
        let def = max minTraceSeverity $ cgMinSeverity cg
        let mapSeverity = cgMapSeverity cg
        let find_s lname = case HM.lookup lname mapSeverity of
                Nothing ->
                    case dropToDot lname of
                        Nothing     -> def
                        Just lname' -> find_s lname'
                Just sev -> sev
        let cachedSeverity = HM.lookup name (cgMapSeverityCache cg)
        -- look if severity is already cached
        return $ case cachedSeverity of
            -- if no cached severity found; search the appropriate severity that
            -- they must inherit and update the cached map
            Nothing  -> (True, max def $ find_s name)
            Just sev -> (False, sev)

    when updateCache $ setCachedSeverity configuration name $ Just sev
    return sev

getCachedSeverity :: Configuration -> LoggerName -> IO (Maybe Severity)
getCachedSeverity configuration name = do
    cg <- readMVar $ getCG configuration
    return $ HM.lookup name $ cgMapSeverityCache cg
#endif

setSeverity :: Configuration -> Text -> Maybe Severity -> IO ()
setSeverity configuration name sev = do
    modifyMVar_ (getCG configuration) $ \cg ->
        return cg { cgMapSeverity = HM.alter (\_ -> sev) name (cgMapSeverity cg) }
#ifdef MemoizeSeverity
    -- delete cached severity
    setCachedSeverity configuration name Nothing
#endif

#ifdef MemoizeSeverity
setCachedSeverity :: Configuration -> LoggerName -> Maybe Severity -> IO ()
setCachedSeverity configuration name severity =
    modifyMVar_ (getCG configuration) $ \cg ->
        return cg { cgMapSeverityCache = HM.alter (\_ -> severity) name (cgMapSeverityCache cg) }

#endif

\end{code}

\subsubsection{Relation of context name to SubTrace}
A new context may contain a different type of |Trace|.
The function |appendName| (\nameref{code:appendName}) will look up the |SubTrace| for the context's name.
\begin{code}
findSubTrace :: Configuration -> Text -> IO (Maybe SubTrace)
findSubTrace configuration name = do
    cg <- readMVar $ getCG configuration
    return $ HM.lookup name (cgMapSubtrace cg)

setSubTrace :: Configuration -> Text -> Maybe SubTrace -> IO ()
setSubTrace configuration name trafo =
    modifyMVar_ (getCG configuration) $ \cg ->
        return cg { cgMapSubtrace = HM.alter (\_ -> trafo) name (cgMapSubtrace cg) }

\end{code}

\subsubsection{Monitors}

\begin{spec}
Just (
  fromList [
    ("chain.creation.block", Array [
      Object (fromList [("monitor", String "((time > (23 s)) Or (time < (17 s)))")]),
      Object (fromList [("actions", Array [
        String "AlterMinSeverity \"chain.creation\" Debug"])])])
  , ("#aggregation.critproc.observable", Array [
      Object (fromList [("monitor", String "(mean >= (42))")]),
      Object (fromList [("actions", Array [
        String "CreateMessage \"exceeded\" \"the observable has been too long too high!\"",
        String "AlterGlobalMinSeverity Info"])]) ]) ] )
\end{spec}

\begin{code}
getMonitors :: Configuration -> IO (HM.HashMap LoggerName (MEvExpr, [MEvAction]))
getMonitors configuration = do
    cg <- readMVar $ getCG configuration
    return (cgMonitors cg)
\end{code}

\subsubsection{Parse configuration from file}
Parse the configuration into an internal representation first. Then, fill in |Configuration|
after refinement.
\begin{code}
setup :: FilePath -> IO Configuration
setup fp = do
    r <- R.parseRepresentation fp
    setupFromRepresentation r

parseMonitors :: Maybe (HM.HashMap Text Value) -> HM.HashMap LoggerName (MEvExpr, [MEvAction])
parseMonitors Nothing = HM.empty
parseMonitors (Just hmv) = HM.mapMaybe mkMonitor hmv
    where
    mkMonitor (Array a) =
        if Vector.length a == 2
        then do
            e  <- mkExpression $ a Vector.! 0
            as <- mkActions $ a Vector.! 1
            return (e, as)
        else Nothing
    mkMonitor _ = Nothing
    mkExpression :: Value -> Maybe MEvExpr
    mkExpression (Object o1) =
        case HM.lookup "monitor" o1 of
            Nothing            -> Nothing
            Just (String expr) -> MEv.parseMaybe expr
            Just _             -> Nothing
    mkExpression _ = Nothing
    mkActions :: Value -> Maybe [MEvAction]
    mkActions (Object o2) = 
        case HM.lookup "actions" o2 of
            Nothing -> Nothing
            Just (Array as) -> Just $ map (\(String s) -> s) $ Vector.toList as
            Just _             -> Nothing

    mkActions _ = Nothing


setupFromRepresentation :: R.Representation -> IO Configuration
setupFromRepresentation r = do
    let mapseverities0     = HM.lookup "mapSeverity"        (R.options r)
        mapbackends        = HM.lookup "mapBackends"        (R.options r)
        mapsubtrace        = HM.lookup "mapSubtrace"        (R.options r)
        mapscribes0        = HM.lookup "mapScribes"         (R.options r)
        mapaggregatedkinds = HM.lookup "mapAggregatedkinds" (R.options r)
        mapmonitors        = HM.lookup "mapMonitors"        (R.options r)
        mapseverities      = parseSeverityMap mapseverities0
        mapscribes         = parseScribeMap mapscribes0

    cgref <- newMVar $ ConfigurationInternal
        { cgMinSeverity       = R.minSeverity r
        , cgMapSeverity       = mapseverities
#ifdef MemoizeSeverity
        , cgMapSeverityCache  = mapseverities
#endif
        , cgMapSubtrace       = parseSubtraceMap mapsubtrace
        , cgOptions           = R.options r
        , cgMapBackend        = parseBackendMap mapbackends
        , cgDefBackendKs      = R.defaultBackends r
        , cgSetupBackends     = R.setupBackends r
        , cgMapScribe         = mapscribes
        , cgMapScribeCache    = mapscribes
        , cgDefScribes        = r_defaultScribes r
        , cgSetupScribes      = fillRotationParams (R.rotation r) (R.setupScribes r)
        , cgMapAggregatedKind = parseAggregatedKindMap mapaggregatedkinds
        , cgDefAggregatedKind = StatsAK
        , cgMonitors          = parseMonitors mapmonitors
        , cgPortEKG           = r_hasEKG r
        , cgPortGUI           = r_hasGUI r
        }
    return $ Configuration cgref
  where
    parseSeverityMap :: Maybe (HM.HashMap Text Value) -> HM.HashMap Text Severity
    parseSeverityMap Nothing = HM.empty
    parseSeverityMap (Just hmv) = HM.mapMaybe mkSeverity hmv
      where
        mkSeverity (String s) = Just (read (unpack s) :: Severity)
        mkSeverity _ = Nothing

    fillRotationParams :: Maybe RotationParameters -> [ScribeDefinition] -> [ScribeDefinition]
    fillRotationParams defaultRotation = map $ \sd ->
        if (scKind sd /= StdoutSK) && (scKind sd /= StderrSK)
        then
            sd { scRotation = maybe defaultRotation Just (scRotation sd) }
        else
            -- stdout and stderr cannot be rotated
            sd { scRotation = Nothing }

    parseBackendMap Nothing = HM.empty
    parseBackendMap (Just hmv) = HM.map mkBackends hmv
      where
        mkBackends (Array bes) = catMaybes $ map mkBackend $ Vector.toList bes
        mkBackends _ = []
        mkBackend (String s) = Just (read (unpack s) :: BackendKind)
        mkBackend _ = Nothing

    parseScribeMap Nothing = HM.empty
    parseScribeMap (Just hmv) = HM.map mkScribes hmv
      where
        mkScribes (Array scs) = catMaybes $ map mkScribe $ Vector.toList scs
        mkScribes (String s) = [(s :: ScribeId)]
        mkScribes _ = []
        mkScribe (String s) = Just (s :: ScribeId)
        mkScribe _ = Nothing

    parseSubtraceMap :: Maybe (HM.HashMap Text Value) -> HM.HashMap Text SubTrace
    parseSubtraceMap Nothing = HM.empty
    parseSubtraceMap (Just hmv) = HM.mapMaybe mkSubtrace hmv
      where
        mkSubtrace (String s) = Just (read (unpack s) :: SubTrace)
        mkSubtrace (Object hm) = mkSubtrace' (HM.lookup "tag" hm) (HM.lookup "contents" hm)
        mkSubtrace _ = Nothing
        mkSubtrace' Nothing _ = Nothing
        mkSubtrace' _ Nothing = Nothing
        mkSubtrace' (Just (String tag)) (Just (Array cs)) =
            if tag == "ObservableTrace"
            then Just $ ObservableTrace $ map (\(String s) -> (read (unpack s) :: ObservableInstance)) $ Vector.toList cs
            else Nothing
        mkSubtrace' _ _ = Nothing

    r_hasEKG repr = case (R.hasEKG repr) of
                       Nothing -> 0
                       Just p  -> p
    r_hasGUI repr = case (R.hasGUI repr) of
                       Nothing -> 0
                       Just p  -> p
    r_defaultScribes repr = map (\(k,n) -> pack(show k) <> "::" <> n) (R.defaultScribes repr)

    parseAggregatedKindMap Nothing = HM.empty
    parseAggregatedKindMap (Just hmv) =
        let
            listv = HM.toList hmv
            mapAggregatedKind = HM.fromList $ catMaybes $ map mkAggregatedKind listv
        in
        mapAggregatedKind
      where
        mkAggregatedKind (name, String s) = Just (name, read (unpack s) :: AggregatedKind)
        mkAggregatedKind _ = Nothing

\end{code}

\subsubsection{Setup empty configuration}
\begin{code}
empty :: IO Configuration
empty = do
    cgref <- newMVar $ ConfigurationInternal
                           { cgMinSeverity       = Debug
                           , cgMapSeverity       = HM.empty
#ifdef MemoizeSeverity
                           , cgMapSeverityCache  = HM.empty
#endif
                           , cgMapSubtrace       = HM.empty
                           , cgOptions           = HM.empty
                           , cgMapBackend        = HM.empty
                           , cgDefBackendKs      = []
                           , cgSetupBackends     = []
                           , cgMapScribe         = HM.empty
                           , cgMapScribeCache    = HM.empty
                           , cgDefScribes        = []
                           , cgSetupScribes      = []
                           , cgMapAggregatedKind = HM.empty
                           , cgDefAggregatedKind = StatsAK
                           , cgMonitors          = HM.empty
                           , cgPortEKG           = 0
                           , cgPortGUI           = 0
                           }
    return $ Configuration cgref

\end{code}

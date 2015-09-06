{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE ViewPatterns      #-}

module Main where

import           Control.DeepSeq
import           Control.Exception      (evaluate)
import           Control.Lens           hiding (argument, (.=))
import           Control.Monad.Except
import           Data.Aeson
import           Data.Aeson.Lens
import           Data.ByteString        (ByteString)
import qualified Data.ByteString        as BS
import qualified Data.ByteString.Char8  as BC8
import qualified Data.ByteString.Lazy   as BL
import qualified Data.HashMap.Strict    as HM
import           Data.List
import           Data.Maybe
import           Data.Monoid
import           Data.Text              (Text)
import qualified Data.Text              as T
import qualified Data.Text.Encoding     as T
import qualified Data.Text.IO           as T
import           Network.Http.Client    as HC
import           OpenSSL                (withOpenSSL)
import           Options.Applicative
import           System.Directory
import           System.FilePath
import           System.IO              (hFlush, stderr, stdout)

import           Network.Conduit.Client

data Verbosity = Info | Verbose

logVerbose, logError :: Verbosity -> Text -> IO ()
logVerbose Info _ = pure ()
logVerbose Verbose t = T.hPutStrLn stderr t

logError _ = T.hPutStrLn stderr

data ConfigAuth = ConfigAuthCert !Text !Text -- user/cert
                | ConfigAuthToken !Text
                | ConfigAuthAnon
                deriving Show

data Config = Config
              { cfgApiURL   :: Maybe URL
              , cfgAuth     :: ConfigAuth
              , cfgCallSign :: Maybe RepoCallSign
              } deriving Show

type RepoCallSign = Text

normApiUrl :: Text -> Text
normApiUrl u | T.isSuffixOf "/api/" u = u
             | T.isSuffixOf "/api" u = u <> "/"
             | T.last u == '/'  = u <> "api/"
             | otherwise        = u <> "/api/"


-- | Locate and read CWD-visible Git-repo associated @.arcconfig@
readProjArcConf :: ArcOpts -> IO (Maybe (FilePath, Value))
readProjArcConf ArcOpts {..} = do
    cwd  <- getCurrentDirectory

    findInParents ".git" cwd >>= \case
        [] -> do
            logVerbose verbosity "no Git dir found"
            return Nothing

        ((takeDirectory -> gitdir):_) -> do
            logVerbose verbosity ("Git working copy root at " <> T.pack (show gitdir))
            let arcConf = gitdir </> ".arcconfig"
            doesFileExist arcConf >>= \case
                False -> do
                    logVerbose verbosity ("Couldn't find expected .arcconfig at " <> T.pack (show arcConf))
                    return Nothing
                True -> do
                    logVerbose verbosity ("Reading .arcconfig from " <> T.pack (show arcConf))
                    arcConfJson <- eitherDecodeStrict' <$> BS.readFile arcConf
                    either fail (pure . Just . (,) arcConf) arcConfJson

  where
    verbosity = if verbose then Verbose else Info

    initsDirs = map joinPath . drop 1 . inits . splitDirectories

    findInParents fn0 dir0 = do
        tmp <- forM (reverse $ initsDirs dir0) $ \d -> do
            let fn = d </> fn0
            ex <- (||) <$> doesFileExist fn <*> doesDirectoryExist fn
            if ex then pure (Just fn) else pure Nothing

        return $ catMaybes tmp

readUserConf :: ArcOpts -> IO (Maybe Value)
readUserConf ArcOpts {..} = do
    home <- getHomeDirectory
    let arcRc = home </> ".arcrc"
    doesFileExist arcRc >>= \case
        False -> do
            logVerbose verbosity ("No ~/.arcrc found at " <> T.pack (show arcRc))
            return Nothing

        True -> do
            logVerbose verbosity ("Reading user configuration from " <> T.pack (show arcRc))
            arcRcJson <- eitherDecodeStrict' <$> BS.readFile arcRc
            either fail pure arcRcJson
  where
    verbosity = if verbose then Verbose else Info

readConfig :: ArcOpts -> IO Config
readConfig opts@(ArcOpts {..}) = do

    muserJson <- readUserConf opts
    mprojJson <- readProjArcConf opts

    let dfltUrl = muserJson ^? _Just . key "config" . key "default" . _String
        projUrl = mprojJson ^? _Just . _2 . key "phabricator.uri" . _String

    let apiURL = normApiUrl <$> msum [conUrl, projUrl, dfltUrl]
        cfgApiURL = fmap T.encodeUtf8 apiURL

    logVerbose verbosity ("Phabricator API URL = " <> maybe "" (T.pack . show) apiURL)

    let hostEnt | Just u <- apiURL = muserJson ^? _Just . key "hosts" . key u
                | otherwise = Nothing

    let cfgAuth | Just tok <- conTok = ConfigAuthToken tok
                | Just u <- hostEnt ^? _Just . key "user" . _String
                , Just c <- hostEnt ^? _Just . key "cert" . _String = ConfigAuthCert u c
                | Just tok <- hostEnt ^? _Just . key "token" . _String = ConfigAuthToken tok
                | otherwise = ConfigAuthAnon


    case cfgAuth of
        ConfigAuthCert u _   -> logVerbose verbosity ("Phabricator auth via certificate for user " <> T.pack (show u))
        ConfigAuthToken _    -> logVerbose verbosity "Phabricator auth via API-token"
        ConfigAuthAnon       -> logVerbose verbosity "Phabricator no auth credentials"

    let cfgCallSign = mprojJson ^? _Just . _2 . key "repository.callsign" . _String
    logVerbose verbosity ("Repository Callsign = " <> T.pack (show cfgCallSign))

    return Config {..}
  where
    verbosity = if verbose then Verbose else Info

data ArcCmd
    = ArcList
    | ArcCallConduit !ByteString
    | ArcInstallCert (Maybe String)
    deriving (Eq,Show)

data ArcOpts = ArcOpts
  { verbose :: !Bool
  , conTok  :: Maybe Text
  , conUrl  :: Maybe Text
  , cmd     :: !ArcCmd
  } deriving Show

arcOpts :: Parser ArcOpts
arcOpts = ArcOpts
     <$> switch (long "verbose" <> help "Whether to be verbose")
     <*> optional (txtOption (long "conduit-token" <> metavar "TOKEN" <>
                              help "Ignore configured credentials and use an explicit API token instead"))
     <*> optional (txtOption (long "conduit-uri" <> metavar "URI" <>
                              help "Ignore configured Conduit URI and use an explicit one instead"))
     <*> subparser (mconcat
         [ command "list" (info (pure ArcList) (progDesc "List your open Differential revisions"))
         , command "call-conduit" (info (ArcCallConduit . BC8.pack <$> argument str (metavar "METHOD"))
                                   (progDesc "Perform raw Conduit method call"))
         , command "install-certificate" (info (ArcInstallCert <$> optional (argument str (metavar "URI")))
                                          (progDesc "Installs Conduit credentials into your ~/.arcrc for the given install of Phabricator"))
         ])

txtOption :: Mod OptionFields Text -> Parser Text
txtOption = option (fmap T.pack str)

main :: IO ()
main = execParser opts >>= execArcOpts
  where
    opts = info (helper <*> arcOpts)
           (fullDesc
            <> header "arc-list - Arcanist \"lite\" (CLI tool for Phabricator)"
            <> footer "See http://... for more information")

runConduit :: IO (ConduitResponse b) -> IO b
runConduit act = act >>= \case
    ConduitResult v  -> pure v
    ConduitError c i -> fail (T.unpack $ c <> ": " <> i)

execArcOpts :: ArcOpts -> IO ()
execArcOpts opts@(ArcOpts {..}) = withOpenSSL $ do
    -- let verbosity = if verbose then Verbose else Info
    cfg <- readConfig opts

    let mkCond (Config {..}) = case (cfgAuth, cfgApiURL) of
            (ConfigAuthCert user cert, Just u) -> runConduit $ conduitSessionAuth u user cert
            (ConfigAuthToken tok, Just u)      -> pure $ (conduitAPITokenAuth u tok, PHID "")
            (ConfigAuthAnon, Just u)           -> pure $ (conduitAnonAuth u, PHID "")
            (_, Nothing) -> fail "Could not infer Phabricator API URI"

    case cmd of
        ArcList -> do
            (conduit, uid) <- mkCond cfg
            diffs <- runConduit $ differentialQuery conduit uid

            forM_ diffs $ \DRev {..} -> do
                -- quickndirty padding
                let statusName = T.take 16 (d_statusName <> T.replicate 16 " ")
                T.putStrLn $ mconcat
                    [ " ", statusName, " D", d_id, ": " , d_title ]

        ArcCallConduit meth -> do
            (conduit, _) <- mkCond cfg
            req <- (either fail pure . eitherDecodeStrict') =<< BS.getContents

            res <- callConduitPairs conduit meth (HM.toList (req :: Object))

            -- emulate legacy output format emitted by `arc`
            let res' = object $ case res of
                    ConduitResult v  -> [ "error" .= Null, "errorMessage" .= Null, "response" .= (v :: Value) ]
                    ConduitError c i -> [ "error" .= c, "errorMessage" .= mconcat [c,": ",i], "response" .= Null ]

            BC8.putStrLn (BL.toStrict $ encode res')

        ArcInstallCert muri -> do
            let cfg' | Just uri <- muri  = cfg { cfgApiURL = Just $ T.encodeUtf8 $ normApiUrl $ T.pack uri }
                     | otherwise         = cfg
            (conduit0, _) <- mkCond cfg' { cfgAuth = ConfigAuthAnon }

            -- test API URL anonymously
            _ <- runConduit $ conduitPing conduit0

            -- TODO: check auth-capabilities

            let textUrl = T.dropEnd 4 (T.decodeUtf8 apiurl) <> "conduit/login/"
                apiurl = fromMaybe (error "missing URI") $ cfgApiURL cfg'

            T.putStrLn $ T.unlines
                [ "Open this page in your browser and login to Phabricator if necessary:"
                , ""
                , textUrl
                , ""
                , "Then paste the API Token on that page below."
                ]

            T.putStr "    Paste API Token from that page: " >> hFlush stdout
            tok <- T.strip <$> T.getLine

            -- Verify authentication token works
            (conduit1, _) <- mkCond cfg' { cfgAuth = ConfigAuthToken tok }
            _ <- runConduit $ userWhoami conduit1

            T.putStrLn ""
            T.putStrLn "login successful!"

            updateUserConfHostEntry apiurl tok


updateUserConfHostEntry :: URL -> Text -> IO ()
updateUserConfHostEntry url0 tok = do
    home <- getHomeDirectory
    let arcRc = home </> ".arcrc"
    doesFileExist arcRc >>= \case
        False -> do
            let newcfg = object [ "hosts" .= object [ url .= object [ "token" .= tok ] ] ]
            BL.writeFile arcRc (encode newcfg)
            putStrLn "created new ~/.arcrc file"
            putStrLn "WARNING: you need to 'chmod 600 ~/.arcrc'!"

        True -> do
            -- fail ".arcrc exists already; updating host entries not supported yet"
            putStrLn "updating existing ~/.arcrc file"
            arcRcJson0 <- eitherDecodeStrict' <$> BS.readFile arcRc
            oldcfg <- either fail pure arcRcJson0
            let _ = oldcfg :: Value

            case oldcfg of
                Object _ -> return ()
                _        -> fail "FATAL: top-level structure in .arcrc is not a JSON object"

            let newcfg = case oldcfg ^? key "hosts" of
                    Nothing -> oldcfg & _Object . at "hosts" ?~ object [ url .= object [ "token" .= tok ] ]
                    Just (Object _) -> oldcfg & key "hosts" . _Object . at url ?~ object [ "token" .= tok ]
                    Just _ -> error "FATAL: 'hosts' in .arcrc is not a JSON object"

            evaluate (rnf newcfg)

            BL.writeFile arcRc (encode newcfg)
   where
    url = T.decodeUtf8 url0

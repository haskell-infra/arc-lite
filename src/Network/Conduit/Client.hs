{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeOperators              #-}

-- | See <https://secure.phabricator.com/book/phabdev/article/conduit/>
module Network.Conduit.Client where

import qualified Crypto.Hash.SHA1           as SHA1
import           Data.Aeson
import qualified Data.Aeson.Types           as J
import           Data.ByteString            (ByteString)
import qualified Data.ByteString.Base16     as B16
import qualified Data.ByteString.Char8      as BC8
import qualified Data.ByteString.Lazy       as L
import           Data.Char
import           Data.Maybe
import           Data.Monoid
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as T
import           Data.Time.Clock.POSIX      (getPOSIXTime)
import           GHC.Generics               hiding (to)
import           Network.Http.Client        as HC
import           Network.Http.Client.Extras as HC

----------------------------------------------------------------------------

data ConduitResponse a = ConduitResult a
                       | ConduitError !Text !Text
                       deriving (Show,Functor)

instance FromJSON a => FromJSON (ConduitResponse a) where
    parseJSON = withObject "ConduitResponse" $ \o -> do
        m_result     <- o .: "result"
        m_error_info <- o .: "error_info"
        m_error_code <- o .: "error_code"

        case (m_result, m_error_code, m_error_info) of
            (Nothing, Just ec, mei)    -> pure (ConduitError ec (fromMaybe "" mei))
            (Just v, Nothing, Nothing) -> pure (ConduitResult v)
            (Just _, _, _)             -> fail "ConduitResponse with result & error simultaneously"
            (Nothing, Nothing, _)      -> fail "ConduitResponse with neither result nor error"

-- | Generic call
callConduitPairs :: FromJSON b => Conduit -> ByteString -> [J.Pair] -> IO (ConduitResponse b)
callConduitPairs (Conduit url ConduitAnonAuth) meth parms = performCallRaw url meth parms
callConduitPairs (Conduit url auth) meth parms = performCallRaw url meth $ object (("__conduit__" .= auth) : parms)

-- | Low-level Conduit RPC
performCallRaw :: (ToJSON req, FromJSON res) => URL -> ByteString -> req -> IO (ConduitResponse res)
performCallRaw url meth req
    -- we can't use plain 'postForm' because the CDN in front of Phabricator breaks chunked transfers
    = postFormIdentityEncoded url' formData jsonHandler
  where
    url' = if BC8.last url == '/' then url <> meth else mconcat [ url, "/", meth ]
    formData = [ ("params", L.toStrict (encode req))
               , ("output", "json")
               , ("__conduit__", "1")
               ]

----------------------------------------------------------------------

data GitHash
    = GTCM !Text
    | GTTR !Text
    deriving (Show,Eq,Ord,Generic)

instance FromJSON GitHash where
    parseJSON = genericParseJSON gitHashOpts
      where
        gitHashOpts = J.defaultOptions { J.sumEncoding = J.TwoElemArray
                                       , J.constructorTagModifier = map toLower }

-- | Differential Revision
data DRev = DRev
    { d_activeDiffPHID :: !(PHID Diff)
    , d_authorPHID     :: !(PHID User)
      -- d_auxiliary  TODO
    , d_branch         :: !Text
    , d_ccs            :: [PHID User]
      -- d_commits  TODO
    , d_dateCreated    :: !Text
    , d_dateModified   :: !Text
    , d_diffs          :: [Text]
    , d_hashes         :: [GitHash]
    , d_id             :: !Text
    , d_lineCount      :: !Text
    , d_phid           :: !(PHID DRev)
    , d_repositoryPHID :: !(PHID Repo)
    , d_reviewers      :: [PHID User]
    , d_sourcePath     :: !Text
    , d_status         :: !Text
    , d_statusName     :: !Text
    , d_summary        :: !Text
    , d_testPlan       :: !Text
    , d_title          :: !Text
    , d_uri            :: !Url
    } deriving (Show,Generic)

instance FromJSON DRev where
    parseJSON = genericParseJSON J.defaultOptions { J.fieldLabelModifier = drop 2 }

-- | @differential.query@
differentialQuery :: Conduit -> PHID User -> IO (ConduitResponse [DRev])
differentialQuery conduit author = callConduitPairs conduit "differential.query" params
  where
    params = [ "authors" .= [author], "status"  .= ("status-open" :: Text) ]

--------------------------------------------------------------------------------

-- | @conduit.ping@
conduitPing :: Conduit -> IO (ConduitResponse Text)
conduitPing conduit = callConduitPairs conduit "conduit.ping" []

data ConduitCapabilities = ConduitCapabilities
    { cap_authentication :: [Text]
    , cap_input          :: [Text]
    , cap_output         :: [Text]
    , cap_signatures     :: [Text]
    } deriving (Show,Generic)

instance FromJSON ConduitCapabilities where
    parseJSON = genericParseJSON J.defaultOptions { J.fieldLabelModifier = drop 4 }

-- | @conduit.ping@
conduitGetCapabilities :: Conduit -> IO (ConduitResponse ConduitCapabilities)
conduitGetCapabilities conduit = callConduitPairs conduit "conduit.getcapabilities" []

--------------------------------------------------------------------------------

-- | User information as returned by @user.whoami@
data User = User
    { u_image        :: !Url
    , u_phid         :: !(PHID User)
    , u_primaryEmail :: !Text
    , u_realName     :: !Text
    , u_roles        :: [Text]
    , u_uri          :: !Url
    , u_userName     :: !Text
    } deriving (Show,Generic)

instance FromJSON User where
    parseJSON = genericParseJSON J.defaultOptions { J.fieldLabelModifier = drop 2 }

-- | @user.whoami@
userWhoami :: Conduit -> IO (ConduitResponse User)
userWhoami conduit = callConduitPairs conduit "user.whoami" []

--------------------------------------------------------------------------------

-- | Repository meta-information as returned by @repository.query@
data Repo = Repo
    { repo_callsign    :: !RepoCallSign
    , repo_description :: Maybe Text
    , repo_id          :: !Text
    , repo_isActive    :: !Bool
    , repo_isImporting :: !Bool
    , repo_monogram    :: !Text
    , repo_name        :: !Text
    , repo_phid        :: !(PHID Repo)
    , repo_remoteURI   :: !Url
    -- repo_staging
    , repo_uri         :: Maybe Url
    , repo_vcs         :: !Text
    } deriving (Show,Generic)

instance FromJSON Repo where
    parseJSON = genericParseJSON J.defaultOptions { J.fieldLabelModifier = drop 5 }

type RepoCallSign = Text

-- | @repository.query"
repositoryQuery :: Conduit -> [RepoCallSign] -> IO (ConduitResponse [Repo])
repositoryQuery conduit callsigns
    = callConduitPairs conduit "repository.query" [ "callsigns" .= callsigns ]

--------------------------------------------------------------------------------

data ConduitAuth = ConduitAPITokenAuth !Text -- API token
                 | ConduitSessionAuth !Int !Text -- conn-id/session-key
                 | ConduitAnonAuth
                 deriving (Eq,Show)

instance ToJSON ConduitAuth where
    toJSON (ConduitSessionAuth ci sk) = object [ "connectionID" .= ci, "sessionKey" .= sk ]
    toJSON (ConduitAPITokenAuth tok)  = object [ "token" .= tok ]
    toJSON ConduitAnonAuth = Null

-- | Opaque type describing a Conduit endpoint with authentication information
data Conduit = Conduit !URL !ConduitAuth

conduitAnonAuth :: URL -> Conduit
conduitAnonAuth u = Conduit u ConduitAnonAuth

conduitAPITokenAuth :: URL -> Text -> Conduit
conduitAPITokenAuth u tok = Conduit u (ConduitAPITokenAuth tok)

conduitSessionAuth :: URL -> Text -> Text -> IO (ConduitResponse (Conduit, PHID User))
conduitSessionAuth u user0 cert0 = fmap sess2conduit <$> conduitConnect u user0 cert0
  where
    sess2conduit ConduitSession {..} = (Conduit u (ConduitSessionAuth connectionID sessionKey), userPHID)

    conduitConnect :: URL -> Text -> Text -> IO (ConduitResponse ConduitSession)
    conduitConnect uri user cert = do
        authTok <- round <$> getPOSIXTime

        let req = object [ "client"        .= String "arc-lite"
                         , "clientVersion" .= (6 :: Word)
                         , "clientDescription" .= String "arc-lite (experimental)"

                         , "host"          .= T.decodeLatin1 uri
                         , "user"          .= user
                         , "authToken"     .= (authTok :: Word)
                         , "authSignature" .= authSig
                         ]

            authSig  = T.decodeLatin1 $ B16.encode $ SHA1.hash (T.encodeUtf8 (T.pack (show authTok) <> cert))

        performCallRaw uri "conduit.connect" req

-- helper used by conduitSessionAuth
data ConduitSession = ConduitSession
  { connectionID :: !Int
  , sessionKey   :: !Text
  , userPHID     :: !(PHID User)
  } deriving (Show,Generic)

instance FromJSON ConduitSession

--------------------------------------------------------------------------------

type Url = Text

-- These phantom-types for now, see also 'DRev'
data Diff

newtype PHID p = PHID Text
               deriving (Show,Eq,Ord,FromJSON,ToJSON)

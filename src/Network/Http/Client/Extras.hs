{-# LANGUAGE OverloadedStrings #-}

-- | Additional helpers for use with @http-streams@' "Network.Http.Client"
module Network.Http.Client.Extras
    ( urlPath
    , postFormIdentityEncoded
    ) where

import           Data.ByteString         (ByteString)
import qualified Data.ByteString         as BS
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy    as BL
import qualified Data.Text               as T
import qualified Data.Text.Encoding      as T
import           Network.Http.Client
import           Network.URI             (URI (..), parseURI)
import           System.IO.Streams

-- | Variant of 'postForm' which uses identify transfer-encoding for
-- sending the @POST@ request body
postFormIdentityEncoded :: URL
                           -- ^ Resource to POST to.
                        -> [(ByteString, ByteString)]
                           -- ^ List of name=value pairs. Will be sent URL-encoded.
                        -> (Response -> InputStream ByteString -> IO β)
                           -- ^ Handler function to receive the response from the server.
                        -> IO β
postFormIdentityEncoded u nvs handler
  | Just up <- urlPath u = withConnection (establishConnection u) (process up)
  | otherwise            = fail "postFormIdentityEncoded: failed to parse URL"
  where
    q up sz = buildRequest1 $ do
            http POST up
            setAccept "*/*"
            setContentType "application/x-www-form-urlencoded"
            setContentLength sz

    process up c = do
        body <- outputBuilderToByteString (encodedFormBody nvs)
        sendRequest c (q up (BL.length body)) (lazyByteStringBody body)
        receiveResponse c handler

-- | Convenience function extracting the URL path from an 'URL'.
-- Returns 'Nothing' when parsing fails.
urlPath :: URL -> Maybe ByteString
urlPath = fmap path . parseURI . T.unpack . T.decodeUtf8
  where
    path :: URI -> ByteString
    path u
      | BS.null url  = "/"
      | otherwise    = url
      where
        url = T.encodeUtf8 . T.pack . concat $
              [uriPath u, uriQuery u, uriFragment u]


lazyByteStringBody :: BL.ByteString -> OutputStream BB.Builder -> IO ()
lazyByteStringBody b = write (Just $ BB.lazyByteString b)

outputBuilderToByteString :: (OutputStream BB.Builder -> IO ()) -> IO BL.ByteString
outputBuilderToByteString os = BB.toLazyByteString . mconcat <$> outputToList os

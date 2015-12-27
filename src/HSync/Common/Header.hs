{-# LANGUAGE TypeFamilies #-}
module HSync.Common.Header where

import ClassyPrelude.Yesod
-- import Control.Monad((<=<))
import Data.CaseInsensitive(mk)
import qualified Data.Text as T
import HSync.Common.DateTime(DateTime)
import HSync.Common.Types
import HSync.Common.FileVersion(FileKind)
import Text.Read(reads)
-- import Network.HTTP.Types.Header

-- import Yesod.Core(PathPiece(..), MonadHandler, addHeader, lookupHeader)

--------------------------------------------------------------------------------

class IsTypedHeader h where
  type HeaderValue h

  headerName        :: h -> Text

  parseHeaderValue  :: h -> ByteString    -> Maybe (HeaderValue h)
  encodeHeaderValue :: h -> HeaderValue h -> Text


asHeader     :: IsTypedHeader h => h -> HeaderValue h -> Header
asHeader h x = (headerName' h, encodeUtf8 $ encodeHeaderValue h x)


headerName' :: IsTypedHeader h => h -> HeaderName
headerName' = mkHeaderName . headerName

mkHeaderName :: Text -> HeaderName
mkHeaderName = mk . encodeUtf8


headerValue   :: IsTypedHeader h => h -> ResponseHeaders -> Maybe (HeaderValue h)
headerValue h = parseHeaderValue h <=< lookup (headerName' h)


lookupTypedHeader   :: (MonadHandler m, IsTypedHeader h) => h -> m (Maybe (HeaderValue h))
lookupTypedHeader h = (>>= parseHeaderValue h) <$> lookupHeader (headerName' h)


addTypedHeader   :: (MonadHandler m, IsTypedHeader h) => h -> HeaderValue h -> m ()
addTypedHeader h = addHeader (headerName h) . (encodeHeaderValue h)

--------------------------------------------------------------------------------

data HClientId = HClientId deriving (Show,Eq)


instance IsTypedHeader HClientId where
  type HeaderValue HClientId = ClientId

  headerName        _ = "clientId"
  parseHeaderValue  _ b = case reads . T.unpack . decodeUtf8 $ b of
                            ((i,""):_) -> Just $ ClientId i
                            _          -> Nothing
  encodeHeaderValue _ = T.pack . show . _unClientId


------------------------------

data HFileKind = HFileKind deriving (Show,Eq)


instance IsTypedHeader HFileKind where
  type HeaderValue HFileKind = FileKind

  headerName        _ = "fileKind"
  parseHeaderValue  _ = fromPathPiece . decodeUtf8
  encodeHeaderValue _ = toPathPiece

------------------------------

data HModificationTime = HModificationTime deriving (Show,Eq)


instance IsTypedHeader HModificationTime where
  type HeaderValue HModificationTime = DateTime

  headerName        _ = "modificationTime"
  parseHeaderValue  _ = fromPathPiece . decodeUtf8
  encodeHeaderValue _ = toPathPiece

------------------------------

data HUserName = HUserName deriving (Show,Eq)

instance IsTypedHeader HUserName where
  type HeaderValue HUserName = UserName

  headerName        _ = "userName"
  parseHeaderValue  _ = fromPathPiece . decodeUtf8
  encodeHeaderValue _ = toPathPiece

------------------------------

data HPassword = HPassword deriving (Show,Eq)

instance IsTypedHeader HPassword where
  type HeaderValue HPassword = Password

  headerName        _ = "password"
  parseHeaderValue  _ = fromPathPiece . decodeUtf8
  encodeHeaderValue _ = toPathPiece

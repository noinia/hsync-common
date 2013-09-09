{-# Language  OverloadedStrings
  #-}
module HSync.Common.FileIdent( FileIdent(..)
                             , fileIdent
                             , checkFileIdent
                             , ErrorDescription

                             , checkMTime
                             ) where



import Control.Applicative((<$>),(<*>))
import Data.Monoid
import Data.Text(Text)
import Yesod.Core

import HSync.Common.DateTime
import HSync.Common.Types
import HSync.Common.Import

import HSync.Common.AtomicIO


import System.Directory(doesDirectoryExist)

import qualified Data.Text as T

--------------------------------------------------------------------------------

type HashedFile = Text

data FileIdent = NonExistent
               | Directory DateTime
               | File      DateTime
               deriving (Show,Read,Eq)

dirPrefix :: Text
dirPrefix = "directory_"

filePrefix :: Text
filePrefix = "file_"

instance PathPiece FileIdent where
    toPathPiece NonExistent   = "nonexistent"
    toPathPiece (Directory d) = dirPrefix  <> toPathPiece d
    toPathPiece (File      d) = filePrefix <> toPathPiece d
    fromPathPiece t | t == "nonexistent"        = Just NonExistent
                    | t `startsWith` dirPrefix  = Directory <$> f dirPrefix
                    | t `startsWith` filePrefix = File      <$> f filePrefix
                    | otherwise                 = Nothing
        where
          f s = fromPathPiece $ T.drop (T.length s) t

--------------------------------------------------------------------------------
-- | Computing and comparing File Idents

-- | Given a path, compute the file ident
fileIdent    :: (Functor m, MonadIO m) => FilePath -> m FileIdent
fileIdent fp = exists fp >>= \t -> case t of
                 (False,False) -> return NonExistent
                 (_,    True)  -> Directory <$> mT fp
                 (True,False)  -> File      <$> mT fp
    where
      mT = liftIO . modificationTime

type ErrorDescription = [Text]

-- | Check the fileId. If the result is 'Nothing' then there were no errors found
-- otherwise, we give a description of the error
checkFileIdent                :: MonadIO m => FileIdent -> FilePath ->
                                   m (Maybe ErrorDescription)
checkFileIdent exp p = exists p >>= \(dir,file) -> iError p exp dir file


-- | determine the error depending on the path, expected ident, and the result of
-- the file and directory tests
iError                             :: MonadIO m =>
                                      FilePath -> FileIdent ->
                                      Bool -> Bool -> m (Maybe ErrorDescription)
iError _ NonExistent   False False = noError
iError _ NonExistent   True  False = err "File found, no file or directory expected."
iError _ NonExistent   _     True  = err "Directory found, no file or directory expected."

iError _ (Directory _) False False = err "Nothing found, directory expected."
iError _ (Directory _) True  False = err "File found, directory expected."
iError p (Directory d) _     True  = checkMTime p d

iError _ (File _)      False False = err "Nothing found, file expected."
iError p (File d)      True  False = checkMTime p d
iError _ (File _)      _     True  = err "Directory found, file expected."


-- | Check the modification time of a file.
checkMTime     :: MonadIO m => FilePath -> DateTime -> m (Maybe ErrorDescription)
checkMTime p d = liftIO (modificationTime p) >>= \td ->
                 if td == d then noError
                            else err "Modification date mismatch."

--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- | Helper functions
startsWith :: Text -> Text -> Bool
startsWith = flip T.isPrefixOf


noError :: Monad m => m (Maybe a)
noError = return Nothing

err   :: Monad m => Text -> m (Maybe ErrorDescription)
err t = return . Just $ [t]
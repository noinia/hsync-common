module HSync.Common.Zip where

import ClassyPrelude.Yesod
import System.FilePath
import Data.Conduit
import HSync.Common.DateTime(toEpochTime', fileModificationTime)
import qualified Data.Conduit.List as CL
import qualified Data.Conduit.Serialization.Binary as BC
import qualified Data.Conduit.Combinators as CC
import qualified Codec.Archive.Zip as Zip

--------------------------------------------------------------------------------

-- | Read an file (the second FP) into an Entry in a zip file.
readEntryAs       :: (MonadBaseControl IO m, MonadThrow m, MonadIO m)
                  => FilePath -> FilePath -> m Zip.Entry
readEntryAs rp fp = do
                      mt <- toEpochTime' <$> fileModificationTime fp
                      Zip.toEntry rp mt <$> runResourceT (sourceFile fp $$ CC.sinkLazy)

readArchive     :: (MonadBaseControl IO m, MonadThrow m, MonadIO m)
                => [(FilePath, FilePath)] -> Source m ByteString
readArchive fps = do
  es <- lift $ mapM (uncurry readEntryAs) fps
  let ar = Zip.emptyArchive { Zip.zEntries = es }
  yield ar =$= BC.conduitEncode


-- -- | actually performs lazy IO to read the files from disk.
-- readArchive    :: (MonadIO m, MonadThrow m) => FilePath -> Source m ByteString
-- readArchive fp = do
--     ar <- liftIO $ Zip.addFilesToArchive [Zip.OptRecursive] Zip.emptyArchive [fp]
--     let ar' = ar { Zip.zEntries = map (relativeTo $ takeDirectory' fp) $ Zip.zEntries ar }
--     yield ar' =$= BC.conduitEncode
--   where
--     takeDirectory'   = takeDirectory . dropTrailingPathSeparator
--     relativeTo dir e = let dir' = makeRelative "/" . dropDrive $ dir
--                        in e { Zip.eRelativePath =
--                                 makeRelative dir' $ Zip.eRelativePath e}



-- | Performs lazy IO to write the files to disk
writeArchive    :: (MonadIO m, MonadThrow m) => FilePath -> Sink ByteString m ()
writeArchive fp = BC.conduitDecode =$= sink
  where
    sink = awaitForever $ liftIO . (Zip.extractFilesFromArchive [Zip.OptDestination fp])


-- testx         :: FilePath -> FilePath -> IO ()
-- testx iFp oFp = runResourceT $ readArchive iFp $$ sinkFile oFp

-- extr iFp oFp = runResourceT $ sourceFile iFp $$ writeArchive oFp

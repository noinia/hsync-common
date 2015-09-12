module HSync.Common.Zip where


-- import Codec.Archive.Zip
-- import Data.Conduit.Serialization.Binary
-- import Prelude.Conduit


-- --------------------------------------------------------------------------------

-- -- | Given  a list of files, produce a zip file containing all the file data.
-- zipSource         :: MonadResource m => [ZipOption] -> [FilePath] -> Source m ByteString
-- zipSource opts fs = conduitEncode
--   where
--     archive = emptyArchive

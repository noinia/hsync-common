module HSync.Common.API where

import ClassyPrelude.Yesod
import HSync.Common.Types
import HSync.Common.DateTime
import HSync.Common.FileVersion(FileKind)

data HSyncAPI = HSyncAPI deriving (Show,Read,Eq)

-- mkYesodSubData "HSyncAPI" $(parseRoutesFile "config/api-routes")
mkYesodSubData "HSyncAPI" $(parseRoutesFile "/Users/frank/workspace/hs-projects/hsync/hsync-common/config/api-routes")

{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
module HSync.Common.Notification(-- * Events
                                  EventKind(..)
                                , _Added, _Updated, _Deleted

                                , mkEventKind

                                -- , involvesFile, involvesDirectory
                                , Event'(..), Event
                                , eventKind, newVersion, affectedPath

                                -- , fileAdded , fileRemoved, fileUpdated
                                -- , directoryAdded, directoryRemoved

                                -- * Notifications
                                , Notification(..), event
                                , notification


                                , toLog
                                , matchesNotification
                                ) where

import ClassyPrelude.Yesod

import Control.Lens

import Data.ByteString(ByteString)

import Data.Aeson.TH
import Data.SafeCopy(base, deriveSafeCopy)

import HSync.Common.Types
import HSync.Common.DateTime(DateTime)
import HSync.Common.FileVersion

import qualified Data.ByteString.Char8 as B

--------------------------------------------------------------------------------

-- | The file version stored with the Updated/Deleted EventKind is the *old*
-- version.
data EventKind c = Added
                 | Updated (FileVersion c)
                 | Deleted (FileVersion c)
                 deriving (Show,Read,Eq,Functor,Foldable,Traversable)
$(deriveJSON defaultOptions ''EventKind)
$(deriveSafeCopy 0 'base ''EventKind)
makePrisms ''EventKind



mkEventKind                :: Maybe (FileVersion c) -> (FileVersion c) -> (EventKind c)
mkEventKind Nothing    _   = Added
mkEventKind (Just old) new = case new^.fileKind of
                               NonExistent -> Deleted old
                               _           -> Updated old

type Event = Event' ClientName

data Event' c = Event { _eventKind        :: EventKind c
                      , _newVersion       :: FileVersion c
                      , _affectedRealm    :: RealmId
                      , _affectedPath     :: Path
                      }
             deriving (Show,Read,Eq,Functor,Foldable,Traversable)
$(deriveJSON defaultOptions ''Event')
$(deriveSafeCopy 0 'base ''Event')
makeLenses ''Event'

-- fileAdded        :: Path -> Event
-- fileRemoved      :: Path -> FileIdent -> Event
-- fileUpdated      :: Path -> FileIdent -> Event

-- fileAdded   p    = Event FileAdded p   NonExistent
-- fileRemoved p fi = Event FileRemoved p fi
-- fileUpdated p fi = Event FileUpdated p fi

-- directoryAdded        :: Path -> Event
-- directoryRemoved      :: Path -> FileIdent -> Event


-- directoryAdded   p    = Event DirectoryAdded p   NonExistent
-- directoryRemoved p fi = Event DirectoryRemoved p fi


--------------------------------------------------------------------------------

type PublicNotification = Notification (Maybe ClientName)

newtype Notification c = Notification { _event :: Event' c }
                  deriving (Read,Eq,Show,Functor,Foldable,Traversable)
makeLenses ''Notification
$(deriveJSON defaultOptions ''Notification)
$(deriveSafeCopy 0 'base ''Notification)


-- | smart constructor to construct a notification
notification              :: Maybe (FileVersion c) -> FileVersion c -> RealmId -> Path
                          -> Notification c
notification old new ri p = Notification $ Event (mkEventKind old new) new ri p


-- | Notifications are ordered on timestamp
instance Eq c => Ord (Notification c) where
  compare = compare `on` (^.event.newVersion.lastModified.modificationTime)

toLog   :: Show c => (Notification c) -> String
toLog n = show n

  -- let lm = n^.event.newVersion.lastModified
  --         in


  -- intercalate ":" [ show $ lm^.modificationTime
  --                 , show $ lm^.modClient
  --                 , show $ n^.eventKind
  --                 ]

  -- intercalate ":" $ [show ti, show ci, show evt]

fromLog :: ByteString -> Maybe (Notification c)
fromLog = const Nothing --TODO: Implement this


-- | Weather or not the notification is about something from the subtree with
-- path p in realm ri .
matchesNotification        :: RealmId -> Path -> (Notification c) -> Bool
matchesNotification ri p n =  n^.event.affectedRealm == ri
                           && p `isSubPathOf` (n^.event.affectedPath)



-- evtS = "FileAdded (Path \"\" [])"

-- testS = B.pack "2013-08-23-18:40:59.975000000000-UTC:\"\":FileAdded (Path \"\" [])"

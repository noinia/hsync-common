{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# Language TemplateHaskell #-}
{-# Language GeneralizedNewtypeDeriving #-}
{-# Language DeriveDataTypeable #-}
module HSync.Common.MTimeTree( FileData(..)

                             -- , TimedFSTree(..)

                             -- , MTimeTree
                             -- , fileIdentOf
                             -- , readMTimeData
                             -- , readMTimeTree


                             -- , addByFileIdent
                             -- , deleteByFileIdent
                             -- , updateByFileIdent

                             , Max(..)
                             ) where

import           Control.Monad.Trans.Control
import           Control.Applicative((<$>))
import           Control.Exception
import           Control.Lens
import           Control.Monad.IO.Class(MonadIO(..))
import           Data.Aeson.TH
import           Data.Data(Data, Typeable)
import           System.IO.Error(mkIOError, illegalOperationErrorType)
import           Data.Semigroup

import           Data.SafeCopy(base, deriveSafeCopy)

import           HSync.Common.DateTime(DateTime, AsDateTime(..), modificationTime)
import           HSync.Common.FSTree2

import           HSync.Common.FileIdent(FileIdent, HasFileIdent(..))

import           HSync.Common.Notification(Notification)

import           HSync.Common.Types(FileName, SubPath)

import qualified HSync.Common.FileIdent as FI


--------------------------------------------------------------------------------

-- | The second arugment is the modification time
data FileData a = FileData { _extractData          :: a
                           , _modificationTimeData :: DateTime
                           }
                       deriving  (Show,Eq,Ord,Data,Typeable)

makeLenses ''FileData

$(deriveJSON defaultOptions ''FileData)
$(deriveSafeCopy 0 'base ''FileData)

$(deriveJSON defaultOptions ''Max)
$(deriveSafeCopy 0 'base ''Max)

instance Measured (Max DateTime) (FileData a) where
  measure (FileData _ m) = Max m

instance HasFileIdent (FSTree F m (FileData a)) where
  toFileIdent = FI.File . _modificationTimeData . fileData

instance HasFileIdent (FSTree D m (FileData a)) where
  toFileIdent = FI.Directory . _modificationTimeData . fileData


--------------------------------------------------------------------------------
-- | A tree with just the modification times

newtype TimedFSTree a = TimedFSTree { _root :: FSTree D (Max DateTime) (FileData a) }
                        deriving (Show,Eq,Typeable,HasFileIdent)

makeLenses ''TimedFSTree

$(deriveJSON defaultOptions ''TimedFSTree)
$(deriveSafeCopy 0 'base ''TimedFSTree)


withDir                   :: ( FSTree D (Max DateTime) (FileData t)
                               ->
                               FSTree D (Max DateTime) (FileData a)
                             )
                          -> TimedFSTree t -> TimedFSTree a
withDir f (TimedFSTree d) = TimedFSTree $ f d

-- --------------------------------------------------------------------------------

type MTimeTree = TimedFSTree ()

-- | Read an MTimeTree from disk. Returns Nothing if the path does not point to
-- a directory on the filesystem.
readMTimeTree    :: (Functor m, MonadIO m, MonadBaseControl IO m)
                 => FilePath -> m (Either IOException MTimeTree)
readMTimeTree fp = fmap f . readFSTree readMTimeData $ fp
  where
    f e = e >>= \case
            (Left file) -> Left $ mkIOError illegalOperationErrorType "" Nothing (Just fp)
            (Right dir) -> return $ TimedFSTree dir

-- | Read the Modification time data for a single file
readMTimeData    :: (Functor m, MonadIO m) => FilePath -> m (FileData ())
readMTimeData fp = (FileData ()) <$> modificationTime fp


-- | get the fileIdent of a certain file in the tree
fileIdentOf   :: SubPath -> MTimeTree -> FileIdent
fileIdentOf p = toFileIdent . accessFileOrDirectoryAt sp n . _root
  where
    (sp,n) = andLast p

andLast        :: [a] -> ([a],a)
andLast []     = error "andLast: empty list."
andLast [y]    = ([],y)
andLast (x:xs) = let (xs',y) = andLast xs in (x:xs',y)


-- -- | Given a subpath and a fileIdent. Add a new item (i.e. file or directory)
-- -- to the MTimeTree. This operation assumes that all parents of the new item
-- -- already exist in the tree.
-- addByFileIdent                     :: SubPath -> FileIdent -> MTimeTree -> MTimeTree
-- addByFileIdent _ FI.NonExistent    = id
-- addByFileIdent p (FI.File dt)      = let (sp,n) = andLast p in withDir $
--                                      addFileAt      sp (file n $ FileData () dt)
-- addByFileIdent p (FI.Directory dt) = let (sp,n) = andLast p in withDir $
--                                      addDirectoryAt sp (emptyDirectory n $ FileData () dt)

-- -- | Delete the item, i.e. file or directory at the indicated sub path. If the
-- -- item is a directory, this removes that entire subtree. The given dateTime is used
-- -- to (recursive) update the Modification times.
-- deleteByFileIdent :: SubPath -> DateTime -> FileIdent -> MTimeTree -> MTimeTree
-- deleteByFileIdent _ _  FI.NonExistent   = id
-- deleteByFileIdent p dt (FI.File _)      = let (sp,n) = andLast p in withDir $
--                                           deleteFileAt      sp n (Max dt)
-- deleteByFileIdent p dt (FI.Directory _) = let (sp,n) = andLast p in withDir $
--                                           deleteDirectoryAt sp n (Max dt)

-- updateByFileIdent                :: SubPath -> FileIdent -> MTimeTree -> MTimeTree
-- updateByFileIdent p (FI.File dt) = let g f    = f { fileData = FileData () dt }
--                                    in withDir $ updateFileAt p g
-- updateByFileIdent _ _            = error "updateByFileIdent: Only files are supported."

-- -- --------------------------------------------------------------------------------

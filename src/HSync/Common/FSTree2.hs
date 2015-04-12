{-# Language GeneralizedNewtypeDeriving #-}
{-# Language DeriveDataTypeable #-}
{-# Language FlexibleInstances #-}
{-# Language TupleSections #-}
{-# Language FunctionalDependencies #-}
{-# Language MultiParamTypeClasses #-}
{-# Language KindSignatures #-}
{-# Language RankNTypes #-}
{-# Language GADTs #-}
{-# Language DataKinds #-}
{-# Language PolyKinds #-}
module HSync.Common.FSTree2( FSTree(..)
                           , FileType(..)

                           , fileName

                           , files
                           , subDirectories
                           , directoryContents

--                           , directoryContentsL


                           , remeasureDirectory
                           , remeasureAll

                           , renameTo
                           , deleteChild
                           , setChild

                           , accessFile
                           , accessSubDirectory
                           , accessFileOrDirectoryAt


                           -- , move
                           , delete
                           , assignTo
                           , updateAt
                           )

       where

import           Control.Applicative
import           Control.Applicative
import           Control.Lens
import           Data.Aeson
import           Data.Aeson.TH
import           Data.Data(Data, Typeable)
import           Data.Default
import qualified Data.Foldable as F
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as M
import           Data.Maybe(mapMaybe, fromMaybe)
import           Data.SafeCopy(base, deriveSafeCopy)
import           Data.Semigroup
import qualified Data.Text     as T
import qualified Data.Traversable as Tr
import           HSync.Common.Types


--------------------------------------------------------------------------------

class Semigroup m => Measured m a | a -> m where
  measure :: a -> m



--------------------------------------------------------------------------------

-- | A file are either proper Files or directories
data FileType = F | D deriving (Show,Read,Eq,Data,Typeable,Enum,Bounded)

-- | A filesystem. A File and directory may have a payload of type a, and a
-- measurement of type m.
--
-- The measurement of a file is determined (only) by the value of type a
--
-- The measurement of a Directory is the sconcat of all measurements
-- rooted at this subtree. I.e.
--  m = sconcat $ measurement d :| map measurement children
data FSTree (t :: FileType) (m :: *) (a  :: *) where
  File      :: FileName ->      a ->                                 FSTree F m a
  Directory :: FileName -> m -> a -> M.Map FileName (FSTree' m a) -> FSTree D m a


-- | A file or directory without filename.
type FSTree' m a = FileName -> Either (FSTree F m a) (FSTree D m a)

-- | Create a new file
file :: FileName -> a -> FSTree F m a
file = File

-- | Create a new directory
directory         :: Measured m a
                  => FileName -> a -> M.Map FileName (FSTree' m a) -> FSTree D m a
directory n a chs = Directory n m a chs
  where
    m = sconcat $ measure a NonEmpty.:| measureChildren' chs

-- | Create a new directory
emptyDirectory     :: Measured m a => FileName -> a -> FSTree D m a
emptyDirectory n a = Directory n (measure a) a mempty


-- lift             :: (Either (c -> a) (c -> b)) -> (c -> Either a b)
-- lift (Left f) c  = Left $ f c
-- lift (Right f) c = Right $ f c


-- unLift           :: Either a b -> (Either (c -> a) (c -> b))
-- unLift (Left a)  = Left  $ \c -> a
-- unLift (Right b) = Right $ \c -> b

-- | Helper function to construct FSTree' 's
mkFSTree'                       :: FSTree t m a -> FSTree' m a
mkFSTree' f@(File _ _)          = \n -> Left  $ set fileName n f
mkFSTree' d@(Directory _ _ _ _) = \n -> Right $ set fileName n d


-- | Given an either, construct an FSTree
mkFSTree'' :: Either (FSTree F m a) (FSTree D m a) -> FSTree' m a
mkFSTree'' = either mkFSTree' mkFSTree'


--------------------------------------------------------------------------------

instance Measured m a => Measured m (FSTree t m a) where
  measure (File _ a)          = measure a
  measure (Directory _ m _ _) = m


instance Functor (FSTree t m) where
  fmap = Tr.fmapDefault

instance F.Foldable (FSTree t m) where
  foldMap = Tr.foldMapDefault

instance Tr.Traversable (FSTree t m) where
  traverse = unsafeTraverseTopDown




-- | unsafeTraverse does not update any measurements.
unsafeTraverseTopDown :: Applicative f => (a -> f b) -> FSTree t m a -> f (FSTree t m b)
unsafeTraverseTopDown f (File n a)            = (File n) <$> f a
unsafeTraverseTopDown f (Directory n m a chs) = (\a' chs' -> Directory n m a' chs')
                                     <$> f a
                                     <*> M.traverseWithKey g chs
    where
      g n mkF = either (fmap mkFSTree' . unsafeTraverseTopDown f)
                       (fmap mkFSTree' . unsafeTraverseTopDown f) $ mkF n


instance (Show m, Show a) => Show (FSTree t m a) where
  show (File n a) = concat [ "File { _fileName = "
                             , show n
                             , ", _fileData = "
                             , show a
                             , " }"
                             ]
  show (Directory n m a chs) = concat [ "Directory { _fileName = "
                                      , show n
                                      , " , _measurement = "
                                      , show m
                                      , " , _fileData = "
                                      , show a
                                      , " , _directoryContent = fromList "
                                      , show . M.elems . pack $ chs
                                      , " }"
                                      ]
-- | Equality is lexicographic on (name,measure,data, children)
instance (Measured m a, Eq m, Eq a) => Eq (FSTree t m a) where
  (File n a)          == (File n' a')           = (n,measure a,a) == (n',measure a',a')
  (Directory n m a chs) == (Directory n' m' a' chs') =
    (n,m,a, M.elems $ pack chs) == (n',m',a', M.elems $ pack chs')


pack :: M.Map k (k -> v) -> M.Map k v
pack = M.mapWithKey (\n mkF -> mkF n)

--------------------------------------------------------------------------------
-- * Accessor functions

_fileName                     :: FSTree t m a -> FileName
_fileName (File n _)          = n
_fileName (Directory n _ _ _) = n

_measurement :: Measured m a => FSTree t m a -> m
_measurement = measure

_fileData                     :: FSTree t m a -> a
_fileData (File _ a)          = a
_fileData (Directory _ _ a _) = a

_directoryContents                     :: FSTree D m a -> M.Map FileName (FSTree' m a)
_directoryContents (Directory _ _ _ c) = c




-- | Get all files in this directory
files :: FSTree D m a -> M.Map FileName (FileName -> FSTree F m a)
files = filesOrDirs left . _directoryContents

-- | Get all subdirectoreis in this directory
subDirectories :: FSTree D m a -> M.Map FileName (FileName -> FSTree D m a)
subDirectories = filesOrDirs right . _directoryContents







left :: Either a b -> Maybe a
left = either Just (const Nothing)

right :: Either a b -> Maybe b
right = either (const Nothing) Just

filesOrDirs   :: Eq k
              => (Either a b -> Maybe c)
              -> M.Map k (k -> Either a b)
              -> M.Map k (k -> c)
filesOrDirs f = M.fromAscList . mapMaybe f' . M.toAscList
  where
    -- f' :: (k,k -> Either a b)  -> Maybe (k,k -> c)
    f' (k,mkE) = (\c -> (k,\k' -> c)) <$> f (mkE k)



----------------------------------------
-- * Lenses

fileName :: Lens' (FSTree t m a) FileName
fileName = lens _fileName set
  where
    set :: FSTree t m a -> FileName -> FSTree t m a
    set (File _ a) n          = File n a
    set (Directory _ m a c) n = Directory n m a c


directoryContents :: Lens' (FSTree D m a) (M.Map FileName (FSTree' m a))
directoryContents = lens _directoryContents
                         (\(Directory n m a _) chs -> Directory n m a chs)




--------------------------------------------------------------------------------
-- * Fiddling with Measurements

-- | Recomputes the measure of this (and only this) directory.
remeasureDirectory                       :: Measured m a => FSTree D m a -> FSTree D m a
remeasureDirectory (Directory n _ a chs) = directory n a chs

-- | Recomputes the measure from scratch in the entire tree.
remeasureAll                       :: Measured m a => FSTree t m a -> FSTree t m a
remeasureAll f@(File n a)          = f
remeasureAll (Directory n _ a chs) = directory n a chs'
  where
    --    | Remeasure all subdirectories.
    chs' = fmapChildren remeasureAll remeasureAll chs

-- | Gather all measurements from my children.
measureChildren :: Measured m a => FSTree D m a -> [m]
measureChildren = measureChildren' . _directoryContents

-- | Gathers all measurements from a children map
measureChildren' :: (Measured m a, Measured m b)
                    => M.Map k (k -> Either (FSTree t m a) (FSTree t' m b)) -> [m]
measureChildren' = map (either id id) . M.elems
                    . pack . fmapChildren measure measure


-- | Given two functions map them on the left and right element of the children list
fmapChildren     :: (a -> c) -> (b -> d) -> M.Map k (k -> Either a b) -> M.Map k (k -> Either c d)
fmapChildren f g = fmap (fmap (fmapBoth f g))


-- | given two functions, fmap them on the Left and Right of the either.
fmapBoth     :: (a -> c) -> (b -> d) -> Either a b -> Either c d
fmapBoth f g = either (Left . f) (Right . g)

--------------------------------------------------------------------------------
-- * Operations on the root of the Tree

renameTo :: FileName -> FSTree t m a -> FSTree t m a
renameTo = set fileName

-- | Delete a file in the current directory
deleteChild   :: Measured m a => FileName -> FSTree D m a -> FSTree D m a
deleteChild n = remeasureDirectory . unsafeDeleteChild n

-- | Does not update the measure!!
unsafeDeleteChild   :: FileName -> FSTree D m a -> FSTree D m a
unsafeDeleteChild n = over directoryContents (M.delete n)

-- | Set/add a child (the first argument) to the current directory.
setChild   :: Measured m a => FSTree t' m a -> FSTree D m a -> FSTree D m a
setChild t = remeasureDirectory . unsafeSetChild t

-- Does not update the measure!!
unsafeSetChild   :: FSTree t' m a -> FSTree D m a -> FSTree D m a
unsafeSetChild t = over directoryContents (M.insert (t^.fileName) (mkFSTree' t))

accessFile   :: FileName -> FSTree D m a -> Maybe (FSTree F m a)
accessFile n = fmap ($ n) . M.lookup n . files

accessSubDirectory :: FileName -> FSTree D m a -> Maybe (FSTree D m a)
accessSubDirectory n = fmap ( $ n) . M.lookup n . subDirectories


accessFileOrDirectory  :: FileName -> FSTree D m a
                        -> Maybe (Either (FSTree F m a) (FSTree D m a))
accessFileOrDirectory n = fmap ($ n) . M.lookup n . _directoryContents


-- * Operations as a filesytem


accessFileOrDirectoryAt           :: SubPath -> FileName -> FSTree D m a
                                  -> Maybe (Either (FSTree F m a) (FSTree D m a))
accessFileOrDirectoryAt []    n t = accessFileOrDirectory n t
accessFileOrDirectoryAt (d:p) n t = accessSubDirectory d t >>= accessFileOrDirectoryAt p n



-- -- | Given the current path, and a new path, move the file under consideration
-- move :: (SubPath,FileName) -> (SubPath,FileName) -> FSTree D m a -> FSTree D m a
-- move (oldP,oldN) (newP,newN) = undefined



-- | Given a path to a directory d, and a filename n. Delete the file or
-- directory with name n from directory d.
delete           :: Measured m a => SubPath -> FileName -> FSTree D m a -> FSTree D m a
delete []    n t = deleteChild n t
delete (d:p) n t = maybe t (flip setChild t) msd
  where
    -- The new child directory after deleting the item.
    msd = delete p n <$> accessSubDirectory d t


-- | Given a path to a directory t, and a FSTree f, insert f into d. All
-- intermediate directories are created if neccesary (and assigned the default
-- value of type a.)
assignTo          :: (Measured m a, Default a)
                  => SubPath -> FSTree t' m a -> FSTree D m a -> FSTree D m a
assignTo []    c t = setChild c t
assignTo (n:p) c t = let d = fromMaybe (emptyDirectory n def) $ accessSubDirectory n t
                     in setChild (assignTo p c d) t


-- | Given a path to a directory d, a filename n, a function f, and the root of
-- the tree. Update the file (or directory) with name n in dir d by using
-- function f.
--
-- We even allow renaming here, although the FileType should stay the same, i.e.
-- we cannot just replace af ile by a directory or vice versa.
updateAt             :: (Measured m a, Default a)
                     => SubPath -> FileName
                     -> (forall t'. FSTree t' m a -> FSTree t' m a)
                     -> FSTree D m a -> FSTree D m a
updateAt []    n f t = maybe t setChild' mNewChild
  where
    -- mNewChild is the new file or directory we need to insert. If nc
    -- mNewChild :: Maybe (Either File Directory)
    mNewChild = fmap (fmapBoth f f . ($ n)) . M.lookup n . _directoryContents $ t
    -- We need to delete the old item.
    t'  = unsafeDeleteChild n t
    -- And insert the new item (in the tree in which the old item is removed.)
    -- note that setChild takes care of recomputing the measure :)
    setChild' = either (flip setChild t') (flip setChild t')
updateAt (d:p) n f t = setChild (updateAt p n f dd) t
  where
    dd = fromMaybe (emptyDirectory d def) $ accessSubDirectory d t

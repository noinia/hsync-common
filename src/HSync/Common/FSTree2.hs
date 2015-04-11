{-# Language GeneralizedNewtypeDeriving #-}
{-# Language DeriveDataTypeable #-}
{-# Language FunctionalDependencies #-}
{-# Language MultiParamTypeClasses #-}
{-# Language KindSignatures #-}
{-# Language RankNTypes #-}
{-# Language GADTs #-}
{-# Language DataKinds #-}
{-# Language PolyKinds #-}
module HSync.Common.FSTree2 where

import           Control.Applicative
import           Control.Applicative
import           Control.Lens
import           Data.Aeson
import           Data.Aeson.TH
import           Data.Data(Data, Typeable)
import qualified Data.Foldable as F
import qualified Data.Map as M
import           Data.Maybe(mapMaybe)
import           Data.SafeCopy(base, deriveSafeCopy)
import           Data.Semigroup
import qualified Data.Text     as T
import qualified Data.Traversable as Tr
import           HSync.Common.Types


--------------------------------------------------------------------------------

class Semigroup m => Measured m a | a -> m where
  measure :: a -> m



--------------------------------------------------------------------------------


data FSTree (t :: FileType) (m :: *) (a  :: *) where
  File      :: FileName -> m -> a ->                                 FSTree F m a
  Directory :: FileName -> m -> a -> M.Map FileName (FSTree' m a) -> FSTree D m a


data FileType = F | D deriving (Show,Read,Eq,Data,Typeable,Enum,Bounded)


type FSTree' m a = FileName -> Either (FSTree F m a) (FSTree D m a)


lift             :: (Either (c -> a) (c -> b)) -> (c -> Either a b)
lift (Left f) c  = Left $ f c
lift (Right f) c = Right $ f c


unLift           :: Either a b -> (Either (c -> a) (c -> b))
unLift (Left a)  = Left  $ \c -> a
unLift (Right b) = Right $ \c -> b


instance Functor (FSTree t m) where
  fmap = Tr.fmapDefault

instance F.Foldable (FSTree t m) where
  foldMap = Tr.foldMapDefault

-- | Note: Traverse, and Functor etc. do not update the measurements!!!!!!
instance Tr.Traversable (FSTree t m) where
  traverse f (File n m a)          = (File n m) <$> f a
  traverse f (Directory n m a chs) = (\a' chs' -> Directory n m a' chs')
                                     <$> f a
                                     <*> M.traverseWithKey g chs
    where
      -- g       :: FileName -> FSTree' m a -> f (FSTree' m b)
      g n mkF = case mkF n of
                  Left fl -> (\f' n -> Left  $ set fileName n f') <$> Tr.traverse f fl
                  Right d -> (\d' n -> Right $ set fileName n d') <$> Tr.traverse f d


instance (Show m, Show a) => Show (FSTree t m a) where
  show (File n m a) = concat [ "File { _fileName = "
                             , show n
                             , ", _measurement = "
                             , show m
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

instance (Eq m, Eq a) => Eq (FSTree t m a) where
  (File n m a)          == (File n' m' a')           = (n,m,a) == (n',m',a')
  (Directory n m a chs) == (Directory n' m' a' chs') =
    (n,m,a, M.elems $ pack chs) == (n',m',a', M.elems $ pack chs')


pack :: M.Map k (k -> v) -> M.Map k v
pack = M.mapWithKey (\n mkF -> mkF n)




_fileName                     :: FSTree t m a -> FileName
_fileName (File n _ _)        = n
_fileName (Directory n _ _ _) = n

_measurement                     :: FSTree t m a -> m
_measurement (File _ m _)        = m
_measurement (Directory _ m _ _) = m

_fileData                     :: FSTree t m a -> a
_fileData (File _ _ a)        = a
_fileData (Directory _ _ a _) = a

_directoryContents                     :: FSTree D m a -> M.Map FileName (FSTree' m a)
_directoryContents (Directory _ _ _ c) = c



-- files :: FSTree D m a -> M.Map FileName (FileName -> FSTree F m a)
-- files = filesOrDirs left . _directoryContents



left :: Either a b -> Maybe a
left = either Just (const Nothing)

right :: Either a b -> Maybe b
right = either (const Nothing) Just

filesOrDirs   :: Eq k => (Either a b -> Maybe c) -> M.Map k (Either a b) -> M.Map k c
filesOrDirs f = M.fromAscList . mapMaybe f' . M.toAscList
  where
    -- f' :: (k,Either a b)  -> Maybe (k,Maybe c)
    f' (k,e) = (\v -> (k,v)) <$> f e

----------------------------------------

fileName :: Lens' (FSTree t m a) FileName
fileName = lens _fileName set
  where
    set :: FSTree t m a -> FileName -> FSTree t m a
    set (File _ m a) n        = File n m a
    set (Directory _ m a c) n = Directory n m a c





--------------------------------------------------------------------------------
-- * Operations on the root of the Tree


renameTo :: FileName -> FSTree t m a -> FSTree t m a
renameTo = undefined


-- | Delete a file in the current directory
deleteChild :: FileName -> FSTree D m a -> FSTree D m a
deleteChild = undefined

-- | Set/add a child (the first argument) to the current directory.
setChild :: FSTree t' m a -> FSTree D m a -> FSTree D m a
setChild = undefined




-- * Operations as a filesytem


-- | Given the current path, and a new path, move the file under consideration
move :: (Path,FileName) -> (Path,FileName) -> FSTree D m a -> FSTree D m a
move = undefined

-- | Given a path, delete the node with that path. Returns a Maybe since if the
-- path is empty, we remove the (entire) subtree itself.
delete :: Path -> FSTree t m a -> Maybe (FSTree t m a)
delete = undefined

-- | Given a path and a FSTRee, assign that FSTree to the value indicated by
-- the path. All intermediate directories are created if neccesary.
assign :: Path -> FSTree t' m a -> FSTree D m a -> FSTree D m a
assign = undefined


-- * Helper Functions

{-# Language DeriveFunctor #-}
{-# Language FlexibleContexts #-}
module HSync.Common.FileOperations where


import           Control.Lens(lens, Lens)
import           Control.Monad.Free
import qualified Data.Map as M
import           HSync.Common.Types
import           Prelude hiding (readFile, writeFile)


import           Control.Monad.IO.Class
import           System.Directory


data Operation path fContent dContent next =
         ReadFile              path (fContent -> next)
       | ReadDirectory         path ((dContent, [FileName], [FileName]) -> next)
       | WriteFile             path fContent                           next
       | DeleteFile            path                                    next
       | WriteDirectory        path dContent                           next
       | DeleteDirectory       path                                    next
       deriving (Functor)


path :: Lens (Operation p f d n) (Operation p' f d n) p p'
path = lens get set
  where
    get (ReadFile p _)         = p
    get (ReadDirectory p _)    = p
    get (WriteFile p _ _)      = p
    get (DeleteFile p _)       = p
    get (WriteDirectory p _ _) = p
    get (DeleteDirectory p _)  = p

    set (ReadFile _ k)         p = ReadFile p k
    set (ReadDirectory _ k)    p = ReadDirectory p k
    set (WriteFile _ c k)      p = WriteFile p c k
    set (DeleteFile _ k)       p = DeleteFile p k
    set (WriteDirectory _ c k) p = WriteDirectory p c k
    set (DeleteDirectory _ k)  p = DeleteDirectory p k



type FSOperation path fContent dContent = Free (Operation path fContent dContent)


readFile p = liftF (ReadFile p id)

readDirectory p = liftF (ReadDirectory p id)

writeFile p x = liftF (WriteFile p x ())

writeDirectory p x = liftF (WriteDirectory p x ())

deleteFile p = liftF (DeleteFile p ())

deleteDirectory p = liftF (DeleteDirectory p ())

-- Think about creating files/directories. That should somewhow add a dir. to the output right? Or don't we care about that here.


-- | TODO, create a lock before
atomicallyInterpetIO = undefined



-- mapPath     :: (path -> f path') -> FSOperation path fC dC a -> f (FSOperation path' fC dC a)
-- mapPath mkP (ReadFile p k) = (\p' -> ReadFile p' k) <$> mkP p
-- mapPath mkP (ReadDirectory p k) = (\p' -> ReadDirectory p k
-- mapPath mkP (WriteFile p c k) = (\p' -> WriteFile p c k
-- mapPath mkP (DeleteFile p k) =  (\p' -> DeleteFile p k
-- mapPath mkP (WriteDirectory p () k) = (\p' -> WriteDirectory p () k
-- mapPath mkP (DeleteDirectory p k) = (\p' -> DeleteDirectory p k



type SS a = (Source m a, Sink m a)

interpretIO          :: MonadIO m => FSOperation FilePath (SS m ByteString) () a -> m a
interpretIO (Pure a) = return a
interpretIO (Free op) = case op of
    ReadFile p k          -> undefined
    ReadDirectory p k     -> do
                               all <- liftIO $ getDirectoryContents p
                               let files = [] -- TODO
                                   subDirectories = [] -- TODO
                               interpretIO $ k ((), files, subDirectories)
    WriteFile p c k       -> let src = fst c
                             in do src $$ sinkFile p
                                   interpretIO k
    DeleteFile p k        -> do
                               liftIO $ removeFile p
                               interpretIO k
    WriteDirectory p () k -> interpretIO k -- nothing to do here. Maybe create if not exists
    DeleteDirectory p k   -> do
                               liftIO $ removeDirectoryRecursive p
                               interpretIO k


-- interpretVFS :: FSOperation SubPath content content a -> StateT (FSTree D m content) m a

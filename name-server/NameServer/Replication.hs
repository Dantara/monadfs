{-# LANGUAGE TupleSections #-}

module NameServer.Replication where

import           Control.Concurrent
import           Control.Concurrent.STM.TVar
import           Control.Monad
import           Control.Monad.STM
import           Data.Bifunctor
import           Data.Containers.ListUtils     (nubOrd)
import           Data.Either
import           Data.Map                      (Map)
import qualified Data.Map.Strict               as Map
import           Data.Set                      (isSubsetOf)
import qualified Data.Set                      as Set
import           MonadFS.API.Types
import           MonadFS.FileTree
import           NameServer.Client.Requests
import           NameServer.Server.Models.Root
import           NameServer.Types


data FileForReplication = FileForReplication {
    filePath :: FilePath
  , fileNode :: FileNode
  , takeFrom :: ServerAddr
  , sendTo   :: [ServerAddr]
  } deriving (Show)


runReplication :: AppState -> Int -> IO ()
runReplication state sec = forever $ do
  let mng = globalManager state
  let addrs = ssAddrs state
  let tServers = avaliableSSs state
  let tTree = fileTree state
  let r = amountOfReplicas state

  tree <- readTVarIO tTree
  servers <- proceedRequestM mng $ lookupSSs addrs

  let (replicas, addrsForUpdate, tree') = findFilesToReplicate tree r servers

  atomically $ writeTVar tServers servers
  atomically $ writeTVar tTree tree'

  trees <- proceedRequestM mng $ fetchStorageTrees addrsForUpdate

  let treesCommands = zip addrsForUpdate
        $ concat
        $ uncurry fixTree
        <$> zip (repeat tree') trees

  proceedRequestM mng
    $ runFixCommands (treesCommands <> replicasToCommands replicas)

  threadDelay (sec * 1000000)


replicasToCommands :: [FileForReplication] -> [(ServerAddr, FixCommand)]
replicasToCommands = foldr
  (\r -> (<>) ((, LoadMissingFile
                 $ LoadFile (filePath r) (takeFrom r)) <$> sendTo r)) []


findFilesToReplicate :: VFS -> Int -> [StorageServer]
  -> ([FileForReplication], [ServerAddr], VFS)
findFilesToReplicate tree r servers = let go' = go tree (DirPath "/") in
  ( fst go'
  , nubOrd $ concat $ sendTo <$> fst go'
  , snd go'
  )
  where
    go :: VFS -> DirPath -> ([FileForReplication], VFS)
    go tree' path = (snd (updateDirs path) <> snd updateFiles
                    , FileTree (fst (updateDirs path)) (fst updateFiles))
        where
          updateFiles :: (Map FileName FileInfo, [FileForReplication])
          updateFiles = first (\x -> Map.fromList (nodeToFile <$> x))
            $ splitNodeAndReplication
            $ removeMaybe
                $ (\(name, info) ->
                     checkFile (FileNode name info) (fullPath path name) r servers)
                        <$> Map.toList (files tree')

          fullPath (DirPath d) (FileName f) = d <> f

          removeMaybe :: [Either (Maybe a) b] -> [Either a b]
          removeMaybe []                   = []
          removeMaybe ((Left (Just x)):xs) = Left x : removeMaybe xs
          removeMaybe ((Left Nothing):xs)  = removeMaybe xs
          removeMaybe ((Right x):xs)       = Right x : removeMaybe xs

          splitNodeAndReplication :: [Either FileForReplication FileNode]
            -> ([FileNode], [FileForReplication])
          splitNodeAndReplication list = (rights' <> (fileNode <$> lefts'), lefts')
            where
              lefts' = (\(Left x) -> x) <$> filter isLeft list
              rights' = (\(Right x) -> x) <$> filter isRight list

          nodeToFile (FileNode name info) = (name, info)

          updateDirs :: DirPath -> (Map DirName VFS, [FileForReplication])
          updateDirs (DirPath path') = bimap Map.fromList concat
            $ unzip
            $ (\(dm@(DirName n), t) ->
                 let go' = go t (DirPath $ path' <> n <> "/") in
                   ((dm, snd go'), fst go'))
                        <$> Map.toList (directories tree')


-- If no alive storage servers left for a given file,
-- file will be removed.
checkFile :: FileNode -> FilePath -> Int -> [StorageServer]
  -> Either (Maybe FileForReplication) FileNode
checkFile fn@(FileNode name (FileInfo size addrs)) path r servers
  | addrsSet `isSubsetOf` serversSet && Set.size addrsSet == r = Right fn
  | Set.null stillAlive = Left Nothing
  | otherwise = Left
    $ Just
    $ FileForReplication path newFileNode from to
  where
    addrsSet = Set.fromList addrs
    serversSet = Set.fromList (ssAddr <$> servers)
    stillAlive = Set.intersection addrsSet serversSet

    from = Set.elemAt 0 stillAlive
    to = takeNOrLess (r - Set.size stillAlive)
      (Set.toList (Set.difference serversSet stillAlive))
    newAddrs = Set.toList stillAlive <> to
    newFileNode = FileNode name (FileInfo size newAddrs)

    takeNOrLess _ []     = []
    takeNOrLess 0 _      = []
    takeNOrLess n (x:xs) = x : takeNOrLess (n - 1) xs



fixTree :: FileTree a -> FileTree b -> [FixCommand]
fixTree ref tar = go ref tar (DirPath "/")
  where
    go :: FileTree a -> FileTree b -> DirPath -> [FixCommand]
    go ref' tar' (DirPath path) = fixFiles <> fixMissingDirs <> fixRedundantDirs <> recFix
      where
        fixFiles = (\(FileName name) -> RemoveFile $ path <> name)
          <$> Set.toList
                (Set.difference
                        (Map.keysSet $ files tar') (Map.keysSet $ files ref'))

        fixMissingDirs = (\(DirName name) -> CreateDir $ DirPath $ path <> name)
          <$> Set.toList (Set.difference (Map.keysSet refDirs) (Map.keysSet tarDirs))

        fixRedundantDirs = (\(DirName name) -> CreateDir $ DirPath $ path <> name)
          <$> Set.toList (Set.difference (Map.keysSet tarDirs) (Map.keysSet refDirs))

        recFix :: [FixCommand]
        recFix = concat
          $ (\(dm@(DirName name), t) ->
               go t (maybeToVFS $ Map.lookup dm tarDirs) (DirPath $ path <> name <> "/"))
          <$> Map.toList refDirs
          where
            maybeToVFS (Just x) = x
            maybeToVFS Nothing  = initVFS

        tarDirs = directories tar'
        refDirs = directories ref'

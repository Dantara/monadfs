{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module MonadFS.API.Types where

import           Data.Aeson
import           Data.Bifunctor
import           Data.Bits               (shiftL, shiftR)
import           Data.ByteString.Lazy    (ByteString)
import qualified Data.ByteString.Lazy    as BS
import           Data.Int                (Int64)
import           Data.String             (fromString)
import           Data.Text               (Text)
import qualified Data.Text               as T
import qualified Data.Text.Lazy.Encoding as T (decodeUtf8')
import           Data.Word               (Word16, Word8)
import           GHC.Generics
import           Path
import           Servant

data FileStatus a
  = FileOk a
  | FileError FileError
  deriving (Eq, Show, Generic, FromJSON, ToJSON)


data FileError
  = FileExists
  | FileDoesNotExist
  | IncorrectFilePath
  | SystemFileError SystemError
  | CustomFileError Text
  deriving (Eq, Show, Generic, FromJSON, ToJSON)


data DirStatus a
  = DirOk a
  | DirError DirError
  deriving (Eq, Show, Generic, FromJSON, ToJSON)


data DirError
  = DirExists
  | DirDoesNotExist
  | IncorrectDirPath
  | SystemDirError SystemError
  | CustomDirError Text
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

data StorageServerStatus
  = StorageServerOk Size
  | StorageServerError Text
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

data SystemStatus
  = SystemOk Size
  | SystemError SystemError
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

newtype Size = Size Integer
  deriving (Eq, Ord, Show, Generic, FromJSON, ToJSON)


data SystemError
  = NoStorageServersAvaliable
  | CustomSystemError Text
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

data ServerAddr = ServerAddr String Int
  deriving (Eq, Ord, Show, Generic, FromJSON, ToJSON)


data FileInfo = FileInfo Size [ServerAddr]
  deriving (Eq, Show, Generic, FromJSON, ToJSON)


newtype AbsDirPath = AbsDirPath (Path Abs Dir)
  deriving (Eq, Show, Generic, FromJSON, ToJSON)


newtype AbsFilePath = AbsFilePath (Path Abs File)
  deriving (Eq, Show, Generic, FromJSON, ToJSON)


instance ToHttpApiData AbsDirPath where
  toUrlPiece (AbsDirPath p) = T.pack $ fromAbsDir p

instance FromHttpApiData AbsDirPath where
  parseUrlPiece t = either (Left . T.pack . show) (Right . AbsDirPath)
    $ parseAbsDir
    $ T.unpack t


instance ToHttpApiData AbsFilePath where
  toUrlPiece (AbsFilePath p) = T.pack $ fromAbsFile p

instance FromHttpApiData AbsFilePath where
  parseUrlPiece t = either (Left . T.pack . show) (Right . AbsFilePath)
    $ parseAbsFile
    $ T.unpack t


newtype FileName = FileName String
  deriving (Eq, Ord, Show, Generic, FromJSON,
            ToJSON, FromJSONKey, ToJSONKey)


newtype DirInfo = DirInfo [FileName]
  deriving (Eq, Show, Generic, FromJSON, ToJSON)


data NewFile = NewFile AbsFilePath Size
  deriving (Eq, Show, Generic, FromJSON, ToJSON)


data LoadFile = LoadFile AbsFilePath ServerAddr
  deriving (Eq, Show, Generic, FromJSON, ToJSON)


data SourceDest = SourceDest AbsFilePath AbsFilePath
  deriving (Eq, Show, Generic, FromJSON, ToJSON)


newtype FileContent = FileContent ByteString

data FileWrite = FileWrite AbsFilePath FileContent


instance MimeRender OctetStream FileContent where
  mimeRender _ (FileContent c) = c

instance MimeUnrender OctetStream FileContent where
  mimeUnrender _ s = Right $ FileContent s


maxPathLength :: Int64
maxPathLength = 4096

instance MimeRender OctetStream FileWrite where
  mimeRender ctype (FileWrite p c) = encodedPathLength <> encodedPath <> encodedContent
    where
      encodedPath :: ByteString
      encodedPath = fromString $ show p

      encodedPathLength = BS.pack
        $ w16ToW8
        $ fromIntegral
        $ BS.length encodedPath

      encodedContent = mimeRender ctype c

      w16ToW8 :: Word16 -> [Word8]
      w16ToW8 w = [fromIntegral $ shiftR w 8, fromIntegral $ shiftR (shiftL w 8) 8]


instance MimeUnrender OctetStream FileWrite where
  mimeUnrender _ s = either Left (\p -> Right $ FileWrite (AbsFilePath p) content)
    $ eitherPath >>= pathParser
    where
      (pathLength, encodedPayload) = first (fromIntegral . w8ToInt . BS.unpack)
        $ BS.splitAt 2 s

      (encodedPath, encodedContent) = BS.splitAt pathLength encodedPayload

      content = FileContent encodedContent
      eitherPath = bimap show show $ T.decodeUtf8' encodedPath
      pathParser = first show . parseAbsFile

      w8ToInt :: [Word8] -> Int
      w8ToInt = foldl (\a x -> shiftL a 8 + fromIntegral x) 0

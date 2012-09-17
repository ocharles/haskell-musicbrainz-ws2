{-# LANGUAGE OverloadedStrings #-}
module MusicBrainz.Web.Handler.Release (release) where

import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as Text

import Control.Applicative
import Control.Monad (join)
import Data.Either (partitionEithers)
import Data.Maybe (fromMaybe)
import Data.Text (Text, intercalate)
import Data.Text.Encoding (decodeUtf8)
import Data.Traversable
import Snap.Core
import Snap.Snaplet

import MusicBrainz.Data.Country ()
import MusicBrainz.Data.GetByRef
import MusicBrainz.Data.Language ()
import MusicBrainz.Data.Release
import MusicBrainz.Data.ReleaseStatus ()
import MusicBrainz.Data.Script ()
import MusicBrainz.Types
import MusicBrainz.Web.Snaplet
import qualified MusicBrainz.XML as XML

data ReleaseInc = Media

release :: Handler MusicBrainz MusicBrainz ()
release = do
  mbid' <- join . fmap (parseMbid . BS.unpack) <$> getParam "mbid"
  case mbid' of
    Nothing -> invalidMbid
    Just mbid -> do
      inc <- parseIncParameters . decodeUtf8 . fromMaybe "" <$> getParam "inc"
      case inc of
        Left invalid -> invalidIncParameters invalid
        Right _ -> do
          release <- with database (findRelease mbid)
          case release of
            Nothing -> notFound
            Just r ->
              XML.render . XML.release =<< (with database $ do
                status <- traverse getByRef (releaseStatus . entityData $ r)
                language <- traverse getByRef (releaseLanguage . entityData $ r)
                script <- traverse getByRef (releaseScript . entityData $ r)
                country <- traverse getByRef (releaseCountry . entityData $ r)
                return (r, status, language, script, country))
  where
    invalidMbid = XML.renderError "Invalid MBID."
    notFound = XML.renderError "Not Found"
    invalidIncParameters i = XML.renderError $ Text.append
      "The following inc parameters are not valid for this request: "
      (Text.intercalate ", " i)
    parseIncParameters = distributeEither . map parseIncParameter . Text.splitOn " "
    parseIncParameter i = case i of "media" -> Right Media
                                    inc -> Left inc
    distributeEither e = let (invalid, valid) = partitionEithers e
                         in if null invalid then Right valid else Left invalid

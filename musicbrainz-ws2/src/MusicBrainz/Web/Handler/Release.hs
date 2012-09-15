{-# LANGUAGE OverloadedStrings #-}
module MusicBrainz.Web.Handler.Release (release) where

import qualified Data.ByteString.Char8 as BS

import Control.Monad (join)
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

release :: Handler MusicBrainz MusicBrainz ()
release = do
  Just mbid <- fmap (join . fmap (parseMbid . BS.unpack)) (getParam "mbid")
  XML.render . XML.release =<< (with database $ do
    Just r <- findRelease mbid
    status <- traverse getByRef (releaseStatus . entityData $ r)
    language <- traverse getByRef (releaseLanguage . entityData $ r)
    script <- traverse getByRef (releaseScript . entityData $ r)
    country <- traverse getByRef (releaseCountry . entityData $ r)
    return (r, status, language, script, country))

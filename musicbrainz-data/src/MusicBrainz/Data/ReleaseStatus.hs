{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module MusicBrainz.Data.ReleaseStatus where

import Control.Applicative
import Database.PostgreSQL.Simple.SqlQQ
import Snap.Snaplet.PostgresqlSimple

import MusicBrainz.Data.GetByRef
import MusicBrainz.Data.Mapping ()
import MusicBrainz.Types

instance GetByRef ReleaseStatus where
  getByRef (ReleaseStatusRef cid) = head <$> query q (Only cid)
    where q = [sql| SELECT id, name FROM release_status WHERE id = ? |]

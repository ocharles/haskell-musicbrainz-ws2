{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module MusicBrainz.Data.Language where

import Control.Applicative
import Database.PostgreSQL.Simple.SqlQQ
import Snap.Snaplet.PostgresqlSimple

import MusicBrainz.Data.GetByRef
import MusicBrainz.Data.Mapping ()
import MusicBrainz.Types

instance GetByRef Language where
  getByRef (LanguageRef cid) = head <$> query q (Only cid)
    where q = [sql| SELECT id, name, iso_code_3 FROM language WHERE id = ? |]

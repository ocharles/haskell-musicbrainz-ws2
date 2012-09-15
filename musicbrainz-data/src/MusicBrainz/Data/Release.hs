{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module MusicBrainz.Data.Release where

import Control.Applicative
import Data.Maybe (listToMaybe)
import Database.PostgreSQL.Simple.SqlQQ
import Snap.Snaplet.PostgresqlSimple

import MusicBrainz.Internal
import MusicBrainz.Data.Mapping ()
import MusicBrainz.Types

findRelease :: (Functor m, HasPostgres m)
            => MBID Release -> m (Maybe (Entity Release))
findRelease mbid = listToMaybe <$> query q (Only mbid)
  where q = [sql| SELECT release.id, name.name, release.artist_credit,
                      release.release_group, release.packaging, release.country,
                      release.status, release.date_year, release.date_month,
                      release.date_day, release.barcode, release.language,
                      release.script, release.quality
                    FROM release
                    JOIN release_name name ON name.id = release.name
                    WHERE release.gid = ? |]

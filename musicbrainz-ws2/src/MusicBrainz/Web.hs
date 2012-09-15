{-# LANGUAGE OverloadedStrings #-}
module MusicBrainz.Web ( musicBrainzWs2 ) where

import Snap.Snaplet (SnapletInit, makeSnaplet, nestSnaplet, addRoutes)
import Snap.Snaplet.PostgresqlSimple (pgsInit)

import MusicBrainz.Web.Handler.Release
import MusicBrainz.Web.Snaplet

musicBrainzWs2 :: SnapletInit MusicBrainz MusicBrainz
musicBrainzWs2 = makeSnaplet "musicbrainz" "MusicBrainz WS 2" Nothing $ do
    dbSnaplet <- nestSnaplet "db" database pgsInit
    addRoutes [ ("/ws/2/release/:mbid", release) ]
    return $ makeMbSnaplet dbSnaplet

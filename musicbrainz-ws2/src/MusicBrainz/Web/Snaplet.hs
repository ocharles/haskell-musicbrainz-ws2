{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module MusicBrainz.Web.Snaplet where

import Control.Monad.State.Class (get)
import Data.Lens.Template (makeLenses)
import Snap.Snaplet (Snaplet, Handler, with)
import Snap.Snaplet.PostgresqlSimple (Postgres, HasPostgres(..))

data MusicBrainz = MusicBrainz { _database :: Snaplet Postgres }

$( makeLenses [''MusicBrainz] )

instance HasPostgres (Handler b MusicBrainz) where
  getPostgresState = with database get

makeMbSnaplet :: Snaplet Postgres
              -> MusicBrainz
makeMbSnaplet = MusicBrainz

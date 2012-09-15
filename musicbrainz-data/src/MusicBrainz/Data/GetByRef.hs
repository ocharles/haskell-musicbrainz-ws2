module MusicBrainz.Data.GetByRef where

import Snap.Snaplet.PostgresqlSimple

import MusicBrainz.Types

class GetByRef a where
  getByRef :: (Functor m, HasPostgres m) => Ref a -> m (Entity a)

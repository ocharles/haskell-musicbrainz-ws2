{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module MusicBrainz.Internal where

import Data.UUID (UUID, fromString)

newtype MBID a = MBID { unMbid :: UUID } deriving (Eq)

instance Show (MBID a) where
  show (MBID uuid) = show uuid

data family Ref a :: *

data Entity a = Entity { entityRef :: Ref a
                       , entityData :: a
                       }

deriving instance (Eq (Ref a), Eq a) => Eq (Entity a)
deriving instance (Show (Ref a), Show a) => Show (Entity a)

parseMbid :: String -> Maybe (MBID a)
parseMbid = fmap MBID . fromString

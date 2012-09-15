{-# LANGUAGE FlexibleInstances #-}
module MusicBrainz.Data.Mapping (parsePartialDate) where

import Blaze.ByteString.Builder.Char8 (fromString)
import Control.Applicative
import Data.Maybe (isNothing)
import Data.UUID (toString)
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.Ok
import Database.PostgreSQL.Simple.ToField

import MusicBrainz.Internal
import MusicBrainz.Types

--------------------------------------------------------------------------------
-- FromField Serializations

instance FromField (Ref ArtistCredit) where
  fromField f v = ArtistCreditRef <$> fromField f v


instance FromField Barcode where
  fromField f v = do
    b <- fromField f v
    Ok $ case b of
      "" -> NoBarcode
      s -> Barcode s

instance FromField (Ref Country) where
  fromField f v = CountryRef <$> fromField f v

instance FromField (Ref Language) where
  fromField f v = LanguageRef <$> fromField f v

instance FromField (Ref Packaging) where
  fromField f v = PackagingRef <$> fromField f v

instance FromField Quality where
  fromField f v = do
    q <- fromField f v :: Ok Int
    case q of
      -1 -> Ok Normal
      0 -> Ok Low
      1 -> Ok Normal
      2 -> Ok High
      _ -> returnError ConversionFailed f "Unknown quality"

instance FromField (Ref ReleaseGroup) where
  fromField f v = ReleaseGroupRef <$> fromField f v

instance FromField (Ref ReleaseStatus) where
  fromField f v = ReleaseStatusRef <$> fromField f v

instance FromField (Ref Script) where
  fromField f v = ScriptRef <$> fromField f v

--------------------------------------------------------------------------------
-- ToField Serializations
instance ToField (MBID a) where
  toField (MBID mbid) = Plain $ inQuotes (fromString $ toString mbid)

--------------------------------------------------------------------------------
-- FromRow

instance FromRow (Entity Country) where
  fromRow = do
    id' <- field
    c <- Country <$> field <*> field
    return $ Entity { entityRef = CountryRef id'
                    , entityData = c
                    }


instance FromRow (Entity Language) where
  fromRow = do
    id' <- field
    l <- Language <$> field <*> field
    return $ Entity { entityRef = LanguageRef id'
                    , entityData = l
                    }

instance FromRow (Entity ReleaseStatus) where
  fromRow = do
    id' <- field
    rs <- ReleaseStatus <$> field
    return $ Entity { entityRef = ReleaseStatusRef id'
                    , entityData = rs
                    }

instance FromRow (Entity Release) where
  fromRow = do
    mbid <- field
    release <- Release <$> field <*> field <*> field <*> field
                       <*> field <*> field <*> parsePartialDate
                       <*> field <*> field <*> field <*> field
    return $ Entity { entityRef = ReleaseRef mbid
                    , entityData = release
                    }

instance FromRow (Entity Script) where
  fromRow = do
    id' <- field
    s <- Script <$> field <*> field
    return $ Entity { entityRef = ScriptRef id'
                    , entityData = s
                    }

--------------------------------------------------------------------------------
-- Utilities
parsePartialDate :: RowParser (Maybe PartialDate)
parsePartialDate = do
  year <- field
  month <- field
  day <- field
  return $ if all isNothing [year, month, day]
    then Nothing
    else Just $ PartialDate year month day

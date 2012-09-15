{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module MusicBrainz.Types
  ( MBID, parseMbid
  , Entity, entityData, entityRef
  , Ref (..)

  , Alias (..)
  , Artist (..)
  , ArtistCredit (..)
  , ArtistCreditName (..)
  , ArtistType (..)
  , Barcode (..)
  , Country (..)
  , Gender (..)
  , Label (..)
  , LabelType (..)
  , Language (..)
  , Medium (..)
  , MediumFormat (..)
  , Packaging (..)
  , PartialDate (..)
  , Quality (..)
  , Recording (..)
  , Release (..)
  , ReleaseGroup (..)
  , ReleaseGroupType (..)
  , ReleaseStatus (..)
  , Script (..)
  , Tracklist (..)
  , Track (..)
  , Work (..)
  , WorkType (..)
  ) where

import Data.List (intercalate)
import Data.Maybe (fromJust, isJust)
import Data.Text (Text)
import Data.Typeable

import MusicBrainz.Internal

data instance Ref (Alias Artist) = ArtistAliasRef Int deriving (Eq, Show)
data instance Ref Artist = ArtistRef Int deriving (Eq, Show)
data instance Ref ArtistCredit = ArtistCreditRef Int deriving (Eq, Show)
data instance Ref ArtistType = ArtistTypeRef Int deriving (Eq, Show)
data instance Ref Country = CountryRef Int deriving (Eq, Show)
data instance Ref Gender = GenderRef Int deriving (Eq, Show)
data instance Ref Language = LanguageRef Int deriving (Eq, Show)
data instance Ref Packaging = PackagingRef Int deriving (Eq, Show)
data instance Ref Recording = RecordingRef Int deriving (Eq, Show)
data instance Ref Release = ReleaseRef Int deriving (Eq, Show)
data instance Ref ReleaseGroup = ReleaseGroupRef Int deriving (Eq, Show)
data instance Ref ReleaseGroupType = ReleaseGroupTypeRef Int deriving (Eq, Show)
data instance Ref ReleaseStatus = ReleaseStatusRef Int deriving (Eq, Show)
data instance Ref Script = ScriptRef Int deriving (Eq, Show)

data Alias a = Alias { aliasName :: Text
                     , aliasParent :: Ref a
                     }

deriving instance (Eq (Ref a), Eq a) => Eq (Alias a)
deriving instance (Show (Ref a), Show a) => Show (Alias a)

data PartialDate = PartialDate (Maybe Int) (Maybe Int) (Maybe Int) deriving (Eq)

instance Show PartialDate where
  show (PartialDate y m d) =
    intercalate "-" $ map (show . fromJust)  $ takeWhile isJust [y, m, d]

data Gender = Gender { genderName :: Text } deriving (Eq, Show)

data Country = Country { countryName :: Text
                       , countryIsoCode :: Text
                       }

data ArtistType = ArtistType { artistTypeName :: Text }

data LabelType = LabelType { labelTypeName :: Text}

data Artist = Artist { artistName :: Text
                     , artistSortName :: Text
                     , artistBeginDate :: Maybe PartialDate
                     , artistEndDate :: Maybe PartialDate
                     , artistGender :: Maybe (Ref Gender)
                     , artistCountry :: Maybe (Ref Country)
                     , artistType :: Maybe (Ref ArtistType)
                     , artistComment :: Maybe Text
                     } deriving (Eq, Show)

data ArtistCredit = ArtistCredit { artistCredits :: [ArtistCreditName] }

data ArtistCreditName = ArtistCreditName { artistCreditArtist :: Ref Artist
                                         , artistCreditName :: Text
                                         , artistCreditSuffix :: Text
                                         }

data Barcode = NoBarcode | Barcode String

data Label = Label { labelName :: Text
                   , labelSortName :: Text
                   , labelBeginDate :: Maybe PartialDate
                   , labelEndDate :: Maybe PartialDate
                   , labelType :: Maybe (Ref LabelType)
                   }

data Language = Language { languageName :: Text
                         , languageIsoCode3 :: Text
                         }

data Medium = Medium { mediumName :: Maybe Text
                     , mediumFormat :: Maybe (Ref MediumFormat)
                     , mediumTracklist :: Ref Tracklist
                     }

data MediumFormat = MediumFormat { mediumFormatName :: Text }

data Packaging = Packaging { packagingName :: Text }

data Quality = Low | Normal | High deriving (Typeable)

data Recording = Recording { recordingName :: Text
                           , recordingArtistCredit :: Ref ArtistCredit
                           , recordingLength :: Maybe Int
                           }

data Release = Release { releaseName :: Text
                       , releaseArtistCredit :: Ref ArtistCredit
                       , releaseReleaseGroup :: Ref ReleaseGroup
                       , releasePackaging :: Maybe (Ref Packaging)
                       , releaseCountry :: Maybe (Ref Country)
                       , releaseStatus :: Maybe (Ref ReleaseStatus)
                       , releaseDate :: Maybe PartialDate
                       , releaseBarcode :: Maybe Barcode
                       , releaseLanguage :: Maybe (Ref Language)
                       , releaseScript :: Maybe (Ref Script)
                       , releaseQuality :: Quality
                       }

data ReleaseGroup = ReleaseGroup { releaseGroupName :: Text
                                 , releaseGroupArtistCredit :: Ref ArtistCredit
                                 , releaseGroupType :: Maybe (Ref ReleaseGroupType)
                                 }

data ReleaseStatus = ReleaseStatus { releaseStatusName :: Text }

data ReleaseGroupType = ReleaseGroupType { releaseGroupTypeName :: Text }

data Script = Script { scriptName :: Text
                     , scriptIsoCode :: Text
                     }

data Track = Track { trackNumber :: Int
                   , trackName :: Text
                   , trackLength :: Maybe Int
                   , trackArtistCredit :: Ref ArtistCredit
                   , trackRecording :: Ref Recording
                   }

data Tracklist = Tracklist { tracklistTracks :: [Track] }

data Work = Work { workName :: Text
                 , workType :: Ref WorkType
                 }

data WorkType = WorkType { workTypeName :: Text }

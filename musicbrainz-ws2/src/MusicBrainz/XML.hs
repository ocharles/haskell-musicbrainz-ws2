{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
module MusicBrainz.XML (release, render) where

import Control.Monad.Trans.Writer
import Data.Map (empty)
import Data.Text (Text, pack)
import Snap.Core (MonadSnap, writeLazyText)
import Text.XML

import MusicBrainz.Types

release :: ( Entity Release
           , Maybe (Entity ReleaseStatus)
           , Maybe (Entity Language)
           , Maybe (Entity Script)
           , Maybe (Entity Country)
           ) -> Writer [Node] ()
release (r, status, language, script, country) = do
  element "release" $ do
    element' "title" (releaseName . entityData $ r)
    optionalElement "status" status (releaseStatusName . entityData)
    quality (releaseQuality . entityData $ r)
    textRepresentation (language, script)
    optionalElement "date" (releaseDate . entityData $ r) (pack . show)
    optionalElement "country" country (countryIsoCode . entityData)
    optionalElement "barcode" (releaseBarcode . entityData $ r) formatBarcode

formatBarcode :: Barcode -> Text
formatBarcode NoBarcode = ""
formatBarcode (Barcode b) = pack b

mbName :: Text -> Name
mbName n = Name { nameLocalName = n
                , nameNamespace = Just "http://musicbrainz.org/ns/mmd-2.0#"
                , namePrefix = Nothing
                }

element' :: Text -> Text -> Writer [Node] ()
element' n v = tell [ NodeElement $ Element (mbName n) empty [NodeContent v] ]

element :: Text -> Writer [Node] a -> Writer [Node] ()
element n w = tell [ NodeElement $ Element (mbName n) empty $ execWriter w ]

optionalElement :: Text -> Maybe a -> (a -> Text) -> Writer [Node] ()
optionalElement _ Nothing _ = return ()
optionalElement n (Just e) l = element' n (l e)

quality :: Quality -> Writer [Node] ()
quality q = element' "quality" $ case q of
  Low -> "low"
  Normal -> "normal"
  High -> "high"

textRepresentation :: (Maybe (Entity Language), Maybe (Entity Script)) -> Writer [Node] ()
textRepresentation (Nothing, Nothing) = return ()
textRepresentation (l, s) =
  element "text-representation" $ do
    optionalElement "language" l (languageIsoCode3 . entityData)
    optionalElement "script" s (scriptIsoCode . entityData)

render :: MonadSnap m => Writer [Node] a -> m ()
render = writeLazyText . renderText def . wrapMetadata . execWriter
  where wrapMetadata nodes =
          Document (Prologue [] Nothing [])
                   (Element (mbName "metadata") empty nodes)
                   []

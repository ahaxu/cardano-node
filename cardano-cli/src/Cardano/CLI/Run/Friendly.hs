{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | User-friendly pretty-printing for textual user interfaces (TUI)
module Cardano.CLI.Run.Friendly (friendlyTxBodyBS) where

import           Cardano.Prelude
import qualified Prelude

import           Data.Aeson (Object, ToJSON, Value (..), object, toJSON, (.=))
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Map.Strict as Map
import           Data.Yaml (array)
import           Data.Yaml.Pretty (defConfig, encodePretty, setConfCompare)

import           Cardano.Api as Api (AddressInEra (..),
                   AddressTypeInEra (ByronAddressInAnyEra, ShelleyAddressInEra),
                   CardanoEra (ShelleyEra), IsCardanoEra (cardanoEra),
                   ShelleyBasedEra (ShelleyBasedEraAllegra, ShelleyBasedEraMary, ShelleyBasedEraShelley),
                   ShelleyEra, TxAuxScripts (..), TxBody, TxBodyContent (..), TxCertificates (..),
                   TxFee (..), TxMetadata (..), TxMetadataInEra (..), TxMetadataValue (..),
                   TxMintValue (..), TxOut (..), TxOutValue (..), TxUpdateProposal (..),
                   TxValidityLowerBound (..), TxValidityUpperBound (..), TxWithdrawals (..),
                   auxScriptsSupportedInEra, certificatesSupportedInEra, displayError,
                   getTransactionBodyContent, multiAssetSupportedInEra, serialiseAddress,
                   serialiseAddressForTxOut, txMetadataSupportedInEra, updateProposalSupportedInEra,
                   validityLowerBoundSupportedInEra, validityUpperBoundSupportedInEra,
                   withdrawalsSupportedInEra)
import           Cardano.Api.Byron (Lovelace (..))
import           Cardano.Api.Shelley (Address (ShelleyAddress), StakeAddress (..),
                   TxBody (ShelleyTxBody), fromShelleyAddr, fromShelleyStakeAddr)
import qualified Shelley.Spec.Ledger.API as Shelley

import           Cardano.CLI.Helpers (textShow)

friendlyTxBodyBS
  :: IsCardanoEra era => Api.TxBody era -> Either Prelude.String ByteString
friendlyTxBodyBS =
  fmap (encodePretty $ setConfCompare compare defConfig) . friendlyTxBody

friendlyTxBody
  :: forall era
  .  IsCardanoEra era => Api.TxBody era -> Either Prelude.String Value
friendlyTxBody txbody =
  case getTransactionBodyContent txbody of
    Right
      TxBodyContent
        { txIns
        , txOuts
        , txFee
        , txValidityRange = txValidityRange@(_, upperBound)
        , txMetadata
        , txAuxScripts
        , txWithdrawals
        , txCertificates
        , txUpdateProposal
        , txMintValue
        } ->
      Right $
      object
        $   [ "era"     .= era
            , "fee"     .= friendlyFee txFee
            , "inputs"  .= txIns
            , "outputs" .= map friendlyTxOut txOuts
            ]
        ++  [ "auxiliary scripts" .= friendlyAuxScripts txAuxScripts
            | Just _ <- [auxScriptsSupportedInEra era]
            ]
        ++  [ "certificates" .= friendlyCertificates txCertificates
            | Just _ <- [certificatesSupportedInEra era]
            ]
        ++  [ "metadata" .= friendlyMetadata txMetadata
            | Just _ <- [txMetadataSupportedInEra era]
            ]
        ++  [ "mint" .= friendlyMintValue txMintValue
            | Right _ <- [multiAssetSupportedInEra era]
            ]
        ++  [ "update proposal" .= friendlyUpdateProposal txUpdateProposal
            | Just _ <- [updateProposalSupportedInEra era]
            ]
        ++  case era of
              ShelleyEra -> ["time to live" .= ttl] where
                TxValidityUpperBound _ ttl = upperBound
              _ ->
                case
                  ( validityLowerBoundSupportedInEra era
                  , validityUpperBoundSupportedInEra era
                  )
                of
                  (Nothing, Nothing) -> []
                  _ ->
                    ["validity range" .= friendlyValidityRange txValidityRange]
        ++  [ "withdrawals" .= friendlyWithdrawals txWithdrawals
            | Just _ <- [withdrawalsSupportedInEra era]
            ]
    Left err -> Left $ displayError err
  where
    era = cardanoEra @era

friendlyValidityRange
  :: forall era
  .  IsCardanoEra era
  => (TxValidityLowerBound era, TxValidityUpperBound era)
  -> Value
friendlyValidityRange (lowerBound, upperBound) =
  object $
    [ "lower bound" .=
        case lowerBound of
          TxValidityNoLowerBound   -> Null
          TxValidityLowerBound _ s -> toJSON s
    | Just _ <- [validityLowerBoundSupportedInEra $ cardanoEra @era]
    ]
    ++
    [ "upper bound" .=
        case upperBound of
          TxValidityNoUpperBound _ -> Null
          TxValidityUpperBound _ s -> toJSON s
    | Just _ <- [validityUpperBoundSupportedInEra $ cardanoEra @era]
    ]

friendlyWithdrawals :: TxWithdrawals era -> Value
friendlyWithdrawals TxWithdrawalsNone = Null
friendlyWithdrawals (TxWithdrawals _ withdrawals) =
  array
    [ object
        [ "address"     .= serialiseAddress addr
        , "network"     .= net
        , "credential"  .= cred
        , "amount"      .= friendlyLovelace amount
        ]
    | (addr@(StakeAddress net cred), amount) <- withdrawals
    ]

friendlyTxOut :: TxOut era -> Value
friendlyTxOut (TxOut addr amount) =
  case addr of
    AddressInEra ByronAddressInAnyEra _ ->
      object $ ("address era" .= String "Byron") : common
    AddressInEra (ShelleyAddressInEra _) (ShelleyAddress net cred stake) ->
      object $
        "address era"         .= String "Shelley"             :
        "network"             .= net                          :
        "payment credential"  .= cred                         :
        "stake reference"     .= friendlyStakeReference stake :
        common
  where
    common :: [(Text, Value)]
    common =
      [ "address" .= serialiseAddressForTxOut addr
      , "amount"  .= friendlyTxOutValue amount
      ]

friendlyStakeReference :: Shelley.StakeReference crypto -> Value
friendlyStakeReference = \case
  Shelley.StakeRefBase cred -> toJSON cred
  Shelley.StakeRefNull -> Null
  Shelley.StakeRefPtr ptr -> toJSON ptr

assertObject :: HasCallStack => Value -> Object
assertObject = \case
  Object obj -> obj
  val -> panic $ "expected JSON Object, but got " <> typ
    where
      typ =
        case val of
          Array{}  -> "an Array"
          Bool{}   -> "a Boolean"
          Null     -> "Null"
          Number{} -> "a Number"
          Object{} -> "an Object"
          String{} -> "a String"

friendlyUpdateProposal :: TxUpdateProposal era -> Value
friendlyUpdateProposal = \case
  TxUpdateProposalNone -> Null
  TxUpdateProposal _ p -> String $ textShow p

friendlyCertificates :: TxCertificates era -> Value
friendlyCertificates = \case
  TxCertificatesNone  -> Null
  TxCertificates _ cs -> toJSON $ map textShow cs

friendlyFee :: TxFee era -> Value
friendlyFee = \case
  TxFeeImplicit _     -> "implicit"
  TxFeeExplicit _ fee -> friendlyLovelace fee

friendlyLovelace :: Lovelace -> Value
friendlyLovelace (Lovelace value) = String $ textShow value <> " Lovelace"

friendlyMintValue :: TxMintValue era -> Value
friendlyMintValue = \case
  TxMintNone      -> Null
  TxMintValue _ v -> toJSON v

friendlyTxOutValue :: TxOutValue era -> Value
friendlyTxOutValue = \case
  TxOutAdaOnly _ lovelace -> friendlyLovelace lovelace
  TxOutValue _ multiasset -> toJSON multiasset

friendlyMetadata :: TxMetadataInEra era -> Value
friendlyMetadata = \case
  TxMetadataNone                   -> Null
  TxMetadataInEra _ (TxMetadata m) -> toJSON $ friendlyMetadataValue <$> m

friendlyMetadataValue :: TxMetadataValue -> Value
friendlyMetadataValue = \case
  TxMetaNumber int  -> toJSON int
  -- TxMetaBytes
  TxMetaText   text -> toJSON text

friendlyAuxScripts :: TxAuxScripts era -> Value
friendlyAuxScripts = \case
  TxAuxScriptsNone -> Null

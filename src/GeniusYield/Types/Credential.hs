{-|
Module      : GeniusYield.Types.Credential
Copyright   : (c) 2023 GYELD GMBH
License     : Apache 2.0
Maintainer  : support@geniusyield.co
Stability   : develop

-}
module GeniusYield.Types.Credential (
    GYPaymentCredential (..)
  , paymentCredentialToApi
  , paymentCredentialFromApi
  , paymentCredentialToPlutus
  , paymentCredentialToHexText
  , paymentCredentialToBech32
  ) where


import qualified Cardano.Api                  as Api
import           Data.Hashable                (Hashable (..))
import           Data.Text                    (Text)
import           GeniusYield.Types.PubKeyHash (GYPubKeyHash, pubKeyHashFromApi,
                                               pubKeyHashToApi,
                                               pubKeyHashToPlutus)
import           GeniusYield.Types.Script     (GYValidatorHash,
                                               validatorHashFromApi,
                                               validatorHashToApi,
                                               validatorHashToPlutus)
import           GeniusYield.Utils            (serialiseToBech32WithPrefix)
import qualified PlutusLedgerApi.V1           as Plutus (Credential (..))
import qualified Text.Printf                  as Printf

-- | Payment credential.
data GYPaymentCredential
       = GYPaymentCredentialByKey !GYPubKeyHash
       | GYPaymentCredentialByScript !GYValidatorHash
    deriving (Show, Eq, Ord)

instance Printf.PrintfArg GYPaymentCredential where
  formatArg (GYPaymentCredentialByKey pkh) = Printf.formatArg $ "Payment key credential: " <> Api.serialiseToRawBytesHexText (pubKeyHashToApi pkh)
  formatArg (GYPaymentCredentialByScript sh) = Printf.formatArg $ "Payment script credential: " <> Api.serialiseToRawBytesHexText (validatorHashToApi sh)

instance Hashable GYPaymentCredential where
    hashWithSalt salt cred = hashWithSalt salt $ paymentCredentialToHexText cred

-- | Convert @GY@ type to corresponding type in @cardano-node@ library.
paymentCredentialToApi :: GYPaymentCredential -> Api.PaymentCredential
paymentCredentialToApi (GYPaymentCredentialByKey pkh) = Api.PaymentCredentialByKey (pubKeyHashToApi pkh)
paymentCredentialToApi (GYPaymentCredentialByScript sh) = Api.PaymentCredentialByScript (validatorHashToApi sh)

-- | Get @GY@ type from corresponding type in @cardano-node@ library.
paymentCredentialFromApi :: Api.PaymentCredential -> GYPaymentCredential
paymentCredentialFromApi (Api.PaymentCredentialByKey pkh) = GYPaymentCredentialByKey (pubKeyHashFromApi pkh)
paymentCredentialFromApi (Api.PaymentCredentialByScript sh) = GYPaymentCredentialByScript (validatorHashFromApi sh)

-- | Convert @GY@ type to corresponding type in @plutus@ library.
paymentCredentialToPlutus :: GYPaymentCredential -> Plutus.Credential
paymentCredentialToPlutus (GYPaymentCredentialByKey pkh) = Plutus.PubKeyCredential (pubKeyHashToPlutus pkh)
paymentCredentialToPlutus (GYPaymentCredentialByScript sh) = Plutus.ScriptCredential (validatorHashToPlutus sh)

-- | Get hexadecimal value of payment credential.
paymentCredentialToHexText :: GYPaymentCredential -> Text
paymentCredentialToHexText =
  \case
    GYPaymentCredentialByKey pkh -> Api.serialiseToRawBytesHexText (pubKeyHashToApi pkh)
    GYPaymentCredentialByScript sh -> Api.serialiseToRawBytesHexText (validatorHashToApi sh)

-- | Get the bech32 encoding for the given credential.
paymentCredentialToBech32 :: GYPaymentCredential -> Text
paymentCredentialToBech32 (GYPaymentCredentialByKey pkh) = serialiseToBech32WithPrefix "addr_vkh" $ pubKeyHashToApi pkh
paymentCredentialToBech32 (GYPaymentCredentialByScript sh) = serialiseToBech32WithPrefix "addr_shared_vkh" $ validatorHashToApi sh


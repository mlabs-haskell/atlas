{-# LANGUAGE LambdaCase #-}
-- Test to signify correct functionality of reference inputs implementation.
-- TODO: Atlas currently doesn't support referring to the uninlined datum of reference input. But if that support is added, tests can be written utilising it here.
{-|
Module      : GeniusYield.Test.RefInput
Copyright   : (c) 2023 GYELD GMBH
License     : Apache 2.0
Maintainer  : support@geniusyield.com
Stability   : develop

-}
module GeniusYield.Test.RefInput
    ( refInputTests
    ) where

import           Test.Tasty                                           (TestTree,
                                                                       testGroup)

import           GeniusYield.Imports
import           GeniusYield.Test.Clb
import           GeniusYield.Test.Clb.Utils
import           GeniusYield.Test.GYTxBody                            (mockTxId)
import           GeniusYield.Test.OnChain.GuessRefInputDatum.Compiled
import           GeniusYield.Test.Utils
import           GeniusYield.TxBuilder
import           GeniusYield.Types
import GeniusYield.HTTP.Errors (someBackendError)
import qualified Data.Text as T
import GeniusYield.Test.Privnet.Setup
import Test.Tasty.HUnit (testCaseSteps)
import GeniusYield.Test.Privnet.Ctx

gyGuessRefInputDatumValidator :: GYValidator 'PlutusV2
gyGuessRefInputDatumValidator = validatorFromPlutus guessRefInputDatumValidator

refInputTests :: Setup -> TestTree
refInputTests setup = testGroup "Reference Input"
    [ mkTestFor "Inlined datum" $ refInputTrace True 5 5
    , mkTestFor "Inlined datum - Wrong guess" $ mustFail . refInputTrace True 5 4
    , mkTestFor "Reference input must not be consumed" tryRefInputConsume
    , testCaseSteps "Inlined datum - privnet" $ \info -> withSetup setup info $ \ctx -> do
        runGYPrivnet ctx (ctxUserF ctx) . refInputTrace True 5 5 $ ctxUsers ctx
    ]

guessRefInputRun :: GYTestMonad m => GYTxOutRef -> GYTxOutRef -> Integer -> m ()
guessRefInputRun refInputORef consumeRef guess = do
  let redeemer = Guess guess
      skeleton :: GYTxSkeleton 'PlutusV2 =
        mustHaveInput GYTxIn
          { gyTxInTxOutRef = consumeRef
          , gyTxInWitness  = GYTxInWitnessScript
              (GYInScript gyGuessRefInputDatumValidator)
              (datumFromPlutusData ())
              (redeemerFromPlutusData redeemer)
          } <>
        mustHaveRefInput refInputORef
  void $ sendSkeleton skeleton

refInputTrace :: GYTestMonad m => Bool -> Integer -> Integer -> Wallets -> m ()
refInputTrace toInline actual guess Wallets{..} = do
  let myGuess :: Integer = guess
      outValue :: GYValue = valueFromLovelace 20_000_000
  mMOref <- runWallet w1 $ addRefInput toInline (walletAddress w9) (datumFromPlutusData (RefInputDatum actual))
  case mMOref of
    Nothing -> throwAppError $ someBackendError "Unable to create utxo to reference"
    Just refInputORef ->
      void $ runWallet w1 $ withWalletBalancesCheckSimple [w1 := valueFromLovelace 0] $ do
        -- liftClb $ logInfo $ printf "Reference input ORef %s" refInputORef
        addr <- scriptAddress gyGuessRefInputDatumValidator
        (tx, txId) <- sendSkeleton (mustHaveOutput $ mkGYTxOut addr outValue (datumFromPlutusData ()))
        let mOrefIndices = findLockedUtxosInBody addr tx
        orefIndices <- maybe (throwAppError $ someBackendError "Unable to get GYAddress from some Plutus.Address in txBody") return mOrefIndices
        oref        <- case fmap (txOutRefFromApiTxIdIx (txIdToApi txId) . wordToApiIx) orefIndices of
          [oref']        -> return oref'
          _non_singleton -> throwAppError $ someBackendError "expected exactly one reference"
        -- liftClb $ logInfo $ printf "Locked ORef %s" oref
        guessRefInputRun refInputORef oref myGuess

tryRefInputConsume :: GYTestMonad m => Wallets -> m ()
tryRefInputConsume Wallets{..} = do
  -- Approach: Create a new output with 60% of total ada. Mark this UTxO as reference input and try sending this same 60%, or any amount greater than 40% of this original balance. Since coin balancer can't consume this UTxO, it won't be able to build for it.
  void $ runWallet w1 $ do
    walletBalance <- balance w1
    let walletLovelaceBalance = fst $ valueSplitAda walletBalance
        lovelaceToSend = (walletLovelaceBalance `div` 10) * 6  -- send 60% of total ada
        lovelaceToSendValue = valueFromLovelace lovelaceToSend
    (tx, txId) <- sendSkeleton (mustHaveOutput $ mkGYTxOutNoDatum (walletAddress w1) lovelaceToSendValue)
    bodyUtxos <- utxosInBody tx txId
    let bodyUtxos' = catMaybes bodyUtxos
    unless (length bodyUtxos == length bodyUtxos') $ throwAppError . someBackendError . T.pack $ printf "Shouldn't happen: Not all UTxOs reflected, originally %s but got %s and they are %s" (show $ length bodyUtxos) (show $ length bodyUtxos') (show bodyUtxos')
    desiredOutputRef <- case utxoRef <$> find (\GYUTxO{ utxoValue } -> utxoValue == lovelaceToSendValue) bodyUtxos' of
          Nothing  -> throwAppError $ someBackendError "Shouldn't happen: Couldn't find the desired UTxO"
          Just ref -> pure ref
    (snd <$> sendSkeleton (mustHaveRefInput @'PlutusV2 desiredOutputRef <> mustHaveOutput  (mkGYTxOutNoDatum (walletAddress w1) lovelaceToSendValue)))
      `catchError` (
        \case
          GYApplicationException e -> do
            -- liftClb $ logInfo $ printf "Successfully caught expected exception %s" (show e)
            pure mockTxId
          e -> throwAppError . someBackendError . T.pack $ printf "Unexpected exception %s" (show e)
        )

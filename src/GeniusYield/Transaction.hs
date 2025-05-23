{- |
Module      : GeniusYield.Transaction
Description : Tools to build balanced transactions
Copyright   : (c) 2023 GYELD GMBH
License     : Apache 2.0
Maintainer  : support@geniusyield.co
Stability   : develop

Balancing algorithm.

Inputs:

    * Transaction inputs
    * Transaction outputs
    * Transaction minted value

Additionally:

    * Set of additional UTxOs which can be spent
    * Collateral UTxO
    * Change address

The algorithm should produce sets of inputs and outputs
such the total value is @input + minted = outputs@.

The algorithms used to select inputs & produce change outputs are defined in 'GeniusYield.Transaction.CoinSelection'.

Each output should be big enough
(contain enough ADA, 'Api.calculateMinimumUTxO').
Algorithm may adjust them to include additional value.

There are also transaction fees which should also be taken into account.
We over-approximate the fees, and let 'Api.makeTransactionBodyAutoBalance' balance fees.
(We can be more precise here, and only slightly over-approximate,
 but @cardano-api@ doesn't provide a handy helpers to fill in execution units).

We make the algorithm iterative over the fee over-approximation. In particular, we start off
with a small over-approximation, and if tx finalization fails, we increase it. The very first
success is returned. Any over approximation (above the actual required fees), leads to generation of change output (besides those generated by coin selection) by `Api.makeTransactionBodyAutoBalance`. This new change output may fail minimum ada requirement, in which case we iterate with increased fee approximate.

Collateral input is needed when scripts are executed,
i.e. transaction mints tokens or consumes script outputs.

See 'Api.evaluateTransactionBalance' and 'Api.makeTransactionBodyAutoBalance'
(this function balances ADA only and doesn't add inputs, i.e. it calculates the ADA change).
-}
module GeniusYield.Transaction (
  -- * Top level build interface
  GYBuildTxEnv (..),
  buildUnsignedTxBody,
  GYBuildTxError (..),
  GYCoinSelectionStrategy (..),

  -- * Balancing only
  balanceTxStep,
  finalizeGYBalancedTx,
  GYBalancingError (..),

  -- * Utility type
  GYTxInDetailed (..),
) where

import Cardano.Api qualified as Api
import Cardano.Api.Ledger qualified as Ledger
import Cardano.Api.Shelley qualified as Api
import Cardano.Api.Shelley qualified as Api.S
import Cardano.Crypto.DSIGN (
  sizeSigDSIGN,
  sizeVerKeyDSIGN,
 )
import Cardano.Ledger.Alonzo.PParams qualified as Ledger
import Cardano.Ledger.Alonzo.Scripts qualified as AlonzoScripts
import Cardano.Ledger.Alonzo.Tx qualified as AlonzoTx
import Cardano.Ledger.Binary qualified as CBOR
import Cardano.Ledger.Binary.Crypto qualified as CBOR
import Cardano.Ledger.Conway.PParams qualified as Ledger
import Cardano.Ledger.Core (EraTx (sizeTxF), eraProtVerLow)
import Cardano.Ledger.Core qualified as Ledger
import Cardano.Ledger.Keys (DSIGN)
import Cardano.Ledger.Keys.WitVKey (WitVKey (..))
import Cardano.Ledger.Shelley.API.Wallet qualified as Shelley
import Cardano.Slotting.Time (SystemStart)
import Control.Arrow ((&&&))
import Control.Lens (view, (^.))
import Control.Monad.Random (MonadRandom)
import Control.Monad.Trans.Except (runExceptT, throwE)
import Data.Bifunctor qualified
import Data.Bifunctor qualified as Data.Binfunctor
import Data.ByteString.Lazy qualified as LBS
import Data.Foldable (
  Foldable (foldMap'),
  for_,
 )
import Data.List (delete)
import Data.Map qualified as Map
import Data.Ratio ((%))
import Data.Semigroup (Sum (..))
import Data.Set qualified as Set
import GHC.IsList (IsList (..))
import GeniusYield.Imports hiding (toList)
import GeniusYield.Transaction.CBOR
import GeniusYield.Transaction.CoinSelection
import GeniusYield.Transaction.Common
import GeniusYield.Types
import GeniusYield.Types.TxCert.Internal

-- | A container for various network parameters, and user wallet information, used by balancer.
data GYBuildTxEnv v = GYBuildTxEnv
  { gyBTxEnvSystemStart :: !SystemStart
  , gyBTxEnvEraHistory :: !Api.EraHistory
  , gyBTxEnvProtocolParams :: !ApiProtocolParameters
  , gyBTxEnvPools :: !(Set Api.S.PoolId)
  , gyBTxEnvOwnUtxos :: !GYUTxOs
  -- ^ own utxos available for use as _additional_ input
  , gyBTxEnvChangeAddr :: !GYAddress
  , gyBTxEnvCollateral :: !GYUTxO
  , gyBTxEnvExtraConfiguration :: !(GYTxExtraConfiguration v)
  }

-------------------------------------------------------------------------------
-- Top level wrappers around core balancing logic
-------------------------------------------------------------------------------

{- | This is the lovelace overshoot we start with; the balancer will try with bigger amounts if this one fails.

The overshoot is not only to cover fees, but also to cover min deposits for change output(s).
-}
extraLovelaceStart :: Natural
extraLovelaceStart = 1_000_000

{- | This is the extra lovelace ceiling, after which - random improve algo will no longer be tried.

Due to the way RandomImprove works, depending on wallet state - it may not be computationally efficient to use
it when the extraLovelace param has built up a lot. Falling back to largest first may be a better choice so as to not
time out.
-}
randImproveExtraLovelaceCeil :: Natural
randImproveExtraLovelaceCeil = 20_000_000

-- | Pure interface to build the transaction body given necessary information.
buildUnsignedTxBody ::
  forall m v.
  (HasCallStack, MonadRandom m) =>
  GYBuildTxEnv v ->
  GYCoinSelectionStrategy ->
  [GYTxInDetailed v] ->
  [GYTxOut v] ->
  -- | reference inputs
  GYUTxOs ->
  -- | minted values
  Maybe (GYValue, [(GYBuildScript v, GYRedeemer)]) ->
  -- | withdrawals
  [GYTxWdrl v] ->
  -- | certificates
  [GYTxCert v] ->
  Maybe GYSlot ->
  Maybe GYSlot ->
  Set GYPubKeyHash ->
  Maybe GYTxMetadata ->
  GYTxVotingProcedures v ->
  [(GYProposalProcedurePB, GYTxBuildWitness v)] ->
  Natural ->
  m (Either GYBuildTxError GYTxBody)
buildUnsignedTxBody env cstrat insOld outsOld refIns mmint wdrls certs lb ub signers mbTxMetadata vps pps donation = buildTxLoop cstrat extraLovelaceStart
 where
  certsFinalised = finaliseTxCert (gyBTxEnvProtocolParams env) <$> certs

  step :: GYCoinSelectionStrategy -> Natural -> m (Either GYBuildTxError ([GYTxInDetailed v], GYUTxOs, [GYTxOut v]))
  step stepStrat = fmap (first GYBuildTxBalancingError) . balanceTxStep env mmint wdrls certsFinalised vps pps donation insOld outsOld stepStrat

  buildTxLoop :: GYCoinSelectionStrategy -> Natural -> m (Either GYBuildTxError GYTxBody)
  buildTxLoop stepStrat n
    -- Stop trying with RandomImprove if extra lovelace has hit the pre-determined ceiling.
    | stepStrat /= GYLargestFirstMultiAsset && n >= randImproveExtraLovelaceCeil = buildTxLoop GYLargestFirstMultiAsset n
    | otherwise = do
        res <- f stepStrat n
        case res of
          {- These errors generally indicate the input selection process selected less ada
          than necessary. Try again with double the extra lovelace amount -}
          Left (GYBuildTxBodyErrorAutoBalance Api.TxBodyErrorAdaBalanceNegative {}) -> buildTxLoop stepStrat (n * 2)
          Left (GYBuildTxBodyErrorAutoBalance Api.TxBodyErrorAdaBalanceTooSmall {}) -> buildTxLoop stepStrat (n * 2)
          -- @RandomImprove@ may result into many change outputs, where their minimum ada requirements might be unsatisfiable with available ada.
          Left (GYBuildTxBalancingError err@(GYBalancingErrorChangeShortFall _)) ->
            retryIfRandomImprove
              stepStrat
              n
              (GYBuildTxBalancingError err)
          {- RandomImprove may end up selecting too many inputs to fit in the transaction.
          In this case, try with LargestFirst and dial back the extraLovelace param.
          -}
          Left (GYBuildTxExUnitsTooBig maxUnits currentUnits) ->
            retryIfRandomImprove
              stepStrat
              n
              (GYBuildTxExUnitsTooBig maxUnits currentUnits)
          Left (GYBuildTxSizeTooBig maxPossibleSize currentSize) ->
            retryIfRandomImprove
              stepStrat
              n
              (GYBuildTxSizeTooBig maxPossibleSize currentSize)
          Right x -> pure $ Right x
          {- The most common error here would be:
          - InsufficientFunds
          - Script validation failure
          - Tx not within validity range specified timeframe
          - If fee UTxO is provided, then it's insufficient to cover for fees and ADA of subsequent change output.

          No need to try again for these.
          -}
          other -> pure other

  f :: GYCoinSelectionStrategy -> Natural -> m (Either GYBuildTxError GYTxBody)
  f stepStrat pessimisticFee = do
    stepRes <- step stepStrat pessimisticFee
    pure $
      stepRes >>= \(ins, collaterals, outs) ->
        finalizeGYBalancedTx
          env
          GYBalancedTx
            { gybtxIns = ins
            , gybtxCollaterals = collaterals
            , gybtxOuts = outs
            , gybtxMint = mmint
            , gybtxWdrls = wdrls
            , gybtxCerts = certsFinalised
            , gybtxInvalidBefore = lb
            , gybtxInvalidAfter = ub
            , gybtxSigners = signers
            , gybtxRefIns = refIns
            , gybtxMetadata = mbTxMetadata
            , gybtxVotingProcedures = vps
            , gybtxProposalProcedures = pps
            , gybtxDonation = donation
            }
          (length outsOld)

  retryIfRandomImprove GYRandomImproveMultiAsset n _ = buildTxLoop GYLargestFirstMultiAsset (if n == extraLovelaceStart then extraLovelaceStart else n `div` 2)
  retryIfRandomImprove _ _ err = pure $ Left err

-------------------------------------------------------------------------------
-- Primary balancing logic
-------------------------------------------------------------------------------

{- | An independent "step" of the balancing algorithm.

This step is meant to be run with different 'extraLovelace' values. If the 'extraLovelace' amount
is too small, there will not be enough ada to pay for the final fees + min deposits, when finalizing
the tx with 'finalizeGYBalancedTx'. If such is the case, 'balanceTxStep' should be called again with a higher
'extraLovelace' amount.
-}
balanceTxStep ::
  (HasCallStack, MonadRandom m) =>
  GYBuildTxEnv v ->
  -- | minting
  Maybe (GYValue, [(GYBuildScript v, GYRedeemer)]) ->
  -- | withdrawals
  [GYTxWdrl v] ->
  -- | certificates
  [GYTxCert' v] ->
  -- | voting procedures
  GYTxVotingProcedures v ->
  -- | proposal procedures
  [(GYProposalProcedurePB, GYTxBuildWitness v)] ->
  -- | donation
  Natural ->
  -- | transaction inputs
  [GYTxInDetailed v] ->
  -- | transaction outputs
  [GYTxOut v] ->
  -- | Coin selection strategy to use
  GYCoinSelectionStrategy ->
  -- | extra lovelace to look for on top of output value
  Natural ->
  m (Either GYBalancingError ([GYTxInDetailed v], GYUTxOs, [GYTxOut v]))
balanceTxStep
  GYBuildTxEnv
    { gyBTxEnvProtocolParams = pp
    , gyBTxEnvOwnUtxos = ownUtxos
    , gyBTxEnvChangeAddr = changeAddr
    , gyBTxEnvCollateral = collateral
    , gyBTxEnvPools = pools
    , gyBTxEnvExtraConfiguration = ec
    }
  mmint
  wdrls
  certs
  vps
  pps
  donation
  ins
  outs
  cstrat =
    let adjustedOuts = map (adjustTxOut (minimumUTxO pp)) outs
        valueMint = maybe mempty fst mmint
        needsCollateral = valueMint /= mempty || any (isScriptWitness . gyTxInWitness . gyTxInDet) ins || any (isCertScriptWitness . gyTxCertWitness') certs || any (isPlutusScriptWitness . gyTxWdrlWitness) wdrls || any (isPlutusScriptWitness . fst) (Map.elems vps) || any (isPlutusScriptWitness . snd) pps
        (stakeCredDeregsAmt :: Natural, stakeCredRegsAmt :: Natural) =
          foldl'
            ( \acc@(!accDeregsAmt, !accRegsAmt) (gyTxCertCertificate' -> cert) -> case cert of
                GYStakeAddressDeregistrationCertificate amt _ -> (accDeregsAmt + amt, accRegsAmt)
                GYStakeAddressRegistrationCertificate amt _ -> (accDeregsAmt, accRegsAmt + amt)
                GYStakeAddressRegistrationDelegationCertificate amt _ _ -> (accDeregsAmt, accRegsAmt + amt)
                _ -> acc
            )
            (0, 0)
            certs
        (drepDeregsAmt :: Natural, drepRegsAmt :: Natural) =
          foldl'
            ( \acc@(!accDeregsAmt, !accRegsAmt) (gyTxCertCertificate' -> cert) -> case cert of
                GYDRepRegistrationCertificate amt _ _ -> (accDeregsAmt, accRegsAmt + amt)
                GYDRepUnregistrationCertificate _ amt -> (accDeregsAmt + amt, accRegsAmt)
                _ -> acc
            )
            (0, 0)
            certs
        ppDeposit :: Natural = pp ^. Ledger.ppPoolDepositL & fromIntegral
        spRegsAmt :: Natural =
          foldl'
            ( \(!accRegsAmt) (gyTxCertCertificate' -> cert) -> case cert of
                GYStakePoolRegistrationCertificate poolParams -> (if Set.member (stakePoolIdToApi (poolId poolParams)) pools then accRegsAmt else accRegsAmt + ppDeposit)
                -- Retirement does not add ADA source.
                _ -> accRegsAmt
            )
            0
            certs
        govActionDeposit :: Natural = pp ^. Ledger.ppGovActionDepositL & fromIntegral
        govActionsAmt :: Natural = fromIntegral (length pps) * govActionDeposit
        -- Extra ada is received from withdrawals and stake credential deregistration.
        adaSource =
          let wdrlsAda = getSum $ foldMap' (coerce . gyTxWdrlAmount) wdrls
           in wdrlsAda + stakeCredDeregsAmt + drepDeregsAmt
        -- Ada lost due to stake credential registration, etc.
        adaSink = stakeCredRegsAmt + drepRegsAmt + spRegsAmt + govActionsAmt + donation
        collaterals
          | needsCollateral = utxosFromUTxO collateral
          | otherwise = mempty
     in \extraLovelace -> runExceptT $ do
          for_ adjustedOuts $ \txOut ->
            unless (valueNonNegative $ gyTxOutValue txOut)
              . throwE
              $ GYBalancingErrorNonPositiveTxOut txOut
          (addIns, changeOuts) <-
            selectInputs
              GYCoinSelectionEnv
                { existingInputs = ins
                , requiredOutputs = (gyTxOutAddress &&& gyTxOutValue) <$> adjustedOuts
                , mintValue = valueMint
                , changeAddr = changeAddr
                , ownUtxos = ownUtxos
                , extraLovelace =
                    case gytxecFeeUtxo ec of
                      Nothing -> extraLovelace
                      Just _feeUtxo -> 0 -- We add for fee UTxO later and all the required fees must come from it.
                , minimumUTxOF =
                    fromInteger
                      . flip valueAssetClass GYLovelace
                      . gyTxOutValue
                      . adjustTxOut (minimumUTxO pp)
                , maxValueSize = pp ^. Ledger.ppMaxValSizeL
                , adaSource = adaSource
                , adaSink = adaSink
                , inputMapper = gytxecUtxoInputMapper ec
                }
              cstrat
          pure (ins ++ addIns, collaterals, adjustedOuts ++ changeOuts)
   where
    isScriptWitness GYTxInWitnessKey = False
    isScriptWitness GYTxInWitnessScript {} = True
    isScriptWitness GYTxInWitnessSimpleScript {} = False -- Simple (native) scripts don't require collateral.
    isCertScriptWitness (Just p) = isPlutusScriptWitness p
    isCertScriptWitness Nothing = False

    isPlutusScriptWitness GYTxBuildWitnessPlutusScript {} = True
    isPlutusScriptWitness _ = False

retColSup :: Api.BabbageEraOnwards ApiEra
retColSup = Api.BabbageEraOnwardsConway

finalizeGYBalancedTx :: GYBuildTxEnv v -> GYBalancedTx v -> Int -> Either GYBuildTxError GYTxBody
finalizeGYBalancedTx
  GYBuildTxEnv
    { gyBTxEnvSystemStart = ss
    , gyBTxEnvEraHistory = eh
    , gyBTxEnvProtocolParams = pp
    , gyBTxEnvPools = ps
    , gyBTxEnvChangeAddr = changeAddr
    , gyBTxEnvExtraConfiguration = ec
    }
  GYBalancedTx
    { gybtxIns = insBeforeAddingFeeUtxo
    , gybtxCollaterals = collaterals
    , gybtxOuts = outs
    , gybtxMint = mmint
    , gybtxWdrls = wdrls
    , gybtxCerts = certs
    , gybtxInvalidBefore = lb
    , gybtxInvalidAfter = ub
    , gybtxSigners = signers
    , gybtxRefIns = utxosRefInputs
    , gybtxMetadata = mbTxMetadata
    , gybtxVotingProcedures = vps
    , gybtxProposalProcedures = pps
    , gybtxDonation = donation
    }
  numSkeletonOuts = do
    bc <-
      gytxecPostBodyContentMapper ec
        <$> makeTransactionBodyAutoBalanceWrapper
          collaterals
          ss
          eh
          pp
          ps
          (utxosToApi utxos)
          (gytxecPreBodyContentMapper ec body)
          (maybe changeAddr utxoAddress $ gytxecFeeUtxo ec)
          unregisteredStakeCredsMap
          unregisteredDRepCredsMap
          estimateKeyWitnesses
          numSkeletonOuts
          (isJust $ gytxecFeeUtxo ec)
    b <- first (GYBuildTxBodyErrorAutoBalance . Api.TxBodyError) $ Api.createTransactionBody Api.ShelleyBasedEraConway bc
    first GYBuildTxCborSimplificationError $ simplifyGYTxBodyCbor (txBodyFromApi b)
   where
    -- Over-estimate the number of key witnesses required for the transaction.
    -- We do not provide support for byron key witnesses in our estimate as @Api.makeTransactionBodyAutoBalance@ does not consider them, i.e., count of key witnesses returned here are considered as shelley key witnesses by cardano api.
    estimateKeyWitnesses :: Word =
      fromIntegral $
        countUnique $
          mapMaybe (extractPaymentPkhFromAddress . utxoAddress) (utxosToList collaterals)
            <> [apkh | GYTxWdrl {gyTxWdrlWitness = GYTxBuildWitnessKey, gyTxWdrlStakeAddress = saddr} <- wdrls, let sc = stakeAddressToCredential saddr, Just apkh <- [preferCByKey sc]]
            <> [apkh | cert@GYTxCert' {gyTxCertWitness' = Just GYTxBuildWitnessKey} <- certs, let sc = certificateToStakeCredential $ gyTxCertCertificate' cert, Just apkh <- [preferCByKey sc]]
            <> [apkh | (a, GYTxBuildWitnessKey) <- Data.Bifunctor.second fst <$> Map.toList vps, Just apkh <- [voterToPKH a]]
            <> [apkh | (a, GYTxBuildWitnessKey) <- pps, Just apkh <- [propProcToPKH a]]
            <> estimateKeyWitnessesFromInputs ins
            <> Set.toList signers
     where
      extractPaymentPkhFromAddress gyaddr =
        addressToPaymentCredential gyaddr >>= \case
          GYPaymentCredentialByKey pkh -> Just $ toPubKeyHash pkh
          GYPaymentCredentialByScript _ -> Nothing

      preferCByKey (GYCredentialByKey pkh) = Just $ toPubKeyHash pkh
      preferCByKey _otherwise = Nothing

      voterToPKH (CommitteeVoter c) = preferCByKey c
      voterToPKH (DRepVoter c) = preferCByKey c
      voterToPKH (StakePoolVoter kh) = Just $ toPubKeyHash kh

      propProcToPKH GYProposalProcedurePB {propProcPBReturnAddr} = stakeAddressToCredential propProcPBReturnAddr & preferCByKey

      countUnique :: Ord a => [a] -> Int
      countUnique = Set.size . Set.fromList

      estimateKeyWitnessesFromInputs txInDets =
        -- Count key witnesses.
        [apkh | txInDet@GYTxInDetailed {gyTxInDet = GYTxIn {gyTxInWitness = GYTxInWitnessKey}} <- txInDets, let gyaddr = gyTxInDetAddress txInDet, Just apkh <- [extractPaymentPkhFromAddress gyaddr]]
          ++
          -- Estimate key witnesses required by native scripts.
          map toPubKeyHash (Set.toList $ foldl' estimateKeyWitnessesFromNativeScripts mempty txInDets)
       where
        estimateKeyWitnessesFromNativeScripts acc (gyTxInWitness . gyTxInDet -> GYTxInWitnessSimpleScript gyInSS) =
          case gyInSS of
            GYBuildSimpleScriptInlined s -> getTotalKeysInSimpleScript s <> acc
            GYBuildSimpleScriptReference _ s -> getTotalKeysInSimpleScript s <> acc
        estimateKeyWitnessesFromNativeScripts acc _ = acc

    inRefs :: Api.TxInsReference ApiEra
    inRefs = case inRefs' of
      [] -> Api.TxInsReferenceNone
      _ -> Api.TxInsReference Api.BabbageEraOnwardsConway inRefs'

    inRefs' :: [Api.TxIn]
    inRefs' = [txOutRefToApi r | r <- utxosRefs utxosRefInputs]

    ins = insBeforeAddingFeeUtxo <> (case gytxecFeeUtxo ec of Nothing -> mempty; Just feeUtxo -> [utxoToTxInDetailed feeUtxo GYTxInWitnessKey])

    -- utxos for inputs
    utxosIn :: GYUTxOs
    utxosIn = utxosFromList $ utxoFromTxInDetailed <$> ins

    -- Map to lookup information for various utxos.
    utxos :: GYUTxOs
    utxos = utxosIn <> utxosRefInputs <> collaterals

    outs' :: [Api.S.TxOut Api.S.CtxTx ApiEra]
    outs' = txOutToApi <$> outs

    ins' :: [(Api.TxIn, Api.BuildTxWith Api.BuildTx (Api.Witness Api.WitCtxTxIn ApiEra))]
    ins' = [txInToApi (isInlineDatum $ gyTxInDetDatum i) (gyTxInDet i) | i <- ins]

    collaterals' :: Api.TxInsCollateral ApiEra
    collaterals' = case utxosRefs collaterals of
      [] -> Api.TxInsCollateralNone
      orefs -> Api.TxInsCollateral Api.AlonzoEraOnwardsConway $ txOutRefToApi <$> orefs

    -- will be filled by makeTransactionBodyAutoBalance
    fee :: Api.TxFee ApiEra
    fee = Api.TxFeeExplicit Api.ShelleyBasedEraConway $ Ledger.Coin 0

    lb' :: Api.TxValidityLowerBound ApiEra
    lb' =
      maybe
        Api.TxValidityNoLowerBound
        (Api.TxValidityLowerBound Api.AllegraEraOnwardsConway . slotToApi)
        lb

    ub' :: Api.TxValidityUpperBound ApiEra
    ub' = Api.TxValidityUpperBound Api.ShelleyBasedEraConway $ slotToApi <$> ub

    extra :: Api.TxExtraKeyWitnesses ApiEra
    extra = case toList signers of
      [] -> Api.TxExtraKeyWitnessesNone
      pkhs -> Api.TxExtraKeyWitnesses Api.AlonzoEraOnwardsConway $ pubKeyHashToApi <$> pkhs

    mint :: Api.TxMintValue Api.BuildTx ApiEra
    mint = case mmint of
      Nothing -> Api.TxMintNone
      Just (v, xs) ->
        let policyIdWit = Map.fromList [(mintingPolicyIdFromWitness p, (p, r)) | (p, r) <- xs]
            mintVal ::
              Map
                Api.S.PolicyId
                ( Api.S.PolicyAssets
                , Api.S.BuildTxWith
                    Api.S.BuildTx
                    (Api.S.ScriptWitness Api.S.WitCtxMint ApiEra)
                )
            mintVal =
              valueToList v
                & foldl'
                  ( \acc (asc, amt) -> case asc of
                      GYLovelace -> error "absurd: trying to mint ada value"
                      GYToken pid tn -> case Map.lookup pid policyIdWit of
                        Nothing -> error $ "absurd: policy id " <> show pid <> " not found in wit map " <> show policyIdWit
                        Just (p, r) ->
                          Map.insertWith
                            (\(pa1, btw1) (pa2, _) -> (pa1 <> pa2, btw1))
                            (mintingPolicyIdToApi pid)
                            ( Api.PolicyAssets $ Map.singleton (tokenNameToApi tn) (Api.Quantity amt)
                            , Api.BuildTxWith
                                ( case p of
                                    GYBuildPlutusScript s -> gyMintingScriptWitnessToApiPlutusSW s (redeemerToApi r) (Api.ExecutionUnits 0 0)
                                    GYBuildSimpleScript s -> simpleScriptWitnessToApi s
                                )
                            )
                            acc
                  )
                  mempty
         in Api.TxMintValue Api.MaryEraOnwardsConway mintVal

    -- Putting `TxTotalCollateralNone` & `TxReturnCollateralNone` would have them appropriately calculated by `makeTransactionBodyAutoBalance` but then return collateral it generates is only for ada. To support multi-asset collateral input we therefore calculate correct values ourselves and put appropriate entries here to have `makeTransactionBodyAutoBalance` calculate appropriate overestimated fees.
    (dummyTotCol :: Api.TxTotalCollateral ApiEra, dummyRetCol :: Api.TxReturnCollateral Api.CtxTx ApiEra) =
      if mempty == collaterals
        then
          (Api.TxTotalCollateralNone, Api.TxReturnCollateralNone)
        else
          ( -- Total collateral must be <= lovelaces available in collateral inputs.
            Api.TxTotalCollateral retColSup (Ledger.Coin $ fst $ valueSplitAda collateralTotalValue)
          , -- Return collateral must be <= what is in collateral inputs.
            Api.TxReturnCollateral retColSup $ txOutToApi $ GYTxOut changeAddr collateralTotalValue Nothing Nothing
          )
     where
      collateralTotalValue :: GYValue
      collateralTotalValue = foldMapUTxOs utxoValue collaterals

    txMetadata :: Api.TxMetadataInEra ApiEra
    txMetadata = maybe Api.TxMetadataNone toMetaInEra mbTxMetadata
     where
      toMetaInEra :: GYTxMetadata -> Api.TxMetadataInEra ApiEra
      toMetaInEra gymd =
        let md = txMetadataToApi gymd
         in if md == mempty then Api.TxMetadataNone else Api.TxMetadataInEra Api.ShelleyBasedEraConway md

    wdrls' :: Api.TxWithdrawals Api.BuildTx ApiEra
    wdrls' = if wdrls == mempty then Api.TxWithdrawalsNone else Api.TxWithdrawals Api.ShelleyBasedEraConway $ map txWdrlToApi wdrls

    certs' =
      if certs == mempty
        then Api.TxCertificatesNone
        else
          let apiCerts = map (Data.Binfunctor.second pure . txCertToApi) certs
           in Api.TxCertificates Api.ShelleyBasedEraConway (fromList apiCerts)

    unregisteredStakeCredsMap = Map.fromList [(stakeCredentialToApi sc, fromIntegral amt) | GYStakeAddressDeregistrationCertificate amt sc <- map gyTxCertCertificate' certs]

    unregisteredDRepCredsMap = Map.fromList [(credentialToLedger sc, fromIntegral amt) | GYDRepUnregistrationCertificate sc amt <- map gyTxCertCertificate' certs]

    vps' =
      if vps == mempty
        then Nothing
        else
          let vpsApi =
                Api.TxVotingProcedures (votingProceduresToLedger (Map.map snd vps)) $
                  Api.BuildTxWith
                    ( Map.map fst vps
                        -- https://github.com/IntersectMBO/cardano-api/issues/722.
                        & Map.filter
                          ( \case
                              GYTxBuildWitnessKey -> False
                              GYTxBuildWitnessPlutusScript _ _ -> True
                              GYTxBuildWitnessSimpleScript _ -> True
                          )
                        & Map.mapKeys voterToLedger
                        & Map.map unsafeBuildScriptWitnessToApi
                    )
           in Just vpsApi >>= Api.mkFeatured
    pps' =
      if pps == mempty
        then Nothing
        else
          let ppsApi =
                Api.mkTxProposalProcedures
                  ( map
                      ( \(propProc, wit) ->
                          let propProc' = completeProposalProcedure propProc (pp ^. Ledger.ppGovActionDepositL & fromIntegral) & propProcToLedger
                           in ( propProc'
                              , case wit of
                                  GYTxBuildWitnessKey -> Nothing
                                  w@(GYTxBuildWitnessPlutusScript _ _) -> Just $ unsafeBuildScriptWitnessToApi w
                                  w@(GYTxBuildWitnessSimpleScript _) -> Just $ unsafeBuildScriptWitnessToApi w
                              )
                      )
                      pps
                  )
           in Just ppsApi >>= Api.mkFeatured

    body :: Api.TxBodyContent Api.BuildTx ApiEra
    body =
      Api.TxBodyContent
        { Api.txIns = ins'
        , Api.txInsCollateral = collaterals'
        , Api.txInsReference = inRefs
        , Api.txOuts = outs'
        , Api.txTotalCollateral = dummyTotCol
        , Api.txReturnCollateral = dummyRetCol
        , Api.txFee = fee
        , Api.txValidityLowerBound = lb'
        , Api.txValidityUpperBound = ub'
        , Api.txMetadata = txMetadata
        , Api.txAuxScripts = Api.TxAuxScriptsNone
        , Api.txExtraKeyWits = extra
        , Api.txProtocolParams = Api.BuildTxWith $ Just $ Api.S.LedgerProtocolParameters pp
        , Api.txWithdrawals = wdrls'
        , Api.txCertificates = certs'
        , Api.txUpdateProposal = Api.TxUpdateProposalNone
        , Api.txMintValue = mint
        , Api.txScriptValidity = Api.TxScriptValidityNone
        , Api.txProposalProcedures = pps'
        , Api.txVotingProcedures = vps'
        , Api.txCurrentTreasuryValue = Nothing -- FIXME:?
        , Api.txTreasuryDonation = if donation == 0 then Nothing else Api.mkFeatured (fromIntegral donation)
        }

{- | Wraps around 'Api.makeTransactionBodyAutoBalance' just to verify the final ex units and tx size are within limits.

If not checked, the returned txbody may fail during submission.
-}
makeTransactionBodyAutoBalanceWrapper ::
  GYUTxOs ->
  SystemStart ->
  Api.S.EraHistory ->
  ApiProtocolParameters ->
  Set Api.S.PoolId ->
  Api.S.UTxO ApiEra ->
  Api.S.TxBodyContent Api.S.BuildTx ApiEra ->
  GYAddress ->
  Map.Map Api.StakeCredential Ledger.Coin ->
  Map.Map (Ledger.Credential Ledger.DRepRole) Ledger.Coin ->
  Word ->
  Int ->
  -- | Whether we are using a separate UTxO solely for fees.
  Bool ->
  Either GYBuildTxError (Api.S.TxBodyContent Api.S.BuildTx ApiEra)
makeTransactionBodyAutoBalanceWrapper collaterals ss eh pp poolids utxos body changeAddr stakeDelegDeposits drepDelegDeposits nkeys numSkeletonOuts isFeeUtxo = do
  let Ledger.ExUnits
        { exUnitsSteps = maxSteps
        , exUnitsMem = maxMemory
        } = pp ^. Ledger.ppMaxTxExUnitsL
  let maxTxSize = fromIntegral $ pp ^. Ledger.ppMaxTxSizeL
      changeAddrApi :: Api.S.AddressInEra ApiEra = addressToApi' changeAddr

  -- First we obtain the calculated fees to correct for our collaterals.
  bodyBeforeCollUpdate@(Api.BalancedTxBody _ _ _ (Ledger.Coin feeOld)) <-
    Api.makeTransactionBodyAutoBalance
      Api.ShelleyBasedEraConway
      ss
      (Api.toLedgerEpochInfo eh)
      (Api.LedgerProtocolParameters pp)
      poolids
      stakeDelegDeposits
      drepDelegDeposits
      utxos
      body
      changeAddrApi
      (Just nkeys)
      & first
        ( \case
            e@Api.TxBodyErrorAdaBalanceNegative {} -> if isFeeUtxo then GYBuildTxFeeUtxoAdaInsufficient e else GYBuildTxBodyErrorAutoBalance e
            e@Api.TxBodyErrorAdaBalanceTooSmall {} -> if isFeeUtxo then GYBuildTxFeeUtxoAdaInsufficient e else GYBuildTxBodyErrorAutoBalance e
            anyOtherBuildError -> GYBuildTxBodyErrorAutoBalance anyOtherBuildError
        )

  -- We should call `makeTransactionBodyAutoBalance` again with updated values of collaterals so as to get slightly lower fee estimate.
  Api.BalancedTxBody txBodyContent unsignedLTx extraOut _ <-
    if collaterals == mempty
      then return bodyBeforeCollUpdate
      else
        let
          collateralTotalValue :: GYValue = foldMapUTxOs utxoValue collaterals
          collateralTotalLovelace :: Integer = fst $ valueSplitAda collateralTotalValue
          balanceNeeded :: Integer = ceiling $ (feeOld * toInteger (pp ^. ppCollateralPercentageL)) % 100
         in
          do
            (txColl, collRet) <-
              if collateralTotalLovelace >= balanceNeeded
                then
                  return
                    ( Api.TxTotalCollateral retColSup (Ledger.Coin balanceNeeded)
                    , Api.TxReturnCollateral retColSup $ txOutToApi $ GYTxOut changeAddr (collateralTotalValue `valueMinus` valueFromLovelace balanceNeeded) Nothing Nothing
                    )
                else Left $ GYBuildTxCollateralShortFall (fromInteger balanceNeeded) (fromInteger collateralTotalLovelace)

            -- In this case `makeTransactionBodyAutoBalance` doesn't return
            -- an error but instead returns `(Api.TxTotalCollateralNone, Api.TxReturnCollateralNone)`

            first GYBuildTxBodyErrorAutoBalance $
              Api.makeTransactionBodyAutoBalance
                Api.ShelleyBasedEraConway
                ss
                (Api.toLedgerEpochInfo eh)
                (Api.LedgerProtocolParameters pp)
                poolids
                stakeDelegDeposits
                drepDelegDeposits
                utxos
                body {Api.txTotalCollateral = txColl, Api.txReturnCollateral = collRet}
                changeAddrApi
                (Just nkeys)

  let
    Api.S.ShelleyTx _ ltx = Api.Tx unsignedLTx []
    -- This sums up the ExUnits for all embedded Plutus Scripts anywhere in the transaction:
    AlonzoScripts.ExUnits
      { AlonzoScripts.exUnitsSteps = steps
      , AlonzoScripts.exUnitsMem = mem
      } = AlonzoTx.totExUnits ltx
    txSize :: Natural =
      let
        -- This low level code is taken verbatim from here: https://github.com/IntersectMBO/cardano-ledger/blob/6db84a7b77e19af58feb2f45dfc50aa70435967b/eras/shelley/impl/src/Cardano/Ledger/Shelley/API/Wallet.hs#L475-L494, as this is what is referred by @cardano-api@ under the hood.
        -- This does not take into account the bootstrap (byron) witnesses.
        version = eraProtVerLow @ShelleyBasedConwayEra
        sigSize = fromIntegral $ sizeSigDSIGN (Proxy @DSIGN)
        dummySig =
          fromRight
            (error "corrupt dummy signature")
            ( CBOR.decodeFullDecoder
                version
                "dummy signature"
                CBOR.decodeSignedDSIGN
                (CBOR.serialize version $ LBS.replicate sigSize 0)
            )
        vkeySize = fromIntegral $ sizeVerKeyDSIGN (Proxy @DSIGN)
        dummyVKey w =
          let padding = LBS.replicate paddingSize 0
              paddingSize = vkeySize - LBS.length sw
              sw = CBOR.serialize version w
              keyBytes = CBOR.serialize version $ padding <> sw
           in fromRight (error "corrupt dummy vkey") (CBOR.decodeFull version keyBytes)
       in
        fromInteger $ view sizeTxF $ Shelley.addKeyWitnesses ltx (Set.fromList [WitVKey (dummyVKey x) dummySig | x <- [1 .. nkeys]])
  -- See: Cardano.Ledger.Alonzo.Rules.validateExUnitsTooBigUTxO
  unless (steps <= maxSteps && mem <= maxMemory) $
    Left $
      GYBuildTxExUnitsTooBig (maxSteps, maxMemory) (steps, mem)
  -- See: Cardano.Ledger.Shelley.Rules.validateMaxTxSizeUTxO
  unless (txSize <= maxTxSize) $
    Left (GYBuildTxSizeTooBig maxTxSize txSize)
  if isFeeUtxo
    then pure txBodyContent
    else
      first GYBuildTxCollapseExtraOutError $ collapseExtraOut extraOut txBodyContent numSkeletonOuts

{- | Collapses the extra out generated in the last step of tx building into
    another change output (If one exists)

    The amount of outputs that should not be modified is needed. In other words,
    the amount of outputs described in the GYSkeleton. It is assumed that these
    outputs are at the start of the txOuts list.
-}
collapseExtraOut ::
  -- | The extra output generated by @makeTransactionBodyAutoBalance@.
  Api.TxOut Api.S.CtxTx ApiEra ->
  -- | The body content generated by @makeTransactionBodyAutoBalance@.
  Api.TxBodyContent Api.S.BuildTx ApiEra ->
  -- | The number of skeleton outputs we don't want to touch.
  Int ->
  -- | The updated body with the collapsed outputs
  Either Api.S.TxBodyError (Api.TxBodyContent Api.S.BuildTx ApiEra)
collapseExtraOut apiOut@(Api.TxOut _ outVal _ _) bodyContent@Api.TxBodyContent {txOuts} numSkeletonOuts
  | Api.txOutValueToLovelace outVal == 0 = pure bodyContent
  | otherwise =
      case delete apiOut changeOuts of
        [] -> pure bodyContent
        ((Api.TxOut sOutAddr sOutVal sOutDat sOutRefScript) : remOuts) ->
          let
            nOutVal =
              Api.TxOutValueShelleyBased Api.ShelleyBasedEraConway $
                Api.toLedgerValue Api.MaryEraOnwardsConway $
                  foldMap' Api.txOutValueToValue [sOutVal, outVal]

            -- nOut == new Out == The merging of both apiOut and sOut
            nOut = Api.TxOut sOutAddr nOutVal sOutDat sOutRefScript
            -- nOuts == new Outs == The new list of outputs
            nOuts = skeletonOuts ++ remOuts ++ [nOut]
           in
            pure $ bodyContent {Api.txOuts = nOuts}
 where
  (skeletonOuts, changeOuts) = splitAt numSkeletonOuts txOuts

type ShelleyBasedConwayEra = Api.S.ShelleyLedgerEra ApiEra

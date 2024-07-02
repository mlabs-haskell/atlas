{-# LANGUAGE LambdaCase #-}

{-|
Module      : GeniusYield.Test.Clb
Copyright   : (c) 2023 GYELD GMBH
License     : Apache 2.0
Maintainer  : support@geniusyield.co
Stability   : develop

-}
module GeniusYield.Test.Clb
    ( Wallet (..)
    , walletPubKeyHash
    , GYTxMonadClb
    , asClb
    , asRandClb
    , liftClb
    , ownAddress
    , sendSkeleton'
    , sendSkeletonWithWallets
    , dumpUtxoState
    , mustFail
    , getNetworkId
    ) where

import           Control.Lens                              ((^.))
import           Control.Monad.Except
import           Control.Monad.Random
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Foldable                             (foldMap')
import           Data.List                                 (singleton)
import           Data.List.NonEmpty                        (NonEmpty (..))
import qualified Data.Map.Strict                           as Map
import           Data.Semigroup                            (Sum (..))
import qualified Data.Sequence                             as Seq
import qualified Data.Set                                  as Set
import           Data.SOP.NonEmpty                         (NonEmpty (NonEmptyCons, NonEmptyOne))
import           Data.Time.Clock                           (NominalDiffTime,
                                                            UTCTime)
import           Data.Time.Clock.POSIX                     (posixSecondsToUTCTime)

import qualified Cardano.Api                               as Api
import qualified Cardano.Api.Script                        as Api
import qualified Cardano.Api.Shelley                       as Api.S
import qualified Cardano.Ledger.Address                    as L
import qualified Cardano.Ledger.Alonzo.Core                as AlonzoCore
import qualified Cardano.Ledger.Shelley.API                as L
import qualified Cardano.Ledger.Api                        as L
import qualified Cardano.Ledger.Babbage.TxOut              as L
import qualified Cardano.Ledger.Compactible                as L
import qualified Cardano.Ledger.Plutus.TxInfo              as L
import qualified Cardano.Ledger.Shelley.API                as L.S
import           Cardano.Slotting.Time                     (RelativeTime (RelativeTime),
                                                            mkSlotLength)
import           Clb                                       (ClbState (..), ClbT, EmulatedLedgerState (..),
                                                            Log (Log), LogEntry (LogEntry), LogLevel (..),
                                                            MockConfig(..), OnChainTx (getOnChainTx), SlotConfig(..),
                                                            ValidationResult (..), getCurrentSlot, txOutRefAt,
                                                            txOutRefAtPaymentCred, sendTx, fromLog, unLog, getFails,
                                                            logInfo, logError)
import qualified Clb                                       (dumpUtxoState)
import qualified Ouroboros.Consensus.Cardano.Block         as Ouroboros
import qualified Ouroboros.Consensus.HardFork.History      as Ouroboros
import qualified PlutusLedgerApi.V2                        as Plutus

import           GeniusYield.Imports
import           GeniusYield.Transaction                   (GYCoinSelectionStrategy (GYRandomImproveMultiAsset),
                                                            BuildTxException (BuildTxBalancingError),
                                                            BalancingError(BalancingErrorInsufficientFunds))
import           GeniusYield.Transaction.Common            (adjustTxOut,
                                                            minimumUTxO)
import           GeniusYield.Test.Utils
import           GeniusYield.TxBuilder.Class
import           GeniusYield.TxBuilder.Common
import           GeniusYield.TxBuilder.Errors
import           GeniusYield.Types
import GeniusYield.HTTP.Errors (someBackendError)
import qualified Data.Text as T

-- FIXME: Fix this type synonym upstream.
type Clb = ClbT Identity

newtype GYTxRunEnv = GYTxRunEnv { runEnvWallet :: Wallet }

newtype GYTxMonadClb a = GYTxMonadClb
    { unGYTxMonadClb :: ExceptT GYTxMonadException (StateT GYTxRunState (ReaderT GYTxRunEnv (RandT StdGen Clb))) a
    }
    deriving newtype (Functor, Applicative, Monad, MonadReader GYTxRunEnv, MonadState GYTxRunState)

instance MonadRandom GYTxMonadClb where
    getRandomR  = GYTxMonadClb . getRandomR
    getRandom   = GYTxMonadClb getRandom
    getRandomRs = GYTxMonadClb . getRandomRs
    getRandoms  = GYTxMonadClb getRandoms

asRandClb :: Wallet
          -> GYTxMonadClb a
          -> RandT StdGen Clb (Maybe a)
asRandClb w m = do
    e <- runReaderT (evalStateT (runExceptT $ unGYTxMonadClb m) $ GYTxRunState Map.empty) $ GYTxRunEnv w
    case e of
        Left err -> lift (logError (show err)) >> return Nothing
        Right a  -> return $ Just a

asClb :: StdGen
      -> Wallet
      -> GYTxMonadClb a
      -> Clb (Maybe a)
asClb g w m = evalRandT (asRandClb w m) g

ownAddress :: GYTxMonadClb GYAddress
ownAddress = do
    nid <- networkId
    asks $ addressFromPaymentKeyHash nid . paymentKeyHash . paymentVerificationKey . walletPaymentSigningKey . runEnvWallet

liftClb :: Clb a -> GYTxMonadClb a
liftClb = GYTxMonadClb . lift . lift . lift . lift

{- | Try to execute an action, and if it fails, restore to the current state
 while preserving logs. If the action succeeds, logs an error as we expect
 it to fail. Use 'mustFailWith' and 'mustFailWithBlock' to provide custom
 error message or/and failure action name.
 FIXME: should we move it to CLB?
-}
mustFail :: GYTxMonadClb a -> GYTxMonadClb ()
mustFail act = do
    (st, preFails) <- liftClb $ do
        st <- get
        preFails <- getFails
        pure (st, preFails)
    -- FIXME: Should we ignore all application errors to prioritze `getFails` instead?
    catchError (void act) $ \case
        GYApplicationException _ -> pure ()
        e -> throwError e
    postFails <- liftClb $ getFails
    if noNewErrors preFails postFails
        then liftClb $ logError "Expected action to fail but it succeeds"
    else do
        infoLog <- liftClb $ gets mockInfo
        liftClb $ put
            st
                { mockInfo = infoLog <> mkMustFailLog preFails postFails
                -- , mustFailLog = mkMustFailLog preFails postFails
                }
    where
      noNewErrors (fromLog -> a) (fromLog -> b) = length a == length b
      mkMustFailLog (unLog -> pre) (unLog -> post) =
        Log $ second (LogEntry Error . ((msg  <> ":") <> ). show) <$> Seq.drop (Seq.length pre) post
      msg = "Unnamed failure action"

getNetworkId :: GYTxMonadClb GYNetworkId
getNetworkId = do
    magic <- liftClb $ gets (mockConfigNetworkId . mockConfig)
    -- TODO: Add epoch slots and network era to clb and retrieve from there.
    pure . GYPrivnet $ GYNetworkInfo
        { gyNetworkMagic = Api.S.unNetworkMagic $ Api.S.toNetworkMagic magic
        , gyNetworkEpochSlots = 500
        , gyNetworkEra = GYBabbage
        }

instance MonadError GYTxMonadException GYTxMonadClb where

    throwError = GYTxMonadClb . throwError

    catchError m handler = GYTxMonadClb . catchError (unGYTxMonadClb m) $ unGYTxMonadClb . handler

instance GYTxQueryMonad GYTxMonadClb where

    networkId = getNetworkId

    lookupDatum :: GYDatumHash -> GYTxMonadClb (Maybe GYDatum)
    lookupDatum h = liftClb $ do
        mdh <- gets mockDatums
        return $ do
            d <- Map.lookup (datumHashToPlutus h) mdh
            return $ datumFromPlutus d

    utxosAtAddress addr mAssetClass = do
        gyLogDebug' "" $ "utxosAtAddress, addr: " <> show addr
        refs  <- liftClb $ txOutRefAt $ addressToApi' addr
        gyLogDebug' "" $ "utxosAtAddress, refs: " <> show refs
        utxos <- wither f refs
        let utxos' =
              case mAssetClass of
                Nothing -> utxos
                Just ac -> filter (\GYUTxO {..} -> valueAssetClass utxoValue ac > 0) utxos
        return $ utxosFromList utxos'
      where
        f :: Plutus.TxOutRef -> GYTxMonadClb (Maybe GYUTxO)
        f ref = do
            case txOutRefFromPlutus ref of
                Left _     -> return Nothing -- TODO: should it error?
                Right ref' -> utxoAtTxOutRef ref'

    utxosAtPaymentCredential :: GYPaymentCredential -> Maybe GYAssetClass -> GYTxMonadClb GYUTxOs
    utxosAtPaymentCredential cred mAssetClass = do
        refs  <- liftClb $ txOutRefAtPaymentCred $ paymentCredentialToPlutus cred
        utxos <- wither f refs
        pure
            . utxosFromList
            $ filter (\GYUTxO{utxoValue} -> maybe True (>0) $ valueAssetClass utxoValue <$> mAssetClass)
            utxos
      where
        f :: Plutus.TxOutRef -> GYTxMonadClb (Maybe GYUTxO)
        f ref = case txOutRefFromPlutus ref of
            Left _     -> return Nothing
            Right ref' -> utxoAtTxOutRef ref'

    utxoAtTxOutRef ref = do
        -- All UTxOs map
        utxos <- liftClb $ gets (L.unUTxO . L.S.utxosUtxo . L.S.lsUTxOState . _memPoolState . emulatedLedgerState)
        -- Maps keys to Plutus TxOutRef
        let m = Map.mapKeys (txOutRefToPlutus . txOutRefFromApi . Api.S.fromShelleyTxIn) utxos

        return $ do
            o <- Map.lookup (txOutRefToPlutus ref) m

            let a = addressFromApi . Api.S.fromShelleyAddrToAny . either id L.decompactAddr $ o ^. L.addrEitherBabbageTxOutL
                v = valueFromApi . Api.S.fromMaryValue . either id L.fromCompact $ o ^. L.valueEitherBabbageTxOutL

            d <- case o ^. L.datumBabbageTxOutL of
                L.NoDatum -> pure GYOutDatumNone
                L.DatumHash dh -> GYOutDatumHash <$> rightToMaybe (datumHashFromPlutus $ L.transDataHash dh)
                L.Datum binaryData -> pure $
                    GYOutDatumInline
                    . datumFromPlutus
                    . Plutus.Datum
                    . Plutus.dataToBuiltinData
                    . L.getPlutusData
                    . L.binaryDataToData
                    $ binaryData

            let s = case o ^. L.referenceScriptBabbageTxOutL of
                        L.S.SJust x  -> someScriptFromReferenceApi
                                        $ Api.fromShelleyScriptToReferenceScript Api.ShelleyBasedEraBabbage x
                        L.S.SNothing -> Nothing

            return GYUTxO
                { utxoRef       = ref
                , utxoAddress   = a
                , utxoValue     = v
                , utxoOutDatum  = d
                , utxoRefScript = s
                }

    stakeAddressInfo = const $ pure Nothing

    slotConfig = do
        (zero, len) <- slotConfig'
        return $ simpleSlotConfig zero len

    slotOfCurrentBlock = liftClb $ slotFromApi <$> Clb.getCurrentSlot

    logMsg _ns s msg = do
        -- let doc = lines msg
        let doc = msg
        liftClb $ logInfo $ case s of
            GYDebug   -> LogEntry Debug doc
            GYInfo    -> LogEntry Info doc
            GYWarning -> LogEntry Warning doc
            GYError   -> LogEntry Error doc

instance GYTxMonad GYTxMonadClb where

    ownAddresses = singleton <$> do
        nid <- networkId
        asks $ addressFromPaymentKeyHash nid . paymentKeyHash . paymentVerificationKey . walletPaymentSigningKey . runEnvWallet

    availableUTxOs = do
        addrs <- ownAddresses
        utxosAtAddresses addrs

    someUTxO lang = do
        addrs <- ownAddresses
        utxos <- availableUTxOs
        case lang of
          PlutusV2 ->
            case someTxOutRef utxos of
                Nothing       -> throwError $ GYQueryUTxOException $ GYNoUtxosAtAddress addrs
                Just (ref, _) -> return ref
          PlutusV1 ->
            case find utxoTranslatableToV1 $ utxosToList utxos of
              Just u  -> return $ utxoRef u
              Nothing -> throwError . GYQueryUTxOException $ GYNoUtxosAtAddress addrs  -- TODO: Better error message here?

    randSeed = return 42


instance GYTestMonad GYTxMonadClb where
    runWallet w action = do
        ma <- runWalletClb w action
        case ma of
            Nothing -> throwAppError $ someBackendError "Run wallet action returned Nothing"
            Just a  -> pure a
    sendSkeleton skeleton =
        first
            ( txFromApi
            . Api.S.ShelleyTx Api.S.ShelleyBasedEraBabbage
            . L.extractTx
            . Clb.getOnChainTx
            )
        <$> sendSkeleton' skeleton []

-- | Runs a `GYTxMonadClb` action using the given wallet.
runWalletClb :: Wallet -> GYTxMonadClb a -> GYTxMonadClb (Maybe a)
runWalletClb w action = liftClb $ flip evalRandT pureGen $ asRandClb w action

-- Send skeletons with multiple signatures from wallet
sendSkeletonWithWallets :: GYTxSkeleton v -> [Wallet] -> GYTxMonadClb GYTxId
sendSkeletonWithWallets skeleton ws = snd <$> sendSkeleton' skeleton ws

sendSkeleton' :: GYTxSkeleton v -> [Wallet] -> GYTxMonadClb (OnChainTx, GYTxId)
sendSkeleton' skeleton ws = do
    w <- asks runEnvWallet
    let sigs = map walletPaymentSigningKey $ w : ws
    body <- skeletonToTxBody skeleton
    pp <- protocolParameters
    modify (updateWalletState w pp body)
    dumpBody body

    let tx = signGYTxBody body sigs
    gyLogDebug' "" $ "encoded tx: " <> txToHex tx

    -- Submit
    vRes <- liftClb $ sendTx $ txToApi tx
    case vRes of
        Success _state onChainTx -> pure (onChainTx, txBodyTxId body)
        Fail _ err -> throwAppError . someBackendError . T.pack $ show err

  where
    -- Updates the wallet state.
    -- Updates extra lovelace required for fees & minimum ada requirements against the wallet sending this transaction.
    updateWalletState :: Wallet -> AlonzoCore.PParams (Api.S.ShelleyLedgerEra Api.S.BabbageEra) -> GYTxBody -> GYTxRunState -> GYTxRunState
    updateWalletState w pp body GYTxRunState {..} = GYTxRunState $ Map.insertWith mappend (walletAddress w) v walletExtraLovelace
      where
        v = ( coerce $ txBodyFee body
            , coerce $ flip valueAssetClass GYLovelace $
                foldMap'
                  (\o ->
                    -- If this additional ada is coming back to one's own self, we need not account for it.
                    if gyTxOutAddress o == walletAddress w then
                      mempty
                    else gyTxOutValue (adjustTxOut (minimumUTxO pp) o) `valueMinus` gyTxOutValue o
                  ) $ gytxOuts skeleton
            )

    -- TODO: use Prettyprinter
    dumpBody :: GYTxBody -> GYTxMonadClb ()
    dumpBody body = do
        ins <- mapM utxoAtTxOutRef' $ txBodyTxIns body
        refIns <- mapM utxoAtTxOutRef' $ txBodyTxInsReference body
        gyLogDebug' "" $
            printf "fee: %d lovelace\nmint value: %s\nvalidity range: %s\ncollateral: %s\ntotal collateral: %d\ninputs:\n\n%sreference inputs:\n\n%soutputs:\n\n%s"
                (txBodyFee body)
                (txBodyMintValue body)
                (show $ txBodyValidityRange body)
                (show $ txBodyCollateral body)
                (txBodyTotalCollateralLovelace body)
                (concatMap dumpInUTxO ins)
                (concatMap dumpInUTxO refIns)
                (concatMap dumpOutUTxO $ utxosToList $ txBodyUTxOs body)

    dumpInUTxO :: GYUTxO -> String
    dumpInUTxO GYUTxO{..} = printf " - ref:        %s\n"   utxoRef             <>
                            printf "   addr:       %s\n"   utxoAddress         <>
                            printf "   value:      %s\n"   utxoValue           <>
                            printf "   datum:      %s\n"   (show utxoOutDatum) <>
                            printf "   ref script: %s\n\n" (show utxoRefScript)

    dumpOutUTxO :: GYUTxO -> String
    dumpOutUTxO GYUTxO{..} = printf " - addr:       %s\n"   utxoAddress         <>
                             printf "   value:      %s\n"   utxoValue           <>
                             printf "   datum:      %s\n"   (show utxoOutDatum) <>
                             printf "   ref script: %s\n\n" (show utxoRefScript)

skeletonToTxBody :: GYTxSkeleton v -> GYTxMonadClb GYTxBody
skeletonToTxBody skeleton = do
    ss <- systemStart
    eh <- eraHistory
    pp <- protocolParameters
    ps <- stakePools

    addr <- ownAddress
    e    <- buildTxCore ss eh pp ps GYRandomImproveMultiAsset (const id) [addr] addr Nothing [Identity skeleton]
    case e of
        Left err  -> throwAppError err
        Right res -> case res of
            GYTxBuildSuccess (Identity body :| _) -> return body
            GYTxBuildFailure (BalancingErrorInsufficientFunds v) -> throwAppError . BuildTxBalancingError $ BalancingErrorInsufficientFunds v
            GYTxBuildFailure _                    -> error "impossible case"
            GYTxBuildPartialSuccess _ _           -> error "impossible case"
            GYTxBuildNoInputs                     -> error "impossible case"

slotConfig' :: GYTxMonadClb (UTCTime, NominalDiffTime)
slotConfig' = liftClb $ do
    sc <- gets $ mockConfigSlotConfig . mockConfig
    let len  = fromInteger (scSlotLength sc) / 1000
        zero = posixSecondsToUTCTime $ timeToPOSIX $ timeFromPlutus $ scSlotZeroTime sc
    return (zero, len)

systemStart :: GYTxMonadClb Api.SystemStart
systemStart = gyscSystemStart <$> slotConfig

protocolParameters :: GYTxMonadClb (AlonzoCore.PParams (Api.S.ShelleyLedgerEra Api.S.BabbageEra))
protocolParameters = do
    pparams <- liftClb $ gets $ mockConfigProtocol . mockConfig
    pure $ coerce pparams


stakePools :: GYTxMonadClb (Set Api.S.PoolId)
stakePools = pure Set.empty
-- stakePools = do
--     pids <- liftClb $ gets $ Map.keys . stake'pools . mockStake
--     foldM f Set.empty pids
--   where
--     f :: Set Api.S.PoolId -> Api.S.PoolId -> GYTxMonadClb (Set Api.S.PoolId)
--     f s pid = either
--         (\e -> throwError $ GYConversionException $ GYLedgerToCardanoError $ DeserialiseRawBytesError ("stakePools, error: " <> fromString (show e)))
--         (\pid' -> return $ Set.insert pid' s)
--         $ Api.deserialiseFromRawBytes (Api.AsHash Api.AsStakePoolKey) bs
--       where
--         Plutus.BuiltinByteString bs = Plutus.getPubKeyHash $ unPoolId pid

eraHistory :: GYTxMonadClb Api.EraHistory
eraHistory = do
    (_, len) <- slotConfig'
    return $ Api.EraHistory $ eh len
  where
    eh :: NominalDiffTime -> Ouroboros.Interpreter (Ouroboros.CardanoEras Ouroboros.StandardCrypto)
    eh = Ouroboros.mkInterpreter . Ouroboros.Summary
                . NonEmptyCons byronEra
                . NonEmptyCons shelleyEra
                . NonEmptyCons allegraEra
                . NonEmptyCons maryEra
                . NonEmptyCons alonzoEra
                . NonEmptyOne . babbageEra

    byronEra =
        Ouroboros.EraSummary
            { eraStart = Ouroboros.Bound {boundTime = RelativeTime 0, boundSlot = 0, boundEpoch = 0}
            , eraEnd = Ouroboros.EraEnd (Ouroboros.Bound {boundTime = RelativeTime 0, boundSlot = 0, boundEpoch = 0})
            , eraParams = Ouroboros.EraParams {eraEpochSize = 4320, eraSlotLength = mkSlotLength 20, eraSafeZone = Ouroboros.StandardSafeZone 864}
            }
    shelleyEra =
        Ouroboros.EraSummary
            { eraStart = Ouroboros.Bound {boundTime = RelativeTime 0, boundSlot = 0, boundEpoch = 0}
            , eraEnd = Ouroboros.EraEnd (Ouroboros.Bound {boundTime = RelativeTime 0, boundSlot = 0, boundEpoch = 0})
            , eraParams = Ouroboros.EraParams {eraEpochSize = 86400, eraSlotLength = mkSlotLength 1, eraSafeZone = Ouroboros.StandardSafeZone 25920}
            }
    allegraEra =
        Ouroboros.EraSummary
            { eraStart = Ouroboros.Bound {boundTime = RelativeTime 0, boundSlot = 0, boundEpoch = 0}
            , eraEnd = Ouroboros.EraEnd (Ouroboros.Bound {boundTime = RelativeTime 0, boundSlot = 0, boundEpoch = 0})
            , eraParams = Ouroboros.EraParams {eraEpochSize = 86400, eraSlotLength = mkSlotLength 1, eraSafeZone = Ouroboros.StandardSafeZone 25920}
            }
    maryEra =
        Ouroboros.EraSummary
            { eraStart = Ouroboros.Bound {boundTime = RelativeTime 0, boundSlot = 0, boundEpoch = 0}
            , eraEnd = Ouroboros.EraEnd (Ouroboros.Bound {boundTime = RelativeTime 0, boundSlot = 0, boundEpoch = 0})
            , eraParams = Ouroboros.EraParams {eraEpochSize = 86400, eraSlotLength = mkSlotLength 1, eraSafeZone = Ouroboros.StandardSafeZone 25920}
            }
    alonzoEra =
        Ouroboros.EraSummary
            { eraStart = Ouroboros.Bound {boundTime = RelativeTime 0, boundSlot = 0, boundEpoch = 0}
            , eraEnd = Ouroboros.EraEnd (Ouroboros.Bound {boundTime = RelativeTime 0, boundSlot = 0, boundEpoch = 0})
            , eraParams = Ouroboros.EraParams {eraEpochSize = 86400, eraSlotLength = mkSlotLength 1, eraSafeZone = Ouroboros.StandardSafeZone 25920}
            }
    babbageEra len =
        Ouroboros.EraSummary
            { eraStart = Ouroboros.Bound {boundTime = RelativeTime 0, boundSlot = 0, boundEpoch = 0}
            , eraEnd = Ouroboros.EraUnbounded
            , eraParams = Ouroboros.EraParams {eraEpochSize = 86400, eraSlotLength = mkSlotLength len, eraSafeZone = Ouroboros.StandardSafeZone 25920}
            }

dumpUtxoState :: GYTxMonadClb ()
dumpUtxoState = liftClb Clb.dumpUtxoState


{-# LANGUAGE PatternSynonyms #-}

{-|
Module      : GeniusYield.Test.Utils
Copyright   : (c) 2023 GYELD GMBH
License     : Apache 2.0
Maintainer  : support@geniusyield.co
Stability   : develop

-}
module GeniusYield.Test.Utils
    ( Wallet (..)
    , Wallets (..)
    , GYTxRunState (..)
    , GYTestMonad (..)
    , walletsToList
    , walletPubKeyHash
    , fakeCoin, fakeGold, fakeIron
    , afterAllSucceed
    , feesFromLovelace
    , withMaxQCTests
    , pureGen
    , withWalletBalancesCheckSimple
    , balance
    , addRefScript
    , addRefInput
    , findLockedUtxosInBody
    , utxosInBody
    , pattern (:=)
    ) where

import           Control.Monad.Random
import           Control.Monad.State
import qualified Data.Map.Strict                  as Map
import           Data.Maybe                       (fromJust)
import           Data.Semigroup                   (Sum (getSum))

import qualified PlutusLedgerApi.V1.Value         as Plutus

import qualified Test.Tasty                       as Tasty
import qualified Test.Tasty.QuickCheck            as Tasty
import qualified Test.Tasty.Runners               as Tasty

import           GeniusYield.Imports
import           GeniusYield.Test.FakeCoin
import           GeniusYield.TxBuilder
import           GeniusYield.Types
import GeniusYield.HTTP.Errors (someBackendError)
import qualified Data.Text as T
import qualified Cardano.Api.Shelley as Api.S
import qualified Cardano.Ledger.Api as L
import qualified Cardano.Ledger.Babbage as L.B
import Data.List (findIndex)
import Control.Lens ((^.))
import qualified Cardano.Ledger.Babbage.TxOut as L.B
import qualified Data.Maybe.Strict as StrictMaybe
import qualified Cardano.Ledger.Address as L
import qualified Cardano.Ledger.Binary as L
import qualified Data.Sequence.Strict as StrictSeq
import qualified Cardano.Ledger.Babbage.Tx as L.B
import qualified PlutusLedgerApi.V2 as PlutusV2

-------------------------------------------------------------------------------
-- tasty tools
-------------------------------------------------------------------------------

-- | Runs the second 'Tasty.TestTree' after all tests in the first 'Tasty.TestTree' succeed
afterAllSucceed :: Tasty.TestTree -> Tasty.TestTree -> Tasty.TestTree
afterAllSucceed = Tasty.after Tasty.AllSucceed . pat where
    pat :: Tasty.TestTree -> String
    pat dep = case dep of
        Tasty.SingleTest tn _        -> tn
        Tasty.TestGroup tn _         -> tn
        Tasty.After _ _ dep'         -> pat dep'
        Tasty.PlusTestOptions _ dep' -> pat dep'
        Tasty.WithResource _ f       -> pat (f (fail "Not running IO"))
        Tasty.AskOptions f           -> pat (f mempty)

-------------------------------------------------------------------------------
-- QC
-------------------------------------------------------------------------------

-- | Adjust the number of QuickCheck cases to generate.
withMaxQCTests :: Int -> Tasty.TestTree -> Tasty.TestTree
withMaxQCTests n = Tasty.adjustOption f where
    f :: Tasty.QuickCheckTests -> Tasty.QuickCheckTests
    f (Tasty.QuickCheckTests m) = Tasty.QuickCheckTests (min m n)

-------------------------------------------------------------------------------
-- test assets
-------------------------------------------------------------------------------

class    FromFakeCoin a                 where fromFakeCoin :: FakeCoin -> a
instance FromFakeCoin FakeCoin          where fromFakeCoin = id
instance FromFakeCoin GYAssetClass      where fromFakeCoin = fromRight (error "invalid asset class") . assetClassFromPlutus . fakeCoin
instance FromFakeCoin Plutus.AssetClass where fromFakeCoin = fakeCoin

-- | This allows to write e.g. @'fakeGold' 1000 :: GYValue@.
instance (a ~ Integer, b ~ GYValue) => FromFakeCoin (a -> b) where
    fromFakeCoin c = fromRight (error "invalid value") . valueFromPlutus . fakeValue c

-- | Fake \"Gold\" coin to use during tests.
-- Can represent a 'GYAssetClass' or a Plutus 'Plutus.AssetClass'
fakeGold :: FromFakeCoin a => a
fakeGold = fromFakeCoin $ FakeCoin "Gold"

-- | Fake \"Iron\" coin to use during tests
-- Can represent a 'GYAssetClass' or a Plutus 'Plutus.AssetClass'
fakeIron :: FromFakeCoin a => a
fakeIron = fromFakeCoin $ FakeCoin "Iron"

type FeesLovelace = Sum Integer
type MinAdaLovelace = Sum Integer

-- Used by 'withWalletBalancesCheckSimple'
newtype GYTxRunState = GYTxRunState { walletExtraLovelace :: Map GYAddress (FeesLovelace, MinAdaLovelace) }

-- | Testing Wallet representation.
data Wallet = Wallet
    { walletPaymentSigningKey :: !GYPaymentSigningKey
    , walletStakeSigningKey   :: !(Maybe GYStakeSigningKey)
    , walletAddress           :: !GYAddress
    }
    deriving (Show, Eq, Ord)

-- | Gets a GYPubKeyHash of a testing wallet.
walletPubKeyHash :: Wallet -> GYPubKeyHash
walletPubKeyHash = fromJust . addressToPubKeyHash . walletAddress

-- | Available wallets.
data Wallets = Wallets
    { w1 :: !Wallet
    , w2 :: !Wallet
    , w3 :: !Wallet
    , w4 :: !Wallet
    , w5 :: !Wallet
    , w6 :: !Wallet
    , w7 :: !Wallet
    , w8 :: !Wallet
    , w9 :: !Wallet
    } deriving (Show, Eq, Ord)

walletsToList :: Wallets -> [Wallet]
walletsToList Wallets{..} = [w1, w2, w3, w4, w5, w6, w7, w8, w9]

class (GYTxQueryMonad m, MonadState GYTxRunState m) => GYTestMonad m where
    runWallet :: Wallet -> m a -> m a
    sendSkeleton :: GYTxSkeleton v -> m (GYTx, GYTxId)

{- | Gets the balance from anything that `HasAddress`. The usual case will be a
     testing wallet.
    -}
balance :: GYTxQueryMonad m => Wallet -> m GYValue
balance w = do
    let addr = walletAddress w
    utxos <- utxosAtAddress addr Nothing
    pure $ foldMapUTxOs utxoValue utxos

{- | Computes a 'GYTxQueryMonad' action, checking that the 'Wallet' balances
        change according to the input list. This is a simplified version of `withWalletBalancesCheck` where the input list need not consider lovelaces required for fees & to satisfy the min ada requirements as these are added automatically. It is therefore recommended to use this function over `withWalletBalancesCheck` to avoid hardcoding the lovelaces required for fees & min ada constraints.
Notes:
* An empty list means no checks are performed.
* The 'GYValue' should be negative to check if the Wallet lost those funds.
-}
withWalletBalancesCheckSimple
    :: GYTestMonad m
    => [(Wallet, GYValue)]
    -> m a
    -> m a
withWalletBalancesCheckSimple wallValueDiffs m = do
  bs <- mapM (balance . fst) wallValueDiffs
  a <- m
  walletExtraLovelaceMap <- gets walletExtraLovelace
  bs' <- mapM (balance . fst) wallValueDiffs

  forM_ (zip3 wallValueDiffs bs' bs) $
    \((w, v), b', b) ->
      let extraLovelace = case Map.lookup (walletAddress w) walletExtraLovelaceMap of
            Nothing -> 0
            Just (extraLovelaceForFees, extraLovelaceForMinAda) -> getSum $ extraLovelaceForFees <> extraLovelaceForMinAda
          newBalance = b' <> valueFromLovelace extraLovelace
          diff = newBalance `valueMinus` b
        in unless (diff == v) .
            throwAppError . someBackendError . T.pack
                $ printf "Wallet: %s. Old balance: %s. New balance: %s. New balance after adding extra lovelaces (%d): %s. Expected balance difference of %s, but the actual difference was %s"
                    (walletAddress w)
                    b
                    b'
                    extraLovelace
                    newBalance
                    v
                    diff
  return a


{- | Returns the list of outputs of the transaction for the given address.
     Returns Nothing if it fails to decode an address contained in the
      transaction outputs.
-}
findLockedUtxosInBody :: Num a => GYAddress -> GYTx -> Maybe [a]
findLockedUtxosInBody addr tx =
  let
    os = getTxOutputs tx
    findAllMatches (_, [], acc) = Just acc
    findAllMatches (index, txOut : os', acc) =
        let txOutAddr = addressFromApi . Api.S.fromShelleyAddrToAny . either id L.decompactAddr $ L.B.getEitherAddrBabbageTxOut txOut
        in if txOutAddr == addr
            then findAllMatches (index + 1, os', index : acc)
            else findAllMatches (index + 1, os', acc)
  in
    findAllMatches (0, os, [])

-- | Given a transaction and the corresponding transaction id, gives the list of UTxOs generated by that body /provided they still exist/. This function is usually expected to be called immediately after the transaction's submission.
utxosInBody :: GYTxQueryMonad m => GYTx -> GYTxId -> m [Maybe GYUTxO]
utxosInBody tx txId = do
    let os = getTxOutputs tx
    mapM (\i -> utxoAtTxOutRef (txOutRefFromTuple (txId, fromInteger $ toInteger i))) [0 .. (length os - 1)]


-- | Adds the given script to the given address and returns the reference for it.
addRefScript :: GYTestMonad m => GYAddress -> GYValidator 'PlutusV2 -> m (Maybe GYTxOutRef)
addRefScript addr script = do
    let script' = validatorToScript script
    (tx, txId) <- sendSkeleton
        $ mustHaveOutput
            (mkGYTxOutNoDatum addr mempty)
            { gyTxOutRefS = Just $ GYPlutusScript script' }

    let index = findIndex
            (\o ->
                let lsh = fmap (apiHashToPlutus . Api.S.ScriptHash) $ L.hashScript <$> (o ^. L.B.referenceScriptBabbageTxOutL)
                in lsh == StrictMaybe.SJust (scriptPlutusHash script')
            )
            $ getTxOutputs tx
    pure $ (Just . txOutRefFromApiTxIdIx (txIdToApi txId) . wordToApiIx . fromInteger) . toInteger =<< index

-- | Adds an input (whose datum we'll refer later) and returns the reference to it.
addRefInput :: GYTestMonad m
            => Bool       -- ^ Whether to inline this datum?
            -> GYAddress  -- ^ Where to place this output?
            -> GYDatum    -- ^ Our datum.
            -> m (Maybe GYTxOutRef)
addRefInput toInline addr dat = do
    (tx@(txToApi -> Api.S.ShelleyTx _ ledgerTx), txId) <- sendSkeleton
        (mustHaveOutput
            $ GYTxOut addr mempty (Just (dat, if toInline then GYTxOutUseInlineDatum else GYTxOutDontUseInlineDatum)) Nothing
        )

    let L.TxDats datumMap = ledgerTx ^. L.witsTxL . L.datsTxWitsL
        datumWits         = datumFromLedgerData <$> datumMap
    let outputsWithResolvedDatums = map
            (\o ->
                resolveDatumFromLedger datumWits $ o ^. L.B.datumBabbageTxOutL
            )
            $ getTxOutputs tx
    let mIndex = findIndex (\d -> Just dat == d) outputsWithResolvedDatums
    pure $ (Just . txOutRefFromApiTxIdIx (txIdToApi txId) . wordToApiIx . fromInteger) . toInteger =<< mIndex

resolveDatumFromLedger :: L.Era era => Map (L.DataHash (L.EraCrypto era)) GYDatum -> L.Datum era -> Maybe GYDatum
resolveDatumFromLedger _ (L.Datum d) = Just
                                        . datumFromLedgerData
                                        $ L.binaryDataToData d
resolveDatumFromLedger datumMap (L.DatumHash dh) = Map.lookup dh datumMap
resolveDatumFromLedger _ L.NoDatum = Nothing
-- TODO: Add to CLB upstream?
getTxOutputs :: GYTx -> [L.B.BabbageTxOut (L.BabbageEra L.StandardCrypto)]
getTxOutputs (txToApi -> Api.S.ShelleyTx _ ledgerTx) = fmap L.sizedValue
    . toList
    . StrictSeq.fromStrict
    . L.B.btbOutputs
    $ L.B.body ledgerTx

datumFromLedgerData :: L.Data era -> GYDatum
datumFromLedgerData = datumFromPlutusData
    . PlutusV2.BuiltinData
    . L.getPlutusData

{- | Abstraction for explicitly building a Value representing the fees of a
     transaction.
-}
feesFromLovelace :: Integer -> GYValue
feesFromLovelace = valueFromLovelace

-------------------------------------------------------------------------------
-- Extras
-------------------------------------------------------------------------------

-- | Pattern to create pairs easily.
pattern (:=) :: x -> y -> (x, y)
pattern (:=) x y = (x, y)

infix 0 :=

-------------------------------------------------------------------------------
-- Preset StdGen
-------------------------------------------------------------------------------

pureGen :: StdGen
pureGen = mkStdGen 42


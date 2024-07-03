{-|
Module      : GeniusYield.Test.Privnet.Ctx
Copyright   : (c) 2023 GYELD GMBH
License     : Apache 2.0
Maintainer  : support@geniusyield.co
Stability   : develop

-}
module GeniusYield.Test.Privnet.Ctx (
    -- * Context
    Ctx (..),
    ctxNetworkId,
    -- * Wallet
    Wallet (..),
    CreateUserConfig (..),

    GYTxRunState (..),

    GYPrivnetMonad,
    runGYPrivnet,

    userPkh,
    userPaymentPkh,
    userStakePkh,
    userVKey,
    userPaymentVKey,
    userStakeVKey,
    -- * Operations
    ctxEval,
    ctxRunI,
    ctxRunIWithStrategy,
    ctxRunC,
    ctxRunCWithStrategy,
    ctxRunF,
    ctxRunFWithStrategy,
    ctxRunFWithCollateral,
    ctxSlotOfCurrentBlock,
    ctxWaitNextBlock,
    ctxWaitUntilSlot,
    ctxProviders,
    ctxSlotConfig,
    submitTx,
    submitTx',
    -- * Helpers
    newTempUserCtx,
    ctxQueryBalance,
    findOutput,
    addRefScriptCtx,
    addRefInputCtx,
    ctxUserF,
) where

import qualified Cardano.Api                as Api
import           Data.Default               (Default (..))
import qualified Data.Map.Strict            as Map
import           Data.Monoid                (Sum (Sum))
import qualified Data.Set                   as Set
import qualified GeniusYield.Examples.Limbo as Limbo
import           GeniusYield.Imports
import           GeniusYield.Providers.Node
import           GeniusYield.Transaction
import           GeniusYield.TxBuilder
import           GeniusYield.Types
import           Test.Tasty.HUnit           (assertFailure)
import qualified Cardano.Ledger.Alonzo.Core as AlonzoCore
import qualified Cardano.Api.Shelley as Api.S
import Data.Foldable (foldMap')
import GeniusYield.Transaction.Common (adjustTxOut, minimumUTxO)
import GeniusYield.Test.Utils
import Control.Monad.Reader
import Control.Monad.State.Strict

data CreateUserConfig =
     CreateUserConfig
       { -- | Create collateral output of 5 ada?
         cucGenerateCollateral :: !Bool,
         -- | Create a stake key for the user?
         cucGenerateStakeKey   :: !Bool
       }

instance Default CreateUserConfig where
   def = CreateUserConfig { cucGenerateCollateral = False, cucGenerateStakeKey = False }

{-# DEPRECATED userVKey "Use userPaymentVKey." #-}
userVKey :: Wallet -> GYPaymentVerificationKey
userVKey = paymentVerificationKey . walletPaymentSigningKey

userPaymentVKey :: Wallet -> GYPaymentVerificationKey
userPaymentVKey = userVKey

userStakeVKey :: Wallet -> Maybe GYStakeVerificationKey
userStakeVKey = fmap stakeVerificationKey . walletStakeSigningKey

userPkh :: Wallet -> GYPubKeyHash
userPkh = toPubKeyHash . paymentKeyHash . paymentVerificationKey . walletPaymentSigningKey

userPaymentPkh :: Wallet -> GYPaymentKeyHash
userPaymentPkh = paymentKeyHash . paymentVerificationKey . walletPaymentSigningKey

userStakePkh :: Wallet -> Maybe GYStakeKeyHash
userStakePkh = fmap (stakeKeyHash . stakeVerificationKey) . walletStakeSigningKey

data Ctx = Ctx
    { ctxNetworkInfo       :: !GYNetworkInfo
    , ctxInfo              :: !Api.LocalNodeConnectInfo
    -- FIXME: There are now multiple genesis users (since cardano-testnet usage).
    , ctxUsers             :: Wallets
    , ctxGold              :: !GYAssetClass  -- ^ asset used in tests
    , ctxIron              :: !GYAssetClass  -- ^ asset used in tests
    , ctxLog               :: !GYLog
    , ctxLookupDatum       :: !GYLookupDatum
    , ctxAwaitTxConfirmed  :: !GYAwaitTx
    , ctxQueryUtxos        :: !GYQueryUTxO
    , ctxGetParams         :: !GYGetParameters
    }

-- | Obtain the funding/genesis user.
-- Note: The first wallet is guaranteed to be a genesis user, but there may be more genesis users.
ctxUserF :: Ctx -> Wallet
ctxUserF = w1 . ctxUsers

ctxNetworkId :: Ctx -> GYNetworkId
ctxNetworkId Ctx {ctxNetworkInfo} = GYPrivnet ctxNetworkInfo

-- | Creates a new user with the given balance. Note that the actual balance which this user get's could be more than what is provided to satisfy minimum ada requirement of a UTxO.
newTempUserCtx:: Ctx
              -> Wallet          -- ^ Wallet which will fund this new user.
              -> GYValue         -- ^ Describes balance of new user.
              -> CreateUserConfig
              -> IO Wallet
newTempUserCtx ctx fundUser fundValue CreateUserConfig {..} = do
  newPaymentSKey <- generatePaymentSigningKey
  newStakeSKey <- if cucGenerateStakeKey then Just <$> generateStakeSigningKey else pure Nothing
  let newPaymentVKey = paymentVerificationKey newPaymentSKey
      newStakeVKey = stakeVerificationKey <$> newStakeSKey
      newPaymentKeyHash = paymentKeyHash newPaymentVKey
      newStakeKeyHash = stakeKeyHash <$> newStakeVKey
      newAddr = addressFromCredential (ctxNetworkId ctx) (GYPaymentCredentialByKey newPaymentKeyHash) (GYStakeCredentialByKey <$> newStakeKeyHash)
      (adaInValue, otherValue) = valueSplitAda fundValue

  -- We want this new user to have at least 5 ada if we want to create collateral.
  -- Our balancer would add minimum ada required for other utxo in case of equality
  when (cucGenerateCollateral && adaInValue < collateralLovelace) $ fail "Given value for new user has less than 5 ada"

  txBody <- ctxRunI ctx fundUser $ return $
    if cucGenerateCollateral then
      mustHaveOutput (mkGYTxOutNoDatum newAddr (otherValue <> (valueFromLovelace adaInValue `valueMinus` collateralValue))) <>
      mustHaveOutput (mkGYTxOutNoDatum newAddr collateralValue)
    else
      mustHaveOutput (mkGYTxOutNoDatum newAddr fundValue)

  void $ submitTx ctx fundUser txBody
  return $ Wallet {walletPaymentSigningKey = newPaymentSKey, walletStakeSigningKey = newStakeSKey, walletAddress = newAddr}


ctxRunF :: forall t v. Traversable t => Ctx -> Wallet -> GYTxMonadNode (t (GYTxSkeleton v)) -> IO (t GYTxBody)
ctxRunF ctx w =  runGYTxMonadNodeF GYRandomImproveMultiAsset (ctxNetworkId ctx) (ctxProviders ctx) [userAddr] userAddr Nothing
  where
    userAddr = walletAddress w

ctxRunFWithStrategy :: forall t v. Traversable t => GYCoinSelectionStrategy -> Ctx -> Wallet -> GYTxMonadNode (t (GYTxSkeleton v)) -> IO (t GYTxBody)
ctxRunFWithStrategy strat ctx w =  runGYTxMonadNodeF strat (ctxNetworkId ctx) (ctxProviders ctx) [userAddr] userAddr Nothing
  where
    userAddr = walletAddress w

-- | Variant of `ctxRunF` where caller can also give the UTxO to be used as collateral.
ctxRunFWithCollateral :: forall t v. Traversable t
                      => Ctx
                      -> Wallet
                      -> GYTxOutRef  -- ^ Reference to UTxO to be used as collateral.
                      -> Bool        -- ^ To check whether this given collateral UTxO has value of exact 5 ada? If it doesn't have exact 5 ada, it would be ignored.
                      -> GYTxMonadNode (t (GYTxSkeleton v))
                      -> IO (t GYTxBody)
ctxRunFWithCollateral ctx w coll toCheck5Ada =  runGYTxMonadNodeF GYRandomImproveMultiAsset (ctxNetworkId ctx) (ctxProviders ctx) [userAddr] userAddr $ Just (coll, toCheck5Ada)
  where
    userAddr = walletAddress w

ctxRunC :: forall a. Ctx -> Wallet -> GYTxMonadNode a -> IO a
ctxRunC = coerce (ctxRunF @(Const a))

ctxRunCWithStrategy :: forall a. GYCoinSelectionStrategy -> Ctx -> Wallet -> GYTxMonadNode a -> IO a
ctxRunCWithStrategy = coerce (ctxRunFWithStrategy @(Const a))

ctxRunI :: Ctx -> Wallet -> GYTxMonadNode (GYTxSkeleton v) -> IO GYTxBody
ctxRunI = coerce (ctxRunF @Identity)

ctxRunIWithStrategy :: GYCoinSelectionStrategy -> Ctx -> Wallet -> GYTxMonadNode (GYTxSkeleton v) -> IO GYTxBody
ctxRunIWithStrategy = coerce (ctxRunFWithStrategy @Identity)

{- | Evaluate 'GYTxMonadNode' under 'Ctx' _with no collateral specified_.

The evaluation result of this function will *only* be consistent with 'ctxRunF' family of
functions _if and only if_ the arguments are *equivalent*.
-}
ctxEval :: Ctx -> Wallet -> GYTxMonadNode a -> IO a
ctxEval ctx w = evalGYTxMonadNode (ctxNetworkId ctx) (ctxProviders ctx) [userAddr] userAddr Nothing
  where
    userAddr = walletAddress w

ctxSlotOfCurrentBlock :: Ctx -> IO GYSlot
ctxSlotOfCurrentBlock (ctxProviders -> providers) =
    gyGetSlotOfCurrentBlock providers

ctxWaitNextBlock :: Ctx -> IO ()
ctxWaitNextBlock (ctxProviders -> providers) = void $ gyWaitForNextBlock providers

ctxWaitUntilSlot :: Ctx -> GYSlot -> IO ()
ctxWaitUntilSlot (ctxProviders -> providers) slot = void $ gyWaitUntilSlot providers slot

ctxSlotConfig :: Ctx -> IO GYSlotConfig
ctxSlotConfig (ctxProviders -> providers) = gyGetSlotConfig providers

ctxQueryBalance :: Ctx -> Wallet -> IO GYValue
ctxQueryBalance ctx u = ctxRunC ctx u $ do
    queryBalance $ walletAddress u

ctxProviders :: Ctx -> GYProviders
ctxProviders ctx = GYProviders
    { gyLookupDatum      = ctxLookupDatum ctx
    , gySubmitTx         = nodeSubmitTx (ctxInfo ctx)
    , gyAwaitTxConfirmed = ctxAwaitTxConfirmed ctx
    , gySlotActions      = nodeSlotActions (ctxInfo ctx)
    , gyGetParameters    = ctxGetParams ctx
    , gyQueryUTxO        = ctxQueryUtxos ctx
    , gyLog'             = ctxLog ctx
    , gyGetStakeAddressInfo = nodeStakeAddressInfo (ctxInfo ctx)
    }

newtype GYPrivnetMonad a = GYPrivnetMonad (ReaderT Ctx (ReaderT Wallet (StateT GYTxRunState IO)) a)
  deriving newtype ( Functor
                   , Applicative
                   , Monad
                   , MonadReader Ctx
                   , MonadState GYTxRunState
                   , MonadIO
                   )

runGYPrivnet :: Ctx -> Wallet -> GYPrivnetMonad a -> IO a
runGYPrivnet ctx w = fmap fst . runGYPrivnet' ctx w (GYTxRunState mempty)

runGYPrivnet' :: Ctx -> Wallet -> GYTxRunState -> GYPrivnetMonad a -> IO (a, GYTxRunState)
runGYPrivnet' ctx w st (GYPrivnetMonad act) = do
    flip runStateT st $ runReaderT (runReaderT act ctx) w

instance MonadError GYTxMonadException GYPrivnetMonad where
  throwError = liftIO . throwIO
  catchError act handler = GYPrivnetMonad $ do
      ctx <- ask
      w <- lift ask
      s <- get
      (res, s') <- liftIO $ catch
          (runGYPrivnet' ctx w s act)
          (runGYPrivnet' ctx w s . handler)
      put s'
      pure res

fromGYTxMonadNode :: GYTxMonadNode a -> GYPrivnetMonad a
fromGYTxMonadNode act = do
    ctx <- ask
    w <- GYPrivnetMonad $ lift ask
    liftIO $ ctxEval ctx w act

instance GYTxQueryMonad GYPrivnetMonad where
    networkId = fromGYTxMonadNode networkId
    lookupDatum = fromGYTxMonadNode . lookupDatum
    utxoAtTxOutRef = fromGYTxMonadNode . utxoAtTxOutRef
    utxoAtTxOutRefWithDatum = fromGYTxMonadNode . utxoAtTxOutRefWithDatum
    utxosAtTxOutRefs = fromGYTxMonadNode . utxosAtTxOutRefs
    utxosAtTxOutRefsWithDatums = fromGYTxMonadNode . utxosAtTxOutRefsWithDatums
    utxosAtAddress addr = fromGYTxMonadNode . utxosAtAddress addr
    utxosAtAddressWithDatums addr = fromGYTxMonadNode . utxosAtAddressWithDatums addr
    utxosAtAddresses = fromGYTxMonadNode . utxosAtAddresses
    utxosAtAddressesWithDatums = fromGYTxMonadNode . utxosAtAddressesWithDatums
    utxoRefsAtAddress = fromGYTxMonadNode . utxoRefsAtAddress
    utxosAtPaymentCredential cred = fromGYTxMonadNode . utxosAtPaymentCredential cred
    utxosAtPaymentCredentialWithDatums cred = fromGYTxMonadNode . utxosAtPaymentCredentialWithDatums cred
    utxosAtPaymentCredentials = fromGYTxMonadNode . utxosAtPaymentCredentials
    utxosAtPaymentCredentialsWithDatums = fromGYTxMonadNode . utxosAtPaymentCredentialsWithDatums
    stakeAddressInfo = fromGYTxMonadNode . stakeAddressInfo
    slotConfig = fromGYTxMonadNode slotConfig
    slotOfCurrentBlock = fromGYTxMonadNode slotOfCurrentBlock
    logMsg ns sev = fromGYTxMonadNode . logMsg ns sev

instance GYTestMonad GYPrivnetMonad where
    runWallet w (GYPrivnetMonad act) = GYPrivnetMonad $ do
        ctx <- ask
        lift . lift $ runReaderT (runReaderT (put (GYTxRunState mempty) >> act) ctx) w
    sendSkeleton skeleton = do
        ctx <- ask
        user <- GYPrivnetMonad $ lift ask
        txBodyPlace <- liftIO $ ctxRunI ctx user (pure skeleton)
        apiPp       <- liftIO $ gyGetProtocolParameters $ ctxProviders ctx

        pp <- case Api.toLedgerPParams Api.ShelleyBasedEraBabbage apiPp of
                Left e   -> liftIO $ throwIO $ BuildTxPPConversionError e
                Right pp -> pure pp

        modify (updateWalletState user pp txBodyPlace)

        liftIO $ submitTx_ ctx user txBodyPlace
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

submitTx :: Ctx -> Wallet -> GYTxBody -> IO GYTxId
submitTx ctx w = fmap snd . submitTx_ ctx w

submitTx_ :: Ctx -> Wallet -> GYTxBody -> IO (GYTx, GYTxId)
submitTx_ ctx Wallet {..} txBody = do
    let reqSigs = txBodyReqSignatories txBody
        tx =
          signGYTxBody' txBody $
            case walletStakeSigningKey of
              Nothing -> [GYSomeSigningKey walletPaymentSigningKey]
              -- It might be the case that @cardano-api@ is clever enough to not add signature if it is not required but cursory look at their code suggests otherwise.
              Just stakeKey -> if Set.member (toPubKeyHash . stakeKeyHash . stakeVerificationKey $ stakeKey) reqSigs then [GYSomeSigningKey walletPaymentSigningKey, GYSomeSigningKey stakeKey] else [GYSomeSigningKey walletPaymentSigningKey]
    txId <- submitTx' ctx tx
    pure (tx, txId)

submitTx' :: Ctx -> GYTx -> IO GYTxId
submitTx' ctx@Ctx { ctxInfo } tx = do
    txId <- nodeSubmitTx ctxInfo tx
    gyAwaitTxConfirmed
      (ctxProviders ctx)
      (GYAwaitTxParameters { maxAttempts = 30, checkInterval = 1_000_000, confirmations = 0 })
      txId
    return txId

-- | Function to find for the first locked output in the given `GYTxBody` at the given `GYAddress`.
findOutput :: GYAddress -> GYTxBody -> IO GYTxOutRef
findOutput addr txBody = do
    let utxos = txBodyUTxOs txBody
    maybe (assertFailure "expecting an order in utxos") return $
        findFirst (\utxo -> if utxoAddress utxo == addr then Just (utxoRef utxo) else Nothing) $ utxosToList utxos

-- | Function to add for a reference script. It adds the script in so called "Always failing" validator so that it won't be later possible to spend this output. There is a slight optimisation here in that if the desired reference script already exists then we don't add another one and return the reference for the found one else, we create a new one.
addRefScriptCtx :: Ctx                 -- ^ Given context.
                -> Wallet                -- ^ Wallet which will execute the transaction (if required).
                -> GYScript 'PlutusV2  -- ^ Given script.
                -> IO GYTxOutRef       -- ^ Returns the reference for the desired output.
addRefScriptCtx ctx user script = do
  txBodyRefScript <- ctxRunF ctx user $ Limbo.addRefScript script
  case txBodyRefScript of
    Left ref -> return ref
    Right body -> do
      let refs = Limbo.findRefScriptsInBody body
      ref <- case Map.lookup (Some script) refs of
        Just ref -> return ref
        Nothing  -> fail "Shouldn't happen: no ref in body"
      void $ submitTx ctx user body
      return ref

-- | Function to add for a reference input.
addRefInputCtx :: Ctx            -- ^ Given context.
               -> Wallet           -- ^ Wallet which will execute this transaction.
               -> Bool           -- ^ Whether to inline the datum.
               -> GYAddress      -- ^ Address to put this output at.
               -> GYDatum        -- ^ The datum to put.
               -> IO GYTxOutRef  -- ^ Returns the reference for the required output.
addRefInputCtx ctx user toInline addr ourDatum = do
  txBody <- ctxRunI ctx user $ return $ mustHaveOutput (GYTxOut addr mempty (Just (ourDatum, if toInline then GYTxOutUseInlineDatum else GYTxOutDontUseInlineDatum)) Nothing)
  let utxos = utxosToList $ txBodyUTxOs txBody
      ourDatumHash = hashDatum ourDatum
      mRefInputUtxo = find (\utxo ->
        case utxoOutDatum utxo of
          GYOutDatumHash dh  -> ourDatumHash == dh
          GYOutDatumInline d -> ourDatum == d
          GYOutDatumNone     -> False
        ) utxos
  case mRefInputUtxo of
    Nothing               -> fail "Shouldn't happen: Couldn't find desired UTxO in tx outputs"
    Just GYUTxO {utxoRef} -> do
      void $ submitTx ctx user txBody
      return utxoRef

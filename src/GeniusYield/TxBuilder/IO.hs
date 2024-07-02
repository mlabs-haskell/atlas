{-|
Module      : GeniusYield.TxBuilder.IO
Copyright   : (c) 2023 GYELD GMBH
License     : Apache 2.0
Maintainer  : support@geniusyield.co
Stability   : develop

-}
module GeniusYield.TxBuilder.IO (
    GYTxMonadIO,
    GYTxBuildResult(..),
    runGYTxMonadIO,
    runGYTxMonadIOWithStrategy,
    runGYTxMonadIOC,
    runGYTxMonadIOF,
    runGYTxMonadIOParallel,
    runGYTxMonadIOParallelWithStrategy,
    runGYTxMonadIOParallelF,
    runGYTxMonadIOChaining,
    runGYTxMonadIOChainingF,
) where

import qualified Cardano.Api                     as Api

import           Control.Monad.IO.Class          (MonadIO (..))
import           Control.Monad.Trans.Maybe       (MaybeT (runMaybeT))
import qualified Data.ByteString                 as BS
import qualified Data.List.NonEmpty              as NE
import qualified Data.Set                        as Set
import           GHC.Stack                       (withFrozenCallStack)

import           GeniusYield.Imports
import           GeniusYield.Transaction
import           GeniusYield.TxBuilder.Class
import           GeniusYield.TxBuilder.Common
import           GeniusYield.TxBuilder.Errors
import           GeniusYield.Types

-------------------------------------------------------------------------------
-- GY implementation
-------------------------------------------------------------------------------

-- | 'GYTxMonad' interpretation run under IO.
newtype GYTxMonadIO a = GYTxMonadIO { unGYTxMonadIO :: GYTxIOEnv -> IO a }
  deriving stock (Functor)

type role GYTxMonadIO representational

instance Applicative GYTxMonadIO where
    pure x = GYTxMonadIO $ \_ -> return x
    (<*>) = ap

instance Monad GYTxMonadIO where
    m >>= k = GYTxMonadIO $ \env -> do
        x <- unGYTxMonadIO m env
        unGYTxMonadIO (k x) env

instance MonadIO GYTxMonadIO where
    liftIO = GYTxMonadIO . const

data GYTxIOEnv = GYTxIOEnv
    { envNid           :: !GYNetworkId
    , envProviders     :: !GYProviders
    , envAddrs         :: ![GYAddress]
    , _envChangeAddr   :: !GYAddress
    , envCollateral    :: !(Maybe GYUTxO)
    , envUsedSomeUTxOs :: !(Set GYTxOutRef)
    }

instance MonadError GYTxMonadException GYTxMonadIO where
    throwError = liftIO . throwIO

    catchError action handler = GYTxMonadIO $ \env -> catch
        (unGYTxMonadIO action env)
        (\err -> unGYTxMonadIO (handler err) env)

instance GYTxQueryMonad GYTxMonadIO where
    networkId = GYTxMonadIO $ \env ->
        return $ envNid env

    lookupDatum h = GYTxMonadIO $ \env ->
        gyLookupDatum (envProviders env) h

    utxosAtAddress addr mAssetClass = GYTxMonadIO $ \env ->
        gyQueryUtxosAtAddress (envProviders env) addr mAssetClass

    utxosAtAddresses addrs = GYTxMonadIO $ \env ->
        gyQueryUtxosAtAddresses (envProviders env) addrs

    utxosAtPaymentCredentials pcs = GYTxMonadIO $ \env ->
        gyQueryUtxosAtPaymentCredentials (envProviders env) pcs

    utxosAtAddressWithDatums addr mAssetClass = GYTxMonadIO $ \env ->
        gyQueryUtxosAtAddressWithDatums (envProviders env) addr mAssetClass

    utxosAtPaymentCredential cred mAssetClass = GYTxMonadIO $ \env ->
        gyQueryUtxosAtPaymentCredential (envProviders env) cred mAssetClass

    utxosAtAddressesWithDatums addrs = GYTxMonadIO $ \env ->
        gyQueryUtxosAtAddressesWithDatums (envProviders env) addrs

    utxosAtPaymentCredentialWithDatums cred mAssetClass = GYTxMonadIO $ \env ->
        gyQueryUtxosAtPaymentCredWithDatums (envProviders env) cred mAssetClass

    utxosAtPaymentCredentialsWithDatums pcs = GYTxMonadIO $ \env ->
        gyQueryUtxosAtPaymentCredsWithDatums (envProviders env) pcs

    utxoRefsAtAddress addr = GYTxMonadIO $ \env ->
        gyQueryUtxoRefsAtAddress (envProviders env) addr

    utxoAtTxOutRef oref = GYTxMonadIO $ \env ->
        gyQueryUtxoAtTxOutRef (envProviders env) oref

    utxosAtTxOutRefs oref = GYTxMonadIO $ \env ->
        gyQueryUtxosAtTxOutRefs (envProviders env) oref

    utxosAtTxOutRefsWithDatums refs = GYTxMonadIO $ \env ->
        gyQueryUtxosAtTxOutRefsWithDatums (envProviders env) refs

    stakeAddressInfo stakeAddr = GYTxMonadIO $ \env ->
        gyGetStakeAddressInfo (envProviders env) stakeAddr

    slotConfig = GYTxMonadIO $ \env ->
        gyGetSlotConfig (envProviders env)

    slotOfCurrentBlock = GYTxMonadIO $ \env ->
        gyGetSlotOfCurrentBlock (envProviders env)

    logMsg ns s msg = GYTxMonadIO $ \env ->
        withFrozenCallStack $ gyLog (envProviders env) ns s msg

-- TODO Note: randSeed implementation should use some internal state #30
--            state so randSeed returns different seeds if called multiple times.
--            (https://github.com/geniusyield/atlas/issues/30)
instance GYTxMonad GYTxMonadIO where

    ownAddresses = GYTxMonadIO $ return . envAddrs

    availableUTxOs = do
        addrs         <- ownAddresses
        mCollateral   <- getCollateral
        usedSomeUTxOs <- getUsedSomeUTxOs
        utxos         <- utxosAtAddresses addrs
        return $ utxosRemoveTxOutRefs (maybe usedSomeUTxOs ((`Set.insert` usedSomeUTxOs) . utxoRef) mCollateral) utxos
      where
        getCollateral    = GYTxMonadIO $ return . envCollateral
        getUsedSomeUTxOs = GYTxMonadIO $ return . envUsedSomeUTxOs

    someUTxO lang = do
        addrs           <- ownAddresses
        utxosToConsider <- availableUTxOs
        case lang of
          PlutusV2 ->
            case someTxOutRef utxosToConsider  of
                Just (oref, _) -> return oref
                Nothing        -> throwError . GYQueryUTxOException $ GYNoUtxosAtAddress addrs
          PlutusV1 ->
            case find utxoTranslatableToV1 $ utxosToList utxosToConsider of
              Just u  -> return $ utxoRef u
              Nothing -> throwError . GYQueryUTxOException $ GYNoUtxosAtAddress addrs  -- TODO: Better error message here?

    -- inject non-determinism from own-address
    -- thus different users will get different random seeds.
    randSeed = foldl' (\ m w -> 256 * m + fromIntegral w) 0
               . concatMap (BS.unpack . Api.serialiseToRawBytes . addressToApi)
               <$> ownAddresses

instance MonadRandom GYTxMonadIO where
    getRandomR  = GYTxMonadIO . const . getRandomR
    getRandom   = GYTxMonadIO $ const getRandom
    getRandomRs = GYTxMonadIO . const . getRandomRs
    getRandoms  = GYTxMonadIO $ const getRandoms

runGYTxMonadIO
    :: GYNetworkId                     -- ^ Network ID.
    -> GYProviders                     -- ^ Provider.
    -> [GYAddress]                     -- ^ Addresses belonging to wallet.
    -> GYAddress                       -- ^ Change address.
    -> Maybe (GYTxOutRef, Bool)        -- ^ If `Nothing` is provided, framework would pick up a suitable UTxO as collateral and in such case is also free to spend it. If something is given with boolean being `False` then framework will use the given `GYTxOutRef` as collateral and would reserve it as well. But if boolean is `True`, framework would only use it as collateral and reserve it, if value in the given UTxO is exactly 5 ada.
    -> GYTxMonadIO (GYTxSkeleton v)    -- ^ Skeleton.
    -> IO GYTxBody
runGYTxMonadIO = coerce (runGYTxMonadIOF @Identity GYRandomImproveMultiAsset)

runGYTxMonadIOWithStrategy
    :: GYCoinSelectionStrategy        -- ^ Coin selection strategy.
    -> GYNetworkId                     -- ^ Network ID.
    -> GYProviders                     -- ^ Provider.
    -> [GYAddress]                     -- ^ Addresses belonging to wallet.
    -> GYAddress                       -- ^ Change address.
    -> Maybe (GYTxOutRef, Bool)        -- ^ If `Nothing` is provided, framework would pick up a suitable UTxO as collateral and in such case is also free to spend it. If something is given with boolean being `False` then framework will use the given `GYTxOutRef` as collateral and would reserve it as well. But if boolean is `True`, framework would only use it as collateral and reserve it, if value in the given UTxO is exactly 5 ada.
    -> GYTxMonadIO (GYTxSkeleton v)  -- ^ Skeleton.
    -> IO GYTxBody
runGYTxMonadIOWithStrategy strat = coerce (runGYTxMonadIOF @Identity strat)

runGYTxMonadIOParallel
    :: GYNetworkId                     -- ^ Network ID.
    -> GYProviders                     -- ^ Provider.
    -> [GYAddress]                     -- ^ Addresses belonging to wallet.
    -> GYAddress                       -- ^ Change address.
    -> Maybe (GYTxOutRef, Bool)        -- ^ If `Nothing` is provided, framework would pick up a suitable UTxO as collateral and in such case is also free to spend it. If something is given with boolean being `False` then framework will use the given `GYTxOutRef` as collateral and would reserve it as well. But if boolean is `True`, framework would only use it as collateral and reserve it, if value in the given UTxO is exactly 5 ada.
    -> GYTxMonadIO [GYTxSkeleton v]    -- ^ Skeleton(s).
    -> IO (GYTxBuildResult Identity)
runGYTxMonadIOParallel = coerce (runGYTxMonadIOParallelF @Identity GYRandomImproveMultiAsset)

runGYTxMonadIOParallelWithStrategy
    :: GYCoinSelectionStrategy         -- ^ Coin selection strategy.
    -> GYNetworkId                     -- ^ Network ID.
    -> GYProviders                     -- ^ Provider.
    -> [GYAddress]                     -- ^ Addresses belonging to wallet.
    -> GYAddress                       -- ^ Change address.
    -> Maybe (GYTxOutRef, Bool)        -- ^ If `Nothing` is provided, framework would pick up a suitable UTxO as collateral and in such case is also free to spend it. If something is given with boolean being `False` then framework will use the given `GYTxOutRef` as collateral and would reserve it as well. But if boolean is `True`, framework would only use it as collateral and reserve it, if value in the given UTxO is exactly 5 ada.
    -> GYTxMonadIO [GYTxSkeleton v]    -- ^ Skeleton(s).
    -> IO (GYTxBuildResult Identity)
runGYTxMonadIOParallelWithStrategy strat = coerce (runGYTxMonadIOParallelF @Identity strat)

runGYTxMonadIOChaining
    :: GYNetworkId                     -- ^ Network ID.
    -> GYProviders                     -- ^ Provider.
    -> [GYAddress]                     -- ^ Addresses belonging to wallet.
    -> GYAddress                       -- ^ Change address.
    -> Maybe (GYTxOutRef, Bool)        -- ^ If `Nothing` is provided, framework would pick up a suitable UTxO as collateral and in such case is also free to spend it. If something is given with boolean being `False` then framework will use the given `GYTxOutRef` as collateral and would reserve it as well. But if boolean is `True`, framework would only use it as collateral and reserve it, if value in the given UTxO is exactly 5 ada.
    -> GYTxMonadIO [GYTxSkeleton v]    -- ^ Skeleton(s).
    -> IO (GYTxBuildResult Identity)
runGYTxMonadIOChaining = coerce (runGYTxMonadIOChainingF @Identity GYRandomImproveMultiAsset)

runGYTxMonadIOC
    :: forall a.
       GYNetworkId               -- ^ Network ID.
    -> GYProviders               -- ^ Provider.
    -> [GYAddress]               -- ^ Addresses belonging to wallet.
    -> GYAddress                 -- ^ Change address.
    -> Maybe (GYTxOutRef, Bool)  -- ^ If `Nothing` is provided, framework would pick up a suitable UTxO as collateral and in such case is also free to spend it. If something is given with boolean being `False` then framework will use the given `GYTxOutRef` as collateral and would reserve it as well. But if boolean is `True`, framework would only use it as collateral and reserve it, if value in the given UTxO is exactly 5 ada.
    -> GYTxMonadIO a             -- ^ When we don't want to build a skeleton.
    -> IO a
runGYTxMonadIOC = coerce (runGYTxMonadIOF @(Const a) GYRandomImproveMultiAsset)

{- | The most basic version of 'GYTxMonadIO' interpreter over a generic 'Traversable'.

== NOTE ==
This is not meant to be used with structures containing _multiple_ 'GYTxSkeleton's. As the balancer
will end up using the same utxos across the different txs.

Consider using 'runGYTxMonadIOParallel' or 'runGYTxMonadIOChaining' instead.
-}
runGYTxMonadIOF
    :: forall f v. Traversable f
    => GYCoinSelectionStrategy             -- ^ Coin selection strategy.
    -> GYNetworkId                         -- ^ Network ID.
    -> GYProviders                         -- ^ Provider.
    -> [GYAddress]                         -- ^ Addresses belonging to wallet.
    -> GYAddress                           -- ^ Change address.
    -> Maybe (GYTxOutRef, Bool)            -- ^ If `Nothing` is provided, framework would pick up a suitable UTxO as collateral and in such case is also free to spend it. If something is given with boolean being `False` then framework will use the given `GYTxOutRef` as collateral and would reserve it as well. But if boolean is `True`, framework would only use it as collateral and reserve it, if value in the given UTxO is exactly 5 ada.
    -> GYTxMonadIO (f (GYTxSkeleton v))    -- ^ Skeleton(s).
    -> IO (f GYTxBody)
runGYTxMonadIOF cstrat nid providers addrs change collateral m = do
    x <- runGYTxMonadIOCore (const id) cstrat nid providers addrs change collateral $ (:[]) <$> m
    case x of
      GYTxBuildSuccess ne          -> pure $ NE.head ne
      GYTxBuildPartialSuccess be _ -> throwIO $ BuildTxBalancingError be
      GYTxBuildFailure be          -> throwIO $ BuildTxBalancingError be
      -- We know there is precisely one input.
      GYTxBuildNoInputs            -> error "runGYTxMonadIOF: absurd"

{- | A multi transaction building 'GYTxMonadIO' interpreter.

This does not perform chaining, i.e does not use utxos created by one of the given transactions in the next one.
However, it does ensure that the balancer does not end up using the same own utxos when building multiple
transactions at once.

This supports failure recovery by utilizing 'GYTxBuildResult'.
-}
runGYTxMonadIOParallelF
    :: Traversable f
    => GYCoinSelectionStrategy             -- ^ Coin selection strategy.
    -> GYNetworkId                         -- ^ Network ID.
    -> GYProviders                         -- ^ Provider.
    -> [GYAddress]                         -- ^ Addresses belonging to wallet.
    -> GYAddress                           -- ^ Change address.
    -> Maybe (GYTxOutRef, Bool)            -- ^ If `Nothing` is provided, framework would pick up a suitable UTxO as collateral and in such case is also free to spend it. If something is given with boolean being `False` then framework will use the given `GYTxOutRef` as collateral and would reserve it as well. But if boolean is `True`, framework would only use it as collateral and reserve it, if value in the given UTxO is exactly 5 ada.
    -> GYTxMonadIO [f (GYTxSkeleton v)]    -- ^ Skeleton(s).
    -> IO (GYTxBuildResult f)
runGYTxMonadIOParallelF cstrat nid providers addrs change collateral m = do
    runGYTxMonadIOCore updateOwnUtxosParallel cstrat nid providers addrs change collateral m

{- | A chaining transaction building 'GYTxMonadIO' interpreter.

This will perform chaining, i.e it will use utxos created by one of the given transactions, when building the next one.

This supports failure recovery by utilizing 'GYTxBuildResult'.

**EXPERIMENTAL**
-}
runGYTxMonadIOChainingF :: Traversable f
    => GYCoinSelectionStrategy             -- ^ Coin selection strategy.
    -> GYNetworkId                         -- ^ Network ID.
    -> GYProviders                         -- ^ Provider.
    -> [GYAddress]                         -- ^ Addresses belonging to wallet.
    -> GYAddress                           -- ^ Change address.
    -> Maybe (GYTxOutRef, Bool)            -- ^ If `Nothing` is provided, framework would pick up a suitable UTxO as collateral and in such case is also free to spend it. If something is given with boolean being `False` then framework will use the given `GYTxOutRef` as collateral and would reserve it as well. But if boolean is `True`, framework would only use it as collateral and reserve it, if value in the given UTxO is exactly 5 ada.
    -> GYTxMonadIO [f (GYTxSkeleton v)]    -- ^ Skeleton(s).
    -> IO (GYTxBuildResult f)
runGYTxMonadIOChainingF cstrat nid providers addrs change collateral m = do
    runGYTxMonadIOCore (updateOwnUtxosChaining $ Set.fromList addrs) cstrat nid providers addrs change collateral m

{- | The core implementation of 'GYTxMonadIO' interpreter for building 'GYTxBody's out of one or more 'GYTxSkeleton's.

Peculiarly, this is parameterized on:

- An "own utxo update" function, this is meant to govern how the set of known "own utxos" is updated after building a transaction skeleton.

  If the user chooses not to update this set, based on the newly created 'GYTxBody', the same own utxos set will be used for the next
  transaction in the list (if any). Which may lead to the balancer choosing the same utxo inputs again - resulting in a transaction
  conflict.

See 'runGYTxMonadIOF' for an example which _does not update_ used up own utxos for multi 'GYTxSkeleton' builds.

See 'runGYTxMonadIOParallel' for an example which  _removes_ used up own utxos for next 'GYTxSkeleton's (if any).

See 'runGYTxMonadIOChaining' for an example which _removes_ used up own utxos, **and** _adds_ newly created utxos addressed to
own wallet, for next 'GYTxSkeleton's (if any).

The function recovers successfully built tx skeletons, in case the list contains several of them. See: 'GYTxBuildResult'.
-}
runGYTxMonadIOCore
    :: forall f v. Traversable f
    => (GYTxBody -> GYUTxOs -> GYUTxOs)    -- ^ Function governing how to update UTxO set when building for multiple skeletons.
    -> GYCoinSelectionStrategy             -- ^ Coin selection strategy.
    -> GYNetworkId                         -- ^ Network ID.
    -> GYProviders                         -- ^ Provider.
    -> [GYAddress]                         -- ^ Addresses belonging to wallet.
    -> GYAddress                           -- ^ Change address.
    -> Maybe (GYTxOutRef, Bool)            -- ^ If `Nothing` is provided, framework would pick up a suitable UTxO as collateral and in such case is also free to spend it. If something is given with boolean being `False` then framework will use the given `GYTxOutRef` as collateral and would reserve it as well. But if boolean is `True`, framework would only use it as collateral and reserve it, if value in the given UTxO is exactly 5 ada.
    -> GYTxMonadIO [f (GYTxSkeleton v)]    -- ^ Skeleton(s).
    -> IO (GYTxBuildResult f)
runGYTxMonadIOCore ownUtxoUpdateF cstrat nid providers addrs change collateral action = do

    -- Obtain constant parameters to be used across several 'GYTxBody' generations.
    ss          <- gyGetSystemStart providers
    eh          <- gyGetEraHistory providers
    apiPp       <- gyGetProtocolParameters providers
    ps          <- gyGetStakePools providers

    -- bpp <- case Api.bundleProtocolParams Api.BabbageEra pp of
    --             Left e     -> throwIO $ BuildTxPPConversionError e
    --             Right bpp' -> pure bpp'

    pp <- case Api.toLedgerPParams Api.ShelleyBasedEraBabbage apiPp of
                Left e   -> throwIO $ BuildTxPPConversionError e
                Right pp -> pure pp

    collateral' <- obtainCollateral

    e <- unGYTxMonadIO (buildTxCore ss eh pp ps cstrat ownUtxoUpdateF addrs change collateral' loggedAction) GYTxIOEnv
            { envNid           = nid
            , envProviders     = providers
            , envAddrs         = addrs
            ,_envChangeAddr    = change
            , envCollateral    = collateral'
            , envUsedSomeUTxOs = mempty
            }
    case e of
        Left err  -> throwIO err
        Right res -> return res

    where
      obtainCollateral :: IO (Maybe GYUTxO)
      obtainCollateral = runMaybeT $ do
        (collateralRef, toCheck) <- hoistMaybe collateral
        collateralUtxo <- liftIO $ gyQueryUtxoAtTxOutRef providers collateralRef
            >>= maybe (throwIO . GYQueryUTxOException $ GYNoUtxoAtRef collateralRef) pure
        if not toCheck || (utxoValue collateralUtxo == collateralValue) then return collateralUtxo
        else hoistMaybe Nothing

      loggedAction :: GYTxMonadIO [f (GYTxSkeleton v)]
      loggedAction = action >>= \skeletons -> logSkeletons skeletons
                     >> return skeletons

      logSkeletons :: [f (GYTxSkeleton v)] -> GYTxMonadIO ()
      logSkeletons = mapM_ (mapM_ (logMsg "runGYTxMonadIOCore" GYDebug . show))

-- | Update own utxo set by removing any utxos used up in the given tx.
updateOwnUtxosParallel :: GYTxBody -> GYUTxOs -> GYUTxOs
updateOwnUtxosParallel txBody = utxosRemoveTxOutRefs (Set.fromList txIns)
  where
    txIns = txBodyTxIns txBody

{- | Update own utxo set by removing any utxos used up in the given tx,
**and** adding newly created utxos addressed to own wallet. -}
updateOwnUtxosChaining :: Set GYAddress -> GYTxBody -> GYUTxOs -> GYUTxOs
updateOwnUtxosChaining ownAddrs txBody utxos = utxosRemoveTxOutRefs (Set.fromList txIns) utxos <> txOutsOwn
  where
    txIns = txBodyTxIns txBody
    txOuts = txBodyUTxOs txBody
    txOutsOwn = filterUTxOs (\GYUTxO {utxoAddress} -> utxoAddress `Set.member` ownAddrs) txOuts
{-|
Module      : GeniusYield.Providers.CachedQueryUTxOs
Copyright   : (c) 2023 GYELD GMBH
License     : Apache 2.0
Maintainer  : support@geniusyield.co
Stability   : develop

-}
module GeniusYield.Providers.CachedQueryUTxOs (
    CachedQueryUTxO,
    makeCachedQueryUTxO,
) where

import qualified Data.Cache          as Cache

import           GeniusYield.Imports
import           GeniusYield.Types

data CachedQueryUTxO = CachedQueryUTxO
    { _cquAddrCache        :: !(Cache.Cache (GYAddress, Maybe GYAssetClass) GYUTxOs)
    , _cquRefCache         :: !(Cache.Cache GYTxOutRef (Maybe GYUTxO))
    , _cquPaymentCredCache :: !(Cache.Cache GYPaymentCredential GYUTxOs)
    , _cquInfo             :: !GYQueryUTxO
    , _cquLog              :: !GYLog
    }

-- | Return a cached 'GYQueryUTxO' and a cache clearing function.
makeCachedQueryUTxO :: GYQueryUTxO -> GYLog -> IO (GYQueryUTxO, IO ())
makeCachedQueryUTxO query log' = do
    addrCache        <- Cache.newCache Nothing
    refCache         <- Cache.newCache Nothing
    paymentCredCache <- Cache.newCache Nothing
    let purge = Cache.purge addrCache >> Cache.purge refCache >> Cache.purge paymentCredCache
    return (cachedQueryUTxO $ CachedQueryUTxO addrCache refCache paymentCredCache query log', purge)

cachedQueryUTxO :: CachedQueryUTxO -> GYQueryUTxO
cachedQueryUTxO q = GYQueryUTxO
    (cachedUtxosAtTxOutRefs q)
    Nothing  -- Will use the default implementation.
    (cachedUtxoAtTxOutRef q)
    (gyQueryUtxoRefsAtAddressDefault $ cachedUtxosAtAddress q)
    (cachedUtxosAtAddress q)
    Nothing
    (gyQueryUtxoAtAddressesDefault $ cachedUtxosAtAddress q)
    Nothing  -- Will use the default implementation.
    (cachedUtxosAtPaymentCred q)
    Nothing  -- Will use the default implementation.

-------------------------------------------------------------------------------
-- Queries & caching
-------------------------------------------------------------------------------

cachedUtxoAtTxOutRef :: CachedQueryUTxO -> GYTxOutRef -> IO (Maybe GYUTxO)
cachedUtxoAtTxOutRef (CachedQueryUTxO _ cache _ q _) ref = do
  m <- Cache.lookup' cache ref
  case m of
    Nothing -> do
      res <- gyQueryUtxoAtTxOutRef' q ref
      Cache.insert cache ref res
      return res

    Just res -> do
      return res

cachedUtxosAtTxOutRefs :: CachedQueryUTxO -> [GYTxOutRef] -> IO GYUTxOs
cachedUtxosAtTxOutRefs ctx@(CachedQueryUTxO _ cache _ q (GYLog log' _)) refs = do
    step1 <- forM refs $ \ref -> (ref, ) <$> Cache.lookup' cache ref

    -- refs with no results in cache.
    let refs' = mapMaybe (\(ref, res) -> if isJust res then Nothing else Just ref) step1
    log' mempty GYDebug $ "TxOutRefs not in cache:\n" ++ unlines (map show refs')

    -- query node for things not in cache
    utxos <- gyQueryUtxosAtTxOutRefs' q refs'
    storeCacheUTxO ctx utxos

    -- Note: technically we should filter non-refs' from utxos,
    -- there shouldn't be any such.
    -- But we assume that utxosAtTxOutRefs well-behaves.

    -- combine
    return $ utxos <> utxosFromList (mapMaybe (join . snd) step1)

-- When we query complete UTxOs,
-- we can store the pieces of resulting UTxOs in per-txoutref cache.
storeCacheUTxO :: CachedQueryUTxO -> GYUTxOs -> IO ()
storeCacheUTxO (CachedQueryUTxO _ cache _ _ _) utxos = forUTxOs_ utxos $ \utxo ->
    let ref = utxoRef utxo
    in  Cache.insert cache ref (Just utxo)

cachedUtxosAtAddress :: CachedQueryUTxO -> GYAddress -> Maybe GYAssetClass -> IO GYUTxOs
cachedUtxosAtAddress ctx@(CachedQueryUTxO cache _ _ q (GYLog log' _)) addr mAssetClass = do
  m <- Cache.lookup' cache (addr, mAssetClass)
  case m of
    Nothing  -> do
      log' mempty GYDebug $ "address not cached: " <> show addr
      res <- gyQueryUtxosAtAddress' q addr mAssetClass
      Cache.insert cache (addr, mAssetClass) res
      storeCacheUTxO ctx res
      return  res
    Just res -> do
      log' mempty GYDebug $ "address cached:"  <> show addr
      return res

cachedUtxosAtPaymentCred :: CachedQueryUTxO -> GYPaymentCredential -> IO GYUTxOs
cachedUtxosAtPaymentCred ctx@(CachedQueryUTxO _ _ cache q (GYLog log' _)) cred = do
  m <- Cache.lookup' cache cred
  case m of
    Nothing    -> do
      log' mempty GYDebug $ "payment credential not cached: " <> show cred
      res <- gyQueryUtxosAtPaymentCredential' q cred
      Cache.insert cache cred res
      storeCacheUTxO ctx res
      return  res
    Just res -> do
      log' mempty GYDebug $ "payment credential cached:"  <> show cred
      return res

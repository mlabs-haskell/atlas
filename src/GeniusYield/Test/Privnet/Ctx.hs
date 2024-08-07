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
    -- * User
    User (..),
    CreateUserConfig (..),
    ctxUsers,
    userPkh,
    userPaymentPkh,
    userStakePkh,
    userVKey,
    userPaymentVKey,
    userStakeVKey,
    -- * Operations
    ctxRun,
    ctxRunQuery,
    ctxRunBuilder,
    ctxRunBuilderWithCollateral,
    ctxSlotOfCurrentBlock,
    ctxWaitNextBlock,
    ctxWaitUntilSlot,
    ctxProviders,
    ctxSlotConfig,
    -- * Helpers
    newTempUserCtx,
    ctxQueryBalance,
    findOutput,
    addRefScriptCtx,
    addRefInputCtx,
) where

import qualified Cardano.Api                as Api
import           Data.Default               (Default (..))
import qualified Data.Map.Strict            as Map

import qualified GeniusYield.Examples.Limbo as Limbo
import           GeniusYield.HTTP.Errors    (someBackendError)
import           GeniusYield.Imports
import           GeniusYield.Providers.Node
import           GeniusYield.TxBuilder
import           GeniusYield.Types
import           Test.Tasty.HUnit           (assertFailure)

data CreateUserConfig =
     CreateUserConfig
       { -- | Create collateral output of 5 ada?
         cucGenerateCollateral :: !Bool,
         -- | Create a stake key for the user?
         cucGenerateStakeKey   :: !Bool
       }

instance Default CreateUserConfig where
   def = CreateUserConfig { cucGenerateCollateral = False, cucGenerateStakeKey = False }


data Ctx = Ctx
    { ctxNetworkInfo       :: !GYNetworkInfo
    , ctxInfo              :: !Api.LocalNodeConnectInfo
    -- FIXME: There are now multiple genesis users (since cardano-testnet usage).
    , ctxUserF             :: !User  -- ^ Funder. All other users begin with same status of funds.
    , ctxUser2             :: !User
    , ctxUser3             :: !User
    , ctxUser4             :: !User
    , ctxUser5             :: !User
    , ctxUser6             :: !User
    , ctxUser7             :: !User
    , ctxUser8             :: !User
    , ctxUser9             :: !User
    , ctxGold              :: !GYAssetClass  -- ^ asset used in tests
    , ctxIron              :: !GYAssetClass  -- ^ asset used in tests
    , ctxLog               :: !GYLogConfiguration
    , ctxLookupDatum       :: !GYLookupDatum
    , ctxAwaitTxConfirmed  :: !GYAwaitTx
    , ctxQueryUtxos        :: !GYQueryUTxO
    , ctxGetParams         :: !GYGetParameters
    }

ctxNetworkId :: Ctx -> GYNetworkId
ctxNetworkId Ctx {ctxNetworkInfo} = GYPrivnet ctxNetworkInfo

-- | List of context sibling users - all of which begin with same balance.
-- FIXME: Some of these users are actually genesis users.
ctxUsers :: Ctx -> [User]
ctxUsers ctx = ($ ctx) <$> [ctxUser2, ctxUser3, ctxUser4, ctxUser5, ctxUser6, ctxUser7, ctxUser8, ctxUser9]

-- | Creates a new user with the given balance. Note that the actual balance which this user get's could be more than what is provided to satisfy minimum ada requirement of a UTxO.
newTempUserCtx:: Ctx
              -> User            -- ^ User which will fund this new user.
              -> GYValue         -- ^ Describes balance of new user.
              -> CreateUserConfig
              -> IO User
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

  ctxRun ctx fundUser $ do
    txBody <- buildTxBody $
      if cucGenerateCollateral then
        mustHaveOutput (mkGYTxOutNoDatum newAddr (otherValue <> (valueFromLovelace adaInValue `valueMinus` collateralValue))) <>
        mustHaveOutput (mkGYTxOutNoDatum newAddr collateralValue)
      else
        mustHaveOutput (mkGYTxOutNoDatum newAddr fundValue)
    signAndSubmitConfirmed_ txBody

  pure $ User' {userPaymentSKey' = newPaymentSKey, userAddr = newAddr, userStakeSKey' = newStakeSKey}


ctxRun :: Ctx -> User -> GYTxMonadIO a -> IO a
ctxRun ctx User' {..} = runGYTxMonadIO (ctxNetworkId ctx) (ctxProviders ctx) userPaymentSKey' userStakeSKey' [userAddr] userAddr Nothing

ctxRunQuery :: Ctx -> GYTxQueryMonadIO a -> IO a
ctxRunQuery ctx = runGYTxQueryMonadIO (ctxNetworkId ctx) (ctxProviders ctx)

ctxRunBuilder :: Ctx -> User -> GYTxBuilderMonadIO a -> IO a
ctxRunBuilder ctx User' {..} = runGYTxBuilderMonadIO (ctxNetworkId ctx) (ctxProviders ctx) [userAddr] userAddr Nothing

-- | Variant of `ctxRun` where caller can also give the UTxO to be used as collateral.
ctxRunBuilderWithCollateral :: Ctx
                     -> User
                     -> GYTxOutRef  -- ^ Reference to UTxO to be used as collateral.
                     -> Bool        -- ^ To check whether this given collateral UTxO has value of exact 5 ada? If it doesn't have exact 5 ada, it would be ignored.
                     -> GYTxBuilderMonadIO a
                     -> IO a
ctxRunBuilderWithCollateral ctx User' {..} coll toCheck5Ada = runGYTxBuilderMonadIO
    (ctxNetworkId ctx)
    (ctxProviders ctx)
    [userAddr]
    userAddr
    (Just (coll, toCheck5Ada))

ctxSlotOfCurrentBlock :: Ctx -> IO GYSlot
ctxSlotOfCurrentBlock (ctxProviders -> providers) =
    gyGetSlotOfCurrentBlock providers

ctxWaitNextBlock :: Ctx -> IO ()
ctxWaitNextBlock (ctxProviders -> providers) = void $ gyWaitForNextBlock providers

ctxWaitUntilSlot :: Ctx -> GYSlot -> IO ()
ctxWaitUntilSlot (ctxProviders -> providers) slot = void $ gyWaitUntilSlot providers slot

ctxSlotConfig :: Ctx -> IO GYSlotConfig
ctxSlotConfig ctx = ctxRunQuery ctx slotConfig

ctxQueryBalance :: Ctx -> User -> IO GYValue
ctxQueryBalance ctx u = ctxRunQuery ctx $ do
    queryBalance $ userAddr u

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

-- | Function to find for the first locked output in the given `GYTxBody` at the given `GYAddress`.
findOutput :: GYAddress -> GYTxBody -> IO GYTxOutRef
findOutput addr txBody = do
    let utxos = txBodyUTxOs txBody
    maybe (assertFailure "expecting an order in utxos") return $
        findFirst (\utxo -> if utxoAddress utxo == addr then Just (utxoRef utxo) else Nothing) $ utxosToList utxos

-- | Function to add for a reference script. It adds the script in so called "Always failing" validator so that it won't be later possible to spend this output. There is a slight optimisation here in that if the desired reference script already exists then we don't add another one and return the reference for the found one else, we create a new one.
addRefScriptCtx :: Ctx                 -- ^ Given context.
                -> User                -- ^ User which will execute the transaction (if required).
                -> GYScript 'PlutusV2  -- ^ Given script.
                -> IO GYTxOutRef       -- ^ Returns the reference for the desired output.
addRefScriptCtx ctx user script = ctxRun ctx user $ do
  txBodyRefScript <- Limbo.addRefScript script >>= traverse buildTxBody
  case txBodyRefScript of
    Left ref -> pure ref
    Right body -> do
      let refs = Limbo.findRefScriptsInBody body
      ref <- case Map.lookup (Some script) refs of
        Just ref -> return ref
        Nothing  -> throwAppError $ someBackendError "Shouldn't happen: no ref in body"
      signAndSubmitConfirmed_ body
      pure ref

-- | Function to add for a reference input.
addRefInputCtx :: Ctx            -- ^ Given context.
               -> User           -- ^ User which will execute this transaction.
               -> Bool           -- ^ Whether to inline the datum.
               -> GYAddress      -- ^ Address to put this output at.
               -> GYDatum        -- ^ The datum to put.
               -> IO GYTxOutRef  -- ^ Returns the reference for the required output.
addRefInputCtx ctx user toInline addr ourDatum = ctxRun ctx user $ do
  txBody <- buildTxBody $ mustHaveOutput (GYTxOut addr mempty (Just (ourDatum, if toInline then GYTxOutUseInlineDatum else GYTxOutDontUseInlineDatum)) Nothing)
  let utxos = utxosToList $ txBodyUTxOs txBody
      ourDatumHash = hashDatum ourDatum
      mRefInputUtxo = find (\utxo ->
        case utxoOutDatum utxo of
          GYOutDatumHash dh  -> ourDatumHash == dh
          GYOutDatumInline d -> ourDatum == d
          GYOutDatumNone     -> False
        ) utxos
  case mRefInputUtxo of
    Nothing               -> throwAppError $ someBackendError "Shouldn't happen: Couldn't find desired UTxO in tx outputs"
    Just GYUTxO {utxoRef} -> do
      signAndSubmitConfirmed_ txBody
      pure utxoRef

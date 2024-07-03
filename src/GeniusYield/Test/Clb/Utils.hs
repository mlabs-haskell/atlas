module GeniusYield.Test.Clb.Utils (mkTestFor, waitUntilSlot) where


import           Prettyprinter                    (PageWidth(AvailablePerLine), defaultLayoutOptions, layoutPageWidth,
                                                   layoutPretty)
import           Prettyprinter.Render.String      (renderString)

import qualified Cardano.Ledger.Api               as L
import qualified Clb                              (Clb, ClbState (mockInfo), MockConfig,
                                                   checkErrors, defaultBabbage, initClb, intToKeyPair, ppLog, runClb,
                                                   waitSlot)

import qualified Test.Cardano.Ledger.Core.KeyPair as TL

import qualified Test.Tasty                       as Tasty
import           Test.Tasty.HUnit                 (assertFailure, testCaseInfo)

import           GeniusYield.Test.Clb
import           GeniusYield.Test.Utils
import           GeniusYield.Types


-------------------------------------------------------------------------------
-- helpers
-------------------------------------------------------------------------------

{- | Given a test name, runs the trace for every wallet, checking there weren't
     errors.
-}
mkTestFor :: String -> (Wallets -> GYTxMonadClb a) -> Tasty.TestTree
mkTestFor name action =
    testNoErrorsTraceClb v w Clb.defaultBabbage name $ do
      asClb pureGen (w1 wallets) $ action wallets
  where
    v = valueFromLovelace 1_000_000_000_000_000 <>
        fakeGold                  1_000_000_000 <>
        fakeIron                  1_000_000_000

    w = valueFromLovelace 1_000_000_000_000 <>
        fakeGold                  1_000_000 <>
        fakeIron                  1_000_000

    wallets :: Wallets
    wallets = Wallets (mkSimpleWallet (Clb.intToKeyPair 1))
                      (mkSimpleWallet (Clb.intToKeyPair 2))
                      (mkSimpleWallet (Clb.intToKeyPair 3))
                      (mkSimpleWallet (Clb.intToKeyPair 4))
                      (mkSimpleWallet (Clb.intToKeyPair 5))
                      (mkSimpleWallet (Clb.intToKeyPair 6))
                      (mkSimpleWallet (Clb.intToKeyPair 7))
                      (mkSimpleWallet (Clb.intToKeyPair 8))
                      (mkSimpleWallet (Clb.intToKeyPair 9))

    -- | Helper for building tests
    testNoErrorsTraceClb :: GYValue -> GYValue -> Clb.MockConfig -> String -> Clb.Clb a -> Tasty.TestTree
    testNoErrorsTraceClb funds walletFunds cfg msg act =
        testCaseInfo msg
            $ maybe (pure mockLog) assertFailure
            $ mbErrors >>= \errors -> pure (mockLog <> "\n\nError :\n-------\n" <>  errors)
        where
            -- _errors since we decided to store errors in the log as well.
            (mbErrors, mock) = Clb.runClb (act >> Clb.checkErrors) $ Clb.initClb cfg (valueToApi funds) (valueToApi walletFunds)
            mockLog = "\nEmulator log :\n--------------\n" <> logString
            options = defaultLayoutOptions { layoutPageWidth = AvailablePerLine 150 1.0}
            logDoc = Clb.ppLog $ Clb.mockInfo mock
            logString = renderString $ layoutPretty options logDoc


    mkSimpleWallet :: TL.KeyPair r L.StandardCrypto -> Wallet
    mkSimpleWallet kp = let skey = paymentSigningKeyFromLedgerKeyPair kp
        in Wallet
            { walletPaymentSigningKey = skey
            , walletAddress           = addressFromPaymentKeyHash GYTestnetPreprod
                                        . paymentKeyHash
                                        $ paymentVerificationKey skey
            , walletStakeSigningKey   = Nothing
            }

{- -----------------------------------------------------------------------------
  CLB
----------------------------------------------------------------------------- -}

-- | Waits until a certain 'GYSlot'.
-- Silently returns if the given slot is greater than the current slot.
waitUntilSlot :: GYSlot -> GYTxMonadClb ()
waitUntilSlot slot = liftClb $ Clb.waitSlot $ slotToApi slot

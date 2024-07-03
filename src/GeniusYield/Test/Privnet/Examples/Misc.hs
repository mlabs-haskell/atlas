{-|
Module      : GeniusYield.Test.Privnet.Examples.Misc
Copyright   : (c) 2023 GYELD GMBH
License     : Apache 2.0
Maintainer  : support@geniusyield.co
Stability   : develop

-}

module GeniusYield.Test.Privnet.Examples.Misc (tests) where

import           Control.Concurrent                     (threadDelay)
import           Test.Tasty                             (TestTree, testGroup)
import           Test.Tasty.HUnit                       (testCaseSteps)

import           GeniusYield.Imports
import           GeniusYield.Scripts.TestToken
import           GeniusYield.Types

import           GeniusYield.Examples.Limbo             (addRefScript)
import           GeniusYield.Test.Privnet.Asserts
import           GeniusYield.Test.Privnet.Ctx
import           GeniusYield.Test.Privnet.Examples.Gift (resolveRefScript)
import           GeniusYield.Test.Privnet.Setup
import           GeniusYield.TxBuilder.Class
import GeniusYield.Test.Utils (Wallets(..))

tests :: Setup -> TestTree
tests setup = testGroup "misc"
    [ testCaseSteps "Reference script for minting policy" $ \info -> withSetup setup info $ \ctx -> do

        utxoAsParam <- ctxRunC ctx (w2 $ ctxUsers ctx) $ someUTxO PlutusV1
        let amt    = 1
            tn     = "mintByRef"
            policy = testTokenPolicy amt tn utxoAsParam
            policyAsScript = mintingPolicyToScript policy
            ac = GYToken (mintingPolicyId policy) tn

        txBodyRefScript <- ctxRunF ctx (ctxUserF ctx) $ addRefScript policyAsScript

        refScript <- resolveRefScript ctx txBodyRefScript (Some policyAsScript)
        -- wait a tiny bit.
        threadDelay 1_000_000

        balance <- ctxQueryBalance ctx (w2 $ ctxUsers ctx)

        txBodyMint <- ctxRunI ctx (w2 $ ctxUsers ctx) $ do
            return $
                 mustHaveInput (GYTxIn utxoAsParam GYTxInWitnessKey)
              <> mustMint (GYMintReference refScript policyAsScript) unitRedeemer tn amt
        void $ submitTx ctx (w2 $ ctxUsers ctx) txBodyMint

        -- wait a tiny bit.
        threadDelay 1_000_000

        balance' <- ctxQueryBalance ctx (w2 $ ctxUsers ctx)

        let diff = valueMinus balance' balance

        assertEqual "Must have gained 1 mint token" (valueAssetClass diff ac) 1
    ]

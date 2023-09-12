module GeniusYield.Test.Providers.Mashup
  ( providersMashupTests
  ) where

import qualified Cardano.Api                  as Api
import           Control.Concurrent           (threadDelay)
import           Control.Exception            (handle)
import           Data.Default                 (def)
import           Data.List                    (isInfixOf)
import           Data.Maybe                   (fromJust)
import qualified Data.Set                     as Set (delete, fromList)
import           GeniusYield.GYConfig
import           GeniusYield.Imports
import           GeniusYield.Providers.Common (SubmitTxException, datumFromCBOR)
import           GeniusYield.TxBuilder
import           GeniusYield.Types
import           Test.Tasty                   (TestTree, testGroup)
import           Test.Tasty.HUnit             (assertBool, assertFailure,
                                               testCase)

providersMashupTests :: [GYCoreConfig] -> TestTree
providersMashupTests configs =
  testGroup "Providers Mashup"
    [ testCase "Datum lookup - GYLookupDatum" $ do
        threadDelay 1_000_000
        dats <- forM configs $ \config -> withCfgProviders config mempty $ \GYProviders {..} -> fromJust <$> gyLookupDatum "a7ed3e81ef2e98a85c8d5649ed6344b7f7b36a31103ab18395ef4e80b8cac565"  -- A datum hash seen at always fail script's address.
        assertBool "Datums are not all equal" $ all (== head dats) (tail dats)
    , testCase "Parameters" $ do
        paramsList <- forM configs $ \config -> withCfgProviders config mempty $ \provider -> do
           threadDelay 1_000_000
           protocolParams <- gyGetProtocolParameters provider
           threadDelay 1_000_000
           systemStart <- gyGetSystemStart provider
           threadDelay 1_000_000
           Api.EraHistory mode interpreter <- gyGetEraHistory provider  -- `mode` here doesn't appear to have `Eq` instance, comparing via it's `Show` instance should be fine.
           threadDelay 1_000_000
           stakePools <- gyGetStakePools provider
           threadDelay 1_000_000
           slotConfig' <- gyGetSlotConfig provider
           pure (protocolParams, systemStart, (show mode, interpreter), stakePools, slotConfig')
        assertBool "Parameters are not all equal" $ all (== head paramsList) (tail paramsList)
    , testCase "Query UTxOs" $ do
        let
            -- Blockfrost is unable to get the preimage of the involved datum hash, thus it's being deleted for in our set so that test still passes.
            utxoBug1 = (GYUTxO {utxoRef = "6d2174d3956d8eb2b3e1e198e817ccf1332a599d5d7320400bfd820490d706be#0", utxoAddress = unsafeAddressFromText "addr_test1wpgexmeunzsykesf42d4eqet5yvzeap6trjnflxqtkcf66g0kpnxt", utxoValue = valueFromList [(GYLovelace,50000000)], utxoOutDatum = GYOutDatumHash "15461aa490b224fe541f3568e5d7704e0d88460cde9f418f700e2b6864d8d3c9", utxoRefScript = Nothing},Just (either (error "absurd - Mashup: parsing datum failed") id $ datumFromCBOR "19077a"))
            utxoBug2 = (GYUTxO {utxoRef = "6d2174d3956d8eb2b3e1e198e817ccf1332a599d5d7320400bfd820490d706be#0", utxoAddress = unsafeAddressFromText "addr_test1wpgexmeunzsykesf42d4eqet5yvzeap6trjnflxqtkcf66g0kpnxt", utxoValue = valueFromList [(GYLovelace,50000000)], utxoOutDatum = GYOutDatumHash "15461aa490b224fe541f3568e5d7704e0d88460cde9f418f700e2b6864d8d3c9", utxoRefScript = Nothing}, Nothing)
        utxosProviders <- forM configs $ \config -> withCfgProviders config mempty $ \provider -> do
          let alwaysFailAddress = unsafeAddressFromText "addr_test1wpgexmeunzsykesf42d4eqet5yvzeap6trjnflxqtkcf66g0kpnxt"
              myAddrList = [alwaysFailAddress]  -- always fail script's address. It has all the cases, reference scripts, inline datums, many UTxOs, etc.
          threadDelay 1_000_000
          utxosAtAddresses' <- gyQueryUtxosAtAddresses provider myAddrList
          threadDelay 1_000_000
          utxosAtAddressesWithDatums' <- gyQueryUtxosAtAddressesWithDatums provider myAddrList
          -- All the below refs were taken from always fail address.
          let refWithDatumHash = "dc1c7958f94b7a458dffa224d18b5b8464f81f6360913c26eca4199f67ac6435#1"
              outputRefs =
                [ "930de0fd6718fc87cd99be663f1b1dd099cad6cec3dede49d82b3554a1e8eb86#0"  -- Contains reference script.
                ,  refWithDatumHash -- Contains datum hash.
                , "930de0fd6718fc87cd99be663f1b1dd099cad6cec3dede49d82b3554a1e8eb86#0"  -- Contains inline datum.
                ]
          threadDelay 1_000_000
          utxosAtRefs <- gyQueryUtxosAtTxOutRefs provider outputRefs
          threadDelay 1_000_000
          utxoRefsAtAddress' <- gyQueryUtxoRefsAtAddress provider alwaysFailAddress
          threadDelay 1_000_000
          utxosAtRefsWithDatums' <- gyQueryUtxosAtTxOutRefsWithDatums provider outputRefs
          threadDelay 1_000_000
          utxoAtRefWithDatum' <- runGYTxQueryMonadNode (cfgNetworkId config) provider $ utxoAtTxOutRefWithDatum refWithDatumHash
          pure (utxosAtAddresses', utxoBug2 `Set.delete` (utxoBug1 `Set.delete` Set.fromList utxosAtAddressesWithDatums'), utxosAtRefs, Set.fromList utxoRefsAtAddress', Set.fromList utxosAtRefsWithDatums', utxoAtRefWithDatum')
        assertBool "Utxos are not all equal" $ all (== head utxosProviders) (tail utxosProviders)
    , testCase "Checking presence of error message when submitting an invalid transaction" $ do
        let
            handler :: SubmitTxException -> IO GYTxId
            handler e =
              let errorText = show e
              in ( if "BadInputsUTxO" `isInfixOf` errorText then
                     pure "6c751d3e198c5608dfafdfdffe16aeac8a28f88f3a769cf22dd45e8bc84f47e8"  -- Any transaction ID.
                   else error "Not satisfied"
                 )
        forM_ configs $ \config -> withCfgProviders config mempty $ \GYProviders {..} -> do
          threadDelay 1_000_000
          handle handler $ gySubmitTx . fromRight (error "absurd") $ txFromHexBS "84a30083825820cd1fd0900870f19cba004ad73996d5f01f22790c7e2efcd46b7d1bacbf97ca6d00825820cd1fd0900870f19cba004ad73996d5f01f22790c7e2efcd46b7d1bacbf97ca6d01825820cd1fd0900870f19cba004ad73996d5f01f22790c7e2efcd46b7d1bacbf97ca6d020183a200581d6007fb2b78b3917d3f6bfa3a59de61f3c225cbf0d5564a1cbc6f96d6eb011a3b3b81d5a200581d6007fb2b78b3917d3f6bfa3a59de61f3c225cbf0d5564a1cbc6f96d6eb011a002dc6c0a200581d6007fb2b78b3917d3f6bfa3a59de61f3c225cbf0d5564a1cbc6f96d6eb011a001bc75b021a0002bd25a10081825820b8ee2dc03ba6f88baa7e4430675df3e559d63a1282f7763de71227041e351023584020313e571def2f09145ae6b0eb26e99260d14885789929a354fea4a585d5f053fc2eae86d36f484269b0d95a25abb7acc3b15033565d00afd83af83f24d9be0ef5f6"
    , testCase "Submitting a valid transaction" $ do
        skey <- readPaymentSigningKey "preprod-submit-test-wallet.skey"
        let nid = GYTestnetPreprod
            senderAddress = addressFromPubKeyHash GYTestnetPreprod $ pubKeyHash $ paymentVerificationKey skey
        forM_ configs $ \config -> withCfgProviders config mempty $ \provider@GYProviders {..} -> do
          threadDelay 1_000_000
          senderUTxOs <- runGYTxQueryMonadNode nid provider $ utxosAtAddress senderAddress
          threadDelay 1_000_000
          let totalSenderFunds = foldMapUTxOs utxoValue senderUTxOs
              valueToSend     = totalSenderFunds `valueMinus` valueFromLovelace 5_000_000
              -- This way, UTxO distribution in test wallet should remain same.
          txBody <- runGYTxMonadNode nid provider [senderAddress] senderAddress Nothing $ pure $ mustHaveOutput $ mkGYTxOutNoDatum @'PlutusV2 senderAddress valueToSend
          threadDelay 1_000_000
          let signedTxBody = signGYTxBody txBody [skey]
          printf "Signed tx: %s\n" (txToHex signedTxBody)
          tid    <- gySubmitTx signedTxBody
          printf "Submitted tx: %s\n" tid
          gyAwaitTxConfirmed (GYAwaitTxParameters {maxAttempts = 20, confirmations = 1, checkInterval = 10_000_000}) tid
    , testCase "Await Tx Confirmed - Submitted Tx" $
        forM_ configs $ \config -> withCfgProviders config mempty $
            \GYProviders {..} -> gyAwaitTxConfirmed def "c67b57d63e846c6dc17f0c2647893d5f7376690cde62b8b392ecfcb75a4697e2"  -- A transaction id which generated UTxO at the always fail address. Thus it is guaranteed to be always there (unless of course network is respun).
    , testCase "Await Tx Confirmed - Timeout for non-existing Tx" $ do
        let handleAwaitTxException (GYAwaitTxException _) = return ()
        forM_ configs $ \config -> withCfgProviders config mempty $
          \GYProviders {..} -> handle handleAwaitTxException $ do
              gyAwaitTxConfirmed def{maxAttempts = 2, checkInterval = 1_000_000} "9b50152cc5cfca6a842f32b1e886a3ffdc1a1704fa87a15a88837996b6a9df36"  -- <-- A non-existing transaction id.
              assertFailure "Exepected GYAwaitTxException to be raised"
    ]

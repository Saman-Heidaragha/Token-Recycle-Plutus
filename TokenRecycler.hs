module Contracts.TokenRecycler where

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedString #-}

import Data.Map qualified as Map
import Jambhala.Plutus
import Jambhala.Utils



type SenderPKH = PubKeyHash

type HostPKH = PubKeyHash

type TokenValue = Value


  -- Define RecycleDatum data type
data RecycleDatum = RecycleDatum 
  { recieverPKH :: SenderPKH
  , tokenValue :: TokenValue
  , policyId :: CurrencySymbol
  }
PlutusTx.makeIsDataIndexed  ''RecycleDatum [(''RecycleDatum, 0)]


-- | Helper function to check if the given value has at least one token of the given currency symbol and token name.
hasToken :: CurrencySymbol -> TokenName -> Value -> Bool
hasToken _ _ v = pany (\(_, _, q) -> q #>= 1) $ flattenValue v
{-# INLINEABLE hasToken #-}

setReceiverAddress :: ScriptContext -> Maybe PubKeyHash
setReceiverAddress (ScriptContext txInfo _) =
    listToMaybe $ txInfoSignatories txInfo
{-#INLINABLE setReceiverAddress #-}


    -- Function to calculate the reward based on the output values in txInfo
rewardX :: ScriptContext -> Value
rewardX (ScriptContext txInfo _) =
  case txInfoOutputs txInfo of
    [] -> mempty
    (rewardTxOut:_) -> scaleValueByThree $ txOutValue rewardTxOut
  where
    scaleValueByThree :: Value -> Value
    scaleValueByThree value = scale 3 value
{-#INLINABLE rewardX #-}


validatorR :: SenderPKH -> Datum -> Redeemer -> ScriptContext -> Bool
validatorR senderPKH datum _ ctx@(ScriptContext txInfo _) =
  case setReceiverAddress ctx of
    Nothing -> False
    Just receiverPKH ->
      let rewardValue = rewardX ctx
          datum = RecycleDatum
          outputValue = case txInfoOutputs txInfo of
            [] -> mempty
            (output : _) -> txOutValue output
      in  hasToken (CurrencySymbol "") (TokenName "") outputValue
          && outputValue #== rewardValue
          && receiverPKH #== senderPKH
{-#INLINABLE validatorR #-}

untypedLambda :: SenderPKH -> UntypedValidator  
untypedLambda = mkUntypedValidator . validatorR
{-#INLINABLE untypedLambda #-}

type TokenRecycle = ValidatorContract "cyc"

sample :: SenderPKH
sample = "7fee02606ae2e089831ec9c574d92b55d1ccec80a5a73c1230fde947"

mkRecycleDatum :: SenderPKH -> TokenValue -> CurrencySymbol -> RecycleDatum
mkRecycleDatum senderPKH tokenValue cr =
  RecycleDatum
    { recieverPKH = senderPKH
    , tokenValue = tokenValue
    , policyId = cr
    }
          
compiledValidator :: SenderPKH -> TokenRecycle
compiledValidator pkh  = mkValidatorContract ($$(compile[|| untypedLambda ||]) `applyCode` liftCode pkh)

exports :: JambExports
exports =
  export
    (defExports $ compiledValidator sample)
      { dataExports =
          [ mkRecycleDatum sample (singleton  (CurrencySymbol "636861726c6573") "CYC" 50) policy `toJSONfile` "recycleData1"
          ]
      }
  where pkh = sample 
        policy = "847f25daf4873bde08f98368b3b557ca104dc71a625b01c65c928b4e"

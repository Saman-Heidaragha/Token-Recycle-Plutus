use aiken/hash.{Blake2b_224, Hash}
use aiken/transaction.{OutputReference, ScriptContext, Transaction, Input, Output}
use aiken/transaction/value.{PolicyId, AssetName, Value}
use aiken/transaction/credential.{VerificationKey}

type RewardToken {
  policy_id: PolicyId,
  token_name: AssetName,
  quantity: Int,
}

fn check_input(
  policy_id: PolicyId,
  token_name: AssetName,
  quantity: Int,
) -> Bool {
  quantity >= 1
  }


validator(sender: Hash<Blake2b_224, VerificationKey>) {
  fn cs(
    policy_id: PolicyId,
    token_name: AssetName,
    quantity: Int,
  ) -> Bool {
      check_input(
        policy_id, 
        token_name, 
        quantity,
      )?
                                  
  }
}

//fn send_reward( )

//validator(hash: Hash<Blake2b_224, Script>) {
//  fn reward_send()
//}
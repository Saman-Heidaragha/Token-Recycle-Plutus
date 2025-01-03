# Token Recycler Contract

## Overview

The Token Recycler contract is written in Plutus. It allows users to send tokens to the contract, which then calculates a reward based on the amount of tokens sent and returns the tokens to the user.

## Use Case

This contract is useful for encouraging users to recycle tokens. For instance, you might want to incentivize users to recycle specific tokens, and this contract can provide rewards for doing so. The reward given is three times the value of the tokens sent.

## How It Works

1. **Send Tokens**: A user sends tokens to the contract.
2. **Calculate Reward**: The contract calculates a reward based on the value of the tokens sent. The reward is three times the value of the tokens.
3. **Return Tokens**: The contract sends the tokens back to the user along with the calculated reward.

## Steps to Interact with the Contract

### 1. Prepare the Tokens

Make sure you have the tokens you want to send to the contract. These tokens should be of a type that the contract recognizes, matching the expected `CurrencySymbol` and `TokenName`.

### 2. Send Tokens to the Contract

When you send tokens to the contract, it captures the details of the transaction. The contract expects the following details:

- **SenderPKH**: The public key hash of the sender.
- **TokenValue**: The value of the tokens being sent.
- **PolicyId**: The policy ID of the token.

### 3. Contract Processing

Upon receiving the tokens, the contract performs several checks and calculations:

- **Check for Tokens**: It verifies that the input contains the required tokens.
- **Calculate Reward**: It calculates the reward, which is three times the quantity of tokens.
- **Validate Transaction**: It ensures the transaction includes the expected output, which is the reward calculated and sent back to the sender.

### 4. Receive Tokens

If all checks pass, the contract sends back the tokens to the sender's address along with the calculated reward.

## Key Components

### Data Types

- **RecycleDatum**: Contains the receiver's public key hash, the token value, and the policy ID.
- **SenderPKH**: Public key hash of the sender.
- **TokenValue**: Value of the tokens.
- **PolicyId**: Policy ID of the token.

### Helper Functions

- **hasToken**: Checks if a value contains at least one token of a specified currency symbol and token name.
- **setReceiverAddress**: Sets the receiver address based on the script context.
- **rewardX**: Calculates the reward by multiplying the token value by three.

### Main Validator Function

- **validatorR**: Validates the transaction by checking:
  - If the receiver address is set.
  - If the output value contains at least one token.
  - If the output value matches the calculated reward.
  - If the receiver's public key hash matches the sender's public key hash.

### Untyped Validator

- **untypedLambda**: Converts the typed validator function into an untyped validator.


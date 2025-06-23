# 🕵️ Truthdrop - Whistleblower NFT Bounties

> **Anonymous disclosures rewarded with NFTs and STX bounties** 🎯

Truthdrop is a decentralized platform built on Stacks that enables anonymous whistleblowing through a bounty system. Organizations and individuals can create bounties for specific information disclosures, while whistleblowers can submit evidence anonymously and receive NFT certificates plus STX rewards.

## 🌟 Features

- **🎯 Bounty Creation**: Create targeted bounties for specific information with STX rewards
- **🔒 Anonymous Submissions**: Submit evidence anonymously with cryptographic proof
- **🏆 NFT Certificates**: Receive unique NFTs as proof of disclosure submission
- **✅ Community Verification**: Decentralized verification system with reputation scoring
- **💰 Automatic Rewards**: Smart contract handles reward distribution automatically
- **📊 Reputation System**: Build reputation through verified disclosures
- **⏰ Time-bound Bounties**: Set expiration dates for bounty submissions

## 🚀 Getting Started

### Prerequisites

- [Clarinet](https://github.com/hirosystems/clarinet) installed
- Stacks wallet for testing

### Installation

```bash
git clone <your-repo>
cd truthdrop
clarinet check
```

### Testing

```bash
clarinet test
```

## 📖 Usage Guide

### Creating a Bounty 💼

```clarity
(contract-call? .truthdrop create-bounty 
  "Corporate Fraud Evidence" 
  "Looking for evidence of financial misconduct at XYZ Corp"
  u10000000  ;; 10 STX reward
  u1000      ;; Valid for 1000 blocks
  "corporate"
  true       ;; Verification required
  u200)      ;; Minimum evidence score
```

### Submitting a Disclosure 📝

```clarity
(contract-call? .truthdrop submit-disclosure 
  u1  ;; Bounty ID
  0x1234567890abcdef...)  ;; Evidence hash
```

### Verifying Disclosures ✅

````clarity
(contract-call? .truthdrop verify-disclosure 
  u1    ;;)# Truthdrop


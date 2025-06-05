# 🏞️ Smart Land Registry System

A decentralized land registry system built on Stacks blockchain that enables secure land title registration, ownership transfers, fractional ownership through shares, and dispute resolution mechanisms.

## 🚀 Features

- **🏡 Land Registration**: Register land titles with location, size, and type information
- **📋 Ownership Management**: Transfer land ownership between parties
- **💰 Fractional Ownership**: Buy and sell land shares for investment cooperatives
- **⚖️ Dispute Resolution**: File and resolve land disputes through governance
- **🗳️ Voting System**: Shareholders can vote on transfer proposals
- **👥 Authorized Registrars**: Control who can register new land titles

## 📋 Contract Functions

### Public Functions

#### `authorize-registrar`
Authorize a principal to register land (contract owner only)
```clarity
(authorize-registrar principal)
```

#### `register-land`
Register a new land title with fractional ownership capability
```clarity
(register-land location size land-type total-shares price-per-share)
```

#### `transfer-land-ownership`
Transfer complete ownership of land to another principal
```clarity
(transfer-land-ownership land-id new-owner transfer-price)
```

#### `buy-land-shares`
Purchase shares of a registered land property
```clarity
(buy-land-shares land-id shares-to-buy)
```

#### `file-dispute`
File a dispute regarding land ownership or boundaries
```clarity
(file-dispute land-id respondent description)
```

#### `resolve-dispute`
Resolve a pending dispute (contract owner only)
```clarity
(resolve-dispute dispute-id resolution)
```

#### `create-transfer-proposal`
Create a proposal for land transfer that shareholders can vote on
```clarity
(create-transfer-proposal land-id to-owner transfer-price expiry-blocks)
```

#### `vote-on-transfer`
Vote on a land transfer proposal (shareholders only)
```clarity
(vote-on-transfer land-id proposal-id vote)
```

### Read-Only Functions

- `get-land-info`: Get complete land registration information
- `get-land-shares`: Get share ownership for a specific shareholder
- `get-dispute-info`: Get dispute details
- `get-transfer-proposal`: Get transfer proposal information
- `is-authorized-registrar`: Check if a principal is authorized to register land
- `get-current-land-id`: Get the latest land ID counter
- `get-current-dispute-id`: Get the latest dispute ID counter
- `get-shareholder-vote`: Get voting record for transfer proposals

## 🛠️ Usage Examples

### Register New Land
```clarity
(contract-call? .land-registry register-land 
  "123 Main St, City, State" 
  u5000 
  "residential" 
  u100 
  u1000)
```

### Buy Land Shares
```clarity
(contract-call? .land-registry buy-land-shares u1 u10)
```

### File a Dispute
```clarity
(contract-call? .land-registry file-dispute 
  u1 
  'SP2J6ZY48GV1EZ5V2V5RB9MP66SW86PYKKNRV9EJ7 
  "Boundary dispute regarding property line")
```

## 🔧 Development Setup

1. Install Clarinet
```bash
curl -L https://github.com/hirosystems/clarinet/releases/download/v1.0.0/clarinet-linux-x64.tar.gz | tar xz
```

2. Initialize project
```bash
clarinet new land-registry
cd land-registry
```

3. Test the contract
```bash
clarinet test
```

4. Deploy locally
```bash
clarinet integrate
```

## 🏗️ Architecture

The system uses several data structures:
- **Land Registry**: Core land information and ownership
- **Land Shares**: Fractional ownership tracking
- **Disputes**: Conflict resolution system
- **Transfer Proposals**: Governance for ownership changes
- **Authorize

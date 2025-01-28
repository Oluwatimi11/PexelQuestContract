# Play-to-Earn Gaming Contract System Readme file
Version: 3.1.0
;;PixelRewards
;;PixelRewardsCore.clar

## Overview
The PixelRewards Gaming Contract System is an advanced Clarity-based contract for blockchain-powered gaming platforms. It integrates a secure token system, quest tracking, hero management, arena handling, item system, alliance formation, and emergency system controls. The contract provides players with incentives in the form of fungible tokens (Pixel Coins) and non-fungible tokens (Hero Badges, Quest Scrolls) while ensuring fair gameplay, error handling, and flexibility for game operators.

## Features
1. Fungible and Non-Fungible Token Integration:
    * Supports pixel-coin for rewards and fungible operations.
    * Issues pixel-hero, quest-scroll, and pixel-item NFTs to players.
2. Hero Management:
    * Register pixel heroes securely.
    * Maintain hero statistics such as pixel-xp, rank, total-pixel-coins, and quests completed.
3. Quest System:
    * Create quests with configurable rewards, rank requirements, and cooldowns.
    * Track hero quests and reward completion.
4. Arena Management:
    * Host arenas with configurable parameters such as entry fees, prize pools, and hero limits.
    * Validate arena details with built-in security.
5. Item System:
    * Create and manage in-game items with various attributes.
    * Allow heroes to equip and unequip items.
6. Alliance System:
    * Create and manage alliances between heroes.
    * Join, leave, and disband alliances.
7. Reward Distribution:
    * Calculate rewards based on hero rank and configurable multipliers.
8. Emergency System:
    * Pause and unpause the system during critical situations.
    * Assign and manage a game master.
9. Error Handling:
    * Comprehensive error codes for debugging and security.
    * Enforces parameter validation to prevent exploits.
10. Read-Only Functions:
    * Retrieve contract details, hero statistics, quests, arenas, items, and alliance information.

## Constants
* contract-owner: The owner of the contract.
* NEWBIE-RANK: Minimum rank for gameplay features.
* PIXEL-MASTER-RANK: Maximum achievable rank.
* DECIMALS: Number of decimals for token calculations.
* DAILY-QUEST-COOLDOWN: Cooldown period for daily quests.
* ARENA-DURATION: Default duration for arenas.
* MAX-INVENTORY-SLOTS: Maximum number of inventory slots per hero.

## Error Codes
| Code | Description |
|------|-------------|
| ERR-NOT-AUTHORIZED | Unauthorized action by a caller. |
| ERR-INVALID-PIXEL-HERO | Hero is not valid or not registered. |
| ERR-INSUFFICIENT-PIXELS | Hero has insufficient pixel coins. |
| ERR-INVALID-PIXEL-AMOUNT | Provided pixel amount is invalid. |
| ERR-QUEST-EXISTS | Quest already exists. |
| ERR-QUEST-COOLDOWN | Quest cooldown is active. |
| ERR-PIXEL-ARENA-NOT-FOUND | Specified arena does not exist. |
| ERR-PIXEL-ARENA-FULL | Arena has reached maximum capacity. |
| ERR-SYSTEM-PAUSED | The system is currently paused. |
| ERR-EXCEED-MAX-SUPPLY | Maximum token or hero supply exceeded. |
| ERR-ALLIANCE-EXISTS | Alliance already exists. |
| ERR-ALLIANCE-NOT-FOUND | Specified alliance does not exist. |
| ERR-ALLIANCE-FULL | Alliance has reached maximum capacity. |

## Data Variables
* total-pixel-coin-supply: Tracks total pixel coins issued.
* pixel-hero-counter: Total registered heroes.
* quest-scroll-counter: Total quests created.
* pixel-arena-counter: Total arenas hosted.
* pixel-item-counter: Total items created.
* alliance-counter: Total alliances formed.
* system-paused: System status (paused/active).
* game-master: Game master administrator.

## Key Functions

### Public Functions

#### Hero Management
* create-pixel-hero: Registers a new pixel hero with a unique badge and stats.

#### Quest System
* create-pixel-quest: Adds a new quest with specific rewards and requirements.
* complete-pixel-quest: Allows a hero to complete a quest and receive rewards.

#### Arena System
* create-pixel-arena: Creates a new arena with entry fees, prize pools, and limits.

#### Item System
* create-pixel-item: Creates a new in-game item with specific attributes.
* equip-item: Allows a hero to equip an item.
* unequip-item: Allows a hero to unequip an item.

#### Alliance System
* create-alliance: Creates a new alliance.
* join-alliance: Allows a hero to join an existing alliance.
* leave-alliance: Allows a hero to leave their current alliance.
* disband-alliance: Allows an alliance leader to disband the alliance.

#### Emergency Controls
* pause-system: Pauses the contract system.
* unpause-system: Resumes the contract system.
* set-emergency-admin: Updates the game master.

### Private Functions
* Security Checks:
    * is-contract-owner
    * is-game-master
* Validation:
    * validate-pixel-amount
    * validate-hero-rank

### Read-Only Functions
* get-contract-info: Retrieves contract-level details.
* get-hero-full-stats: Provides detailed stats for a hero.
* get-alliance-info: Retrieves information about a specific alliance.
* get-hero-alliance: Gets the alliance information for a specific hero.
* get-alliance-members: Retrieves the list of members in an alliance.

## Deployment Instructions
1. Install the Clarity CLI.
2. Deploy the contract using:
   ```bash
   clarity-cli launch PixelRewardsCore.clar

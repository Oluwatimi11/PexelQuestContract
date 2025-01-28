;; PixelQuest: An Immersive Play-to-Earn Gaming Contract System
;; Version: 3.1.0
;; Description: Enhanced system with robust error handling, security features, and immersive gaming elements

;; ============= Contract Configuration =============

;; ============= Constants =============
(define-constant contract-owner tx-sender)
(define-constant NEWBIE-RANK u1)
(define-constant PIXEL-MASTER-RANK u100)
(define-constant MAX-UINT u340282366920938463463374607431768211455)
(define-constant DECIMALS u6)

;; Game-specific constants
(define-constant DAILY-QUEST-COOLDOWN u144) ;; Approximately 24 hours in blocks
(define-constant ARENA-DURATION u720) ;; Approximately 5 days in blocks
(define-constant MAX-INVENTORY-SLOTS u50)
(define-constant ERR-INVALID-QUESTS u100) ;; Example error code for invalid quests
(define-constant ERR-INVALID-ARENAS u101) ;; Example error code for invalid arenas
(define-constant ACHIEVEMENT-TYPES (list 
    "quest_master"     ;; Complete X quests
    "arena_champion"   ;; Win X arena battles
    "pixel_collector"  ;; Collect X pixel-coins
    "alliance_leader"  ;; Lead an alliance for X blocks
    "item_collector"   ;; Collect X different items
))

;; Error Codes
(define-constant ERR-NOT-AUTHORIZED (err u101))
(define-constant ERR-INVALID-PIXEL-HERO (err u102))
(define-constant ERR-INSUFFICIENT-PIXELS (err u103))
(define-constant ERR-INVALID-PIXEL-AMOUNT (err u104))
(define-constant ERR-QUEST-EXISTS (err u105))
(define-constant ERR-QUEST-COOLDOWN (err u106))
(define-constant ERR-PIXEL-ARENA-NOT-FOUND (err u107))
(define-constant ERR-PIXEL-ARENA-FULL (err u108))
(define-constant ERR-PIXEL-ARENA-STARTED (err u109))
(define-constant ERR-PIXEL-ARENA-ENDED (err u110))
(define-constant ERR-INSUFFICIENT-ENTRY-FEE (err u111))
(define-constant ERR-ALREADY-REGISTERED (err u112))
(define-constant ERR-NOT-REGISTERED (err u113))
(define-constant ERR-INVALID-PARAMETERS (err u114))
(define-constant ERR-SYSTEM-PAUSED (err u115))
(define-constant ERR-EXCEED-MAX-SUPPLY (err u116))
(define-constant ERR-INVALID-RANK (err u117))
(define-constant ERR-TRANSFER-FAILED (err u118))
(define-constant ERR-INVENTORY-FULL (err u119))
(define-constant ERR-ITEM-NOT-FOUND (err u120))
(define-constant ERR-INSUFFICIENT-STAMINA (err u121))
(define-constant ERR-ALLIANCE-EXISTS (err u122))
(define-constant ERR-ALLIANCE-NOT-FOUND (err u123))
(define-constant ERR-ALLIANCE-FULL (err u124))
(define-constant ERR-NOT-ALLIANCE-LEADER (err u125))
(define-constant ERR-ALREADY-IN-ALLIANCE (err u126))
(define-constant ERR-NOT-IN-ALLIANCE (err u127))
(define-constant ERR-ACHIEVEMENT-EXISTS (err u128))
(define-constant ERR-INVALID-ACHIEVEMENT-TYPE (err u129))


;; ============= SFT Definitions =============
(define-fungible-token pixel-coin)
(define-non-fungible-token pixel-hero uint)
(define-non-fungible-token quest-scroll uint)
(define-non-fungible-token pixel-item uint)

;; ============= Data Variables =============
(define-data-var total-pixel-coin-supply uint u0)
(define-data-var pixel-hero-counter uint u0)
(define-data-var quest-scroll-counter uint u0)
(define-data-var pixel-arena-counter uint u0)
(define-data-var pixel-item-counter uint u0)
(define-data-var system-paused bool false)
(define-data-var game-master principal contract-owner)
(define-data-var alliance-counter uint u0)

;; ============= Data Maps =============
(define-map PixelHeroStats
    { hero: principal }
    {
        pixel-xp: uint,
        rank: uint,
        total-pixel-coins: uint,
        last-quest-block: uint,
        quests-completed: uint,
        join-block: uint,
        status: (string-ascii 10),
        stamina: uint,
        power: uint,
        agility: uint
    }
)

(define-map PixelQuests 
    { id: uint }
    {
        name: (string-ascii 50),
        description: (string-ascii 200),
        pixel-coin-reward: uint,
        required-rank: uint,
        cooldown-blocks: uint,
        status: (string-ascii 10),
        created-at: uint,
        stamina-cost: uint,
        power-requirement: uint,
        agility-requirement: uint
    }
)

(define-map HeroQuestLog
    { hero: principal, quest-id: uint }
    {
        completed: bool,
        completion-block: uint,
        times-completed: uint,
        last-reward: uint
    }
)

(define-map PixelArenas
    { arena-id: uint }
    {
        name: (string-ascii 50),
        description: (string-ascii 200),
        start-block: uint,
        end-block: uint,
        max-heroes: uint,
        current-heroes: uint,
        entry-fee: uint,
        prize-pool: uint,
        status: (string-ascii 20),
        min-rank-required: uint,
        created-at: uint,
        modified-at: uint,
        arena-type: (string-ascii 20)
    }
)

(define-map HeroInventory
    { hero: principal }
    {
        items: (list 50 uint),
        equipped-weapon: (optional uint),
        equipped-armor: (optional uint)
    }
)

(define-map PixelItems
    { item-id: uint }
    {
        name: (string-ascii 50),
        description: (string-ascii 200),
        item-type: (string-ascii 20),
        power-boost: uint,
        agility-boost: uint,
        stamina-boost: uint,
        rarity: (string-ascii 20)
    }
)

(define-map Alliances
    { alliance-id: uint }
    {
        name: (string-ascii 50),
        description: (string-ascii 200),
        leader: principal,
        members: (list 50 principal),
        created-at: uint,
        alliance-level: uint,
        total-power: uint
    }
)

(define-map HeroAlliance
    { hero: principal }
    { alliance-id: uint }
)

(define-map Achievements
    { achievement-id: uint }
    {
        name: (string-ascii 50),
        description: (string-ascii 200),
        achievement-type: (string-ascii 15),
        requirement: uint,
        pixel-coin-reward: uint,
        power-boost: uint,
        created-at: uint
    }
)

(define-map HeroAchievements
    { hero: principal, achievement-id: uint }
    {
        completed: bool,
        completion-block: uint,
        progress: uint
    }
)

(define-data-var achievement-counter uint u0)

;; ============= Security Functions =============
(define-private (is-contract-owner)
    (is-eq tx-sender contract-owner)
)

(define-private (is-game-master)
    (or (is-eq tx-sender contract-owner)
        (is-eq tx-sender (var-get game-master)))
)

(define-private (assert-not-paused)
    (ok (asserts! (not (var-get system-paused)) ERR-SYSTEM-PAUSED))
)

(define-private (validate-pixel-amount (amount uint))
    (and 
        (> amount u0)
        (<= amount MAX-UINT)
        (<= (+ amount (var-get total-pixel-coin-supply)) MAX-UINT)
    )
)

(define-private (validate-hero-rank (rank uint))
    (and 
        (>= rank NEWBIE-RANK)
        (<= rank PIXEL-MASTER-RANK)
    )
)

;; ============= Enhanced Hero Management =============
(define-public (create-pixel-hero (hero-name (string-ascii 20)))
    (begin
        (try! (assert-not-paused))
        (asserts! (not (is-pixel-hero tx-sender)) ERR-INVALID-PIXEL-HERO)
        
        (let ((new-hero-id (+ (var-get pixel-hero-counter) u1)))
            (asserts! (<= new-hero-id MAX-UINT) ERR-EXCEED-MAX-SUPPLY)
            
            ;; Mint pixel hero NFT
            (try! (nft-mint? pixel-hero new-hero-id tx-sender))
            
            ;; Initialize hero stats
            (map-set PixelHeroStats
                { hero: tx-sender }
                {
                    pixel-xp: u0,
                    rank: NEWBIE-RANK,
                    total-pixel-coins: u0,
                    last-quest-block: block-height,
                    quests-completed: u0,
                    join-block: block-height,
                    status: "active",
                    stamina: u100,
                    power: u10,
                    agility: u10
                }
            )
            
            ;; Initialize hero inventory
            (map-set HeroInventory
                { hero: tx-sender }
                {
                    items: (list),
                    equipped-weapon: none,
                    equipped-armor: none
                }
            )
            
            (var-set pixel-hero-counter new-hero-id)
            (ok new-hero-id)
        )
    )
)

;; ============= Enhanced Quest System =============
(define-public (create-pixel-quest 
    (name (string-ascii 50))
    (description (string-ascii 200))
    (pixel-coin-reward uint)
    (required-rank uint)
    (cooldown-blocks uint)
    (stamina-cost uint)
    (power-requirement uint)
    (agility-requirement uint)
)
    (begin
        (try! (assert-not-paused))
        (asserts! (is-game-master) ERR-NOT-AUTHORIZED)
        (asserts! (validate-pixel-amount pixel-coin-reward) ERR-INVALID-PIXEL-AMOUNT)
        (asserts! (validate-hero-rank required-rank) ERR-INVALID-RANK)
        
        (let ((new-quest-id (+ (var-get quest-scroll-counter) u1)))
            (asserts! (not (is-some (map-get? PixelQuests { id: new-quest-id }))) ERR-QUEST-EXISTS)
            
            (map-set PixelQuests
                { id: new-quest-id }
                {
                    name: name,
                    description: description,
                    pixel-coin-reward: pixel-coin-reward,
                    required-rank: required-rank,
                    cooldown-blocks: cooldown-blocks,
                    status: "active",
                    created-at: block-height,
                    stamina-cost: stamina-cost,
                    power-requirement: power-requirement,
                    agility-requirement: agility-requirement
                }
            )
            
            (var-set quest-scroll-counter new-quest-id)
            (ok new-quest-id)
        )
    )
)


;;============= Achievement Functions ========================
(define-public (create-achievement
    (name (string-ascii 50))
    (description (string-ascii 200))
    (achievement-type (string-ascii 15))
    (requirement uint)
    (pixel-coin-reward uint)
    (power-boost uint)
)
    (begin
        (try! (assert-not-paused))
        (asserts! (is-game-master) ERR-NOT-AUTHORIZED)
        (asserts! (is-some (index-of ACHIEVEMENT-TYPES achievement-type)) ERR-INVALID-ACHIEVEMENT-TYPE)
        
        (let ((new-achievement-id (+ (var-get achievement-counter) u1)))
            (map-set Achievements
                { achievement-id: new-achievement-id }
                {
                    name: name,
                    description: description,
                    achievement-type: achievement-type,
                    requirement: requirement,
                    pixel-coin-reward: pixel-coin-reward,
                    power-boost: power-boost,
                    created-at: block-height
                }
            )
            
            (var-set achievement-counter new-achievement-id)
            (ok new-achievement-id)
        )
    )
)

(define-public (complete-pixel-quest (quest-id uint))
    (let (
        (hero-stats (unwrap! (map-get? PixelHeroStats { hero: tx-sender }) ERR-INVALID-PIXEL-HERO))
        (quest (unwrap! (map-get? PixelQuests { id: quest-id }) ERR-QUEST-EXISTS))
        (quest-log (default-to 
            { completed: false, completion-block: u0, times-completed: u0, last-reward: u0 }
            (map-get? HeroQuestLog { hero: tx-sender, quest-id: quest-id })))
    )
        ;; Optimized validation checks in a single batch
        (asserts! (and
            (>= (get rank hero-stats) (get required-rank quest))
            (>= (- block-height (get last-quest-block hero-stats)) (get cooldown-blocks quest))
            (>= (get stamina hero-stats) (get stamina-cost quest))
            (>= (get power hero-stats) (get power-requirement quest))
            (>= (get agility hero-stats) (get agility-requirement quest))
        ) ERR-INVALID-PARAMETERS)
        
        ;; Batch update hero stats for better efficiency
        (let (
            (new-xp (+ (get pixel-xp hero-stats) (get pixel-coin-reward quest)))
            (new-total-coins (+ (get total-pixel-coins hero-stats) (get pixel-coin-reward quest)))
            (new-quests-completed (+ (get quests-completed hero-stats) u1))
            (new-stamina (- (get stamina hero-stats) (get stamina-cost quest)))
        )
            ;; Mint rewards and update stats in a single transaction
            (try! (mint-pixel-coins tx-sender (get pixel-coin-reward quest)))
            
            (map-set PixelHeroStats
                { hero: tx-sender }
                (merge hero-stats {
                    pixel-xp: new-xp,
                    total-pixel-coins: new-total-coins,
                    last-quest-block: block-height,
                    quests-completed: new-quests-completed,
                    stamina: new-stamina
                })
            )
            
            ;; Update quest log
            (map-set HeroQuestLog
                { hero: tx-sender, quest-id: quest-id }
                (merge quest-log {
                    completed: true,
                    completion-block: block-height,
                    times-completed: (+ (get times-completed quest-log) u1),
                    last-reward: (get pixel-coin-reward quest)
                })
            )
            
            ;; Check and update quest-related achievements
            (try! (check-quest-achievements tx-sender new-quests-completed))
            
            (ok true)
        )
    )
)


;; ============= Enhanced Arena System =============
(define-public (create-pixel-arena 
    (name (string-ascii 50))
    (description (string-ascii 200))
    (max-heroes uint)
    (entry-fee uint)
    (min-rank-required uint)
    (arena-type (string-ascii 20))
)
    (begin
        (try! (assert-not-paused))
        (asserts! (is-game-master) ERR-NOT-AUTHORIZED)
        (try! (validate-arena-parameters block-height ARENA-DURATION max-heroes entry-fee min-rank-required))
        
        (let (
            (new-arena-id (+ (var-get pixel-arena-counter) u1))
            (start-block (+ block-height u10)) ;; Start after 10 blocks
            (end-block (+ start-block ARENA-DURATION))
        )
            (map-set PixelArenas
                { arena-id: new-arena-id }
                {
                    name: name,
                    description: description,
                    start-block: start-block,
                    end-block: end-block,
                    max-heroes: max-heroes,
                    current-heroes: u0,
                    entry-fee: entry-fee,
                    prize-pool: u0,
                    status: "registering",
                    min-rank-required: min-rank-required,
                    created-at: block-height,
                    modified-at: block-height,
                    arena-type: arena-type
                }
            )
            
            (var-set pixel-arena-counter new-arena-id)
            (ok new-arena-id)
        )
    )
)


;; ============= Item System =============
(define-public (create-pixel-item 
        (name (string-ascii 50))
        (description (string-ascii 200))
        (item-type (string-ascii 20))
        (power-boost uint)
        (agility-boost uint)
        (stamina-boost uint)
        (rarity (string-ascii 20))
    )
    (begin
        (try! (assert-not-paused))
        (asserts! (is-game-master) ERR-NOT-AUTHORIZED)
        
        (let ((new-item-id (+ (var-get pixel-item-counter) u1)))
            (map-set PixelItems
                { item-id: new-item-id }
                {
                    name: name,
                    description: description,
                    item-type: item-type,
                    power-boost: power-boost,
                    agility-boost: agility-boost,
                    stamina-boost: stamina-boost,
                    rarity: rarity
                }
            )
            
            (var-set pixel-item-counter new-item-id)
            (ok new-item-id)
        )
    )
)

(define-public (equip-item (item-id uint))
    (let (
        (hero-inventory (unwrap! (map-get? HeroInventory { hero: tx-sender }) ERR-INVALID-PIXEL-HERO))
        (item (unwrap! (map-get? PixelItems { item-id: item-id }) ERR-ITEM-NOT-FOUND))
    )
        (asserts! (is-some (index-of (get items hero-inventory) item-id)) ERR-ITEM-NOT-FOUND)
        
        (map-set HeroInventory
            { hero: tx-sender }
            (merge hero-inventory {
                equipped-weapon: (if (is-eq (get item-type item) "weapon")
                    (some item-id)
                    (get equipped-weapon hero-inventory)),
                equipped-armor: (if (is-eq (get item-type item) "armor")
                    (some item-id)
                    (get equipped-armor hero-inventory))
            })
        )
        
        (ok true)
    )
)

(define-public (unequip-item (item-type (string-ascii 20)))
    (let (
        (hero-inventory (unwrap! (map-get? HeroInventory { hero: tx-sender }) ERR-INVALID-PIXEL-HERO))
    )
        (map-set HeroInventory
            { hero: tx-sender }
            (merge hero-inventory {
                equipped-weapon: (if (is-eq item-type "weapon") none (get equipped-weapon hero-inventory)),
                equipped-armor: (if (is-eq item-type "armor") none (get equipped-armor hero-inventory))
            })
        )
        
        (ok true)
    )
)

;; ============= Alliance System =============
(define-public (create-alliance (name (string-ascii 50)) (description (string-ascii 200)))
    (let (
        (hero-stats (unwrap! (map-get? PixelHeroStats { hero: tx-sender }) ERR-INVALID-PIXEL-HERO))
        (new-alliance-id (+ (var-get alliance-counter) u1))
    )
        (try! (assert-not-paused))
        (asserts! (is-none (map-get? HeroAlliance { hero: tx-sender })) ERR-ALREADY-IN-ALLIANCE)
        
        (map-set Alliances
            { alliance-id: new-alliance-id }
            {
                name: name,
                description: description,
                leader: tx-sender,
                members: (list tx-sender),
                created-at: block-height,
                alliance-level: u1,
                total-power: (get power hero-stats)
            }
        )
        
        (map-set HeroAlliance
            { hero: tx-sender }
            { alliance-id: new-alliance-id }
        )
        
        (var-set alliance-counter new-alliance-id)
        (ok new-alliance-id)
    )
)

(define-public (join-alliance (alliance-id uint))
    (let (
        (hero-stats (unwrap! (map-get? PixelHeroStats { hero: tx-sender }) ERR-INVALID-PIXEL-HERO))
        (alliance (unwrap! (map-get? Alliances { alliance-id: alliance-id }) ERR-ALLIANCE-NOT-FOUND))
    )
        (try! (assert-not-paused))
        (asserts! (is-none (map-get? HeroAlliance { hero: tx-sender })) ERR-ALREADY-IN-ALLIANCE)
        (asserts! (< (len (get members alliance)) u50) ERR-ALLIANCE-FULL)
        
        (map-set Alliances
            { alliance-id: alliance-id }
            (merge alliance {
                members: (unwrap! (as-max-len? (append (get members alliance) tx-sender) u50) ERR-ALLIANCE-FULL),
                total-power: (+ (get total-power alliance) (get power hero-stats))
            })
        )
        
        (map-set HeroAlliance
            { hero: tx-sender }
            { alliance-id: alliance-id }
        )
        
        (ok true)
    )
)

(define-public (leave-alliance)
    (let (
        (hero-alliance (unwrap! (map-get? HeroAlliance { hero: tx-sender }) ERR-NOT-IN-ALLIANCE))
        (alliance (unwrap! (map-get? Alliances { alliance-id: (get alliance-id hero-alliance) }) ERR-ALLIANCE-NOT-FOUND))
        (hero-stats (unwrap! (map-get? PixelHeroStats { hero: tx-sender }) ERR-INVALID-PIXEL-HERO))
    )
        (try! (assert-not-paused))
        (asserts! (not (is-eq tx-sender (get leader alliance))) ERR-NOT-ALLIANCE-LEADER)
        
        (map-set Alliances
            { alliance-id: (get alliance-id hero-alliance) }
            (merge alliance {
                members: (filter not-tx-sender (get members alliance)),
                total-power: (- (get total-power alliance) (get power hero-stats))
            })
        )
        
        (map-delete HeroAlliance { hero: tx-sender })
        
        (ok true)
    )
)

(define-public (disband-alliance)
    (let (
        (hero-alliance (unwrap! (map-get? HeroAlliance { hero: tx-sender }) ERR-NOT-IN-ALLIANCE))
        (alliance (unwrap! (map-get? Alliances { alliance-id: (get alliance-id hero-alliance) }) ERR-ALLIANCE-NOT-FOUND))
    )
        (try! (assert-not-paused))
        (asserts! (is-eq tx-sender (get leader alliance)) ERR-NOT-ALLIANCE-LEADER)
        
        (map-delete Alliances { alliance-id: (get alliance-id hero-alliance) })
        
        (map remove-hero-alliance (get members alliance))
        
        (ok true)
    )
)

;; ============= Utility Functions =============
(define-private (mint-pixel-coins (recipient principal) (amount uint))
    (begin
        (try! (ft-mint? pixel-coin amount recipient))
        (ok (var-set total-pixel-coin-supply (+ (var-get total-pixel-coin-supply) amount)))
    )
)

(define-private (burn-pixel-coins (sender principal) (amount uint))
    (begin
        (try! (ft-burn? pixel-coin amount sender))
        (ok (var-set total-pixel-coin-supply (- (var-get total-pixel-coin-supply) amount)))
    )
)

(define-private (is-pixel-hero (address principal))
    (is-some (map-get? PixelHeroStats { hero: address }))
)

(define-private (validate-arena-parameters 
    (start-block uint)
    (duration uint)
    (max-heroes uint)
    (entry-fee uint)
    (min-rank-required uint)
)
    (begin
        (asserts! (>= start-block block-height) ERR-INVALID-PARAMETERS)
        (asserts! (> duration u0) ERR-INVALID-PARAMETERS)
        (asserts! (> max-heroes u1) ERR-INVALID-PARAMETERS)
        (asserts! (validate-pixel-amount entry-fee) ERR-INVALID-PIXEL-AMOUNT)
        (asserts! (validate-hero-rank min-rank-required) ERR-INVALID-RANK)
        (ok true)
    )
)


(define-private (not-tx-sender (member principal))
    (not (is-eq member tx-sender))
)

(define-private (remove-hero-alliance (hero principal))
    (map-delete HeroAlliance { hero: hero })
)


;; Modify check-quest-achievements to not rely on get-all-achievements
(define-private (check-quest-achievements (hero principal) (quests-completed uint))
    (begin
        ;; Check achievements sequentially
        (try! (check-achievement-type-1 quests-completed))
        (try! (check-achievement-type-2 quests-completed))
        (try! (check-achievement-type-3 quests-completed))
        (ok true)
    )
)

(define-private (check-achievement-type-1 (quests-completed uint))
    (let (
        (achievement (unwrap! (map-get? Achievements { achievement-id: u1 }) (err u0)))
    )
        (if (and 
            (is-eq (get achievement-type achievement) "quest_master")
            (>= quests-completed (get requirement achievement))
        )
            (award-achievement u1)
            (ok true)
        )
    )
)

(define-private (check-achievement-type-2 (quests-completed uint))
    (let (
        (achievement (unwrap! (map-get? Achievements { achievement-id: u2 }) (err u0)))
    )
        (if (and 
            (is-eq (get achievement-type achievement) "quest_master")
            (>= quests-completed (get requirement achievement))
        )
            (award-achievement u2)
            (ok true)
        )
    )
)

(define-private (check-achievement-type-3 (quests-completed uint))
    (let (
        (achievement (unwrap! (map-get? Achievements { achievement-id: u3 }) (err u0)))
    )
        (if (and 
            (is-eq (get achievement-type achievement) "quest_master")
            (>= quests-completed (get requirement achievement))
        )
            (award-achievement u3)
            (ok true)
        )
    )
)

(define-private (award-achievement (achievement-id uint))
    (let (
        (achievement (unwrap! (map-get? Achievements { achievement-id: achievement-id }) (err u0)))
        (hero-achievement (default-to 
            { completed: false, completion-block: u0, progress: u0 }
            (map-get? HeroAchievements { hero: tx-sender, achievement-id: achievement-id })))
    )
        (if (get completed hero-achievement)
            (ok true)
            (begin
                (try! (mint-pixel-coins tx-sender (get pixel-coin-reward achievement)))
                (map-set HeroAchievements
                    { hero: tx-sender, achievement-id: achievement-id }
                    {
                        completed: true,
                        completion-block: block-height,
                        progress: (get requirement achievement)
                    }
                )
                (ok true)
            )
        )
    )
)

;; ============= Read-Only Functions =============
(define-read-only (get-achievement-count)
    (var-get achievement-counter)
)

(define-read-only (get-achievement-by-id (achievement-id uint))
    (map-get? Achievements { achievement-id: achievement-id })
)

(define-read-only (get-hero-achievement-status (hero principal) (achievement-id uint))
    (map-get? HeroAchievements { hero: hero, achievement-id: achievement-id })
)

(define-read-only (get-contract-info)
    {
        owner: contract-owner,
        game-master: (var-get game-master),
        total-pixel-coin-supply: (var-get total-pixel-coin-supply),
        hero-count: (var-get pixel-hero-counter),
        quest-count: (var-get quest-scroll-counter),
        arena-count: (var-get pixel-arena-counter),
        item-count: (var-get pixel-item-counter),
        alliance-count: (var-get alliance-counter),
        system-status: (if (var-get system-paused) "paused" "active")
    }
)

(define-read-only (get-hero-full-stats (hero principal))
    (let (
        (hero-stats (unwrap! (map-get? PixelHeroStats { hero: hero }) ERR-INVALID-PIXEL-HERO))
        (hero-inventory (unwrap! (map-get? HeroInventory { hero: hero }) ERR-INVALID-PIXEL-HERO))
    )
        (ok {
            stats: hero-stats,
            inventory: hero-inventory
        })
    )
)

(define-read-only (get-hero-quests (hero principal))
    (map-get? HeroQuestLog { hero: hero, quest-id: u0 })
)


(define-read-only (get-hero-arenas (hero principal))
    (ok "Arena participation data")
)

(define-read-only (get-pixel-quest (quest-id uint))
    (map-get? PixelQuests { id: quest-id })
)

(define-read-only (get-pixel-arena (arena-id uint))
    (map-get? PixelArenas { arena-id: arena-id })
)

(define-read-only (get-pixel-item (item-id uint))
    (map-get? PixelItems { item-id: item-id })
)

(define-read-only (get-alliance-info (alliance-id uint))
    (map-get? Alliances { alliance-id: alliance-id })
)

(define-read-only (get-hero-alliance (hero principal))
    (map-get? HeroAlliance { hero: hero })
)

(define-read-only (get-alliance-members (alliance-id uint))
    (match (map-get? Alliances { alliance-id: alliance-id })
        alliance (ok (get members alliance))
        (err ERR-ALLIANCE-NOT-FOUND)
    )
)

;; (define-read-only (get-all-achievements)
;;     (let ((achievement-count (var-get achievement-counter)))
;;         (map unwrap-uint 
;;             (map to-uint 
;;                 (range u1 (+ achievement-count u1))))
;;     )
;; )

;; (define-private (unwrap-uint (n uint))
;;     n
;; )

;; (define-private (to-uint (n int))
;;     (to-uint n)
;; )

;; ============= Error Handling =============
(define-public (handle-error (error (response bool uint)))
    (match error
        success (ok success)
        failure (begin
            (print "Error occurred.") ;; Removed unsupported conversion
            (err failure)
        )
    )
)

;; ============= Contract Initialization =============
(begin
    (try! (ft-mint? pixel-coin u1000000000 contract-owner))
    (var-set total-pixel-coin-supply u1000000000)
    (ok true)
)


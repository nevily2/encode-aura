;; Encode-Aura Gaming Ecosystem Smart Contract
;; A blockchain gaming platform for breeding digital creatures influenced by environmental data

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-unauthorized (err u102))
(define-constant err-already-exists (err u103))
(define-constant err-insufficient-balance (err u104))
(define-constant err-invalid-territory (err u105))

;; Data Variables
(define-data-var aura-id-nonce uint u0)
(define-data-var territory-id-nonce uint u0)

;; Data Maps
(define-map auras
    uint
    {
        owner: principal,
        generation: uint,
        genetic-hash: (buff 32),
        traits: (string-ascii 256),
        birth-block: uint,
        territory-id: (optional uint),
        parent1: (optional uint),
        parent2: (optional uint)
    }
)

(define-map territories
    uint
    {
        owner: principal,
        latitude: int,
        longitude: int,
        staked-amount: uint,
        environmental-data: (string-ascii 512),
        last-update-block: uint
    }
)

(define-map territory-auras
    uint
    (list 100 uint)
)

(define-map user-auras
    principal
    (list 200 uint)
)

(define-map user-territories
    principal
    (list 50 uint)
)

;; Token balances (simplified - in production use actual SIP-010 tokens)
(define-map encode-balances principal uint)
(define-map aura-token-balances principal uint)

;; Read-only functions
(define-read-only (get-aura (aura-id uint))
    (map-get? auras aura-id)
)

(define-read-only (get-territory (territory-id uint))
    (map-get? territories territory-id)
)

(define-read-only (get-user-auras (user principal))
    (default-to (list) (map-get? user-auras user))
)

(define-read-only (get-user-territories (user principal))
    (default-to (list) (map-get? user-territories user))
)

(define-read-only (get-encode-balance (user principal))
    (default-to u0 (map-get? encode-balances user))
)

(define-read-only (get-aura-token-balance (user principal))
    (default-to u0 (map-get? aura-token-balances user))
)

;; Private functions
(define-private (add-aura-to-user (user principal) (aura-id uint))
    (let ((current-auras (get-user-auras user)))
        (map-set user-auras user (unwrap-panic (as-max-len? (append current-auras aura-id) u200)))
    )
)

(define-private (add-territory-to-user (user principal) (territory-id uint))
    (let ((current-territories (get-user-territories user)))
        (map-set user-territories user (unwrap-panic (as-max-len? (append current-territories territory-id) u50)))
    )
)

;; Public functions

;; Mint a new Aura
(define-public (mint-aura 
    (genetic-hash (buff 32))
    (traits (string-ascii 256))
    (territory-id (optional uint)))
    (let
        (
            (new-aura-id (+ (var-get aura-id-nonce) u1))
            (sender tx-sender)
        )
        ;; Verify territory ownership if specified
        (match territory-id
            tid
            (let ((territory (unwrap! (map-get? territories tid) err-not-found)))
                (asserts! (is-eq (get owner territory) sender) err-unauthorized)
                true
            )
            true
        )
        
        ;; Create the Aura
        (map-set auras new-aura-id {
            owner: sender,
            generation: u1,
            genetic-hash: genetic-hash,
            traits: traits,
            birth-block: block-height,
            territory-id: territory-id,
            parent1: none,
            parent2: none
        })
        
        ;; Update user's aura list
        (add-aura-to-user sender new-aura-id)
        
        ;; Update nonce
        (var-set aura-id-nonce new-aura-id)
        
        (ok new-aura-id)
    )
)

;; Breed two Auras
(define-public (breed-auras 
    (parent1-id uint)
    (parent2-id uint)
    (offspring-genetic-hash (buff 32))
    (offspring-traits (string-ascii 256)))
    (let
        (
            (parent1 (unwrap! (map-get? auras parent1-id) err-not-found))
            (parent2 (unwrap! (map-get? auras parent2-id) err-not-found))
            (new-aura-id (+ (var-get aura-id-nonce) u1))
            (sender tx-sender)
            (new-generation (+ (max (get generation parent1) (get generation parent2)) u1))
        )
        ;; Verify ownership
        (asserts! (is-eq (get owner parent1) sender) err-unauthorized)
        (asserts! (is-eq (get owner parent2) sender) err-unauthorized)
        
        ;; Create offspring
        (map-set auras new-aura-id {
            owner: sender,
            generation: new-generation,
            genetic-hash: offspring-genetic-hash,
            traits: offspring-traits,
            birth-block: block-height,
            territory-id: (get territory-id parent1),
            parent1: (some parent1-id),
            parent2: (some parent2-id)
        })
        
        ;; Update user's aura list
        (add-aura-to-user sender new-aura-id)
        
        ;; Update nonce
        (var-set aura-id-nonce new-aura-id)
        
        (ok new-aura-id)
    )
)

;; Claim a territory
(define-public (claim-territory 
    (latitude int)
    (longitude int)
    (stake-amount uint))
    (let
        (
            (new-territory-id (+ (var-get territory-id-nonce) u1))
            (sender tx-sender)
            (current-balance (get-encode-balance sender))
        )
        ;; Verify sufficient balance
        (asserts! (>= current-balance stake-amount) err-insufficient-balance)
        
        ;; Deduct stake
        (map-set encode-balances sender (- current-balance stake-amount))
        
        ;; Create territory
        (map-set territories new-territory-id {
            owner: sender,
            latitude: latitude,
            longitude: longitude,
            staked-amount: stake-amount,
            environmental-data: "",
            last-update-block: block-height
        })
        
        ;; Update user's territory list
        (add-territory-to-user sender new-territory-id)
        
        ;; Update nonce
        (var-set territory-id-nonce new-territory-id)
        
        (ok new-territory-id)
    )
)

;; Update territory environmental data (oracle function)
(define-public (update-territory-data 
    (territory-id uint)
    (environmental-data (string-ascii 512)))
    (let
        (
            (territory (unwrap! (map-get? territories territory-id) err-not-found))
        )
        ;; Only owner or contract owner can update
        (asserts! (or (is-eq tx-sender (get owner territory)) 
                      (is-eq tx-sender contract-owner)) 
                  err-unauthorized)
        
        (map-set territories territory-id (merge territory {
            environmental-data: environmental-data,
            last-update-block: block-height
        }))
        
        (ok true)
    )
)

;; Transfer Aura ownership
(define-public (transfer-aura (aura-id uint) (recipient principal))
    (let
        (
            (aura (unwrap! (map-get? auras aura-id) err-not-found))
            (sender tx-sender)
        )
        (asserts! (is-eq (get owner aura) sender) err-unauthorized)
        
        (map-set auras aura-id (merge aura { owner: recipient }))
        
        (ok true)
    )
)

;; Initialize balances (for testing - replace with actual token integration)
(define-public (initialize-balance (amount uint))
    (begin
        (map-set encode-balances tx-sender amount)
        (map-set aura-token-balances tx-sender amount)
        (ok true)
    )
)

;; Helper function for max
(define-private (max (a uint) (b uint))
    (if (> a b) a b)
)
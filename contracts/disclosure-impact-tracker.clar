;; Disclosure Impact Tracker for Truthdrop
;; Tracks the real-world impact of verified disclosures and scales rewards dynamically

;; Error constants
(define-constant err-not-found (err u400))
(define-constant err-unauthorized (err u401))
(define-constant err-already-voted (err u402))
(define-constant err-invalid-score (err u403))
(define-constant err-disclosure-not-verified (err u404))
(define-constant err-voting-closed (err u405))
(define-constant err-insufficient-reputation (err u406))
(define-constant err-invalid-tier (err u407))

;; Data variables
(define-data-var next-impact-id uint u1)
(define-data-var impact-voting-period-blocks uint u2016) ;; ~14 days
(define-data-var min-reputation-for-voting uint u100)
(define-data-var impact-assessment-threshold uint u300) ;; Min votes needed
(define-data-var platform-impact-multiplier uint u150) ;; 1.5x base multiplier

;; Impact tracking for disclosures
(define-map disclosure-impacts
  { disclosure-id: uint }
  {
    impact-score: uint, ;; 0-1000 scale
    total-votes: uint,
    weighted-score: uint,
    assessment-deadline: uint,
    impact-tier: uint, ;; 1-5 scale
    category: (string-ascii 50),
    final-assessment: bool,
    media-coverage: uint,
    regulatory-response: uint,
    policy-changes: uint
  }
)

;; Impact voting by community members
(define-map impact-votes
  { disclosure-id: uint, voter: principal }
  {
    impact-rating: uint, ;; 1-10 scale
    confidence-level: uint, ;; 1-5 scale
    vote-weight: uint,
    evidence-links: (string-ascii 200),
    vote-timestamp: uint
  }
)

;; Impact tier definitions
(define-map impact-tiers
  { tier: uint }
  {
    min-score: uint,
    max-score: uint,
    reward-multiplier: uint, ;; Basis points (10000 = 1x)
    tier-name: (string-ascii 30),
    tier-description: (string-ascii 100)
  }
)

;; Historical impact metrics per user
(define-map user-impact-history
  { user: principal }
  {
    total-disclosures: uint,
    high-impact-count: uint, ;; Tier 4-5 disclosures
    average-impact-score: uint,
    impact-reputation: uint,
    last-high-impact-block: uint
  }
)

;; Platform-wide impact analytics
(define-map platform-impact-stats
  { period-key: (string-ascii 20) }
  {
    total-assessed-disclosures: uint,
    average-impact-score: uint,
    high-impact-percentage: uint,
    policy-changes-triggered: uint,
    regulatory-actions-count: uint
  }
)

;; Bounty impact multipliers based on creator's track record
(define-map creator-impact-bonuses
  { creator: principal }
  {
    successful-high-impact-bounties: uint,
    total-bounties-created: uint,
    impact-success-rate: uint, ;; Percentage
    current-multiplier: uint, ;; Applied to new bounties
    last-updated: uint
  }
)

;; Initialize impact tier system
(define-private (initialize-impact-tiers)
  (begin
    (map-set impact-tiers { tier: u1 } 
      { min-score: u0, max-score: u200, reward-multiplier: u8000, 
        tier-name: "Minimal", tier-description: "Limited immediate impact" })
    (map-set impact-tiers { tier: u2 } 
      { min-score: u201, max-score: u400, reward-multiplier: u10000, 
        tier-name: "Local", tier-description: "Community-level impact" })
    (map-set impact-tiers { tier: u3 } 
      { min-score: u401, max-score: u600, reward-multiplier: u13000, 
        tier-name: "Regional", tier-description: "Industry or regional impact" })
    (map-set impact-tiers { tier: u4 } 
      { min-score: u601, max-score: u800, reward-multiplier: u17000, 
        tier-name: "National", tier-description: "National-level impact" })
    (map-set impact-tiers { tier: u5 } 
      { min-score: u801, max-score: u1000, reward-multiplier: u25000, 
        tier-name: "Global", tier-description: "International significance" })
    (ok true)
  )
)

;; Start impact assessment for a verified disclosure
(define-public (initiate-impact-assessment 
  (disclosure-id uint) 
  (category (string-ascii 50)))
  (let
    (
      (disclosure-data (unwrap! (contract-call? .Truthdrop get-disclosure disclosure-id) err-not-found))
      (verification-data (unwrap! (contract-call? .Truthdrop get-disclosure-verification disclosure-id) err-not-found))
      (assessment-deadline (+ stacks-block-height (var-get impact-voting-period-blocks)))
    )
    (asserts! (get verified verification-data) err-disclosure-not-verified)
    (asserts! (is-none (map-get? disclosure-impacts { disclosure-id: disclosure-id })) err-already-voted)
    
    (map-set disclosure-impacts
      { disclosure-id: disclosure-id }
      {
        impact-score: u0,
        total-votes: u0,
        weighted-score: u0,
        assessment-deadline: assessment-deadline,
        impact-tier: u1,
        category: category,
        final-assessment: false,
        media-coverage: u0,
        regulatory-response: u0,
        policy-changes: u0
      }
    )
    (ok true)
  )
)

;; Vote on disclosure impact
(define-public (vote-impact 
  (disclosure-id uint) 
  (impact-rating uint) 
  (confidence-level uint)
  (evidence-links (string-ascii 200)))
  (let
    (
      (impact-data (unwrap! (map-get? disclosure-impacts { disclosure-id: disclosure-id }) err-not-found))
      (voter-rep (unwrap! (contract-call? .Truthdrop get-user-reputation tx-sender) err-unauthorized))
      (vote-weight (calculate-impact-vote-weight tx-sender))
    )
    (asserts! (>= (get score voter-rep) (var-get min-reputation-for-voting)) err-insufficient-reputation)
    (asserts! (and (>= impact-rating u1) (<= impact-rating u10)) err-invalid-score)
    (asserts! (and (>= confidence-level u1) (<= confidence-level u5)) err-invalid-score)
    (asserts! (< stacks-block-height (get assessment-deadline impact-data)) err-voting-closed)
    (asserts! (is-none (map-get? impact-votes { disclosure-id: disclosure-id, voter: tx-sender })) err-already-voted)
    
    ;; Record the vote
    (map-set impact-votes
      { disclosure-id: disclosure-id, voter: tx-sender }
      {
        impact-rating: impact-rating,
        confidence-level: confidence-level,
        vote-weight: vote-weight,
        evidence-links: evidence-links,
        vote-timestamp: stacks-block-height
      }
    )
    
    ;; Update impact assessment
    (let
      (
        (weighted-rating (* impact-rating confidence-level vote-weight))
        (new-total-votes (+ (get total-votes impact-data) vote-weight))
        (new-weighted-score (+ (get weighted-score impact-data) weighted-rating))
        (new-impact-score (if (> new-total-votes u0) (/ (* new-weighted-score u100) new-total-votes) u0))
        (new-tier (calculate-impact-tier new-impact-score))
      )
      (map-set disclosure-impacts
        { disclosure-id: disclosure-id }
        (merge impact-data {
          total-votes: new-total-votes,
          weighted-score: new-weighted-score,
          impact-score: new-impact-score,
          impact-tier: new-tier
        })
      )
      (ok new-impact-score)
    )
  )
)

;; Calculate dynamic reward multiplier based on user's impact history
(define-public (calculate-dynamic-reward-multiplier (user principal) (base-reward uint))
  (let
    (
      (impact-history (default-to 
        { total-disclosures: u0, high-impact-count: u0, average-impact-score: u0, 
          impact-reputation: u0, last-high-impact-block: u0 }
        (map-get? user-impact-history { user: user })))
      (base-multiplier u10000) ;; 1.0x
    )
    (let
      (
        (high-impact-bonus (if (> (get high-impact-count impact-history) u0)
                             (* (get high-impact-count impact-history) u500) ;; 5% per high-impact disclosure
                             u0))
        (reputation-bonus (if (> (get impact-reputation impact-history) u500)
                            (/ (get impact-reputation impact-history) u10) ;; Up to 10% for max reputation
                            u0))
        (final-multiplier (+ base-multiplier high-impact-bonus reputation-bonus))
        (adjusted-reward (/ (* base-reward final-multiplier) u10000))
      )
      (ok adjusted-reward)
    )
  )
)

;; Private helper functions
(define-private (calculate-impact-vote-weight (user principal))
  (let
    (
      (user-rep (unwrap-panic (contract-call? .Truthdrop get-user-reputation user)))
      (user-stake (contract-call? .Truthdrop get-user-stake user))
      (base-weight (/ (get score user-rep) u50))
    )
    (match user-stake
      stake-data (+ base-weight (/ (get verification-power stake-data) u10))
      base-weight
    )
  )
)

(define-private (calculate-impact-tier (impact-score uint))
  (if (>= impact-score u801) u5
    (if (>= impact-score u601) u4
      (if (>= impact-score u401) u3
        (if (>= impact-score u201) u2
          u1))))
)

;; Read-only functions
(define-read-only (get-disclosure-impact (disclosure-id uint))
  (map-get? disclosure-impacts { disclosure-id: disclosure-id })
)

(define-read-only (get-impact-vote (disclosure-id uint) (voter principal))
  (map-get? impact-votes { disclosure-id: disclosure-id, voter: voter })
)

(define-read-only (get-impact-tier-info (tier uint))
  (map-get? impact-tiers { tier: tier })
)

(define-read-only (get-user-impact-history (user principal))
  (map-get? user-impact-history { user: user })
)

(define-read-only (get-platform-impact-stats (period-key (string-ascii 20)))
  (map-get? platform-impact-stats { period-key: period-key })
)

(define-read-only (get-creator-impact-bonus (creator principal))
  (map-get? creator-impact-bonuses { creator: creator })
)

(define-read-only (get-impact-parameters)
  {
    next-impact-id: (var-get next-impact-id),
    voting-period-blocks: (var-get impact-voting-period-blocks),
    min-reputation-for-voting: (var-get min-reputation-for-voting),
    assessment-threshold: (var-get impact-assessment-threshold),
    platform-multiplier: (var-get platform-impact-multiplier)
  }
)

;; Initialize the system
(initialize-impact-tiers)
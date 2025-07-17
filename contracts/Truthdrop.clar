(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-unauthorized (err u102))
(define-constant err-invalid-amount (err u103))
(define-constant err-already-exists (err u104))
(define-constant err-insufficient-funds (err u105))
(define-constant err-bounty-expired (err u106))
(define-constant err-bounty-not-active (err u107))
(define-constant err-invalid-verification (err u108))
(define-constant err-insufficient-stake (err u109))
(define-constant err-cooldown-active (err u110))
(define-constant err-invalid-tier (err u111))
(define-constant err-stake-locked (err u112))

(define-data-var next-bounty-id uint u1)
(define-data-var next-disclosure-id uint u1)
(define-data-var platform-fee-rate uint u250)
(define-data-var min-bounty-amount uint u1000000)
(define-data-var min-stake-amount uint u5000000)
(define-data-var stake-reward-rate uint u500)
(define-data-var unstake-cooldown-blocks uint u1008)

(define-map bounties
  { bounty-id: uint }
  {
    creator: principal,
    title: (string-ascii 100),
    description: (string-ascii 500),
    reward-amount: uint,
    expiry-block: uint,
    category: (string-ascii 50),
    status: (string-ascii 20),
    verification-required: bool,
    min-evidence-score: uint
  }
)

(define-map disclosures
  { disclosure-id: uint }
  {
    bounty-id: uint,
    whistleblower: principal,
    evidence-hash: (buff 32),
    submission-block: uint,
    verification-score: uint,
    status: (string-ascii 20),
    reward-claimed: bool
  }
)

(define-map bounty-funds
  { bounty-id: uint }
  { amount: uint }
)

(define-map user-reputation
  { user: principal }
  { score: uint, submissions: uint, verified-count: uint }
)

(define-map verifier-votes
  { disclosure-id: uint, verifier: principal }
  { vote: bool, weight: uint }
)

(define-map disclosure-verifications
  { disclosure-id: uint }
  { total-votes: uint, positive-votes: uint, verified: bool }
)

(define-map user-stakes
  { user: principal }
  {
    staked-amount: uint,
    stake-block: uint,
    tier: uint,
    unlock-block: uint,
    pending-rewards: uint,
    last-reward-block: uint,
    verification-power: uint
  }
)

(define-map stake-tiers
  { tier: uint }
  {
    min-amount: uint,
    verification-multiplier: uint,
    reward-multiplier: uint,
    name: (string-ascii 20)
  }
)

(define-map total-platform-stats
  { key: (string-ascii 20) }
  { value: uint }
)

;; (define-nft truthdrop-nft uint)

(define-public (create-bounty 
  (title (string-ascii 100))
  (description (string-ascii 500))
  (reward-amount uint)
  (duration-blocks uint)
  (category (string-ascii 50))
  (verification-required bool)
  (min-evidence-score uint))
  (let
    (
      (bounty-id (var-get next-bounty-id))
      (expiry-block (+ stacks-block-height duration-blocks))
    )
    (asserts! (>= reward-amount (var-get min-bounty-amount)) err-invalid-amount)
    (try! (stx-transfer? reward-amount tx-sender (as-contract tx-sender)))
    (map-set bounties
      { bounty-id: bounty-id }
      {
        creator: tx-sender,
        title: title,
        description: description,
        reward-amount: reward-amount,
        expiry-block: expiry-block,
        category: category,
        status: "active",
        verification-required: verification-required,
        min-evidence-score: min-evidence-score
      }
    )
    (map-set bounty-funds { bounty-id: bounty-id } { amount: reward-amount })
    (var-set next-bounty-id (+ bounty-id u1))
    (ok bounty-id)
  )
)

(define-public (submit-disclosure
  (bounty-id uint)
  (evidence-hash (buff 32)))
  (let
    (
      (disclosure-id (var-get next-disclosure-id))
      (bounty (unwrap! (map-get? bounties { bounty-id: bounty-id }) err-not-found))
    )
    (asserts! (is-eq (get status bounty) "active") err-bounty-not-active)
    (asserts! (< stacks-block-height (get expiry-block bounty)) err-bounty-expired)
    (map-set disclosures
      { disclosure-id: disclosure-id }
      {
        bounty-id: bounty-id,
        whistleblower: tx-sender,
        evidence-hash: evidence-hash,
        submission-block: stacks-block-height,
        verification-score: u0,
        status: "pending",
        reward-claimed: false
      }
    )
    (map-set disclosure-verifications
      { disclosure-id: disclosure-id }
      { total-votes: u0, positive-votes: u0, verified: false }
    )
    (var-set next-disclosure-id (+ disclosure-id u1))
    ;; (try! (nft-mint? truthdrop-nft disclosure-id tx-sender))
    (update-user-reputation tx-sender u1 u0)
    (ok disclosure-id)
  )
)

(define-public (verify-disclosure
  (disclosure-id uint)
  (vote bool)
  (weight uint))
  (let
    (
      (disclosure (unwrap! (map-get? disclosures { disclosure-id: disclosure-id }) err-not-found))
      (user-rep (default-to { score: u0, submissions: u0, verified-count: u0 }
                           (map-get? user-reputation { user: tx-sender })))
      (verification (default-to { total-votes: u0, positive-votes: u0, verified: false }
                                (map-get? disclosure-verifications { disclosure-id: disclosure-id })))
      (verification-power (calculate-verification-power tx-sender))
      (actual-weight (if (> weight verification-power) weight verification-power))
    )
    (asserts! (>= (get score user-rep) u100) err-unauthorized)
    (asserts! (is-none (map-get? verifier-votes { disclosure-id: disclosure-id, verifier: tx-sender })) err-already-exists)
    (map-set verifier-votes
      { disclosure-id: disclosure-id, verifier: tx-sender }
      { vote: vote, weight: actual-weight }
    )
    (let
      (
        (new-total-votes (+ (get total-votes verification) actual-weight))
        (new-positive-votes (if vote (+ (get positive-votes verification) actual-weight) (get positive-votes verification)))
      )
      (map-set disclosure-verifications
        { disclosure-id: disclosure-id }
        {
          total-votes: new-total-votes,
          positive-votes: new-positive-votes,
          verified: (and (>= new-total-votes u300) (>= (* new-positive-votes u100) (* new-total-votes u60)))
        }
      )
      (if (and (>= new-total-votes u300) (>= (* new-positive-votes u100) (* new-total-votes u60)))
        (begin
          (map-set disclosures
            { disclosure-id: disclosure-id }
            (merge disclosure { status: "verified", verification-score: new-positive-votes })
          )
          (update-user-reputation (get whistleblower disclosure) u50 u1)
          (update-user-reputation tx-sender u25 u0)
          (ok true)
        )
        (begin
          (update-user-reputation tx-sender u10 u0)
          (ok true)
        )
      )
    )
  )
)

(define-public (claim-reward (disclosure-id uint))
  (let
    (
      (disclosure (unwrap! (map-get? disclosures { disclosure-id: disclosure-id }) err-not-found))
      (bounty (unwrap! (map-get? bounties { bounty-id: (get bounty-id disclosure) }) err-not-found))
      (verification (unwrap! (map-get? disclosure-verifications { disclosure-id: disclosure-id }) err-not-found))
      (bounty-fund (unwrap! (map-get? bounty-funds { bounty-id: (get bounty-id disclosure) }) err-insufficient-funds))
    )
    (asserts! (is-eq tx-sender (get whistleblower disclosure)) err-unauthorized)
    (asserts! (not (get reward-claimed disclosure)) err-already-exists)
    (asserts! (or (not (get verification-required bounty)) (get verified verification)) err-invalid-verification)
    (asserts! (>= (get verification-score disclosure) (get min-evidence-score bounty)) err-invalid-verification)
    (let
      (
        (platform-fee (/ (* (get reward-amount bounty) (var-get platform-fee-rate)) u10000))
        (whistleblower-reward (- (get reward-amount bounty) platform-fee))
      )
      (try! (as-contract (stx-transfer? whistleblower-reward tx-sender (get whistleblower disclosure))))
      (try! (as-contract (stx-transfer? platform-fee tx-sender contract-owner)))
      (map-set disclosures
        { disclosure-id: disclosure-id }
        (merge disclosure { reward-claimed: true, status: "completed" })
      )
      (map-set bounties
        { bounty-id: (get bounty-id disclosure) }
        (merge bounty { status: "completed" })
      )
      (map-delete bounty-funds { bounty-id: (get bounty-id disclosure) })
      (ok whistleblower-reward)
    )
  )
)

(define-public (cancel-bounty (bounty-id uint))
  (let
    (
      (bounty (unwrap! (map-get? bounties { bounty-id: bounty-id }) err-not-found))
      (bounty-fund (unwrap! (map-get? bounty-funds { bounty-id: bounty-id }) err-not-found))
    )
    (asserts! (is-eq tx-sender (get creator bounty)) err-unauthorized)
    (asserts! (is-eq (get status bounty) "active") err-bounty-not-active)
    (try! (as-contract (stx-transfer? (get amount bounty-fund) tx-sender (get creator bounty))))
    (map-set bounties
      { bounty-id: bounty-id }
      (merge bounty { status: "cancelled" })
    )
    (map-delete bounty-funds { bounty-id: bounty-id })
    (ok true)
  )
)

(define-public (extend-bounty (bounty-id uint) (additional-blocks uint))
  (let
    (
      (bounty (unwrap! (map-get? bounties { bounty-id: bounty-id }) err-not-found))
    )
    (asserts! (is-eq tx-sender (get creator bounty)) err-unauthorized)
    (asserts! (is-eq (get status bounty) "active") err-bounty-not-active)
    (map-set bounties
      { bounty-id: bounty-id }
      (merge bounty { expiry-block: (+ (get expiry-block bounty) additional-blocks) })
    )
    (ok true)
  )
)

(define-public (set-platform-fee (new-rate uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (<= new-rate u1000) err-invalid-amount)
    (var-set platform-fee-rate new-rate)
    (ok true)
  )
)

(define-public (set-min-bounty-amount (new-amount uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (var-set min-bounty-amount new-amount)
    (ok true)
  )
)

(define-private (update-user-reputation (user principal) (score-increase uint) (verified-increase uint))
  (let
    (
      (current-rep (default-to { score: u0, submissions: u0, verified-count: u0 }
                               (map-get? user-reputation { user: user })))
    )
    (map-set user-reputation
      { user: user }
      {
        score: (+ (get score current-rep) score-increase),
        submissions: (+ (get submissions current-rep) u1),
        verified-count: (+ (get verified-count current-rep) verified-increase)
      }
    )
  )
)

(define-private (initialize-stake-tiers)
  (begin
    (map-set stake-tiers { tier: u1 } { min-amount: u5000000, verification-multiplier: u100, reward-multiplier: u100, name: "Bronze" })
    (map-set stake-tiers { tier: u2 } { min-amount: u25000000, verification-multiplier: u150, reward-multiplier: u125, name: "Silver" })
    (map-set stake-tiers { tier: u3 } { min-amount: u100000000, verification-multiplier: u200, reward-multiplier: u150, name: "Gold" })
    (map-set stake-tiers { tier: u4 } { min-amount: u500000000, verification-multiplier: u300, reward-multiplier: u200, name: "Platinum" })
    (map-set total-platform-stats { key: "total-staked" } { value: u0 })
    (map-set total-platform-stats { key: "total-stakers" } { value: u0 })
    (ok true)
  )
)

(define-private (calculate-stake-tier (amount uint))
  (if (>= amount u500000000)
    u4
    (if (>= amount u100000000)
      u3
      (if (>= amount u25000000)
        u2
        u1
      )
    )
  )
)

(define-private (calculate-verification-power (user principal))
  (let
    (
      (user-rep (default-to { score: u0, submissions: u0, verified-count: u0 }
                            (map-get? user-reputation { user: user })))
      (stake-info (map-get? user-stakes { user: user }))
    )
    (match stake-info
      stake-data
      (let
        (
          (tier-info (unwrap-panic (map-get? stake-tiers { tier: (get tier stake-data) })))
          (base-power (/ (get score user-rep) u10))
          (stake-multiplier (get verification-multiplier tier-info))
        )
        (/ (* base-power stake-multiplier) u100)
      )
      (/ (get score user-rep) u10)
    )
  )
)

(define-private (update-stake-rewards (user principal))
  (let
    (
      (stake-info (map-get? user-stakes { user: user }))
    )
    (match stake-info
      stake-data
      (let
        (
          (tier-info (unwrap-panic (map-get? stake-tiers { tier: (get tier stake-data) })))
          (blocks-since-last-reward (- stacks-block-height (get last-reward-block stake-data)))
          (reward-per-block (/ (* (get staked-amount stake-data) (var-get stake-reward-rate) (get reward-multiplier tier-info)) u10000000))
          (new-rewards (* reward-per-block blocks-since-last-reward))
        )
        (map-set user-stakes
          { user: user }
          (merge stake-data {
            pending-rewards: (+ (get pending-rewards stake-data) new-rewards),
            last-reward-block: stacks-block-height,
            verification-power: (calculate-verification-power user)
          })
        )
        (ok true)
      )
      (ok true)
    )
  )
)

(define-public (stake-tokens (amount uint))
  (let
    (
      (tier (calculate-stake-tier amount))
      (current-stake (map-get? user-stakes { user: tx-sender }))
      (total-staked-stat (default-to { value: u0 } (map-get? total-platform-stats { key: "total-staked" })))
      (total-stakers-stat (default-to { value: u0 } (map-get? total-platform-stats { key: "total-stakers" })))
    )
    (asserts! (>= amount (var-get min-stake-amount)) err-insufficient-stake)
    (asserts! (>= tier u1) err-invalid-tier)
    (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
    (match current-stake
      existing-stake
      (let
        (
          (new-total-amount (+ (get staked-amount existing-stake) amount))
          (new-tier (calculate-stake-tier new-total-amount))
        )
        (unwrap-panic (update-stake-rewards tx-sender))
        (map-set user-stakes
          { user: tx-sender }
          (merge existing-stake {
            staked-amount: new-total-amount,
            tier: new-tier,
            verification-power: (calculate-verification-power tx-sender)
          })
        )
        (map-set total-platform-stats { key: "total-staked" } { value: (+ (get value total-staked-stat) amount) })
        (ok new-total-amount)
      )
      (begin
        (map-set user-stakes
          { user: tx-sender }
          {
            staked-amount: amount,
            stake-block: stacks-block-height,
            tier: tier,
            unlock-block: u0,
            pending-rewards: u0,
            last-reward-block: stacks-block-height,
            verification-power: (calculate-verification-power tx-sender)
          }
        )
        (map-set total-platform-stats { key: "total-staked" } { value: (+ (get value total-staked-stat) amount) })
        (map-set total-platform-stats { key: "total-stakers" } { value: (+ (get value total-stakers-stat) u1) })
        (ok amount)
      )
    )
  )
)

(define-public (initiate-unstake (amount uint))
  (let
    (
      (stake-info (unwrap! (map-get? user-stakes { user: tx-sender }) err-not-found))
      (unlock-block (+ stacks-block-height (var-get unstake-cooldown-blocks)))
    )
    (asserts! (>= (get staked-amount stake-info) amount) err-insufficient-stake)
    (asserts! (is-eq (get unlock-block stake-info) u0) err-cooldown-active)
    (unwrap-panic (update-stake-rewards tx-sender))
    (let
      (
        (remaining-amount (- (get staked-amount stake-info) amount))
        (new-tier (if (> remaining-amount u0) (calculate-stake-tier remaining-amount) u0))
      )
      (map-set user-stakes
        { user: tx-sender }
        (merge stake-info {
          staked-amount: remaining-amount,
          tier: new-tier,
          unlock-block: unlock-block,
          verification-power: (calculate-verification-power tx-sender)
        })
      )
      (ok unlock-block)
    )
  )
)

(define-public (complete-unstake)
  (let
    (
      (stake-info (unwrap! (map-get? user-stakes { user: tx-sender }) err-not-found))
      (total-staked-stat (default-to { value: u0 } (map-get? total-platform-stats { key: "total-staked" })))
      (total-stakers-stat (default-to { value: u0 } (map-get? total-platform-stats { key: "total-stakers" })))
    )
    (asserts! (> (get unlock-block stake-info) u0) err-not-found)
    (asserts! (>= stacks-block-height (get unlock-block stake-info)) err-stake-locked)
    (let
      (
        (unstake-amount (- (get staked-amount stake-info) (get staked-amount stake-info)))
        (remaining-amount (get staked-amount stake-info))
      )
      (if (is-eq remaining-amount u0)
        (begin
          (map-delete user-stakes { user: tx-sender })
          (map-set total-platform-stats { key: "total-stakers" } { value: (- (get value total-stakers-stat) u1) })
        )
        (map-set user-stakes
          { user: tx-sender }
          (merge stake-info { unlock-block: u0 })
        )
      )
      (ok true)
    )
  )
)

(define-public (claim-stake-rewards)
  (let
    (
      (stake-info (unwrap! (map-get? user-stakes { user: tx-sender }) err-not-found))
    )
    (unwrap-panic (update-stake-rewards tx-sender))
    (let
      (
        (updated-stake (unwrap! (map-get? user-stakes { user: tx-sender }) err-not-found))
        (rewards (get pending-rewards updated-stake))
      )
      (asserts! (> rewards u0) err-insufficient-funds)
      (try! (as-contract (stx-transfer? rewards tx-sender tx-sender)))
      (map-set user-stakes
        { user: tx-sender }
        (merge updated-stake { pending-rewards: u0 })
      )
      (ok rewards)
    )
  )
)

(define-public (set-stake-parameters (min-amount uint) (reward-rate uint) (cooldown-blocks uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (var-set min-stake-amount min-amount)
    (var-set stake-reward-rate reward-rate)
    (var-set unstake-cooldown-blocks cooldown-blocks)
    (ok true)
  )
)

(define-read-only (get-bounty (bounty-id uint))
  (map-get? bounties { bounty-id: bounty-id })
)

(define-read-only (get-disclosure (disclosure-id uint))
  (map-get? disclosures { disclosure-id: disclosure-id })
)

(define-read-only (get-user-reputation (user principal))
  (map-get? user-reputation { user: user })
)

(define-read-only (get-disclosure-verification (disclosure-id uint))
  (map-get? disclosure-verifications { disclosure-id: disclosure-id })
)

(define-read-only (get-bounty-fund (bounty-id uint))
  (map-get? bounty-funds { bounty-id: bounty-id })
)

(define-read-only (get-platform-fee-rate)
  (var-get platform-fee-rate)
)

(define-read-only (get-min-bounty-amount)
  (var-get min-bounty-amount)
)

(define-read-only (get-next-bounty-id)
  (var-get next-bounty-id)
)

(define-read-only (get-next-disclosure-id)
  (var-get next-disclosure-id)
)

(define-read-only (get-user-stake (user principal))
  (map-get? user-stakes { user: user })
)

(define-read-only (get-stake-tier (tier uint))
  (map-get? stake-tiers { tier: tier })
)

(define-read-only (get-platform-stats (key (string-ascii 20)))
  (map-get? total-platform-stats { key: key })
)

(define-read-only (get-user-verification-power (user principal))
  (calculate-verification-power user)
)

(define-read-only (get-stake-parameters)
  {
    min-stake-amount: (var-get min-stake-amount),
    stake-reward-rate: (var-get stake-reward-rate),
    unstake-cooldown-blocks: (var-get unstake-cooldown-blocks)
  }
)

(define-read-only (calculate-pending-rewards (user principal))
  (let
    (
      (stake-info (map-get? user-stakes { user: user }))
    )
    (match stake-info
      stake-data
      (let
        (
          (tier-info (unwrap-panic (map-get? stake-tiers { tier: (get tier stake-data) })))
          (blocks-since-last-reward (- stacks-block-height (get last-reward-block stake-data)))
          (reward-per-block (/ (* (get staked-amount stake-data) (var-get stake-reward-rate) (get reward-multiplier tier-info)) u10000000))
          (new-rewards (* reward-per-block blocks-since-last-reward))
        )
        (+ (get pending-rewards stake-data) new-rewards)
      )
      u0
    )
  )
)

(initialize-stake-tiers)
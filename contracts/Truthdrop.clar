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

(define-data-var next-bounty-id uint u1)
(define-data-var next-disclosure-id uint u1)
(define-data-var platform-fee-rate uint u250)
(define-data-var min-bounty-amount uint u1000000)

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
    )
    (asserts! (>= (get score user-rep) u100) err-unauthorized)
    (asserts! (is-none (map-get? verifier-votes { disclosure-id: disclosure-id, verifier: tx-sender })) err-already-exists)
    (map-set verifier-votes
      { disclosure-id: disclosure-id, verifier: tx-sender }
      { vote: vote, weight: weight }
    )
    (let
      (
        (new-total-votes (+ (get total-votes verification) weight))
        (new-positive-votes (if vote (+ (get positive-votes verification) weight) (get positive-votes verification)))
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
          (ok true)
        )
        (ok true)
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

(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_NOT_AUTHORIZED (err u100))
(define-constant ERR_LAND_NOT_FOUND (err u101))
(define-constant ERR_ALREADY_EXISTS (err u102))
(define-constant ERR_INVALID_OWNER (err u103))
(define-constant ERR_INSUFFICIENT_SHARES (err u104))
(define-constant ERR_DISPUTE_EXISTS (err u105))
(define-constant ERR_NOT_SHAREHOLDER (err u106))
(define-constant ERR_INVALID_PRICE (err u107))

(define-data-var land-id-counter uint u0)
(define-data-var dispute-id-counter uint u0)

(define-map land-registry
  { land-id: uint }
  {
    owner: principal,
    location: (string-ascii 100),
    size: uint,
    land-type: (string-ascii 50),
    registration-date: uint,
    is-active: bool,
    total-shares: uint,
    price-per-share: uint
  }
)

(define-map land-shares
  { land-id: uint, shareholder: principal }
  { shares: uint }
)

(define-map land-disputes
  { dispute-id: uint }
  {
    land-id: uint,
    complainant: principal,
    respondent: principal,
    description: (string-ascii 200),
    status: (string-ascii 20),
    created-at: uint,
    resolved-at: (optional uint)
  }
)

(define-map authorized-registrars
  { registrar: principal }
  { is-authorized: bool }
)

(define-map land-transfer-proposals
  { land-id: uint, proposal-id: uint }
  {
    from-owner: principal,
    to-owner: principal,
    transfer-price: uint,
    expiry-block: uint,
    is-active: bool
  }
)

(define-map shareholder-votes
  { land-id: uint, voter: principal, proposal-id: uint }
  { vote: bool }
)

(define-public (authorize-registrar (registrar principal))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_NOT_AUTHORIZED)
    (ok (map-set authorized-registrars { registrar: registrar } { is-authorized: true }))
  )
)

(define-public (register-land (location (string-ascii 100)) (size uint) (land-type (string-ascii 50)) (total-shares uint) (price-per-share uint))
  (let
    (
      (new-land-id (+ (var-get land-id-counter) u1))
      (registrar-authorized (default-to false (get is-authorized (map-get? authorized-registrars { registrar: tx-sender }))))
    )
    (asserts! (or (is-eq tx-sender CONTRACT_OWNER) registrar-authorized) ERR_NOT_AUTHORIZED)
    (asserts! (is-none (map-get? land-registry { land-id: new-land-id })) ERR_ALREADY_EXISTS)
    (asserts! (> price-per-share u0) ERR_INVALID_PRICE)
    (map-set land-registry
      { land-id: new-land-id }
      {
        owner: tx-sender,
        location: location,
        size: size,
        land-type: land-type,
        registration-date: stacks-block-height,
        is-active: true,
        total-shares: total-shares,
        price-per-share: price-per-share
      }
    )
    (map-set land-shares
      { land-id: new-land-id, shareholder: tx-sender }
      { shares: total-shares }
    )
    (var-set land-id-counter new-land-id)
    (ok new-land-id)
  )
)

(define-public (transfer-land-ownership (land-id uint) (new-owner principal) (transfer-price uint))
  (let
    (
      (land-data (unwrap! (map-get? land-registry { land-id: land-id }) ERR_LAND_NOT_FOUND))
      (current-owner (get owner land-data))
    )
    (asserts! (is-eq tx-sender current-owner) ERR_NOT_AUTHORIZED)
    (asserts! (get is-active land-data) ERR_LAND_NOT_FOUND)
    (map-set land-registry
      { land-id: land-id }
      (merge land-data { owner: new-owner })
    )
    (map-delete land-shares { land-id: land-id, shareholder: current-owner })
    (map-set land-shares
      { land-id: land-id, shareholder: new-owner }
      { shares: (get total-shares land-data) }
    )
    (ok true)
  )
)

(define-public (buy-land-shares (land-id uint) (shares-to-buy uint))
  (let
    (
      (land-data (unwrap! (map-get? land-registry { land-id: land-id }) ERR_LAND_NOT_FOUND))
      (owner-shares (default-to u0 (get shares (map-get? land-shares { land-id: land-id, shareholder: (get owner land-data) }))))
      (buyer-shares (default-to u0 (get shares (map-get? land-shares { land-id: land-id, shareholder: tx-sender }))))
      (total-cost (* shares-to-buy (get price-per-share land-data)))
    )
    (asserts! (get is-active land-data) ERR_LAND_NOT_FOUND)
    (asserts! (>= owner-shares shares-to-buy) ERR_INSUFFICIENT_SHARES)
    (asserts! (> shares-to-buy u0) ERR_INSUFFICIENT_SHARES)
    (map-set land-shares
      { land-id: land-id, shareholder: (get owner land-data) }
      { shares: (- owner-shares shares-to-buy) }
    )
    (map-set land-shares
      { land-id: land-id, shareholder: tx-sender }
      { shares: (+ buyer-shares shares-to-buy) }
    )
    (ok total-cost)
  )
)

(define-public (file-dispute (land-id uint) (respondent principal) (description (string-ascii 200)))
  (let
    (
      (new-dispute-id (+ (var-get dispute-id-counter) u1))
      (land-data (unwrap! (map-get? land-registry { land-id: land-id }) ERR_LAND_NOT_FOUND))
    )
    (asserts! (get is-active land-data) ERR_LAND_NOT_FOUND)
    (asserts! (is-none (map-get? land-disputes { dispute-id: new-dispute-id })) ERR_DISPUTE_EXISTS)
    (map-set land-disputes
      { dispute-id: new-dispute-id }
      {
        land-id: land-id,
        complainant: tx-sender,
        respondent: respondent,
        description: description,
        status: "pending",
        created-at: stacks-block-height,
        resolved-at: none
      }
    )
    (var-set dispute-id-counter new-dispute-id)
    (ok new-dispute-id)
  )
)

(define-public (resolve-dispute (dispute-id uint) (resolution (string-ascii 20)))
  (let
    (
      (dispute-data (unwrap! (map-get? land-disputes { dispute-id: dispute-id }) ERR_LAND_NOT_FOUND))
    )
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_NOT_AUTHORIZED)
    (asserts! (is-eq (get status dispute-data) "pending") ERR_DISPUTE_EXISTS)
    (map-set land-disputes
      { dispute-id: dispute-id }
      (merge dispute-data {
        status: resolution,
        resolved-at: (some stacks-block-height)
      })
    )
    (ok true)
  )
)

(define-public (create-transfer-proposal (land-id uint) (to-owner principal) (transfer-price uint) (expiry-blocks uint))
  (let
    (
      (land-data (unwrap! (map-get? land-registry { land-id: land-id }) ERR_LAND_NOT_FOUND))
      (proposal-id u1)
    )
    (asserts! (is-eq tx-sender (get owner land-data)) ERR_NOT_AUTHORIZED)
    (asserts! (get is-active land-data) ERR_LAND_NOT_FOUND)
    (map-set land-transfer-proposals
      { land-id: land-id, proposal-id: proposal-id }
      {
        from-owner: tx-sender,
        to-owner: to-owner,
        transfer-price: transfer-price,
        expiry-block: (+ stacks-block-height expiry-blocks),
        is-active: true
      }
    )
    (ok proposal-id)
  )
)

(define-public (vote-on-transfer (land-id uint) (proposal-id uint) (vote bool))
  (let
    (
      (shareholder-data (unwrap! (map-get? land-shares { land-id: land-id, shareholder: tx-sender }) ERR_NOT_SHAREHOLDER))
      (proposal-data (unwrap! (map-get? land-transfer-proposals { land-id: land-id, proposal-id: proposal-id }) ERR_LAND_NOT_FOUND))
    )
    (asserts! (> (get shares shareholder-data) u0) ERR_NOT_SHAREHOLDER)
    (asserts! (get is-active proposal-data) ERR_LAND_NOT_FOUND)
    (asserts! (< stacks-block-height (get expiry-block proposal-data)) ERR_LAND_NOT_FOUND)
    (map-set shareholder-votes
      { land-id: land-id, voter: tx-sender, proposal-id: proposal-id }
      { vote: vote }
    )
    (ok true)
  )
)

(define-read-only (get-land-info (land-id uint))
  (map-get? land-registry { land-id: land-id })
)

(define-read-only (get-land-shares (land-id uint) (shareholder principal))
  (map-get? land-shares { land-id: land-id, shareholder: shareholder })
)

(define-read-only (get-dispute-info (dispute-id uint))
  (map-get? land-disputes { dispute-id: dispute-id })
)

(define-read-only (get-transfer-proposal (land-id uint) (proposal-id uint))
  (map-get? land-transfer-proposals { land-id: land-id, proposal-id: proposal-id })
)

(define-read-only (is-authorized-registrar (registrar principal))
  (default-to false (get is-authorized (map-get? authorized-registrars { registrar: registrar })))
)

(define-read-only (get-current-land-id)
  (var-get land-id-counter)
)

(define-read-only (get-current-dispute-id)
  (var-get dispute-id-counter)
)

(define-read-only (get-shareholder-vote (land-id uint) (voter principal) (proposal-id uint))
  (map-get? shareholder-votes { land-id: land-id, voter: voter, proposal-id: proposal-id })
)

(define-map land-valuations
  { land-id: uint, valuation-id: uint }
  {
    appraiser: principal,
    valuation-amount: uint,
    valuation-date: uint,
    valuation-method: (string-ascii 50),
    is-verified: bool
  }
)

(define-map land-valuation-counters
  { land-id: uint }
  { counter: uint }
)

(define-map certified-appraisers
  { appraiser: principal }
  { is-certified: bool, certification-date: uint }
)

(define-public (certify-appraiser (appraiser principal))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_NOT_AUTHORIZED)
    (ok (map-set certified-appraisers 
      { appraiser: appraiser } 
      { is-certified: true, certification-date: stacks-block-height }))
  )
)

(define-public (submit-land-valuation (land-id uint) (valuation-amount uint) (valuation-method (string-ascii 50)))
  (let
    (
      (land-data (unwrap! (map-get? land-registry { land-id: land-id }) ERR_LAND_NOT_FOUND))
      (appraiser-data (unwrap! (map-get? certified-appraisers { appraiser: tx-sender }) ERR_NOT_AUTHORIZED))
      (current-counter (default-to u0 (get counter (map-get? land-valuation-counters { land-id: land-id }))))
      (new-valuation-id (+ current-counter u1))
    )
    (asserts! (get is-active land-data) ERR_LAND_NOT_FOUND)
    (asserts! (get is-certified appraiser-data) ERR_NOT_AUTHORIZED)
    (asserts! (> valuation-amount u0) ERR_INVALID_PRICE)
    (map-set land-valuations
      { land-id: land-id, valuation-id: new-valuation-id }
      {
        appraiser: tx-sender,
        valuation-amount: valuation-amount,
        valuation-date: stacks-block-height,
        valuation-method: valuation-method,
        is-verified: false
      }
    )
    (map-set land-valuation-counters
      { land-id: land-id }
      { counter: new-valuation-id }
    )
    (ok new-valuation-id)
  )
)

(define-public (verify-valuation (land-id uint) (valuation-id uint))
  (let
    (
      (valuation-data (unwrap! (map-get? land-valuations { land-id: land-id, valuation-id: valuation-id }) ERR_LAND_NOT_FOUND))
    )
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_NOT_AUTHORIZED)
    (map-set land-valuations
      { land-id: land-id, valuation-id: valuation-id }
      (merge valuation-data { is-verified: true })
    )
    (ok true)
  )
)

(define-read-only (get-land-valuation (land-id uint) (valuation-id uint))
  (map-get? land-valuations { land-id: land-id, valuation-id: valuation-id })
)

(define-read-only (get-valuation-count (land-id uint))
  (default-to u0 (get counter (map-get? land-valuation-counters { land-id: land-id })))
)

(define-read-only (is-certified-appraiser (appraiser principal))
  (default-to false (get is-certified (map-get? certified-appraisers { appraiser: appraiser })))
)

(define-map land-leases
  { land-id: uint, lease-id: uint }
  {
    lessor: principal,
    lessee: principal,
    lease-start: uint,
    lease-end: uint,
    monthly-rent: uint,
    usage-type: (string-ascii 50),
    is-active: bool,
    deposit-amount: uint
  }
)

(define-map lease-counters
  { land-id: uint }
  { counter: uint }
)

(define-map lease-payments
  { land-id: uint, lease-id: uint, payment-id: uint }
  {
    amount: uint,
    payment-date: uint,
    payment-period: (string-ascii 20)
  }
)

(define-map lease-payment-counters
  { land-id: uint, lease-id: uint }
  { counter: uint }
)

(define-public (create-lease (land-id uint) (lessee principal) (lease-duration-blocks uint) (monthly-rent uint) (usage-type (string-ascii 50)) (deposit-amount uint))
  (let
    (
      (land-data (unwrap! (map-get? land-registry { land-id: land-id }) ERR_LAND_NOT_FOUND))
      (current-counter (default-to u0 (get counter (map-get? lease-counters { land-id: land-id }))))
      (new-lease-id (+ current-counter u1))
      (lease-end-block (+ stacks-block-height lease-duration-blocks))
    )
    (asserts! (is-eq tx-sender (get owner land-data)) ERR_NOT_AUTHORIZED)
    (asserts! (get is-active land-data) ERR_LAND_NOT_FOUND)
    (asserts! (> monthly-rent u0) ERR_INVALID_PRICE)
    (asserts! (> lease-duration-blocks u0) ERR_INVALID_PRICE)
    (map-set land-leases
      { land-id: land-id, lease-id: new-lease-id }
      {
        lessor: tx-sender,
        lessee: lessee,
        lease-start: stacks-block-height,
        lease-end: lease-end-block,
        monthly-rent: monthly-rent,
        usage-type: usage-type,
        is-active: true,
        deposit-amount: deposit-amount
      }
    )
    (map-set lease-counters
      { land-id: land-id }
      { counter: new-lease-id }
    )
    (ok new-lease-id)
  )
)

(define-public (terminate-lease (land-id uint) (lease-id uint))
  (let
    (
      (lease-data (unwrap! (map-get? land-leases { land-id: land-id, lease-id: lease-id }) ERR_LAND_NOT_FOUND))
    )
    (asserts! (or (is-eq tx-sender (get lessor lease-data)) (is-eq tx-sender (get lessee lease-data))) ERR_NOT_AUTHORIZED)
    (asserts! (get is-active lease-data) ERR_LAND_NOT_FOUND)
    (map-set land-leases
      { land-id: land-id, lease-id: lease-id }
      (merge lease-data { is-active: false })
    )
    (ok true)
  )
)

(define-public (record-lease-payment (land-id uint) (lease-id uint) (amount uint) (payment-period (string-ascii 20)))
  (let
    (
      (lease-data (unwrap! (map-get? land-leases { land-id: land-id, lease-id: lease-id }) ERR_LAND_NOT_FOUND))
      (current-counter (default-to u0 (get counter (map-get? lease-payment-counters { land-id: land-id, lease-id: lease-id }))))
      (new-payment-id (+ current-counter u1))
    )
    (asserts! (is-eq tx-sender (get lessee lease-data)) ERR_NOT_AUTHORIZED)
    (asserts! (get is-active lease-data) ERR_LAND_NOT_FOUND)
    (asserts! (< stacks-block-height (get lease-end lease-data)) ERR_LAND_NOT_FOUND)
    (asserts! (> amount u0) ERR_INVALID_PRICE)
    (map-set lease-payments
      { land-id: land-id, lease-id: lease-id, payment-id: new-payment-id }
      {
        amount: amount,
        payment-date: stacks-block-height,
        payment-period: payment-period
      }
    )
    (map-set lease-payment-counters
      { land-id: land-id, lease-id: lease-id }
      { counter: new-payment-id }
    )
    (ok new-payment-id)
  )
)

(define-read-only (get-lease-info (land-id uint) (lease-id uint))
  (map-get? land-leases { land-id: land-id, lease-id: lease-id })
)

(define-read-only (get-lease-count (land-id uint))
  (default-to u0 (get counter (map-get? lease-counters { land-id: land-id })))
)

(define-read-only (get-lease-payment (land-id uint) (lease-id uint) (payment-id uint))
  (map-get? lease-payments { land-id: land-id, lease-id: lease-id, payment-id: payment-id })
)

(define-read-only (is-lease-active (land-id uint) (lease-id uint))
  (match (map-get? land-leases { land-id: land-id, lease-id: lease-id })
    lease-data (and (get is-active lease-data) (< stacks-block-height (get lease-end lease-data)))
    false
  )
)
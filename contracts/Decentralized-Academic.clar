
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

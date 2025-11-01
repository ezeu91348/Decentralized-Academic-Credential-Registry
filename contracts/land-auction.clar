(define-constant ERR_NOT_LAND_OWNER (err u300))
(define-constant ERR_AUCTION_NOT_FOUND (err u301))
(define-constant ERR_AUCTION_ENDED (err u302))
(define-constant ERR_AUCTION_ACTIVE (err u303))
(define-constant ERR_BID_TOO_LOW (err u304))
(define-constant ERR_NO_BIDS (err u305))
(define-constant ERR_INVALID_DURATION (err u306))

(define-data-var auction-id-counter uint u0)

(define-map land-auctions
  { auction-id: uint }
  {
    land-id: uint,
    seller: principal,
    reserve-price: uint,
    start-block: uint,
    end-block: uint,
    is-active: bool,
    finalized: bool
  }
)

(define-map auction-bids
  { auction-id: uint, bidder: principal }
  { bid-amount: uint }
)

(define-map highest-bid-tracker
  { auction-id: uint }
  { highest-bidder: principal, highest-amount: uint }
)

(define-public (create-auction (land-id uint) (reserve-price uint) (duration-blocks uint))
  (let
    (
      (land-data (unwrap! (contract-call? .Decentralized-Academic get-land-info land-id) ERR_AUCTION_NOT_FOUND))
      (new-auction-id (+ (var-get auction-id-counter) u1))
    )
    (asserts! (is-eq tx-sender (get owner land-data)) ERR_NOT_LAND_OWNER)
    (asserts! (> duration-blocks u0) ERR_INVALID_DURATION)
    (asserts! (> reserve-price u0) ERR_BID_TOO_LOW)
    (map-set land-auctions
      { auction-id: new-auction-id }
      {
        land-id: land-id,
        seller: tx-sender,
        reserve-price: reserve-price,
        start-block: stacks-block-height,
        end-block: (+ stacks-block-height duration-blocks),
        is-active: true,
        finalized: false
      }
    )
    (var-set auction-id-counter new-auction-id)
    (ok new-auction-id)
  )
)

(define-public (place-bid (auction-id uint) (bid-amount uint))
  (let
    (
      (auction-data (unwrap! (map-get? land-auctions { auction-id: auction-id }) ERR_AUCTION_NOT_FOUND))
      (current-highest (map-get? highest-bid-tracker { auction-id: auction-id }))
      (min-bid (if (is-some current-highest) (get highest-amount (unwrap-panic current-highest)) (get reserve-price auction-data)))
    )
    (asserts! (get is-active auction-data) ERR_AUCTION_ENDED)
    (asserts! (< stacks-block-height (get end-block auction-data)) ERR_AUCTION_ENDED)
    (asserts! (> bid-amount min-bid) ERR_BID_TOO_LOW)
    (map-set auction-bids { auction-id: auction-id, bidder: tx-sender } { bid-amount: bid-amount })
    (map-set highest-bid-tracker { auction-id: auction-id } { highest-bidder: tx-sender, highest-amount: bid-amount })
    (ok bid-amount)
  )
)

(define-public (finalize-auction (auction-id uint))
  (let
    (
      (auction-data (unwrap! (map-get? land-auctions { auction-id: auction-id }) ERR_AUCTION_NOT_FOUND))
      (highest-bid-data (map-get? highest-bid-tracker { auction-id: auction-id }))
    )
    (asserts! (get is-active auction-data) ERR_AUCTION_ENDED)
    (asserts! (>= stacks-block-height (get end-block auction-data)) ERR_AUCTION_ACTIVE)
    (asserts! (not (get finalized auction-data)) ERR_AUCTION_ENDED)
    (if (is-some highest-bid-data)
      (let ((winner-data (unwrap-panic highest-bid-data)))
        (try! (contract-call? .Decentralized-Academic transfer-land-ownership 
          (get land-id auction-data) 
          (get highest-bidder winner-data) 
          (get highest-amount winner-data)))
        (map-set land-auctions { auction-id: auction-id } (merge auction-data { is-active: false, finalized: true }))
        (ok { winner: (get highest-bidder winner-data), amount: (get highest-amount winner-data) }))
      (begin
        (map-set land-auctions { auction-id: auction-id } (merge auction-data { is-active: false, finalized: true }))
        ERR_NO_BIDS))
  )
)

(define-read-only (get-auction-info (auction-id uint))
  (map-get? land-auctions { auction-id: auction-id })
)

(define-read-only (get-bidder-bid (auction-id uint) (bidder principal))
  (map-get? auction-bids { auction-id: auction-id, bidder: bidder })
)

(define-read-only (get-highest-bid (auction-id uint))
  (map-get? highest-bid-tracker { auction-id: auction-id })
)

(define-read-only (get-current-auction-id)
  (var-get auction-id-counter)
)
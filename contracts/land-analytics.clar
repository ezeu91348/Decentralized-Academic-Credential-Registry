(define-constant ERR_ANALYTICS_NOT_FOUND (err u200))
(define-constant ERR_INSUFFICIENT_DATA (err u201))
(define-constant MINIMUM_VALUATION_COUNT u2)
(define-constant PERFORMANCE_SCALE u10000)

(define-data-var analytics-update-counter uint u0)

(define-map land-performance-metrics
  { land-id: uint }
  {
    total-revenue: uint,
    average-occupancy-rate: uint,
    appreciation-rate: uint,
    roi-percentage: uint,
    performance-score: uint,
    last-updated: uint,
    metrics-count: uint
  }
)

(define-public (calculate-land-performance (land-id uint))
  (let
    (
      (land-data (unwrap! (contract-call? .Decentralized-Academic get-land-info land-id) ERR_ANALYTICS_NOT_FOUND))
      (valuation-count (contract-call? .Decentralized-Academic get-valuation-count land-id))
      (total-revenue (calculate-total-lease-revenue land-id))
      (appreciation-rate (calculate-appreciation-rate land-id))
      (occupancy-rate (calculate-occupancy-rate land-id))
      (roi-percentage (calculate-roi-percentage land-id total-revenue appreciation-rate))
      (performance-score (calculate-performance-score roi-percentage occupancy-rate appreciation-rate))
      (current-metrics (get metrics-count (default-to { total-revenue: u0, average-occupancy-rate: u0, appreciation-rate: u0, roi-percentage: u0, performance-score: u0, last-updated: u0, metrics-count: u0 } (map-get? land-performance-metrics { land-id: land-id }))))
    )
    (asserts! (>= valuation-count MINIMUM_VALUATION_COUNT) ERR_INSUFFICIENT_DATA)
    (map-set land-performance-metrics
      { land-id: land-id }
      {
        total-revenue: total-revenue,
        average-occupancy-rate: occupancy-rate,
        appreciation-rate: appreciation-rate,
        roi-percentage: roi-percentage,
        performance-score: performance-score,
        last-updated: stacks-block-height,
        metrics-count: (+ current-metrics u1)
      }
    )
    (var-set analytics-update-counter (+ (var-get analytics-update-counter) u1))
    (ok performance-score)
  )
)

(define-private (calculate-total-lease-revenue (land-id uint))
  (+ (get-single-lease-revenue land-id u1)
     (+ (get-single-lease-revenue land-id u2)
        (+ (get-single-lease-revenue land-id u3)
           (+ (get-single-lease-revenue land-id u4)
              (get-single-lease-revenue land-id u5)))))
)

(define-private (get-single-lease-revenue (land-id uint) (lease-id uint))
  (match (contract-call? .Decentralized-Academic get-lease-info land-id lease-id)
    lease-data (if (get is-active lease-data) (* (get monthly-rent lease-data) u12) u0)
    u0
  )
)

(define-private (calculate-appreciation-rate (land-id uint))
  (let
    (
      (valuation-count (contract-call? .Decentralized-Academic get-valuation-count land-id))
      (first-valuation (contract-call? .Decentralized-Academic get-land-valuation land-id u1))
      (latest-valuation (contract-call? .Decentralized-Academic get-land-valuation land-id valuation-count))
    )
    (if (and (is-some first-valuation) (is-some latest-valuation))
      (let 
        (
          (first-val (unwrap-panic first-valuation))
          (latest-val (unwrap-panic latest-valuation))
          (initial-amount (get valuation-amount first-val))
          (current-amount (get valuation-amount latest-val))
        )
        (if (> initial-amount u0) 
          (/ (* (- current-amount initial-amount) PERFORMANCE_SCALE) initial-amount) 
          u0))
      u0)
  )
)

(define-private (calculate-occupancy-rate (land-id uint))
  (let
    (
      (lease-count (contract-call? .Decentralized-Academic get-lease-count land-id))
      (active-leases (+ (count-single-active-lease land-id u1)
                        (+ (count-single-active-lease land-id u2)
                           (+ (count-single-active-lease land-id u3)
                              (+ (count-single-active-lease land-id u4)
                                 (count-single-active-lease land-id u5))))))
    )
    (if (> lease-count u0) (/ (* active-leases PERFORMANCE_SCALE) lease-count) u0)
  )
)

(define-private (count-single-active-lease (land-id uint) (lease-id uint))
  (if (contract-call? .Decentralized-Academic is-lease-active land-id lease-id) u1 u0)
)

(define-private (calculate-roi-percentage (land-id uint) (revenue uint) (appreciation uint))
  (let
    (
      (land-data (contract-call? .Decentralized-Academic get-land-info land-id))
      (total-value (* (get total-shares (unwrap-panic land-data)) (get price-per-share (unwrap-panic land-data))))
    )
    (if (> total-value u0) (/ (* (+ revenue appreciation) PERFORMANCE_SCALE) total-value) u0)
  )
)

(define-private (calculate-performance-score (roi uint) (occupancy uint) (appreciation uint))
  (/ (+ (* roi u4) (* occupancy u3) (* appreciation u3)) u10)
)

(define-read-only (get-land-performance (land-id uint))
  (map-get? land-performance-metrics { land-id: land-id })
)

(define-read-only (get-performance-comparison (land-id-a uint) (land-id-b uint))
  (let
    (
      (metrics-a (map-get? land-performance-metrics { land-id: land-id-a }))
      (metrics-b (map-get? land-performance-metrics { land-id: land-id-b }))
    )
    (if (and (is-some metrics-a) (is-some metrics-b))
      (let
        (
          (data-a (unwrap-panic metrics-a))
          (data-b (unwrap-panic metrics-b))
        )
        (some {
          land-a: land-id-a,
          score-a: (get performance-score data-a),
          land-b: land-id-b,
          score-b: (get performance-score data-b),
          better-performer: (if (> (get performance-score data-a) (get performance-score data-b)) land-id-a land-id-b)
        }))
      none
    )
  )
)

(define-read-only (get-analytics-summary)
  {
    total-updates: (var-get analytics-update-counter),
    last-update-block: stacks-block-height
  }
)

(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_NOT_AUTHORIZED (err u400))
(define-constant ERR_LAND_NOT_FOUND (err u401))
(define-constant ERR_ASSESSMENT_NOT_FOUND (err u402))
(define-constant ERR_INVALID_AMOUNT (err u403))
(define-constant ERR_PAYMENT_EXISTS (err u404))
(define-constant TAX_RATE_BASIS u10000)

(define-data-var default-tax-rate uint u125)
(define-data-var assessment-id-counter uint u0)
(define-data-var payment-id-counter uint u0)

(define-map authorized-tax-assessors
  { assessor: principal }
  { is-authorized: bool }
)

(define-map land-tax-assessments
  { assessment-id: uint }
  {
    land-id: uint,
    tax-year: uint,
    assessed-value: uint,
    tax-amount: uint,
    assessment-date: uint,
    due-date: uint,
    is-paid: bool
  }
)

(define-map tax-payments
  { payment-id: uint }
  {
    assessment-id: uint,
    payer: principal,
    payment-amount: uint,
    payment-date: uint,
    payment-method: (string-ascii 30)
  }
)

(define-map land-assessment-index
  { land-id: uint, tax-year: uint }
  { assessment-id: uint }
)

(define-public (authorize-tax-assessor (assessor principal))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_NOT_AUTHORIZED)
    (ok (map-set authorized-tax-assessors { assessor: assessor } { is-authorized: true }))
  )
)

(define-public (create-tax-assessment (land-id uint) (tax-year uint) (assessed-value uint) (due-date-blocks uint))
  (let
    (
      (land-data (unwrap! (contract-call? .Decentralized-Academic get-land-info land-id) ERR_LAND_NOT_FOUND))
      (new-assessment-id (+ (var-get assessment-id-counter) u1))
      (assessor-auth (default-to false (get is-authorized (map-get? authorized-tax-assessors { assessor: tx-sender }))))
      (tax-amount (/ (* assessed-value (var-get default-tax-rate)) TAX_RATE_BASIS))
      (due-date (+ stacks-block-height due-date-blocks))
    )
    (asserts! assessor-auth ERR_NOT_AUTHORIZED)
    (asserts! (> assessed-value u0) ERR_INVALID_AMOUNT)
    (map-set land-tax-assessments
      { assessment-id: new-assessment-id }
      {
        land-id: land-id,
        tax-year: tax-year,
        assessed-value: assessed-value,
        tax-amount: tax-amount,
        assessment-date: stacks-block-height,
        due-date: due-date,
        is-paid: false
      }
    )
    (map-set land-assessment-index { land-id: land-id, tax-year: tax-year } { assessment-id: new-assessment-id })
    (var-set assessment-id-counter new-assessment-id)
    (ok new-assessment-id)
  )
)

(define-public (record-tax-payment (assessment-id uint) (payment-amount uint) (payment-method (string-ascii 30)))
  (let
    (
      (assessment-data (unwrap! (map-get? land-tax-assessments { assessment-id: assessment-id }) ERR_ASSESSMENT_NOT_FOUND))
      (new-payment-id (+ (var-get payment-id-counter) u1))
    )
    (asserts! (not (get is-paid assessment-data)) ERR_PAYMENT_EXISTS)
    (asserts! (>= payment-amount (get tax-amount assessment-data)) ERR_INVALID_AMOUNT)
    (map-set tax-payments
      { payment-id: new-payment-id }
      {
        assessment-id: assessment-id,
        payer: tx-sender,
        payment-amount: payment-amount,
        payment-date: stacks-block-height,
        payment-method: payment-method
      }
    )
    (map-set land-tax-assessments
      { assessment-id: assessment-id }
      (merge assessment-data { is-paid: true })
    )
    (var-set payment-id-counter new-payment-id)
    (ok new-payment-id)
  )
)

(define-public (update-tax-rate (new-rate uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_NOT_AUTHORIZED)
    (asserts! (<= new-rate u5000) ERR_INVALID_AMOUNT)
    (var-set default-tax-rate new-rate)
    (ok new-rate)
  )
)

(define-read-only (get-tax-assessment (assessment-id uint))
  (map-get? land-tax-assessments { assessment-id: assessment-id })
)

(define-read-only (get-land-tax-by-year (land-id uint) (tax-year uint))
  (match (map-get? land-assessment-index { land-id: land-id, tax-year: tax-year })
    index-data (map-get? land-tax-assessments { assessment-id: (get assessment-id index-data) })
    none
  )
)

(define-read-only (get-payment-info (payment-id uint))
  (map-get? tax-payments { payment-id: payment-id })
)

(define-read-only (is-tax-delinquent (assessment-id uint))
  (match (map-get? land-tax-assessments { assessment-id: assessment-id })
    assessment-data (and (not (get is-paid assessment-data)) (>= stacks-block-height (get due-date assessment-data)))
    false
  )
)

(define-read-only (get-current-tax-rate)
  (var-get default-tax-rate)
)

(define-read-only (is-authorized-assessor (assessor principal))
  (default-to false (get is-authorized (map-get? authorized-tax-assessors { assessor: assessor })))
)

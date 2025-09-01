
;; title: supplier-payment-system
;; version: 1.0.0
;; summary: B2B supplier payment automation with delivery verification
;; description: Manages automated payments to suppliers based on delivery confirmations

;; Error constants
(define-constant ERR_UNAUTHORIZED (err u101))
(define-constant ERR_PAYMENT_NOT_FOUND (err u102))
(define-constant ERR_PAYMENT_ALREADY_PROCESSED (err u103))
(define-constant ERR_INSUFFICIENT_FUNDS (err u104))
(define-constant ERR_INVALID_AMOUNT (err u105))
(define-constant ERR_DELIVERY_NOT_CONFIRMED (err u106))
(define-constant ERR_PAYMENT_EXPIRED (err u107))

;; Contract owner
(define-data-var contract-owner principal tx-sender)

;; Payment status enum
(define-constant PAYMENT_PENDING u0)
(define-constant PAYMENT_PROCESSING u1)
(define-constant PAYMENT_COMPLETED u2)
(define-constant PAYMENT_CANCELLED u3)

;; Delivery verification requirement
(define-data-var delivery-verification-required bool true)

;; Data maps
(define-map payments
  { payment-id: uint }
  {
    supplier: principal,
    buyer: principal,
    amount: uint,
    invoice-hash: (buff 32),
    payment-terms: uint,
    status: uint,
    created-at: uint,
    delivery-confirmed: bool,
    payment-deadline: uint
  }
)

(define-map supplier-balances
  { supplier: principal }
  { balance: uint }
)

(define-data-var next-payment-id uint u1)

;; Public functions

;; Create a new payment agreement
(define-public (create-payment (supplier principal) 
                              (amount uint) 
                              (invoice-hash (buff 32))
                              (payment-terms-days uint))
  (let ((payment-id (var-get next-payment-id))
        (current-block stacks-block-height)
        (deadline (+ current-block payment-terms-days)))
    (asserts! (> amount u0) ERR_INVALID_AMOUNT)
    (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
    (map-set payments
      { payment-id: payment-id }
      {
        supplier: supplier,
        buyer: tx-sender,
        amount: amount,
        invoice-hash: invoice-hash,
        payment-terms: payment-terms-days,
        status: PAYMENT_PENDING,
        created-at: current-block,
        delivery-confirmed: false,
        payment-deadline: deadline
      }
    )
    (var-set next-payment-id (+ payment-id u1))
    (ok payment-id)
  )
)

;; Confirm delivery (only buyer can confirm)
(define-public (confirm-delivery (payment-id uint))
  (let ((payment-data (unwrap! (map-get? payments { payment-id: payment-id }) ERR_PAYMENT_NOT_FOUND)))
    (asserts! (is-eq tx-sender (get buyer payment-data)) ERR_UNAUTHORIZED)
    (asserts! (is-eq (get status payment-data) PAYMENT_PENDING) ERR_PAYMENT_ALREADY_PROCESSED)
    (map-set payments
      { payment-id: payment-id }
      (merge payment-data { delivery-confirmed: true, status: PAYMENT_PROCESSING })
    )
    (ok true)
  )
)

;; Process automatic payment after delivery confirmation
(define-public (process-payment (payment-id uint))
  (let ((payment-data (unwrap! (map-get? payments { payment-id: payment-id }) ERR_PAYMENT_NOT_FOUND)))
    (asserts! (get delivery-confirmed payment-data) ERR_DELIVERY_NOT_CONFIRMED)
    (asserts! (is-eq (get status payment-data) PAYMENT_PROCESSING) ERR_PAYMENT_ALREADY_PROCESSED)
    (asserts! (<= stacks-block-height (get payment-deadline payment-data)) ERR_PAYMENT_EXPIRED)
    
    ;; Transfer payment to supplier
    (try! (as-contract (stx-transfer? (get amount payment-data) tx-sender (get supplier payment-data))))
    
    ;; Update payment status
    (map-set payments
      { payment-id: payment-id }
      (merge payment-data { status: PAYMENT_COMPLETED })
    )
    
    ;; Update supplier balance tracking
    (let ((current-balance (default-to u0 (get balance (map-get? supplier-balances { supplier: (get supplier payment-data) })))))
      (map-set supplier-balances
        { supplier: (get supplier payment-data) }
        { balance: (+ current-balance (get amount payment-data)) }
      )
    )
    
    (ok true)
  )
)

;; Cancel payment (only buyer can cancel before delivery confirmation)
(define-public (cancel-payment (payment-id uint))
  (let ((payment-data (unwrap! (map-get? payments { payment-id: payment-id }) ERR_PAYMENT_NOT_FOUND)))
    (asserts! (is-eq tx-sender (get buyer payment-data)) ERR_UNAUTHORIZED)
    (asserts! (is-eq (get status payment-data) PAYMENT_PENDING) ERR_PAYMENT_ALREADY_PROCESSED)
    (asserts! (not (get delivery-confirmed payment-data)) ERR_DELIVERY_NOT_CONFIRMED)
    
    ;; Refund to buyer
    (try! (as-contract (stx-transfer? (get amount payment-data) tx-sender (get buyer payment-data))))
    
    ;; Update status
    (map-set payments
      { payment-id: payment-id }
      (merge payment-data { status: PAYMENT_CANCELLED })
    )
    
    (ok true)
  )
)

;; Emergency refund for expired payments (contract owner only)
(define-public (emergency-refund (payment-id uint))
  (let ((payment-data (unwrap! (map-get? payments { payment-id: payment-id }) ERR_PAYMENT_NOT_FOUND)))
    (asserts! (is-eq tx-sender (var-get contract-owner)) ERR_UNAUTHORIZED)
    (asserts! (> stacks-block-height (get payment-deadline payment-data)) ERR_PAYMENT_EXPIRED)
    (asserts! (not (is-eq (get status payment-data) PAYMENT_COMPLETED)) ERR_PAYMENT_ALREADY_PROCESSED)
    
    ;; Refund to buyer
    (try! (as-contract (stx-transfer? (get amount payment-data) tx-sender (get buyer payment-data))))
    
    ;; Update status
    (map-set payments
      { payment-id: payment-id }
      (merge payment-data { status: PAYMENT_CANCELLED })
    )
    
    (ok true)
  )
)

;; Read-only functions

(define-read-only (get-payment (payment-id uint))
  (map-get? payments { payment-id: payment-id })
)

(define-read-only (get-supplier-balance (supplier principal))
  (default-to u0 (get balance (map-get? supplier-balances { supplier: supplier })))
)

(define-read-only (get-next-payment-id)
  (var-get next-payment-id)
)

(define-read-only (is-delivery-confirmed (payment-id uint))
  (match (map-get? payments { payment-id: payment-id })
    payment-data (get delivery-confirmed payment-data)
    false
  )
)

(define-read-only (get-payment-status (payment-id uint))
  (match (map-get? payments { payment-id: payment-id })
    payment-data (get status payment-data)
    u404
  )
)

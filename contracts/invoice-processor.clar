
;; title: invoice-processor
;; version: 1.0.0
;; summary: Automated invoice processing and validation system
;; description: Handles invoice creation, validation, and status tracking for supplier payments

;; Error constants
(define-constant ERR_UNAUTHORIZED (err u201))
(define-constant ERR_INVOICE_NOT_FOUND (err u202))
(define-constant ERR_INVOICE_ALREADY_PROCESSED (err u203))
(define-constant ERR_INVALID_INVOICE_DATA (err u204))
(define-constant ERR_INVOICE_EXPIRED (err u205))
(define-constant ERR_SUPPLIER_NOT_APPROVED (err u206))

;; Contract owner
(define-data-var contract-owner principal tx-sender)

;; Invoice status enum
(define-constant INVOICE_PENDING u0)
(define-constant INVOICE_APPROVED u1)
(define-constant INVOICE_REJECTED u2)
(define-constant INVOICE_PAID u3)
(define-constant INVOICE_EXPIRED u4)

;; Default invoice expiry (in blocks)
(define-data-var default-invoice-expiry uint u1440) ;; ~10 days

;; Data maps
(define-map invoices
  { invoice-id: uint }
  {
    supplier: principal,
    buyer: principal,
    amount: uint,
    invoice-hash: (buff 32),
    description: (string-ascii 256),
    due-date: uint,
    status: uint,
    created-at: uint,
    approved-at: (optional uint),
    payment-id: (optional uint)
  }
)

(define-map approved-suppliers
  { supplier: principal }
  { approved: bool, approved-at: uint }
)

(define-map supplier-invoice-count
  { supplier: principal }
  { count: uint }
)

(define-data-var next-invoice-id uint u1)

;; Public functions

;; Create a new invoice
(define-public (create-invoice (buyer principal)
                              (amount uint)
                              (invoice-hash (buff 32))
                              (description (string-ascii 256))
                              (due-date uint))
  (let ((invoice-id (var-get next-invoice-id))
        (current-block stacks-block-height)
        (supplier tx-sender))
    (asserts! (> amount u0) ERR_INVALID_INVOICE_DATA)
    (asserts! (> due-date current-block) ERR_INVALID_INVOICE_DATA)
    (asserts! (is-supplier-approved supplier) ERR_SUPPLIER_NOT_APPROVED)
    
    (map-set invoices
      { invoice-id: invoice-id }
      {
        supplier: supplier,
        buyer: buyer,
        amount: amount,
        invoice-hash: invoice-hash,
        description: description,
        due-date: due-date,
        status: INVOICE_PENDING,
        created-at: current-block,
        approved-at: none,
        payment-id: none
      }
    )
    
    ;; Update supplier invoice count
    (let ((current-count (default-to u0 (get count (map-get? supplier-invoice-count { supplier: supplier })))))
      (map-set supplier-invoice-count
        { supplier: supplier }
        { count: (+ current-count u1) }
      )
    )
    
    (var-set next-invoice-id (+ invoice-id u1))
    (ok invoice-id)
  )
)

;; Approve invoice (only buyer can approve)
(define-public (approve-invoice (invoice-id uint))
  (let ((invoice-data (unwrap! (map-get? invoices { invoice-id: invoice-id }) ERR_INVOICE_NOT_FOUND))
        (current-block stacks-block-height))
    (asserts! (is-eq tx-sender (get buyer invoice-data)) ERR_UNAUTHORIZED)
    (asserts! (is-eq (get status invoice-data) INVOICE_PENDING) ERR_INVOICE_ALREADY_PROCESSED)
    (asserts! (<= current-block (get due-date invoice-data)) ERR_INVOICE_EXPIRED)
    
    (map-set invoices
      { invoice-id: invoice-id }
      (merge invoice-data { 
        status: INVOICE_APPROVED,
        approved-at: (some current-block)
      })
    )
    
    (ok true)
  )
)

;; Reject invoice (only buyer can reject)
(define-public (reject-invoice (invoice-id uint))
  (let ((invoice-data (unwrap! (map-get? invoices { invoice-id: invoice-id }) ERR_INVOICE_NOT_FOUND)))
    (asserts! (is-eq tx-sender (get buyer invoice-data)) ERR_UNAUTHORIZED)
    (asserts! (is-eq (get status invoice-data) INVOICE_PENDING) ERR_INVOICE_ALREADY_PROCESSED)
    
    (map-set invoices
      { invoice-id: invoice-id }
      (merge invoice-data { status: INVOICE_REJECTED })
    )
    
    (ok true)
  )
)

;; Mark invoice as paid (called by payment system)
(define-public (mark-invoice-paid (invoice-id uint) (payment-id uint))
  (let ((invoice-data (unwrap! (map-get? invoices { invoice-id: invoice-id }) ERR_INVOICE_NOT_FOUND)))
    (asserts! (is-eq (get status invoice-data) INVOICE_APPROVED) ERR_INVOICE_ALREADY_PROCESSED)
    
    (map-set invoices
      { invoice-id: invoice-id }
      (merge invoice-data { 
        status: INVOICE_PAID,
        payment-id: (some payment-id)
      })
    )
    
    (ok true)
  )
)

;; Expire old invoices (anyone can call)
(define-public (expire-invoice (invoice-id uint))
  (let ((invoice-data (unwrap! (map-get? invoices { invoice-id: invoice-id }) ERR_INVOICE_NOT_FOUND)))
    (asserts! (> stacks-block-height (get due-date invoice-data)) ERR_INVOICE_EXPIRED)
    (asserts! (is-eq (get status invoice-data) INVOICE_PENDING) ERR_INVOICE_ALREADY_PROCESSED)
    
    (map-set invoices
      { invoice-id: invoice-id }
      (merge invoice-data { status: INVOICE_EXPIRED })
    )
    
    (ok true)
  )
)

;; Approve supplier (contract owner only)
(define-public (approve-supplier (supplier principal))
  (begin
    (asserts! (is-eq tx-sender (var-get contract-owner)) ERR_UNAUTHORIZED)
    (map-set approved-suppliers
      { supplier: supplier }
      { approved: true, approved-at: stacks-block-height }
    )
    (ok true)
  )
)

;; Revoke supplier approval (contract owner only)
(define-public (revoke-supplier-approval (supplier principal))
  (begin
    (asserts! (is-eq tx-sender (var-get contract-owner)) ERR_UNAUTHORIZED)
    (map-set approved-suppliers
      { supplier: supplier }
      { approved: false, approved-at: stacks-block-height }
    )
    (ok true)
  )
)

;; Read-only functions

(define-read-only (get-invoice (invoice-id uint))
  (map-get? invoices { invoice-id: invoice-id })
)

(define-read-only (get-next-invoice-id)
  (var-get next-invoice-id)
)

(define-read-only (is-supplier-approved (supplier principal))
  (default-to false (get approved (map-get? approved-suppliers { supplier: supplier })))
)

(define-read-only (get-supplier-invoice-count (supplier principal))
  (default-to u0 (get count (map-get? supplier-invoice-count { supplier: supplier })))
)

(define-read-only (get-invoice-status (invoice-id uint))
  (match (map-get? invoices { invoice-id: invoice-id })
    invoice-data (get status invoice-data)
    u404
  )
)

(define-read-only (is-invoice-expired (invoice-id uint))
  (match (map-get? invoices { invoice-id: invoice-id })
    invoice-data (> stacks-block-height (get due-date invoice-data))
    false
  )
)

(define-read-only (get-invoice-payment-id (invoice-id uint))
  (match (map-get? invoices { invoice-id: invoice-id })
    invoice-data (get payment-id invoice-data)
    none
  )
)

;; Private functions

(define-private (validate-invoice-data (amount uint) (due-date uint))
  (and 
    (> amount u0)
    (> due-date stacks-block-height)
  )
)

;; Estate Planning Contract
;; Manages will preparation and inheritance coordination

(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u500))
(define-constant ERR-INVALID-INPUT (err u501))
(define-constant ERR-ESTATE-NOT-FOUND (err u502))
(define-constant ERR-BENEFICIARY-EXISTS (err u503))
(define-constant ERR-BENEFICIARY-NOT-FOUND (err u504))

;; Estate planning data structures
(define-map user-estates
  { user: principal }
  {
    total-assets: uint,
    total-debts: uint,
    net-worth: uint,
    has-will: bool,
    has-trust: bool,
    has-power-of-attorney: bool,
    executor: (optional principal),
    estate-tax-estimate: uint,
    last-updated: uint
  }
)

(define-map estate-beneficiaries
  { user: principal, beneficiary-id: uint }
  {
    beneficiary-address: principal,
    beneficiary-name: (string-ascii 50),
    relationship: (string-ascii 20),
    allocation-percentage: uint,
    asset-type: (string-ascii 30),
    contingent: bool
  }
)

(define-map asset-inventory
  { user: principal, asset-id: uint }
  {
    asset-type: (string-ascii 30),
    asset-description: (string-ascii 100),
    estimated-value: uint,
    ownership-type: (string-ascii 20), ;; sole, joint, trust
    beneficiary-designation: bool
  }
)

(define-data-var total-estates uint u0)
(define-data-var estate-tax-threshold uint u1206000000) ;; $12.06M in 2022
(define-data-var next-beneficiary-id uint u1)
(define-data-var next-asset-id uint u1)

;; Public functions

(define-public (create-estate-plan
  (total-assets uint)
  (total-debts uint)
  (has-will bool)
  (has-trust bool)
  (has-power-of-attorney bool)
  (executor (optional principal)))
  (let ((user tx-sender)
        (net-worth (if (> total-assets total-debts) (- total-assets total-debts) u0))
        (estate-tax (calculate-estate-tax net-worth)))

    (asserts! (> total-assets u0) ERR-INVALID-INPUT)

    (map-set user-estates
      { user: user }
      {
        total-assets: total-assets,
        total-debts: total-debts,
        net-worth: net-worth,
        has-will: has-will,
        has-trust: has-trust,
        has-power-of-attorney: has-power-of-attorney,
        executor: executor,
        estate-tax-estimate: estate-tax,
        last-updated: block-height
      })

    (var-set total-estates (+ (var-get total-estates) u1))
    (ok net-worth)))

(define-public (add-beneficiary
  (beneficiary-address principal)
  (beneficiary-name (string-ascii 50))
  (relationship (string-ascii 20))
  (allocation-percentage uint)
  (asset-type (string-ascii 30))
  (contingent bool))
  (let ((user tx-sender)
        (beneficiary-id (var-get next-beneficiary-id)))

    (asserts! (and (> allocation-percentage u0) (<= allocation-percentage u100)) ERR-INVALID-INPUT)
    (asserts! (is-some (map-get? user-estates { user: user })) ERR-ESTATE-NOT-FOUND)

    (map-set estate-beneficiaries
      { user: user, beneficiary-id: beneficiary-id }
      {
        beneficiary-address: beneficiary-address,
        beneficiary-name: beneficiary-name,
        relationship: relationship,
        allocation-percentage: allocation-percentage,
        asset-type: asset-type,
        contingent: contingent
      })

    (var-set next-beneficiary-id (+ beneficiary-id u1))
    (ok beneficiary-id)))

(define-public (add-asset
  (asset-type (string-ascii 30))
  (asset-description (string-ascii 100))
  (estimated-value uint)
  (ownership-type (string-ascii 20))
  (beneficiary-designation bool))
  (let ((user tx-sender)
        (asset-id (var-get next-asset-id)))

    (asserts! (> estimated-value u0) ERR-INVALID-INPUT)
    (asserts! (is-some (map-get? user-estates { user: user })) ERR-ESTATE-NOT-FOUND)

    (map-set asset-inventory
      { user: user, asset-id: asset-id }
      {
        asset-type: asset-type,
        asset-description: asset-description,
        estimated-value: estimated-value,
        ownership-type: ownership-type,
        beneficiary-designation: beneficiary-designation
      })

    (var-set next-asset-id (+ asset-id u1))
    (ok asset-id)))

(define-public (update-estate-values (new-total-assets uint) (new-total-debts uint))
  (let ((user tx-sender)
        (estate (unwrap! (map-get? user-estates { user: user }) ERR-ESTATE-NOT-FOUND)))

    (asserts! (> new-total-assets u0) ERR-INVALID-INPUT)

    (let ((new-net-worth (if (> new-total-assets new-total-debts) (- new-total-assets new-total-debts) u0))
          (new-estate-tax (calculate-estate-tax new-net-worth)))

      (map-set user-estates
        { user: user }
        (merge estate {
          total-assets: new-total-assets,
          total-debts: new-total-debts,
          net-worth: new-net-worth,
          estate-tax-estimate: new-estate-tax,
          last-updated: block-height
        }))

      (ok new-net-worth))))

(define-public (update-estate-documents (has-will bool) (has-trust bool) (has-poa bool))
  (let ((user tx-sender)
        (estate (unwrap! (map-get? user-estates { user: user }) ERR-ESTATE-NOT-FOUND)))

    (map-set user-estates
      { user: user }
      (merge estate {
        has-will: has-will,
        has-trust: has-trust,
        has-power-of-attorney: has-poa,
        last-updated: block-height
      }))

    (ok true)))

;; Read-only functions

(define-read-only (get-user-estate (user principal))
  (map-get? user-estates { user: user }))

(define-read-only (get-beneficiary (user principal) (beneficiary-id uint))
  (map-get? estate-beneficiaries { user: user, beneficiary-id: beneficiary-id }))

(define-read-only (get-asset (user principal) (asset-id uint))
  (map-get? asset-inventory { user: user, asset-id: asset-id }))

(define-read-only (calculate-estate-tax (net-worth uint))
  (if (> net-worth (var-get estate-tax-threshold))
    (let ((taxable-amount (- net-worth (var-get estate-tax-threshold)))
          (tax-rate u40)) ;; 40% federal estate tax rate
      (/ (* taxable-amount tax-rate) u100))
    u0))

(define-read-only (get-estate-planning-score (user principal))
  (match (map-get? user-estates { user: user })
    estate
    (let ((base-score u0)
          (will-score (if (get has-will estate) u25 u0))
          (trust-score (if (get has-trust estate) u20 u0))
          (poa-score (if (get has-power-of-attorney estate) u15 u0))
          (executor-score (if (is-some (get executor estate)) u10 u0))
          (beneficiary-score u30)) ;; Assume beneficiaries are set up
      (+ base-score will-score trust-score poa-score executor-score beneficiary-score))
    u0))

(define-read-only (needs-estate-planning (user principal))
  (match (map-get? user-estates { user: user })
    estate
    (let ((net-worth (get net-worth estate))
          (has-basic-docs (and (get has-will estate) (get has-power-of-attorney estate))))
      (or (> net-worth u100000000) ;; $1M+ needs planning
          (not has-basic-docs)))
    true))

(define-read-only (get-tax-saving-strategies (user principal))
  (match (map-get? user-estates { user: user })
    estate
    (let ((net-worth (get net-worth estate))
          (estate-tax (get estate-tax-estimate estate)))
      (if (> estate-tax u0)
        "Consider: Gifting strategies, Charitable trusts, Life insurance trusts"
        (if (> net-worth u500000000) ;; $5M+
          "Consider: Irrevocable trusts, Family limited partnerships"
          "Basic estate planning documents sufficient")))
    "No estate plan found"))

(define-read-only (calculate-probate-costs (user principal))
  (match (map-get? user-estates { user: user })
    estate
    (let ((total-assets (get total-assets estate))
          (has-trust (get has-trust estate)))
      (if has-trust
        u0 ;; Trust assets avoid probate
        (let ((probate-rate u3)) ;; 3% of assets
          (/ (* total-assets probate-rate) u100))))
    u0))

(define-read-only (get-inheritance-distribution (user principal))
  (match (map-get? user-estates { user: user })
    estate
    (let ((net-worth (get net-worth estate))
          (estate-tax (get estate-tax-estimate estate))
          (probate-costs (calculate-probate-costs user)))
      (- net-worth estate-tax probate-costs))
    u0))

(define-read-only (get-total-estates)
  (var-get total-estates))

;; Admin functions

(define-public (update-estate-tax-threshold (new-threshold uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (var-set estate-tax-threshold new-threshold)
    (ok new-threshold)))

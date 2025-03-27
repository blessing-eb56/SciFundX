;; SciFundX: Decentralized Scientific Research Funding
;; This contract allows researchers to submit proposals, reviewers to vote, funders to support approved projects, and implement a refund mechanism

;; Define constants
(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_NOT_AUTHORIZED (err u100))
(define-constant ERR_INVALID_AMOUNT (err u101))
(define-constant ERR_PROJ_NOT_FOUND (err u102))
(define-constant ERR_ALREADY_FUNDED (err u103))
(define-constant ERR_INVALID_TITLE (err u104))
(define-constant ERR_INVALID_DESCRIPTION (err u105))
(define-constant ERR_INVALID_FUNDING_TARGET (err u106))
(define-constant ERR_INVALID_PROJ_ID (err u107))
(define-constant ERR_INVALID_STATUS (err u108))
(define-constant ERR_ALREADY_VOTED (err u109))
(define-constant ERR_NOT_EVALUATOR (err u110))
(define-constant ERR_INVALID_EVALUATOR (err u111))
(define-constant ERR_NOT_BACKER (err u112))
(define-constant ERR_REFUND_NOT_AVAILABLE (err u113))

;; Define project statuses
(define-constant STATUS_SUBMITTED u0)
(define-constant STATUS_UNDER_EVALUATION u1)
(define-constant STATUS_APPROVED u2)
(define-constant STATUS_REJECTED u3)
(define-constant STATUS_OPEN u4)
(define-constant STATUS_FUNDED u5)
(define-constant STATUS_CLOSED u6)
(define-constant STATUS_REFUNDABLE u7)

;; Define vote options
(define-constant VOTE_APPROVE u1)
(define-constant VOTE_REJECT u0)

;; Define data maps
(define-map science-projects
  { proj-id: uint }
  {
    scientist: principal,
    title: (string-ascii 100),
    description: (string-ascii 1000),
    funding-target: uint,
    current-funding: uint,
    status: uint,
    approve-votes: uint,
    reject-votes: uint,
    deadline: uint
  }
)

(define-map project-backing
  { proj-id: uint, backer: principal }
  { amount: uint }
)

(define-map evaluators
  { evaluator: principal }
  { is-active: bool }
)

(define-map evaluations
  { proj-id: uint, evaluator: principal }
  { vote: uint }
)

;; Define variables
(define-data-var project-counter uint u0)
(define-data-var required-evaluations uint u3)
(define-data-var funding-duration uint u43200) ;; Default to 30 days (in blocks, assuming 1 block every 60 seconds)

;; Helper functions for input validation
(define-private (is-valid-title (title (string-ascii 100)))
  (and (> (len title) u0) (<= (len title) u100))
)

(define-private (is-valid-description (description (string-ascii 1000)))
  (and (> (len description) u0) (<= (len description) u1000))
)

(define-private (is-valid-funding-target (funding-target uint))
  (> funding-target u0)
)

(define-private (is-valid-proj-id (proj-id uint))
  (<= proj-id (var-get project-counter))
)

(define-private (is-valid-status (status uint))
  (and (>= status STATUS_SUBMITTED) (<= status STATUS_REFUNDABLE))
)

(define-private (is-evaluator (account principal))
  (default-to false (get is-active (map-get? evaluators { evaluator: account })))
)

;; Public functions

;; Submit a new research project
(define-public (submit-project (title (string-ascii 100)) (description (string-ascii 1000)) (funding-target uint))
  (begin
    (asserts! (is-valid-title title) ERR_INVALID_TITLE)
    (asserts! (is-valid-description description) ERR_INVALID_DESCRIPTION)
    (asserts! (is-valid-funding-target funding-target) ERR_INVALID_FUNDING_TARGET)
    (let
      (
        (proj-id (+ (var-get project-counter) u1))
        (deadline (+ block-height (var-get funding-duration)))
      )
      (map-set science-projects
        { proj-id: proj-id }
        {
          scientist: tx-sender,
          title: title,
          description: description,
          funding-target: funding-target,
          current-funding: u0,
          status: STATUS_SUBMITTED,
          approve-votes: u0,
          reject-votes: u0,
          deadline: deadline
        }
      )
      (var-set project-counter proj-id)
      (ok proj-id)
    )
  )
)

;; Vote on a project (only for evaluators)
(define-public (evaluate-project (proj-id uint) (vote uint))
  (begin
    (asserts! (is-evaluator tx-sender) ERR_NOT_EVALUATOR)
    (asserts! (is-valid-proj-id proj-id) ERR_INVALID_PROJ_ID)
    (asserts! (or (is-eq vote VOTE_APPROVE) (is-eq vote VOTE_REJECT)) ERR_INVALID_STATUS)
    (let
      (
        (project (unwrap-panic (map-get? science-projects { proj-id: proj-id })))
        (existing-vote (map-get? evaluations { proj-id: proj-id, evaluator: tx-sender }))
      )
      (asserts! (is-eq (get status project) STATUS_UNDER_EVALUATION) ERR_INVALID_STATUS)
      (asserts! (is-none existing-vote) ERR_ALREADY_VOTED)
      (map-set evaluations { proj-id: proj-id, evaluator: tx-sender } { vote: vote })
      (if (is-eq vote VOTE_APPROVE)
        (map-set science-projects { proj-id: proj-id }
          (merge project { approve-votes: (+ (get approve-votes project) u1) }))
        (map-set science-projects { proj-id: proj-id }
          (merge project { reject-votes: (+ (get reject-votes project) u1) }))
      )
      (let
        (
          (updated-project (unwrap-panic (map-get? science-projects { proj-id: proj-id })))
          (total-votes (+ (get approve-votes updated-project) (get reject-votes updated-project)))
        )
        (if (>= total-votes (var-get required-evaluations))
          (if (> (get approve-votes updated-project) (get reject-votes updated-project))
            (map-set science-projects { proj-id: proj-id }
              (merge updated-project { status: STATUS_APPROVED }))
            (map-set science-projects { proj-id: proj-id }
              (merge updated-project { status: STATUS_REJECTED }))
          )
          true
        )
      )
      (ok true)
    )
  )
)

;; Fund a research project
(define-public (back-project (proj-id uint) (amount uint))
  (begin
    (asserts! (is-valid-proj-id proj-id) ERR_INVALID_PROJ_ID)
    (asserts! (> amount u0) ERR_INVALID_AMOUNT)
    (let
      (
        (project (unwrap-panic (map-get? science-projects { proj-id: proj-id })))
        (new-funding (+ (get current-funding project) amount))
      )
      (asserts! (is-eq (get status project) STATUS_OPEN) ERR_INVALID_STATUS)
      (asserts! (<= new-funding (get funding-target project)) ERR_INVALID_AMOUNT)
      (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
      (map-set science-projects
        { proj-id: proj-id }
        (merge project {
          current-funding: new-funding,
          status: (if (is-eq new-funding (get funding-target project)) STATUS_FUNDED STATUS_OPEN)
        })
      )
      (map-set project-backing
        { proj-id: proj-id, backer: tx-sender }
        { amount: (+ amount (default-to u0 (get amount (map-get? project-backing { proj-id: proj-id, backer: tx-sender })))) }
      )
      (ok true)
    )
  )
)

;; Withdraw funds for a fully funded project (only by the scientist)
(define-public (claim-funds (proj-id uint))
  (begin
    (asserts! (is-valid-proj-id proj-id) ERR_INVALID_PROJ_ID)
    (let
      (
        (project (unwrap-panic (map-get? science-projects { proj-id: proj-id })))
      )
      (asserts! (is-eq (get scientist project) tx-sender) ERR_NOT_AUTHORIZED)
      (asserts! (is-eq (get status project) STATUS_FUNDED) ERR_INVALID_STATUS)
      (try! (as-contract (stx-transfer? (get current-funding project) tx-sender (get scientist project))))
      (map-set science-projects
        { proj-id: proj-id }
        (merge project { current-funding: u0, status: STATUS_CLOSED })
      )
      (ok true)
    )
  )
)

;; Update project status (only by CONTRACT_OWNER)
(define-public (update-project-status (proj-id uint) (new-status uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_NOT_AUTHORIZED)
    (asserts! (is-valid-proj-id proj-id) ERR_INVALID_PROJ_ID)
    (asserts! (is-valid-status new-status) ERR_INVALID_STATUS)
    (let
      (
        (project (unwrap-panic (map-get? science-projects { proj-id: proj-id })))
      )
      (map-set science-projects
        { proj-id: proj-id }
        (merge project { status: new-status })
      )
      (ok true)
    )
  )
)

;; Add an evaluator (only by CONTRACT_OWNER)
(define-public (add-evaluator (evaluator principal))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_NOT_AUTHORIZED)
    (asserts! (not (is-eq evaluator CONTRACT_OWNER)) ERR_INVALID_EVALUATOR)
    (map-set evaluators { evaluator: evaluator } { is-active: true })
    (ok true)
  )
)

;; Remove an evaluator (only by CONTRACT_OWNER)
(define-public (remove-evaluator (evaluator principal))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_NOT_AUTHORIZED)
    (asserts! (not (is-eq evaluator CONTRACT_OWNER)) ERR_INVALID_EVALUATOR)
    (map-delete evaluators { evaluator: evaluator })
    (ok true)
  )
)

;; Set required evaluations (only by CONTRACT_OWNER)
(define-public (set-required-evaluations (new-required-evaluations uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_NOT_AUTHORIZED)
    (asserts! (> new-required-evaluations u0) ERR_INVALID_AMOUNT)
    (var-set required-evaluations new-required-evaluations)
    (ok true)
  )
)

;; Set funding duration (only by CONTRACT_OWNER)
(define-public (set-funding-duration (new-funding-duration uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_NOT_AUTHORIZED)
    (asserts! (> new-funding-duration u0) ERR_INVALID_AMOUNT)
    (var-set funding-duration new-funding-duration)
    (ok true)
  )
)

;; Check if a project is eligible for refund
(define-private (is-refund-eligible (project { proj-id: uint }))
  (let
    (
      (project-data (unwrap-panic (map-get? science-projects project)))
    )
    (or
      (and
        (< (get current-funding project-data) (get funding-target project-data))
        (> block-height (get deadline project-data))
      )
      (is-eq (get status project-data) STATUS_CLOSED)
    )
  )
)

;; Request a refund for a project
(define-public (request-refund (proj-id uint))
  (begin
    (asserts! (is-valid-proj-id proj-id) ERR_INVALID_PROJ_ID)
    (let
      (
        (project (unwrap-panic (map-get? science-projects { proj-id: proj-id })))
        (backing (unwrap-panic (map-get? project-backing { proj-id: proj-id, backer: tx-sender })))
      )
      (asserts! (is-refund-eligible { proj-id: proj-id }) ERR_REFUND_NOT_AVAILABLE)
      (asserts! (> (get amount backing) u0) ERR_NOT_BACKER)
      (try! (as-contract (stx-transfer? (get amount backing) tx-sender tx-sender)))
      (map-delete project-backing { proj-id: proj-id, backer: tx-sender })
      (map-set science-projects
        { proj-id: proj-id }
        (merge project { 
          current-funding: (- (get current-funding project) (get amount backing)),
          status: STATUS_REFUNDABLE
        })
      )
      (ok true)
    )
  )
)

;; Read-only functions

;; Get project details
(define-read-only (get-project (proj-id uint))
  (map-get? science-projects { proj-id: proj-id })
)

;; Get total number of projects
(define-read-only (get-project-count)
  (var-get project-counter)
)

;; Get backing amount for a specific project and backer
(define-read-only (get-backing (proj-id uint) (backer principal))
  (map-get? project-backing { proj-id: proj-id, backer: backer })
)

;; Get project status
(define-read-only (get-project-status (proj-id uint))
  (get status (unwrap-panic (map-get? science-projects { proj-id: proj-id })))
)

;; Check if an account is an evaluator
(define-read-only (is-active-evaluator (account principal))
  (is-evaluator account)
)

;; Get required evaluations
(define-read-only (get-required-evaluations)
  (var-get required-evaluations)
)

;; Get funding duration
(define-read-only (get-funding-duration)
  (var-get funding-duration)
)

;; Check if a project is eligible for refund
(define-read-only (check-refund-eligibility (proj-id uint))
  (is-refund-eligible { proj-id: proj-id })
)
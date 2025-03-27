;; SciFundX - Decentralized Scientific Research Funding Platform
;; Clarity Smart Contract for Stacks Blockchain

;; Data Variables
(define-data-var contract-owner principal tx-sender)
(define-data-var platform-fee uint u30) ;; 0.3% fee in basis points
(define-data-var min-proposal-funds uint u10000000) ;; 10 STX minimum funding amount
(define-data-var review-period uint u14) ;; 14 days review period 

;; Data Maps
(define-map Researchers
  { researcher-id: principal }
  {
    name: (string-utf8 256),
    institution: (string-utf8 256),
    field: (string-utf8 100),
    reputation-score: uint,
    total-funded: uint,
    total-completed: uint,
    peer-review-count: uint
  }
)

(define-map Proposals
  { proposal-id: uint }
  {
    researcher: principal,
    title: (string-utf8 256),
    abstract: (string-utf8 1000),
    full-proposal-hash: (buff 32), ;; IPFS hash of full proposal
    funding-goal: uint,
    current-funding: uint,
    review-score: uint,
    review-count: uint,
    status: (string-utf8 20), ;; "draft", "review", "active", "funded", "completed", "failed"
    created-at: uint,
    milestones: (list 10 uint), ;; list of milestone percentages (e.g. [25, 50, 75, 100])
    current-milestone: uint
  }
)

(define-map Reviews
  { proposal-id: uint, reviewer-id: principal }
  {
    score: uint, ;; 1-10 score
    comment-hash: (buff 32), ;; IPFS hash for review comment
    timestamp: uint,
    status: (string-utf8 10) ;; "pending", "approved", "rejected"
  }
)

(define-map Fundings
  { proposal-id: uint, funder-id: principal }
  {
    amount: uint,
    timestamp: uint
  }
)

(define-map MilestoneReports
  { proposal-id: uint, milestone-number: uint }
  {
    report-hash: (buff 32), ;; IPFS hash for milestone report
    timestamp: uint,
    approvals: uint,
    rejections: uint,
    status: (string-utf8 10) ;; "pending", "approved", "rejected"
  }
)

;; Counters
(define-data-var proposal-id-counter uint u0)
(define-data-var platform-treasury uint u0)

;; Error constants
(define-constant ERR_UNAUTHORIZED (err u1001))
(define-constant ERR_INVALID_PROPOSAL (err u1002))
(define-constant ERR_INSUFFICIENT_FUNDS (err u1003))
(define-constant ERR_INVALID_STATE (err u1004))
(define-constant ERR_ALREADY_REVIEWED (err u1005))
(define-constant ERR_ALREADY_FUNDED (err u1006))
(define-constant ERR_NOT_FOUND (err u1007))

;; Read-only functions
(define-read-only (get-researcher (researcher-id principal))
  (map-get? Researchers { researcher-id: researcher-id })
)

(define-read-only (get-proposal (id uint))
  (map-get? Proposals { proposal-id: id })
)

(define-read-only (get-review (proposal-id uint) (reviewer-id principal))
  (map-get? Reviews { proposal-id: proposal-id, reviewer-id: reviewer-id })
)

(define-read-only (get-funding (proposal-id uint) (funder-id principal))
  (map-get? Fundings { proposal-id: proposal-id, funder-id: funder-id })
)

(define-read-only (get-milestone-report (proposal-id uint) (milestone-number uint))
  (map-get? MilestoneReports { proposal-id: proposal-id, milestone-number: milestone-number })
)

;; Function to register as a researcher
(define-public (register-researcher (name (string-utf8 256)) (institution (string-utf8 256)) (field (string-utf8 100)))
  (let
    ((researcher tx-sender))
    (if (is-some (get-researcher researcher))
      ERR_ALREADY_REGISTERED
      (ok (map-set Researchers
        { researcher-id: researcher }
        {
          name: name,
          institution: institution,
          field: field,
          reputation-score: u50, ;; Start with neutral score of 50
          total-funded: u0,
          total-completed: u0,
          peer-review-count: u0
        }
      ))
    )
  )
)

;; Function to submit a new research proposal
(define-public (submit-proposal 
    (title (string-utf8 256)) 
    (abstract (string-utf8 1000)) 
    (full-proposal-hash (buff 32)) 
    (funding-goal uint)
    (milestones (list 10 uint)))
  (let
    ((researcher tx-sender)
     (proposal-id (+ (var-get proposal-id-counter) u1)))
    
    ;; Check if researcher is registered
    (asserts! (is-some (get-researcher researcher)) ERR_UNAUTHORIZED)
    
    ;; Check minimum funding goal
    (asserts! (>= funding-goal (var-get min-proposal-funds)) ERR_INVALID_PROPOSAL)
    
    ;; Check valid milestone percentages (must sum to 100)
    (asserts! (is-eq (fold + milestones u0) u100) ERR_INVALID_PROPOSAL)
    
    ;; Create the proposal
    (try! (map-set Proposals
      { proposal-id: proposal-id }
      {
        researcher: researcher,
        title: title,
        abstract: abstract,
        full-proposal-hash: full-proposal-hash,
        funding-goal: funding-goal,
        current-funding: u0,
        review-score: u0,
        review-count: u0,
        status: "review",
        created-at: block-height,
        milestones: milestones,
        current-milestone: u0
      }
    ))
    
    ;; Increment the proposal counter
    (var-set proposal-id-counter proposal-id)
    
    ;; Return the proposal ID
    (ok proposal-id)
  )
)

;; Function to review a proposal
(define-public (review-proposal (proposal-id uint) (score uint) (comment-hash (buff 32)))
  (let
    ((reviewer tx-sender)
     (proposal (unwrap! (get-proposal proposal-id) ERR_NOT_FOUND)))
    
    ;; Check if reviewer is registered as a researcher
    (asserts! (is-some (get-researcher reviewer)) ERR_UNAUTHORIZED)
    
    ;; Check if proposal is in review status
    (asserts! (is-eq (get status proposal) "review") ERR_INVALID_STATE)
    
    ;; Check if score is valid (1-10)
    (asserts! (and (>= score u1) (<= score u10)) ERR_INVALID_PROPOSAL)
    
    ;; Check if reviewer has already reviewed this proposal
    (asserts! (is-none (get-review proposal-id reviewer)) ERR_ALREADY_REVIEWED)
    
    ;; Create the review
    (try! (map-set Reviews
      { proposal-id: proposal-id, reviewer-id: reviewer }
      {
        score: score,
        comment-hash: comment-hash,
        timestamp: block-height,
        status: "approved"
      }
    ))
    
    ;; Update proposal review stats
    (let
      ((new-count (+ (get review-count proposal) u1))
       (new-score (+ (get review-score proposal) score))
       (researcher-data (unwrap! (get-researcher reviewer) ERR_NOT_FOUND)))
      
      ;; Update proposal review data
      (try! (map-set Proposals
        { proposal-id: proposal-id }
        (merge proposal {
          review-count: new-count,
          review-score: new-score,
          status: (if (>= new-count u3) "active" "review")
        })
      ))
      
      ;; Update reviewer's peer-review count
      (try! (map-set Researchers
        { researcher-id: reviewer }
        (merge researcher-data {
          peer-review-count: (+ (get peer-review-count researcher-data) u1)
        })
      ))
      
      (ok true)
    )
  )
)

;; Function to fund a proposal
(define-public (fund-proposal (proposal-id uint) (amount uint))
  (let
    ((funder tx-sender)
     (proposal (unwrap! (get-proposal proposal-id) ERR_NOT_FOUND))
     (fee-amount (/ (* amount (var-get platform-fee)) u10000)))
    
    ;; Check if proposal is active
    (asserts! (is-eq (get status proposal) "active") ERR_INVALID_STATE)
    
    ;; Check if amount is positive
    (asserts! (> amount u0) ERR_INVALID_PROPOSAL)
    
    ;; Transfer funds from funder to contract (including platform fee)
    (try! (stx-transfer? amount funder (as-contract tx-sender)))
    
    ;; Update platform treasury with fee
    (var-set platform-treasury (+ (var-get platform-treasury) fee-amount))
    
    ;; Update proposal funding
    (let
      ((funding-amount (- amount fee-amount))
       (new-funding (+ (get current-funding proposal) funding-amount)))
      
      ;; Record the funding
      (let
        ((current-amount (default-to u0 (get amount (get-funding proposal-id funder)))))
        (try! (map-set Fundings
          { proposal-id: proposal-id, funder-id: funder }
          {
            amount: (+ current-amount funding-amount),
            timestamp: block-height
          }
        )))
      
      ;; Update proposal funding status
      (try! (map-set Proposals
        { proposal-id: proposal-id }
        (merge proposal {
          current-funding: new-funding,
          status: (if (>= new-funding (get funding-goal proposal)) "funded" "active")
        })
      ))
      
      (ok true)
    )
  )
)

;; Function to submit a milestone report
(define-public (submit-milestone-report (proposal-id uint) (milestone-number uint) (report-hash (buff 32)))
  (let
    ((researcher tx-sender)
     (proposal (unwrap! (get-proposal proposal-id) ERR_NOT_FOUND)))
    
    ;; Check if sender is the proposal researcher
    (asserts! (is-eq researcher (get researcher proposal)) ERR_UNAUTHORIZED)
    
    ;; Check if proposal is funded
    (asserts! (is-eq (get status proposal) "funded") ERR_INVALID_STATE)
    
    ;; Check if this is the correct next milestone
    (asserts! (is-eq milestone-number (+ (get current-milestone proposal) u1)) ERR_INVALID_STATE)
    
    ;; Check if milestone exists in the proposal
    (asserts! (<= milestone-number (len (get milestones proposal))) ERR_INVALID_PROPOSAL)
    
    ;; Create milestone report
    (try! (map-set MilestoneReports
      { proposal-id: proposal-id, milestone-number: milestone-number }
      {
        report-hash: report-hash,
        timestamp: block-height,
        approvals: u0,
        rejections: u0,
        status: "pending"
      }
    ))
    
    (ok true)
  )
)

;; Function to vote on a milestone report
(define-public (vote-on-milestone (proposal-id uint) (milestone-number uint) (approve bool))
  (let
    ((voter tx-sender)
     (proposal (unwrap! (get-proposal proposal-id) ERR_NOT_FOUND))
     (report (unwrap! (get-milestone-report proposal-id milestone-number) ERR_NOT_FOUND)))
    
    ;; Check if voter has funded the proposal
    (asserts! (is-some (get-funding proposal-id voter)) ERR_UNAUTHORIZED)
    
    ;; Check if report is pending
    (asserts! (is-eq (get status report) "pending") ERR_INVALID_STATE)
    
    ;; Update report votes
    (let
      ((new-approvals (if approve (+ (get approvals report) u1) (get approvals report)))
       (new-rejections (if (not approve) (+ (get rejections report) u1) (get rejections report)))
       ;; Total funders is more complex in reality, this is simplified
       (total-funders u5) ;; Mock value - would need a counter in production
       (threshold (/ total-funders u2)))
      
      ;; Update the report
      (try! (map-set MilestoneReports
        { proposal-id: proposal-id, milestone-number: milestone-number }
        (merge report {
          approvals: new-approvals,
          rejections: new-rejections,
          status: (if (> new-approvals threshold) 
                    "approved" 
                    (if (> new-rejections threshold) "rejected" "pending"))
        })
      ))
      
      ;; If approved, release funds and update milestone
      (if (> new-approvals threshold)
        (release-milestone-funds proposal-id milestone-number)
        (ok true))
    )
  )
)

;; Private function to release milestone funds
(define-private (release-milestone-funds (proposal-id uint) (milestone-number uint))
  (let
    ((proposal (unwrap! (get-proposal proposal-id) ERR_NOT_FOUND))
     (researcher (get researcher proposal))
     (milestones (get milestones proposal))
     (milestone-percentage (unwrap! (element-at milestones (- milestone-number u1)) ERR_NOT_FOUND))
     (total-funding (get current-funding proposal))
     (payment-amount (/ (* total-funding milestone-percentage) u100)))
    
    ;; Transfer funds to researcher
    (try! (as-contract (stx-transfer? payment-amount tx-sender researcher)))
    
    ;; Update proposal milestone
    (try! (map-set Proposals
      { proposal-id: proposal-id }
      (merge proposal {
        current-milestone: milestone-number,
        status: (if (is-eq milestone-number (len milestones)) "completed" "funded")
      })
    ))
    
    ;; If completed, update researcher stats
    (if (is-eq milestone-number (len milestones))
      (let
        ((researcher-data (unwrap! (get-researcher researcher) ERR_NOT_FOUND)))
        (try! (map-set Researchers
          { researcher-id: researcher }
          (merge researcher-data {
            total-completed: (+ (get total-completed researcher-data) u1),
            reputation-score: (+ (get reputation-score researcher-data) u10)
          })
        ))
        (ok true))
      (ok true))
  )
)

;; Administrative Functions

;; Function to withdraw platform fees
(define-public (withdraw-platform-fees (amount uint))
  (let
    ((owner (var-get contract-owner)))
    ;; Only contract owner can withdraw
    (asserts! (is-eq tx-sender owner) ERR_UNAUTHORIZED)
    
    ;; Check sufficient funds
    (asserts! (<= amount (var-get platform-treasury)) ERR_INSUFFICIENT_FUNDS)
    
    ;; Transfer fees
    (try! (as-contract (stx-transfer? amount tx-sender owner)))
    
    ;; Update treasury
    (var-set platform-treasury (- (var-get platform-treasury) amount))
    
    (ok true)
  )
)

;; Function to update platform fee
(define-public (update-platform-fee (new-fee uint))
  (begin
    ;; Only contract owner can update
    (asserts! (is-eq tx-sender (var-get contract-owner)) ERR_UNAUTHORIZED)
    
    ;; Check fee is reasonable (max 5%)
    (asserts! (<= new-fee u500) ERR_INVALID_PROPOSAL)
    
    ;; Update fee
    (var-set platform-fee new-fee)
    
    (ok true)
  )
)
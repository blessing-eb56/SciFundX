;; SciFundX: Decentralized Scientific Research Funding Platform
;; This contract allows researchers to submit proposals, reviewers to vote, funders to support approved projects, and implement milestone-based funding

;; Define constants
(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_NOT_AUTHORIZED (err u100))
(define-constant ERR_INVALID_AMOUNT (err u101))
(define-constant ERR_PROPOSAL_NOT_FOUND (err u102))
(define-constant ERR_ALREADY_FUNDED (err u103))
(define-constant ERR_INVALID_TITLE (err u104))
(define-constant ERR_INVALID_ABSTRACT (err u105))
(define-constant ERR_INVALID_FUNDING_GOAL (err u106))
(define-constant ERR_INVALID_PROPOSAL_ID (err u107))
(define-constant ERR_INVALID_STATUS (err u108))
(define-constant ERR_ALREADY_VOTED (err u109))
(define-constant ERR_NOT_REVIEWER (err u110))
(define-constant ERR_INVALID_REVIEWER (err u111))
(define-constant ERR_NOT_FUNDER (err u112))
(define-constant ERR_INVALID_MILESTONE (err u113))
(define-constant ERR_MILESTONE_NOT_COMPLETE (err u114))
(define-constant ERR_ALREADY_REGISTERED (err u115))

;; Define proposal statuses
(define-constant STATUS_SUBMITTED u0)
(define-constant STATUS_UNDER_REVIEW u1)
(define-constant STATUS_APPROVED u2)
(define-constant STATUS_REJECTED u3)
(define-constant STATUS_OPEN_FOR_FUNDING u4)
(define-constant STATUS_FUNDED u5)
(define-constant STATUS_COMPLETED u6)
(define-constant STATUS_MILESTONE_REVIEW u7)

;; Define vote options
(define-constant VOTE_APPROVE u1)
(define-constant VOTE_REJECT u0)

;; Define data maps
(define-map proposals
  { proposal-id: uint }
  {
    researcher: principal,
    title: (string-ascii 100),
    abstract: (string-ascii 1000),
    proposal-hash: (buff 32),
    funding-goal: uint,
    current-funding: uint,
    status: uint,
    approve-votes: uint,
    reject-votes: uint,
    milestone-count: uint,
    current-milestone: uint,
    created-at: uint
  }
)

(define-map fundings
  { proposal-id: uint, funder: principal }
  { amount: uint }
)

(define-map researchers
  { researcher: principal }
  { 
    name: (string-ascii 100),
    institution: (string-ascii 100),
    field: (string-ascii 50),
    reputation-score: uint,
    total-completed: uint,
    is-active: bool 
  }
)

(define-map reviewers
  { reviewer: principal }
  { is-active: bool }
)

(define-map votes
  { proposal-id: uint, reviewer: principal }
  { vote: uint }
)

(define-map milestones
  { proposal-id: uint, milestone-number: uint }
  { 
    percentage: uint,
    description: (string-ascii 200),
    is-completed: bool,
    approve-votes: uint,
    reject-votes: uint
  }
)

(define-map milestone-votes
  { proposal-id: uint, milestone-number: uint, reviewer: principal }
  { vote: uint }
)

;; Define variables
(define-data-var proposal-counter uint u0)
(define-data-var required-votes uint u3)
(define-data-var platform-fee uint u30) ;; 0.3% fee in basis points (30/10000)
(define-data-var platform-treasury uint u0)

;; Helper functions for input validation
(define-private (is-valid-title (title (string-ascii 100)))
  (and (> (len title) u0) (<= (len title) u100))
)

(define-private (is-valid-abstract (abstract (string-ascii 1000)))
  (and (> (len abstract) u0) (<= (len abstract) u1000))
)

(define-private (is-valid-funding-goal (funding-goal uint))
  (> funding-goal u0)
)

(define-private (is-valid-proposal-id (proposal-id uint))
  (<= proposal-id (var-get proposal-counter))
)

(define-private (is-valid-status (status uint))
  (and (>= status STATUS_SUBMITTED) (<= status STATUS_MILESTONE_REVIEW))
)

(define-private (is-researcher (account principal))
  (default-to false (get is-active (map-get? researchers { researcher: account })))
)

(define-private (is-reviewer (account principal))
  (default-to false (get is-active (map-get? reviewers { reviewer: account })))
)

;; Public functions

;; Register as a researcher
(define-public (register-researcher (name (string-ascii 100)) (institution (string-ascii 100)) (field (string-ascii 50)))
  (let
    ((researcher tx-sender))
    (asserts! (is-none (map-get? researchers { researcher: researcher })) ERR_ALREADY_REGISTERED)
    (map-set researchers
      { researcher: researcher }
      {
        name: name,
        institution: institution,
        field: field,
        reputation-score: u50, ;; Start with neutral score
        total-completed: u0,
        is-active: true
      }
    )
    (ok true)
  )
)

;; Submit a new research proposal
(define-public (submit-proposal 
    (title (string-ascii 100)) 
    (abstract (string-ascii 1000)) 
    (proposal-hash (buff 32)) 
    (funding-goal uint)
    (milestone-percentages (list 5 uint))
    (milestone-descriptions (list 5 (string-ascii 200))))
  (begin
    (asserts! (is-researcher tx-sender) ERR_NOT_AUTHORIZED)
    (asserts! (is-valid-title title) ERR_INVALID_TITLE)
    (asserts! (is-valid-abstract abstract) ERR_INVALID_ABSTRACT)
    (asserts! (is-valid-funding-goal funding-goal) ERR_INVALID_FUNDING_GOAL)
    (asserts! (> (len milestone-percentages) u0) ERR_INVALID_MILESTONE)
    (asserts! (is-eq (len milestone-percentages) (len milestone-descriptions)) ERR_INVALID_MILESTONE)
    (asserts! (is-eq (fold + milestone-percentages u0) u100) ERR_INVALID_MILESTONE)
    
    (let
      (
        (proposal-id (+ (var-get proposal-counter) u1))
        (milestone-count (len milestone-percentages))
      )
      ;; Create the proposal
      (map-set proposals
        { proposal-id: proposal-id }
        {
          researcher: tx-sender,
          title: title,
          abstract: abstract,
          proposal-hash: proposal-hash,
          funding-goal: funding-goal,
          current-funding: u0,
          status: STATUS_SUBMITTED,
          approve-votes: u0,
          reject-votes: u0,
          milestone-count: milestone-count,
          current-milestone: u0,
          created-at: block-height
        }
      )
      
      ;; Create all milestones
      (map create-milestone 
        (map unwrap-panic 
          (map to-uint (list u0 u1 u2 u3 u4))
        ) 
        milestone-percentages 
        milestone-descriptions 
        (list proposal-id proposal-id proposal-id proposal-id proposal-id)
      )
      
      ;; Increment proposal counter
      (var-set proposal-counter proposal-id)
      
      ;; Update status to under review
      (map-set proposals
        { proposal-id: proposal-id }
        (merge (unwrap-panic (map-get? proposals { proposal-id: proposal-id }))
          { status: STATUS_UNDER_REVIEW }
        )
      )
      
      (ok proposal-id)
    )
  )
)

;; Helper function to create a milestone
(define-private (create-milestone (index uint) (percentage uint) (description (string-ascii 200)) (proposal-id uint))
  (if (and (< index (len percentage)) (> percentage u0))
    (map-set milestones
      { proposal-id: proposal-id, milestone-number: (+ index u1) }
      {
        percentage: percentage,
        description: description,
        is-completed: false,
        approve-votes: u0,
        reject-votes: u0
      }
    )
    false
  )
)

;; Vote on a proposal (only for reviewers)
(define-public (vote-on-proposal (proposal-id uint) (vote uint))
  (begin
    (asserts! (is-reviewer tx-sender) ERR_NOT_REVIEWER)
    (asserts! (is-valid-proposal-id proposal-id) ERR_INVALID_PROPOSAL_ID)
    (asserts! (or (is-eq vote VOTE_APPROVE) (is-eq vote VOTE_REJECT)) ERR_INVALID_STATUS)
    (let
      (
        (proposal (unwrap-panic (map-get? proposals { proposal-id: proposal-id })))
        (existing-vote (map-get? votes { proposal-id: proposal-id, reviewer: tx-sender }))
      )
      (asserts! (is-eq (get status proposal) STATUS_UNDER_REVIEW) ERR_INVALID_STATUS)
      (asserts! (is-none existing-vote) ERR_ALREADY_VOTED)
      (map-set votes { proposal-id: proposal-id, reviewer: tx-sender } { vote: vote })
      (if (is-eq vote VOTE_APPROVE)
        (map-set proposals { proposal-id: proposal-id }
          (merge proposal { approve-votes: (+ (get approve-votes proposal) u1) }))
        (map-set proposals { proposal-id: proposal-id }
          (merge proposal { reject-votes: (+ (get reject-votes proposal) u1) }))
      )
      (let
        (
          (updated-proposal (unwrap-panic (map-get? proposals { proposal-id: proposal-id })))
          (total-votes (+ (get approve-votes updated-proposal) (get reject-votes updated-proposal)))
        )
        (if (>= total-votes (var-get required-votes))
          (if (> (get approve-votes updated-proposal) (get reject-votes updated-proposal))
            (map-set proposals { proposal-id: proposal-id }
              (merge updated-proposal { status: STATUS_APPROVED }))
            (map-set proposals { proposal-id: proposal-id }
              (merge updated-proposal { status: STATUS_REJECTED }))
          )
          true
        )
      )
      (ok true)
    )
  )
)

;; Open an approved proposal for funding
(define-public (open-for-funding (proposal-id uint))
  (begin
    (asserts! (is-valid-proposal-id proposal-id) ERR_INVALID_PROPOSAL_ID)
    (let
      (
        (proposal (unwrap-panic (map-get? proposals { proposal-id: proposal-id })))
      )
      (asserts! (is-eq (get researcher proposal) tx-sender) ERR_NOT_AUTHORIZED)
      (asserts! (is-eq (get status proposal) STATUS_APPROVED) ERR_INVALID_STATUS)
      (map-set proposals
        { proposal-id: proposal-id }
        (merge proposal { status: STATUS_OPEN_FOR_FUNDING })
      )
      (ok true)
    )
  )
)

;; Fund a research proposal
(define-public (fund-proposal (proposal-id uint) (amount uint))
  (begin
    (asserts! (is-valid-proposal-id proposal-id) ERR_INVALID_PROPOSAL_ID)
    (asserts! (> amount u0) ERR_INVALID_AMOUNT)
    (let
      (
        (proposal (unwrap-panic (map-get? proposals { proposal-id: proposal-id })))
        (fee-amount (/ (* amount (var-get platform-fee)) u10000))
        (funding-amount (- amount fee-amount))
        (new-funding (+ (get current-funding proposal) funding-amount))
      )
      (asserts! (is-eq (get status proposal) STATUS_OPEN_FOR_FUNDING) ERR_INVALID_STATUS)
      (asserts! (<= new-funding (get funding-goal proposal)) ERR_INVALID_AMOUNT)
      (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
      
      ;; Update platform treasury with fee
      (var-set platform-treasury (+ (var-get platform-treasury) fee-amount))
      
      ;; Update proposal funding
      (map-set proposals
        { proposal-id: proposal-id }
        (merge proposal {
          current-funding: new-funding,
          status: (if (is-eq new-funding (get funding-goal proposal)) STATUS_FUNDED STATUS_OPEN_FOR_FUNDING)
        })
      )
      
      ;; Record the funding
      (let
        ((current-amount (default-to u0 (get amount (map-get? fundings { proposal-id: proposal-id, funder: tx-sender })))))
        (map-set fundings
          { proposal-id: proposal-id, funder: tx-sender }
          { amount: (+ current-amount funding-amount) }
        )
      )
      
      (ok true)
    )
  )
)

;; Submit milestone completion
(define-public (submit-milestone-completion (proposal-id uint))
  (begin
    (asserts! (is-valid-proposal-id proposal-id) ERR_INVALID_PROPOSAL_ID)
    (let
      (
        (proposal (unwrap-panic (map-get? proposals { proposal-id: proposal-id })))
        (next-milestone (+ (get current-milestone proposal) u1))
      )
      (asserts! (is-eq (get researcher proposal) tx-sender) ERR_NOT_AUTHORIZED)
      (asserts! (is-eq (get status proposal) STATUS_FUNDED) ERR_INVALID_STATUS)
      (asserts! (<= next-milestone (get milestone-count proposal)) ERR_INVALID_MILESTONE)
      
      ;; Update status to milestone review
      (map-set proposals
        { proposal-id: proposal-id }
        (merge proposal { status: STATUS_MILESTONE_REVIEW })
      )
      
      (ok true)
    )
  )
)

;; Vote on milestone completion (only for reviewers)
(define-public (vote-on-milestone (proposal-id uint) (vote uint))
  (begin
    (asserts! (is-reviewer tx-sender) ERR_NOT_REVIEWER)
    (asserts! (is-valid-proposal-id proposal-id) ERR_INVALID_PROPOSAL_ID)
    (asserts! (or (is-eq vote VOTE_APPROVE) (is-eq vote VOTE_REJECT)) ERR_INVALID_STATUS)
    (let
      (
        (proposal (unwrap-panic (map-get? proposals { proposal-id: proposal-id })))
        (next-milestone (+ (get current-milestone proposal) u1))
        (milestone (unwrap-panic (map-get? milestones { proposal-id: proposal-id, milestone-number: next-milestone })))
        (existing-vote (map-get? milestone-votes { proposal-id: proposal-id, milestone-number: next-milestone, reviewer: tx-sender }))
      )
      (asserts! (is-eq (get status proposal) STATUS_MILESTONE_REVIEW) ERR_INVALID_STATUS)
      (asserts! (is-none existing-vote) ERR_ALREADY_VOTED)
      
      ;; Record the vote
      (map-set milestone-votes 
        { proposal-id: proposal-id, milestone-number: next-milestone, reviewer: tx-sender } 
        { vote: vote }
      )
      
      ;; Update milestone vote counts
      (if (is-eq vote VOTE_APPROVE)
        (map-set milestones { proposal-id: proposal-id, milestone-number: next-milestone }
          (merge milestone { approve-votes: (+ (get approve-votes milestone) u1) }))
        (map-set milestones { proposal-id: proposal-id, milestone-number: next-milestone }
          (merge milestone { reject-votes: (+ (get reject-votes milestone) u1) }))
      )
      
      ;; Check if we have enough votes to make a decision
      (let
        (
          (updated-milestone (unwrap-panic (map-get? milestones { proposal-id: proposal-id, milestone-number: next-milestone })))
          (total-votes (+ (get approve-votes updated-milestone) (get reject-votes updated-milestone)))
        )
        (if (>= total-votes (var-get required-votes))
          (if (> (get approve-votes updated-milestone) (get reject-votes updated-milestone))
            (process-milestone-completion proposal-id next-milestone)
            ;; Return to funded status if rejected
            (map-set proposals { proposal-id: proposal-id }
              (merge proposal { status: STATUS_FUNDED }))
          )
          true
        )
      )
      
      (ok true)
    )
  )
)

;; Process milestone completion and release funds
(define-private (process-milestone-completion (proposal-id uint) (milestone-number uint))
  (let
    (
      (proposal (unwrap-panic (map-get? proposals { proposal-id: proposal-id })))
      (milestone (unwrap-panic (map-get? milestones { proposal-id: proposal-id, milestone-number: milestone-number })))
      (researcher (get researcher proposal))
      (total-funding (get current-funding proposal))
      (payment-amount (/ (* total-funding (get percentage milestone)) u100))
    )
    ;; Mark milestone as completed
    (map-set milestones
      { proposal-id: proposal-id, milestone-number: milestone-number }
      (merge milestone { is-completed: true })
    )
    
    ;; Transfer funds to researcher
    (try! (as-contract (stx-transfer? payment-amount tx-sender researcher)))
    
    ;; Update proposal status
    (if (is-eq milestone-number (get milestone-count proposal))
      (begin
        ;; This was the final milestone
        (map-set proposals
          { proposal-id: proposal-id }
          (merge proposal { 
            current-milestone: milestone-number,
            status: STATUS_COMPLETED 
          })
        )
        
        ;; Update researcher reputation
        (let
          (
            (researcher-data (unwrap-panic (map-get? researchers { researcher: researcher })))
          )
          (map-set researchers
            { researcher: researcher }
            (merge researcher-data {
              total-completed: (+ (get total-completed researcher-data) u1),
              reputation-score: (+ (get reputation-score researcher-data) u5)
            })
          )
        )
      )
      ;; Not the final milestone, return to funded status
      (map-set proposals
        { proposal-id: proposal-id }
        (merge proposal { 
          current-milestone: milestone-number,
          status: STATUS_FUNDED 
        })
      )
    )
    
    (ok true)
  )
)

;; Administrative Functions

;; Update proposal status (only by CONTRACT_OWNER)
(define-public (update-proposal-status (proposal-id uint) (new-status uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_NOT_AUTHORIZED)
    (asserts! (is-valid-proposal-id proposal-id) ERR_INVALID_PROPOSAL_ID)
    (asserts! (is-valid-status new-status) ERR_INVALID_STATUS)
    (let
      (
        (proposal (unwrap-panic (map-get? proposals { proposal-id: proposal-id })))
      )
      (map-set proposals
        { proposal-id: proposal-id }
        (merge proposal { status: new-status })
      )
      (ok true)
    )
  )
)

;; Add a reviewer (only by CONTRACT_OWNER)
(define-public (add-reviewer (reviewer principal))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_NOT_AUTHORIZED)
    (asserts! (not (is-eq reviewer CONTRACT_OWNER)) ERR_INVALID_REVIEWER)
    (map-set reviewers { reviewer: reviewer } { is-active: true })
    (ok true)
  )
)

;; Remove a reviewer (only by CONTRACT_OWNER)
(define-public (remove-reviewer (reviewer principal))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_NOT_AUTHORIZED)
    (asserts! (not (is-eq reviewer CONTRACT_OWNER)) ERR_INVALID_REVIEWER)
    (map-delete reviewers { reviewer: reviewer })
    (ok true)
  )
)

;; Set required votes (only by CONTRACT_OWNER)
(define-public (set-required-votes (new-required-votes uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_NOT_AUTHORIZED)
    (asserts! (> new-required-votes u0) ERR_INVALID_AMOUNT)
    (var-set required-votes new-required-votes)
    (ok true)
  )
)

;; Update platform fee (only by CONTRACT_OWNER)
(define-public (update-platform-fee (new-fee uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_NOT_AUTHORIZED)
    (asserts! (<= new-fee u500) ERR_INVALID_AMOUNT) ;; Max 5%
    (var-set platform-fee new-fee)
    (ok true)
  )
)

;; Withdraw platform fees (only by CONTRACT_OWNER)
(define-public (withdraw-platform-fees (amount uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_NOT_AUTHORIZED)
    (asserts! (<= amount (var-get platform-treasury)) ERR_INVALID_AMOUNT)
    (try! (as-contract (stx-transfer? amount tx-sender CONTRACT_OWNER)))
    (var-set platform-treasury (- (var-get platform-treasury) amount))
    (ok true)
  )
)

;; Read-only functions

;; Get proposal details
(define-read-only (get-proposal (proposal-id uint))
  (map-get? proposals { proposal-id: proposal-id })
)

;; Get total number of proposals
(define-read-only (get-proposal-count)
  (var-get proposal-counter)
)

;; Get funding amount for a specific proposal and funder
(define-read-only (get-funding (proposal-id uint) (funder principal))
  (map-get? fundings { proposal-id: proposal-id, funder: funder })
)

;; Get milestone details
(define-read-only (get-milestone (proposal-id uint) (milestone-number uint))
  (map-get? milestones { proposal-id: proposal-id, milestone-number: milestone-number })
)

;; Get researcher details
(define-read-only (get-researcher-details (researcher principal))
  (map-get? researchers { researcher: researcher })
)

;; Check if account is a reviewer
(define-read-only (is-active-reviewer (account principal))
  (is-reviewer account)
)

;; Get required votes
(define-read-only (get-required-votes)
  (var-get required-votes)
)

;; Get platform fee
(define-read-only (get-platform-fee)
  (var-get platform-fee)
)

;; Get platform treasury
(define-read-only (get-platform-treasury)
  (var-get platform-treasury)
)

;; Get status name
(define-read-only (get-status-name (status uint))
  (match status
    STATUS_SUBMITTED "SUBMITTED"
    STATUS_UNDER_REVIEW "UNDER_REVIEW"
    STATUS_APPROVED "APPROVED"
    STATUS_REJECTED "REJECTED"
    STATUS_OPEN_FOR_FUNDING "OPEN_FOR_FUNDING"
    STATUS_FUNDED "FUNDED"
    STATUS_COMPLETED "COMPLETED"
    STATUS_MILESTONE_REVIEW "MILESTONE_REVIEW"
    "UNKNOWN"
  )
)
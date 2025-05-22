;; Clarinet Insurance and Warranty System
;; This smart contract implements insurance policies and warranties for valuable clarinets
;; It integrates with the Clarinet Brand Investment Comparison contract

;; Data structures and constants
(define-data-var insurance-policies (list 100 {
    policy-id: uint,
    instrument-id: uint,
    brand: (string-ascii 50),
    coverage-amount: uint,
    premium-paid: uint,
    start-date: uint,
    end-date: uint,
    owner: principal,
    active: bool
}) (list))

(define-data-var warranty-records (list 100 {
    warranty-id: uint,
    instrument-id: uint,
    brand: (string-ascii 50), 
    guarantee-percentage: uint,
    start-date: uint,
    duration-years: uint,
    owner: principal,
    active: bool
}) (list))

(define-data-var claims (list 100 {
    claim-id: uint,
    policy-id: uint,
    claim-amount: uint,
    claim-date: uint,
    claim-reason: (string-ascii 100),
    claim-status: (string-ascii 20),
    evidence-hash: (buff 32),
    approved: bool
}) (list))

(define-data-var next-policy-id uint u1)
(define-data-var next-warranty-id uint u1)
(define-data-var next-claim-id uint u1)
(define-data-var contract-owner principal tx-sender)
(define-data-var current-year uint u2023) ;; Tracks current year for date calculations
(define-data-var insurance-fund uint u0) ;; Holds funds for paying out claims
(define-data-var brand-investment-contract principal tx-sender) ;; Will store the principal of the brand investment contract

;; Error codes
(define-constant ERR-UNAUTHORIZED u1)
(define-constant ERR-INVALID-POLICY u2)
(define-constant ERR-INSUFFICIENT-PREMIUM u3)
(define-constant ERR-POLICY-EXPIRED u4)
(define-constant ERR-INVALID-CLAIM u5)
(define-constant ERR-ALREADY-CLAIMED u6)
(define-constant ERR-INSUFFICIENT-FUNDS u7)
(define-constant ERR-INVALID-WARRANTY u8)
(define-constant ERR-WARRANTY-EXPIRED u9)
(define-constant ERR-INVALID-DATE u10)
(define-constant ERR-MINIMUM-PREMIUM u11)

;; Helper function to update a policy's premium paid amount
(define-private (map-policies-update-premium 
    (target-id uint) 
    (amount uint) 
    (activate bool)
    (policies (list 100 {
        policy-id: uint,
        instrument-id: uint,
        brand: (string-ascii 50),
        coverage-amount: uint,
        premium-paid: uint,
        start-date: uint,
        end-date: uint,
        owner: principal,
        active: bool
    })))
    
    (map 
        update-policy-helper
        policies
    )
)

;; Helper function for updating individual policies
(define-private (update-policy-helper 
    (policy {
        policy-id: uint,
        instrument-id: uint,
        brand: (string-ascii 50),
        coverage-amount: uint,
        premium-paid: uint,
        start-date: uint,
        end-date: uint,
        owner: principal,
        active: bool
    }))
    
    ;; This is a placeholder - we'll use fold instead for the actual update logic
    policy
)

;; Corrected helper function using fold instead of map with lambda
(define-private (update-policies-premium-fold
    (policy {
        policy-id: uint,
        instrument-id: uint,
        brand: (string-ascii 50),
        coverage-amount: uint,
        premium-paid: uint,
        start-date: uint,
        end-date: uint,
        owner: principal,
        active: bool
    })
    (acc {
        target-id: uint,
        new-premium: uint,
        new-status: bool,
        updated-policies: (list 100 {
            policy-id: uint,
            instrument-id: uint,
            brand: (string-ascii 50),
            coverage-amount: uint,
            premium-paid: uint,
            start-date: uint,
            end-date: uint,
            owner: principal,
            active: bool
        })
    }))
    
    (let ((updated-policy 
            (if (is-eq (get policy-id policy) (get target-id acc))
                (merge policy {
                    premium-paid: (get new-premium acc),
                    active: (get new-status acc)
                })
                policy
            )))
        
        (merge acc {
            updated-policies: (unwrap! (as-max-len? 
                (append (get updated-policies acc) updated-policy) 
                u100) 
                acc)
        })
    )
)

;; Admin functions
(define-public (set-current-year (year uint))
    (begin
        ;; Check authorization
        (asserts! (is-eq tx-sender (var-get contract-owner)) (err ERR-UNAUTHORIZED))
        (var-set current-year year)
        (ok true)
    )
)

(define-public (set-brand-contract (contract-principal principal))
    (begin
        ;; Check authorization
        (asserts! (is-eq tx-sender (var-get contract-owner)) (err ERR-UNAUTHORIZED))
        (var-set brand-investment-contract contract-principal)
        (ok true)
    )
)

;; Insurance policy functions
(define-public (create-insurance-policy 
    (instrument-id uint) 
    (brand (string-ascii 50)) 
    (coverage-amount uint) 
    (start-date uint) 
    (duration-years uint))
    
    (let (
        (current-date (var-get current-year))
        (end-date (+ start-date duration-years))
        (policy-id (var-get next-policy-id))
        ;; Calculate premium as 2% of coverage amount per year
        (premium-amount (/ (* coverage-amount duration-years u2) u100))
        )
        
        ;; Validate dates
        (asserts! (>= start-date current-date) (err ERR-INVALID-DATE))
        (asserts! (> end-date start-date) (err ERR-INVALID-DATE))
        
        ;; Validate premium amount (minimum 50)
        (asserts! (>= premium-amount u50) (err ERR-MINIMUM-PREMIUM))
        
        ;; Create the new policy
        (let ((new-policy {
                policy-id: policy-id,
                instrument-id: instrument-id,
                brand: brand,
                coverage-amount: coverage-amount,
                premium-paid: u0, ;; Initially no premium paid
                start-date: start-date,
                end-date: end-date,
                owner: tx-sender,
                active: false ;; Not active until premium paid
            }))
            
            ;; Add to policies list
            (let ((updated-policies 
                    (match (as-max-len? (append (var-get insurance-policies) new-policy) u100)
                        success success
                        (var-get insurance-policies) ;; Fallback if list full
                    )))
                
                ;; Update state
                (var-set insurance-policies updated-policies)
                (var-set next-policy-id (+ policy-id u1))
                
                ;; Return policy details
                (ok {
                    policy-id: policy-id,
                    premium-required: premium-amount
                })
            )
        )
    )
)

(define-public (pay-insurance-premium (policy-id uint) (amount uint))
    (let ((policy-result (get-policy policy-id)))
        (match policy-result
            ok-policy 
            (let (
                ;; Calculate premium as 2% of coverage amount per year
                (duration (- (get end-date ok-policy) (get start-date ok-policy)))
                (required-premium (/ (* (get coverage-amount ok-policy) duration u2) u100))
                )
                
                ;; Validate policy ownership
                (asserts! (is-eq (get owner ok-policy) tx-sender) (err ERR-UNAUTHORIZED))
                
                ;; Validate premium amount
                (asserts! (>= amount required-premium) (err ERR-INSUFFICIENT-PREMIUM))
                
                ;; Update policy using fold approach
                (let ((fold-result 
                        (fold update-policies-premium-fold
                            (var-get insurance-policies)
                            {
                                target-id: policy-id,
                                new-premium: amount,
                                new-status: true,
                                updated-policies: (list)
                            })))
                    
                    ;; Add premium to insurance fund
                    (var-set insurance-fund (+ (var-get insurance-fund) amount))
                    (var-set insurance-policies (get updated-policies fold-result))
                    
                    (ok true)
                )
            )
            err-policy (err err-policy)
        )
    )
)

;; Helper function to check if a claim exists for a policy
(define-private (claim-exists-for-policy (policy-id uint))
    (get exists (fold check-claim-fold (var-get claims) {exists: false, target-policy: policy-id}))
)

(define-private (check-claim-fold 
    (entry {
        claim-id: uint,
        policy-id: uint,
        claim-amount: uint,
        claim-date: uint,
        claim-reason: (string-ascii 100),
        claim-status: (string-ascii 20),
        evidence-hash: (buff 32),
        approved: bool
    })
    (acc {exists: bool, target-policy: uint}))
    
    (if (or (get exists acc) (is-eq (get policy-id entry) (get target-policy acc)))
        {exists: true, target-policy: (get target-policy acc)}
        acc
    )
)
;; Claims processing functions
(define-public (file-claim 
    (policy-id uint) 
    (claim-amount uint) 
    (claim-reason (string-ascii 100)) 
    (evidence-hash (buff 32)))
    
    (let ((policy-result (get-policy policy-id)))
        (match policy-result
            ok-policy 
            (let ((current-date (var-get current-year)))
                ;; Validate policy ownership
                (asserts! (is-eq (get owner ok-policy) tx-sender) (err ERR-UNAUTHORIZED))
                
                ;; Check if policy is active
                (asserts! (get active ok-policy) (err ERR-INVALID-POLICY))
                
                ;; Check if policy has not expired
                (asserts! (<= current-date (get end-date ok-policy)) (err ERR-POLICY-EXPIRED))
                
                ;; Validate claim amount cannot exceed coverage
                (asserts! (<= claim-amount (get coverage-amount ok-policy)) (err ERR-INVALID-CLAIM))
                
                ;; Check if already claimed (simplified - in reality would need more complex tracking)
                (asserts! (not (claim-exists-for-policy policy-id)) (err ERR-ALREADY-CLAIMED))
                
                ;; Create new claim
                (let (
                    (claim-id (var-get next-claim-id))
                    (new-claim {
                        claim-id: claim-id,
                        policy-id: policy-id,
                        claim-amount: claim-amount,
                        claim-date: current-date,
                        claim-reason: claim-reason,
                        claim-status: "pending",
                        evidence-hash: evidence-hash,
                        approved: false
                    }))
                    
                    ;; Add to claims list
                    (let ((updated-claims 
                            (match (as-max-len? (append (var-get claims) new-claim) u100)
                                success success
                                (var-get claims) ;; Fallback if list full
                            )))
                        
                        ;; Update state
                        (var-set claims updated-claims)
                        (var-set next-claim-id (+ claim-id u1))
                        
                        (ok claim-id)
                    )
                )
            )
            err-policy (err err-policy)
        )
    )
)
;; Helper to update claim status using fold
(define-private (update-claims-fold
    (claim {
        claim-id: uint,
        policy-id: uint,
        claim-amount: uint,
        claim-date: uint,
        claim-reason: (string-ascii 100),
        claim-status: (string-ascii 20),
        evidence-hash: (buff 32),
        approved: bool
    })
    (acc {
        target-id: uint,
        approve: bool,
        status-message: (string-ascii 20),
        updated-claims: (list 100 {
            claim-id: uint,
            policy-id: uint,
            claim-amount: uint,
            claim-date: uint,
            claim-reason: (string-ascii 100),
            claim-status: (string-ascii 20),
            evidence-hash: (buff 32),
            approved: bool
        })
    }))
    
    (let ((updated-claim 
            (if (is-eq (get claim-id claim) (get target-id acc))
                (merge claim {
                    approved: (get approve acc),
                    claim-status: (get status-message acc)
                })
                claim
            )))
        
        (merge acc {
            updated-claims: (unwrap! (as-max-len? 
                (append (get updated-claims acc) updated-claim) 
                u100) 
                acc)
        })
    )
)

;; Admin function to approve or reject a claim
(define-public (process-claim (claim-id uint) (approve bool) (status-message (string-ascii 20)))
    (begin
        ;; Check authorization
        (asserts! (is-eq tx-sender (var-get contract-owner)) (err ERR-UNAUTHORIZED))
        
        ;; Update claim status using fold
        (let ((fold-result 
                (fold update-claims-fold
                    (var-get claims)
                    {
                        target-id: claim-id,
                        approve: approve,
                        status-message: status-message,
                        updated-claims: (list)
                    })))
            
            (var-set claims (get updated-claims fold-result))
            
            ;; If approved, process payout
            (if approve
                (let ((claim-result (get-claim claim-id)))
                    (match claim-result
                        ok-claim
                        (let (
                            (payout-amount (get claim-amount ok-claim))
                            (fund-balance (var-get insurance-fund))
                            (policy-result (get-policy (get policy-id ok-claim)))
                        )
                            ;; Verify sufficient funds
                            (asserts! (>= fund-balance payout-amount) (err ERR-INSUFFICIENT-FUNDS))
                            
                            ;; In reality, would transfer funds to claimant here
                            (match policy-result
                                ok-policy 
                                (begin
                                    ;; Subtract payout from fund
                                    (var-set insurance-fund (- fund-balance payout-amount))
                                    (ok payout-amount)
                                )
                                err-policy (err err-policy)
                            )
                        )
                        err-claim (err err-claim)
                    )
                )
                (ok u0)
            )
        )
    )
)

;; Warranty system functions
(define-public (create-warranty 
    (instrument-id uint) 
    (brand (string-ascii 50)) 
    (guarantee-percentage uint) 
    (start-date uint) 
    (duration-years uint))
    
    (begin
        ;; Check authorization
        (asserts! (is-eq tx-sender (var-get contract-owner)) (err ERR-UNAUTHORIZED))
        
        (let (
            (current-date (var-get current-year))
            (warranty-id (var-get next-warranty-id))
            )
            
            ;; Validate dates
            (asserts! (>= start-date current-date) (err ERR-INVALID-DATE))
            (asserts! (> duration-years u0) (err ERR-INVALID-DATE))
            
            ;; Create the new warranty
            (let ((new-warranty {
                    warranty-id: warranty-id,
                    instrument-id: instrument-id,
                    brand: brand,
                    guarantee-percentage: guarantee-percentage,
                    start-date: start-date,
                    duration-years: duration-years,
                    owner: tx-sender,
                    active: true
                }))
                
                ;; Add to warranties list
                (let ((updated-warranties 
                        (match (as-max-len? (append (var-get warranty-records) new-warranty) u100)
                            success success
                            (var-get warranty-records) ;; Fallback if list full
                        )))
                    
                    ;; Update state
                    (var-set warranty-records updated-warranties)
                    (var-set next-warranty-id (+ warranty-id u1))
                    
                    (ok warranty-id)
                )
            )
        )
    )
)

;; Function to check if an instrument has value guarantee from warranty
(define-read-only (check-warranty-guarantee (warranty-id uint) (current-value uint))
    (let ((warranty-result (get-warranty warranty-id)))
        (match warranty-result
            ok-warranty
            (let (
                (current-date (var-get current-year))
                (expiry-date (+ (get start-date ok-warranty) (get duration-years ok-warranty)))
                )
                
                ;; Check if warranty is still valid
                (if (and (get active ok-warranty) (<= current-date expiry-date))
                    ;; Calculate guaranteed value based on percentage
                    (let (
                        (initial-value (get-instrument-initial-value (get instrument-id ok-warranty) (get brand ok-warranty)))
                        (guaranteed-percentage (get guarantee-percentage ok-warranty))
                        (min-guaranteed-value (/ (* initial-value guaranteed-percentage) u100))
                        )
                        
                        ;; If current value is below guaranteed minimum
                        (if (< current-value min-guaranteed-value)
                            (ok {
                                has-guarantee: true,
                                guaranteed-value: min-guaranteed-value,
                                shortfall: (- min-guaranteed-value current-value)
                            })
                            (ok {
                                has-guarantee: false,
                                guaranteed-value: min-guaranteed-value,
                                shortfall: u0
                            })
                        )
                    )
                    (err ERR-WARRANTY-EXPIRED)
                )
            )
            err-warranty (err err-warranty)
        )
    )
)

;; Helper function - in real implementation would query the brand investment contract
(define-private (get-instrument-initial-value (instrument-id uint) (brand (string-ascii 50)))
    ;; Simplified placeholder - would call the investment contract in reality
    u100000
)

;; Getter functions
(define-read-only (get-policy (policy-id uint))
    (let ((result (find-policy policy-id)))
        (if (get found result)
            (ok (get policy result))
            (err ERR-INVALID-POLICY)
        )
    )
)

(define-private (find-policy (policy-id uint))
    (fold find-policy-fold
        (var-get insurance-policies)
        {
            found: false,
            policy: {
                policy-id: u0,
                instrument-id: u0,
                brand: "",
                coverage-amount: u0,
                premium-paid: u0,
                start-date: u0,
                end-date: u0,
                owner: tx-sender,
                active: false
            },
            target-id: policy-id
        }
    )
)

(define-private (find-policy-fold
    (entry {
        policy-id: uint,
        instrument-id: uint,
        brand: (string-ascii 50),
        coverage-amount: uint,
        premium-paid: uint,
        start-date: uint,
        end-date: uint,
        owner: principal,
        active: bool
    })
    (acc {
        found: bool,
        policy: {
            policy-id: uint,
            instrument-id: uint,
            brand: (string-ascii 50),
            coverage-amount: uint,
            premium-paid: uint,
            start-date: uint,
            end-date: uint,
            owner: principal,
            active: bool
        },
        target-id: uint
    }))
    
    (if (or (get found acc) (not (is-eq (get policy-id entry) (get target-id acc))))
        acc
        {
            found: true,
            policy: entry,
            target-id: (get target-id acc)
        }
    )
)

(define-read-only (get-claim (claim-id uint))
    (let ((result (find-claim claim-id)))
        (if (get found result)
            (ok (get claim result))
            (err ERR-INVALID-CLAIM)
        )
    )
)

(define-private (find-claim (claim-id uint))
    (fold find-claim-fold
        (var-get claims)
        {
            found: false,
            claim: {
                claim-id: u0,
                policy-id: u0,
                claim-amount: u0,
                claim-date: u0,
                claim-reason: "",
                claim-status: "",
                evidence-hash: 0x0000000000000000000000000000000000000000000000000000000000000000,
                approved: false
            },
            target-id: claim-id
        }
    )
)

(define-private (find-claim-fold
    (entry {
        claim-id: uint,
        policy-id: uint,
        claim-amount: uint,
        claim-date: uint,
        claim-reason: (string-ascii 100),
        claim-status: (string-ascii 20),
        evidence-hash: (buff 32),
        approved: bool
    })
    (acc {
        found: bool,
        claim: {
            claim-id: uint,
            policy-id: uint,
            claim-amount: uint,
            claim-date: uint,
            claim-reason: (string-ascii 100),
            claim-status: (string-ascii 20),
            evidence-hash: (buff 32),
            approved: bool
        },
        target-id: uint
    }))
    
    (if (or (get found acc) (not (is-eq (get claim-id entry) (get target-id acc))))
        acc
        {
            found: true,
            claim: entry,
            target-id: (get target-id acc)
        }
    )
)

(define-read-only (get-warranty (warranty-id uint))
    (let ((result (find-warranty warranty-id)))
        (if (get found result)
            (ok (get warranty result))
            (err ERR-INVALID-WARRANTY)
        )
    )
)

(define-private (find-warranty (warranty-id uint))
    (fold find-warranty-fold
        (var-get warranty-records)
        {
            found: false,
            warranty: {
                warranty-id: u0,
                instrument-id: u0,
                brand: "",
                guarantee-percentage: u0,
                start-date: u0,
                duration-years: u0,
                owner: tx-sender,
                active: false
            },
            target-id: warranty-id
        }
    )
)

(define-private (find-warranty-fold
    (entry {
        warranty-id: uint,
        instrument-id: uint,
        brand: (string-ascii 50),
        guarantee-percentage: uint,
        start-date: uint,
        duration-years: uint,
        owner: principal,
        active: bool
    })
    (acc {
        found: bool,
        warranty: {
            warranty-id: uint,
            instrument-id: uint,
            brand: (string-ascii 50),
            guarantee-percentage: uint,
            start-date: uint,
            duration-years: uint,
            owner: principal,
            active: bool
        },
        target-id: uint
    }))
    
    (if (or (get found acc) (not (is-eq (get warranty-id entry) (get target-id acc))))
        acc
        {
            found: true,
            warranty: entry,
            target-id: (get target-id acc)
        }
    )
)

;; Get all active policies for the caller
(define-read-only (get-my-policies)
    (filter is-policy-owned-by-me (var-get insurance-policies))
)

(define-private (is-policy-owned-by-me 
    (policy {
        policy-id: uint,
        instrument-id: uint,
        brand: (string-ascii 50),
        coverage-amount: uint,
        premium-paid: uint,
        start-date: uint,
        end-date: uint,
        owner: principal,
        active: bool
    }))
    (is-eq (get owner policy) tx-sender)
)

;; Initialize with some test data
(begin
    ;; Setting the current year
    (var-set current-year u2023)
    
    ;; Adding some funds to the insurance pool
    (var-set insurance-fund u1000000)
)
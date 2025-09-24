;; LinguaChain: Language Learning Progress Tracker
;; Version: 1.0.0

;; Constants
(define-constant ACADEMY_CAPACITY u3500000)
(define-constant BASE_LEARNING_REWARD u36)
(define-constant FLUENCY_BONUS u26)
(define-constant MAX_PROFICIENCY_LEVEL u28)
(define-constant ERR_INVALID_STUDY_SESSION u1)
(define-constant ERR_NO_LINGUA_TOKENS u2)
(define-constant ERR_ACADEMY_CAPACITY_EXCEEDED u3)
(define-constant BLOCKS_PER_LEARNING_CYCLE u2736)
(define-constant RESOURCE_INVESTMENT_MULTIPLIER u18)
(define-constant MIN_INVESTMENT_PERIOD u1368)
(define-constant EARLY_WITHDRAWAL_PENALTY u35)

;; Data Variables
(define-data-var total-lingua-tokens-distributed uint u0)
(define-data-var total-study-sessions uint u0)
(define-data-var academy-director principal tx-sender)

;; Data Maps
(define-map learner-study-sessions principal uint)
(define-map learner-lingua-tokens principal uint)
(define-map study-session-start-time principal uint)
(define-map learner-proficiency-level principal uint)
(define-map learner-last-study-session principal uint)
(define-map learner-resource-investments principal uint)
(define-map learner-investment-start-block principal uint)
(define-map language-focus-specialization principal uint)
(define-map learner-language-certifications principal uint)
(define-map conversation-skill-mastery principal uint)

;; Public Functions
(define-public (start-study-session (language-type uint) (lesson-difficulty uint))
  (let
    (
      (learner tx-sender)
    )
    (asserts! (and (> language-type u0) (> lesson-difficulty u0) (<= lesson-difficulty u100)) (err ERR_INVALID_STUDY_SESSION))
    (map-set study-session-start-time learner burn-block-height)
    (map-set language-focus-specialization learner language-type)
    (ok true)
  ))

(define-public (complete-study-session (lesson-difficulty uint) (comprehension-score uint))
  (let
    (
      (learner tx-sender)
      (start-block (default-to u0 (map-get? study-session-start-time learner)))
      (blocks-studying (- burn-block-height start-block))
      (last-session-block (default-to u0 (map-get? learner-last-study-session learner)))
      (proficiency-level (default-to u0 (map-get? learner-proficiency-level learner)))
      (capped-proficiency (if (<= proficiency-level MAX_PROFICIENCY_LEVEL) proficiency-level MAX_PROFICIENCY_LEVEL))
      (conversation-bonus (default-to u0 (map-get? conversation-skill-mastery learner)))
      (comprehension-bonus (/ (* comprehension-score u24) u100))
      (difficulty-bonus (/ lesson-difficulty u2))
      (learning-reward (+ BASE_LEARNING_REWARD (* capped-proficiency FLUENCY_BONUS) conversation-bonus comprehension-bonus difficulty-bonus))
    )
    (asserts! (and (> start-block u0) (>= blocks-studying (/ lesson-difficulty u35)) (<= comprehension-score u100)) (err ERR_INVALID_STUDY_SESSION))
    
    (map-set learner-study-sessions learner (+ (default-to u0 (map-get? learner-study-sessions learner)) u1))
    (map-set learner-lingua-tokens learner (+ (default-to u0 (map-get? learner-lingua-tokens learner)) learning-reward))
    
    (if (< (- burn-block-height last-session-block) BLOCKS_PER_LEARNING_CYCLE)
      (map-set learner-proficiency-level learner (+ proficiency-level u1))
      (map-set learner-proficiency-level learner u1)
    )
    
    (if (>= comprehension-score u98)
      (begin
        (map-set learner-language-certifications learner (+ (default-to u0 (map-get? learner-language-certifications learner)) u1))
        (map-set conversation-skill-mastery learner (+ conversation-bonus u18))
      )
      true
    )
    
    (map-set learner-last-study-session learner burn-block-height)
    (var-set total-study-sessions (+ (var-get total-study-sessions) u1))
    (var-set total-lingua-tokens-distributed (+ (var-get total-lingua-tokens-distributed) learning-reward))
    
    (asserts! (<= (var-get total-lingua-tokens-distributed) ACADEMY_CAPACITY) (err ERR_ACADEMY_CAPACITY_EXCEEDED))
    (ok learning-reward)
  ))

(define-public (claim-lingua-rewards)
  (let
    (
      (learner tx-sender)
      (token-balance (default-to u0 (map-get? learner-lingua-tokens learner)))
    )
    (asserts! (> token-balance u0) (err ERR_NO_LINGUA_TOKENS))
    (map-set learner-lingua-tokens learner u0)
    (ok token-balance)
  ))

;; Learning Resource Investment Features
(define-public (invest-in-learning-resources (amount uint))
  (let
    (
      (learner tx-sender)
    )
    (asserts! (> amount u0) (err ERR_INVALID_STUDY_SESSION))
    (asserts! (>= (var-get total-lingua-tokens-distributed) amount) (err ERR_ACADEMY_CAPACITY_EXCEEDED))
    
    (map-set learner-resource-investments learner amount)
    (map-set learner-investment-start-block learner burn-block-height)
    (var-set total-lingua-tokens-distributed (- (var-get total-lingua-tokens-distributed) amount))
    (ok amount)
  ))

(define-public (withdraw-resource-investment)
  (let
    (
      (learner tx-sender)
      (invested-amount (default-to u0 (map-get? learner-resource-investments learner)))
      (investment-start-block (default-to u0 (map-get? learner-investment-start-block learner)))
      (blocks-invested (- burn-block-height investment-start-block))
      (penalty (if (< blocks-invested MIN_INVESTMENT_PERIOD) (/ (* invested-amount EARLY_WITHDRAWAL_PENALTY) u100) u0))
      (investment-bonus (if (>= blocks-invested MIN_INVESTMENT_PERIOD) (/ (* invested-amount RESOURCE_INVESTMENT_MULTIPLIER) u100) u0))
      (final-amount (+ (- invested-amount penalty) investment-bonus))
    )
    (asserts! (> invested-amount u0) (err ERR_NO_LINGUA_TOKENS))
    
    (map-set learner-resource-investments learner u0)
    (map-set learner-investment-start-block learner u0)
    (var-set total-lingua-tokens-distributed (+ (var-get total-lingua-tokens-distributed) final-amount))
    (ok final-amount)
  ))

(define-public (create-language-course (course-title (string-utf8 128)) (lesson-count uint))
  (let
    (
      (learner tx-sender)
      (proficiency-level (default-to u0 (map-get? learner-proficiency-level learner)))
      (certifications (default-to u0 (map-get? learner-language-certifications learner)))
      (course-bonus (+ (* lesson-count u80) (* certifications u45) (* proficiency-level u35)))
    )
    (asserts! (and (> (len course-title) u0) (>= proficiency-level u22) (> lesson-count u0)) (err ERR_INVALID_STUDY_SESSION))
    
    (map-set learner-lingua-tokens learner (+ (default-to u0 (map-get? learner-lingua-tokens learner)) course-bonus))
    (var-set total-lingua-tokens-distributed (+ (var-get total-lingua-tokens-distributed) course-bonus))
    
    (ok course-bonus)
  ))

(define-public (host-language-exchange (participant-count uint) (exchange-hours uint))
  (let
    (
      (learner tx-sender)
      (proficiency-level (default-to u0 (map-get? learner-proficiency-level learner)))
      (conversation-mastery (default-to u0 (map-get? conversation-skill-mastery learner)))
      (exchange-bonus (+ (* participant-count u60) (* exchange-hours u35) (* conversation-mastery u14)))
    )
    (asserts! (and (> participant-count u0) (> exchange-hours u0) (>= proficiency-level u26)) (err ERR_INVALID_STUDY_SESSION))
    
    (map-set learner-lingua-tokens learner (+ (default-to u0 (map-get? learner-lingua-tokens learner)) exchange-bonus))
    (var-set total-lingua-tokens-distributed (+ (var-get total-lingua-tokens-distributed) exchange-bonus))
    
    (ok exchange-bonus)
  ))

(define-public (take-proficiency-test (test-level uint) (test-fee uint))
  (let
    (
      (learner tx-sender)
      (proficiency-level (default-to u0 (map-get? learner-proficiency-level learner)))
      (certifications (default-to u0 (map-get? learner-language-certifications learner)))
      (test-bonus (+ (* test-level u70) (* certifications u30)))
    )
    (asserts! (and (> test-level u0) (>= proficiency-level u18) (> test-fee u0)) (err ERR_INVALID_STUDY_SESSION))
    (asserts! (>= (var-get total-lingua-tokens-distributed) test-fee) (err ERR_ACADEMY_CAPACITY_EXCEEDED))
    
    (map-set learner-lingua-tokens learner (+ (default-to u0 (map-get? learner-lingua-tokens learner)) test-bonus))
    (var-set total-lingua-tokens-distributed (+ (- (var-get total-lingua-tokens-distributed) test-fee) test-bonus))
    
    (ok test-bonus)
  ))

;; Read-Only Functions
(define-read-only (get-study-session-count (user principal))
  (default-to u0 (map-get? learner-study-sessions user)))

(define-read-only (get-lingua-token-balance (user principal))
  (default-to u0 (map-get? learner-lingua-tokens user)))

(define-read-only (get-proficiency-level (user principal))
  (default-to u0 (map-get? learner-proficiency-level user)))

(define-read-only (get-language-certifications (user principal))
  (default-to u0 (map-get? learner-language-certifications user)))

(define-read-only (get-resource-investments (user principal))
  (default-to u0 (map-get? learner-resource-investments user)))

(define-read-only (get-conversation-mastery (user principal))
  (default-to u0 (map-get? conversation-skill-mastery user)))

(define-read-only (get-academy-stats)
  {
    total-study-sessions: (var-get total-study-sessions),
    total-lingua-tokens-distributed: (var-get total-lingua-tokens-distributed),
    academy-capacity: ACADEMY_CAPACITY
  })

(define-read-only (calculate-learning-reward (proficiency-level uint) (comprehension-score uint) (conversation-bonus uint) (difficulty uint))
  (let
    (
      (capped-proficiency (if (<= proficiency-level MAX_PROFICIENCY_LEVEL) proficiency-level MAX_PROFICIENCY_LEVEL))
      (comprehension-bonus (/ (* comprehension-score u24) u100))
      (difficulty-bonus (/ difficulty u2))
    )
    (+ BASE_LEARNING_REWARD (* capped-proficiency FLUENCY_BONUS) conversation-bonus comprehension-bonus difficulty-bonus)
  ))

;; Private Functions
(define-private (is-academy-director)
  (is-eq tx-sender (var-get academy-director)))

(define-private (validate-learning-parameters (lesson-difficulty uint) (comprehension-score uint))
  (and (> lesson-difficulty u0) (<= comprehension-score u100)))
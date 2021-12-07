#lang racket
(require algorithms)

(define input
  (map string->number (string-split (file->string "day06.in") ",")))

(define (count-occurrences element lst)
  (count (curry equal? element) lst))

(define (can-fishes input)
  (define (make-structure keys)
    (cond [(empty? keys) keys]
          [(cons (cons (car keys)
                       (count-occurrences (car keys) input))
                 (make-structure (cdr keys)))]))
  (make-structure (remove-duplicates input)))

(define (naive-process-fishes fishes)
  (cond [(empty? fishes) fishes]
        [(equal? (car fishes) 0) (list* 6 8 (naive-process-fishes (cdr fishes)))]
        [else (cons (sub1 (car fishes)) (naive-process-fishes (cdr fishes)))]))

(define (naive-process-day fishes days-left)
  (cond [(equal? days-left 0) fishes]
        [else (naive-process-day (naive-process-fishes fishes) (sub1 days-left))]))

(define (naive-solution input days)
  (length (naive-process-day input days)))

(define (process-fishes canned-fishes)
  (cond [(empty? canned-fishes) canned-fishes]
        [(equal? (caar canned-fishes) 0)
         (list* (cons 6 (cdar canned-fishes))
                (cons 8 (cdar canned-fishes))
                (process-fishes (cdr canned-fishes)))]
        [else (cons (cons (sub1 (caar canned-fishes)) (cdar canned-fishes))
                    (process-fishes (cdr canned-fishes)))]))

(define (process-day canned-fishes days-left)
  (define (recan-fishes)
    (define keys (remove-duplicates (map car canned-fishes)))
      (define (compress-fishes keys)
        (cond [(empty? keys) keys]
              [(cons (cons (car keys)
                           (sum (map cdr (filter (λ (can) (equal? (car can) (car keys))) canned-fishes))))
                     (compress-fishes (cdr keys)))]))
    (compress-fishes keys))
  (cond [(equal? days-left 0) canned-fishes]
        [else (process-day (process-fishes (recan-fishes)) (sub1 days-left))]))

(define (solution input days)
  (sum (map cdr (process-day (can-fishes input) days))))


;; Part1
(define (part1a input) (naive-solution input 80))
(define (part1b input) (solution input 80))
(display (part1b input))
(newline)

;;; Part2
(define (part2 input) (solution input 256))
(display (part2 input))
(newline)

;; Tests
(require rackunit)

(define example-input
  '(3 4 3 1 2))

(check-equal? (naive-solution example-input 80) 5934)
(check-equal? (solution example-input 80) 5934)
(check-equal? (solution example-input 256) 26984457539)

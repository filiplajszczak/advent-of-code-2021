#lang racket
(require algorithms)

(define input
  (map string->number (string-split (file->string "day07.in") ",")))


(define (part1 input)
  (define start (apply min input))
  (define stop (apply max input))
  (define (get-fuel1 pos)
    (sum (map (λ (x) (abs (- pos x))) input)))
  (apply min (map get-fuel1 (range start (add1 stop)))))

(define (part2 input)
  (define start (apply min input))
  (define stop (apply max input))
  (define (get-fuel2 pos)
    (sum (map (λ (x) (sum (range (add1 (abs (- pos x)))))) input)))
  (apply min (map get-fuel2 (range start (add1 stop)))))

;;; Part1
(display (part1 input))
(newline)

;;; Part1
(display (part2 input))
(newline)

;; Tests
(require rackunit)

(define example-input
  '(16 1 2 0 4 2 7 1 2 14))

(check-equal? (part1 example-input) 37)
(check-equal? (part2 example-input) 168)

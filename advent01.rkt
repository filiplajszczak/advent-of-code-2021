#lang racket
(require algorithms)

(define input (map string->number (file->lines "day01.in")))

(define (one? l r)
  (cond [(< l r) 1]
        [else 0]))

(define (sums-of-sliding-triads lst)
  (zip-with + (init (init lst)) (init (cdr lst)) (cddr lst)))

;; Part 1
(display (sum (adjacent-map input one?)))
(newline)
;; Part 2
(display (sum (adjacent-map (sums-of-sliding-triads input) one?)))

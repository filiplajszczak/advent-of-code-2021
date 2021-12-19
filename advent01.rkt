#lang racket
(require algorithms)

(define input (map string->number (file->lines "day01.in")))

(define (sums-of-sliding-triads lst)
  (zip-with + (init (init lst)) (init (cdr lst)) (cddr lst)))

;; Part 1
(display (count identity (adjacent-map input <)))
(newline)
;; Part 2
(display (count identity (adjacent-map (sums-of-sliding-triads input) <)))

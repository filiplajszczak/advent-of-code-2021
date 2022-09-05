;; Depends on https://git.sr.ht/~brown121407/f.scm packaged in Guix as "guile-f-scm".
;; adjacent-map, init and zip-with are ported from racket package "algorithms".


(use-modules (srfi srfi-1)
             (srfi srfi-64)
             ((f) #:prefix f:))

(define (zip-with proc list1 . lists)
  (apply map proc list1 lists))

(define (adjacent-map lst f)
  (zip-with f (init lst) (cdr lst)))

(define (init lst)
  (take lst (- (length lst) 1)))

(define (input filename)
  (map string->number (f:read-lines filename)))

(define (sums-of-sliding-triads lst)
  (zip-with + (init (init lst)) (init (cdr lst)) (cddr lst)))

(define (part-1 filename)
  (count identity (adjacent-map (input filename) <)))

(define (part-2 filename)
  (count identity (adjacent-map (sums-of-sliding-triads (input filename)) <)))

;; Part 1
(display (part-1 "day01.in"))
(newline)
;; Part 2
(display (part-2 "day01.in"))
(newline)

;; Tests
(test-begin "example")

(test-equal "Test part 1" 7 (part-1 "day01-example.in"))
(test-equal "Test part 2" 5 (part-2 "day01-example.in"))

(test-end "example")


;; Depends on https://git.sr.ht/~brown121407/f.scm packaged in Guix as "guile-f-scm".
;; adjacent-map, init and zip-with are ported from racket package "algorithms".


(use-modules (srfi srfi-1)
             ((f) #:prefix f:)
             ((f ports) #:prefix p:))

;; ported from 
(define (zip-with proc list1 . lists)
  (apply map proc list1 lists))

(define (adjacent-map lst f)
  (zip-with f (init lst) (cdr lst)))

(define (init lst)
  (take lst (- (length lst) 1)))

(define input (map string->number (f:read-lines "day01.in")))

(define (sums-of-sliding-triads lst)
  (zip-with + (init (init lst)) (init (cdr lst)) (cddr lst)))

;; Part 1
(display (count identity (adjacent-map input <)))
(newline)
;; Part 2
(display (count identity (adjacent-map (sums-of-sliding-triads input) <)))
(newline)

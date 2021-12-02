#lang racket

(define (string-string->symbol-number lst)
  (list (string->symbol (car lst)) (string->number (cadr lst))))

(define input
  (map string-string->symbol-number
    (map string-split (file->lines "day02.in"))))

(define (move order n pos)
  (match order
    ['forward (cons (+ n (car pos)) (cdr pos))]
    ['down (cons (car pos) (+ (cdr pos) n))]
    ['up (cons (car pos) (- (cdr pos) n))]))

(define (move2 order n pos)
  (match order
    ['forward (list
                (+ n (car pos))
                (+ (* n (caddr pos)) (cadr pos))
                (caddr pos))]
    ['down (list
            (car pos)
            (cadr pos)
            (+ (caddr pos) n))]
    ['up (list
            (car pos)
            (cadr pos)
            (- (caddr pos) n))]))

(define (calculate-position pos lst move-proc)
  (cond [(null? lst) pos]
        [else (calculate-position
               (move-proc
                (caar lst)
                (cadar lst) pos)
               (cdr lst) move-proc)]))

;; Part 1
(define final-position (calculate-position (cons 0 0) input move))
(display (* (car final-position) (cdr final-position)))
(newline)
;; Part 2
(define final-position2 (calculate-position (list 0 0 0) input move2))
(display (* (car final-position2) (cadr final-position2)))

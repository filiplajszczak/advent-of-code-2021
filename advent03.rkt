#lang racket

(define (count-occurrences element lst)
  (count (curry equal? element) lst))

(define (zero-or-one lst)
  (define zeros (count-occurrences #\0 lst))
  (define ones (count-occurrences #\1 lst))
  (cond [(> zeros ones) #\0] [else #\1]))

(define (bin-chars->number lst)
  (string->number (list->string (list* #\# #\b lst))))

(define (filter-values lst num pos)
  (filter (λ (element) (eq? (list-ref element pos) num)) lst))

(define (flip-bit bit) (match bit [#\0 #\1] [#\1 #\0]))

(define (get-num lst pos machine)
  (define num (zero-or-one (list-ref (apply map list lst) pos)))
  (match machine
    ['generator num]
    ['scrubber (flip-bit num)]))

(define (rating lst pos machine)
  (cond [(eq? (cdr lst) (list)) (car lst)]
        [else (rating (filter-values lst (get-num lst pos machine) pos) (+ 1 pos) machine)]))

(define input
  (map string->list (file->lines "day03.in")))

;;Part 1
(define (part1 input)
  (define reduced-input (map zero-or-one (apply map list input)))
  (* (bin-chars->number reduced-input) (bin-chars->number (map flip-bit reduced-input))))

(display (part1 input))
(newline)

;;Part 2
(define (part2 input)
  (* (bin-chars->number (rating input 0 'scrubber)) (bin-chars->number (rating input 0 'generator))))
(display (part2 input))
(newline)

;;Tests
(require rackunit)
(define example
  (map string->list
    (list
      "00100" "11110" "10110" "10111" "10101" "01111"
      "00111" "11100" "10000" "11001" "00010" "01010")))

(check-equal? (part1 example) 198)
(check-equal? (part2 example) 230)

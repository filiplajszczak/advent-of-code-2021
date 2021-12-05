#lang racket
(require algorithms)

(define (line->coords line)
  (map (λ (coords) (map string->number (string-split coords ","))) (string-split line " -> ")))

(define input
  (map line->coords (file->lines "day05.in")))

(define size (add1 (apply max (flatten input))))

(define grid (repeat size (repeat size 0)))

(define (x1 coords) (caar coords))

(define (y1 coords) (cadar coords))

(define (x2 coords) (caadr coords))

(define (y2 coords) (car (cdadr coords)))

(define (add-to-row row first-cell last-cell current-cell)
  (cond [(empty? row) row]
        [(and (>= current-cell first-cell) (<= current-cell last-cell))
         (cons (add1 (car row))
               (add-to-row (cdr row) first-cell last-cell (add1 current-cell)))]
        [else (cons (car row)
                    (add-to-row (cdr row) first-cell last-cell (add1 current-cell)))]))

(define (add-to-rows grid first-cell last-cell first-row last-row current-row)
  (cond [(empty? grid) grid]
        [(and (>= current-row first-row) (<= current-row last-row))
         (cons (add-to-row (car grid) first-cell last-cell 0)
               (add-to-rows (cdr grid) first-cell last-cell first-row last-row (add1 current-row)))]
        [else (cons (car grid)
                    (add-to-rows (cdr grid) first-cell last-cell first-row last-row (add1 current-row)))]))

(define (horizontal grid coords)
  (define row (x1 coords))
  (define first-cell (min (y1 coords) (y2 coords)))
  (define last-cell (max (y1 coords) (y2 coords)))
  (add-to-rows grid first-cell last-cell row row 0))

(define (vertical grid coords)
  (define cell (y1 coords))
  (define first-row (min (x1 coords) (x2 coords)))
  (define last-row (max (x1 coords) (x2 coords)))
  (add-to-rows grid cell cell first-row last-row 0))

(define (diagonal grid coords)
  (define sorted-coords
    (cond [(< (x1 coords) (x2 coords)) (list (car coords) (cadr coords))]
          [else (list (cadr coords) (car coords))]))
  (define cell (y1 sorted-coords))
  (define first-row (x1 sorted-coords))
  (define last-row (x2 sorted-coords))
  (define direction (cond [(< (y1 sorted-coords) (y2 sorted-coords)) add1]
                          [else sub1]))
  (define (add-diagonal grid cell current-row)
    (cond [(empty? grid) grid]
          [(and (>= current-row first-row) (<= current-row last-row))
           (cons (add-to-row (car grid) cell cell 0)
                 (add-diagonal (cdr grid) (direction cell) (add1 current-row)))]
          [else (cons (car grid)
                      (add-diagonal (cdr grid) cell (add1 current-row)))]))
  (add-diagonal grid cell 0))

(define (final-grid grid input)
  (cond [(empty? input) grid]
        [(equal? (x1 (car input)) (x2 (car input)))
         (final-grid (horizontal grid (car input)) (cdr input))]
        [(equal? (y1 (car input)) (y2 (car input)))
         (final-grid (vertical grid (car input)) (cdr input))]
        [else (final-grid grid (cdr input))]))

(define (final-grid2 grid input)
  (cond [(empty? input) grid]
        [(equal? (x1 (car input)) (x2 (car input)))
         (final-grid2 (horizontal grid (car input)) (cdr input))]
        [(equal? (y1 (car input)) (y2 (car input)))
         (final-grid2 (vertical grid (car input)) (cdr input))]
        [else (final-grid2 (diagonal grid (car input)) (cdr input))]))

;;; Part1
(define (part1 input) (length (filter (λ (x) (> x 1)) (append* (final-grid grid input)))))
(display (part1 input))
(newline)

;;; Part2
(define (part2 input) (length (filter (λ (x) (> x 1)) (append* (final-grid2 grid input)))))
(display (part2 input))
(newline)

;; Tests
(require rackunit)

(define example-input
  '(((0 9) (5 9))
    ((8 0) (0 8))
    ((9 4) (3 4))
    ((2 2) (2 1))
    ((7 0) (7 4))
    ((6 4) (2 0))
    ((0 9) (2 9))
    ((3 4) (1 4))
    ((0 0) (8 8))
    ((5 5) (8 2))))

(check-equal? (part1 example-input) 5)
(check-equal? (part2 example-input) 12)

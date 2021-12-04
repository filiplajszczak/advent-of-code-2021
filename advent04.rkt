#lang racket
(require algorithms)

(define input
  (file->lines "day04.in"))

(define draws (string-split (car input) ","))

(define (add-bool element) (cons element #f))

(define (map-on-element proc boards)
  (map (λ (board)
          (map (λ (row) (map proc row))
               board))
       boards))

(define boards
  (map-on-element add-bool
                  (map
                    (λ (lst) (map string-split lst))
                    (sliding (cddr input) 5 6))))

(define (winning-row? row)
        (all? (map (λ (element) (cdr element)) row)))

(define (winning-board? board)
  (cond [(or
           (any? (map winning-row? board))
           (any? (map winning-row? (apply map list board))))]
        [else #f]))

(define (check-winner boards)
  (cond [(empty? boards) boards]
        [(winning-board? (car boards)) (car boards)]
        [else (check-winner (cdr boards))]))

(define (apply-draw boards draw)
  (define (check-element element)
          (cond [(equal? draw (car element)) (cons (car element) #t)]
                [else element]))
  (map-on-element check-element boards))

(define (get-winner boards draws last-draw)
  (define winner (check-winner boards))
  (cond [(not (equal? winner '())) (cons winner last-draw)]
        [else (get-winner (apply-draw boards (car draws)) (cdr draws) (car draws))]))

(define (sum-of-unmarked board-draw-pair)
  (sum (map string->number (map car (filter (λ (element) (not (cdr element))) (append* (car board-draw-pair)))))))

;; Part1
(define (part1 boards draws)
  (define winning-pair (get-winner boards draws ""))
  (* (sum-of-unmarked winning-pair) (string->number (cdr winning-pair))))

(display (part1 boards draws))
(newline)

;; Part2
(define (filter-out-winner winner boards)
  (filter (λ (board) (not (equal? winner board))) boards))

(define (filter-out-winners boards old-winner)
  (define winner (check-winner boards))
  (cond [(equal? winner '()) (cons boards old-winner)]
        [else (filter-out-winners (filter-out-winner winner boards) winner)]))

(define (get-looser boards draws last-draw)
  (define boards-winner (filter-out-winners boards '()))
  (cond [(equal? (length (car boards-winner)) 0) (cons (cdr boards-winner) last-draw)]
        [else (get-looser (apply-draw (car boards-winner) (car draws)) (cdr draws) (car draws))]))

(define (part2 boards draws)
  (define loosing-pair (get-looser boards draws ""))
  (* (sum-of-unmarked loosing-pair) (string->number (cdr loosing-pair))))

(display (part2 boards draws))
(newline)

;; Tests
(require rackunit)

(define example-draw
  '("7" "4" "9" "5" "11" "17" "23" "2" "0" "14" "21" "24" "10" "16"
    "13" "6" "15" "25" "12" "22" "18" "20" "8" "19" "3" "26" "1"))

(define example-boards
  '(((("22" . #f) ("13" . #f) ("17" . #f) ("11" . #f) ("0" . #f))
     (("8" . #f) ("2" . #f) ("23" . #f) ("4" . #f) ("24" . #f))
     (("21" . #f) ("9" . #f) ("14" . #f) ("16" . #f) ("7" . #f))
     (("6" . #f) ("10" . #f) ("3" . #f) ("18" . #f) ("5" . #f))
     (("1" . #f) ("12" . #f) ("20" . #f) ("15" . #f) ("19" . #f)))

    ((("3" . #f) ("15" . #f) ("0" . #f) ("2" . #f) ("22" . #f))
     (("9" . #f) ("18" . #f) ("13" . #f) ("17" . #f) ("5" . #f))
     (("19" . #f) ("8" . #f) ("7" . #f) ("25" . #f) ("23" . #f))
     (("20" . #f) ("11" . #f) ("10" . #f) ("24" . #f) ("4" . #f))
     (("14" . #f) ("21" . #f) ("16" . #f) ("12" . #f) ("6" . #f)))

    ((("14" . #f) ("21" . #f) ("17" . #f) ("24" . #f) ("4" . #f))
     (("10" . #f) ("16" . #f) ("15" . #f) ("9" . #f) ("19" . #f))
     (("18" . #f) ("8" . #f) ("23" . #f) ("26" . #f) ("20" . #f))
     (("22" . #f) ("11" . #f) ("13" . #f) ("6" . #f) ("5" . #f))
     (("2" . #f) ("0" . #f) ("12" . #f) ("3" . #f) ("7" . #f)))))

(check-equal? (part1 example-boards example-draw) 4512)
(check-equal? (part2 example-boards example-draw) 1924)

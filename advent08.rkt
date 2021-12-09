#lang racket
(require algorithms)

(define input
  (map (λ (x) (string-split x "|")) (file->lines "day08e.in")))

(define (input1 input)
  (append* (map string-split (map cadr input))))

(define (solution1 input)
  (length (filter (λ (x) (member (string-length x) '(2 3 4 7))) (input1 input))))

(define (input2 input)
  (map (λ (x) (map string-split x)) input))

(define (sort-letters str)
  (list->string (sort (string->list str) char<?)))

(define (digit-from-length patterns len)
  (car (filter (λ (x) (equal? (string-length x) len)) patterns)))

(define (string->set str)
  (list->set (string->list str)))

(define (get-a seven one)
  (car (set->list (set-subtract (string->set seven) (string->set one)))))

(define (get-six patterns seven)
  (car (filter (λ (x) (and (equal? (string-length x) 6)
                           (equal? (set-count (set-subtract (string->set x) (string->set seven))) 4))) patterns)))

(define (get-nine patterns six four)
  (car (filter (λ (x) (and (equal? (string-length x) 6)
                           (not (set=? (string->set six) (string->set x)))
                           (equal? (set-count (set-subtract (string->set x) (string->set four))) 2))) patterns)))

(define (get-zero patterns nine six)
  (car (filter (λ (x) (and (equal? (string-length x) 6)
                           (not (set=? (string->set nine) (string->set x)))
                           (not (set=? (string->set six) (string->set x))))) patterns)))

(define (get-c six eight)
  (car (set->list (set-subtract (string->set eight) (string->set six)))))

(define (get-five patterns six)
  (car (filter (λ (x) (and (equal? (string-length x) 5)
                           (equal? (set-count (set-subtract (string->set x) (string->set six))) 0))) patterns)))

(define (get-three patterns seven)
  (car (filter (λ (x) (and (equal? (string-length x) 5)
                           (equal? (set-count (set-subtract (string->set x) (string->set seven))) 2))) patterns)))

(define (get-two patterns nine)
  (car (filter (λ (x) (and (equal? (string-length x) 5)
                           (equal? (set-count (set-subtract (string->set x) (string->set nine))) 1))) patterns)))

(define (line->number line)
  (let* ([patterns (map sort-letters (car line))]
         [output (map sort-letters (cadr line))]
         [eight (digit-from-length patterns 7)]
         [one (digit-from-length patterns 2)]
         [seven (digit-from-length patterns 3)]
         [four (digit-from-length patterns 4)]
         [six (get-six patterns seven)]
         [nine (get-nine patterns six four)]
         [zero (get-zero patterns six nine)]
         [five (get-five patterns six)]
         [three (get-three patterns seven)]
         [two (get-two patterns nine)]
         [result (λ (x) (match x [(== one) #\1]
                                 [(== two) #\2]
                                 [(== three) #\3]
                                 [(== four) #\4]
                                 [(== five) #\5]
                                 [(== six) #\6]
                                 [(== seven) #\7]
                                 [(== eight) #\8]
                                 [(== nine) #\9]
                                 [(== zero) #\0]))])
        (string->number (list->string (map result output)))))

(define (solution2 input)
  (sum (map line->number (input2 input))))

;;; Part1
(display (solution1 input))
(newline)

;;; Part2
(display (solution2 input))
(newline)

;; Tests
(require rackunit)

(define example-input
'(("be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb "
   " fdgacbe cefdb cefbgd gcbe")
  ("edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec "
   " fcgedb cgb dgebacf gc")
  ("fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef "
   " cg cg fdcagb cbg")
  ("fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega "
   " efabcd cedba gadfec cb")
  ("aecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga "
   " gecf egdcabf bgf bfgea")
  ("fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf "
   " gebdcfa ecba ca fadegcb")
  ("dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf "
   " cefg dcbef fcge gbcadfe")
  ("bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd "
   " ed bcgafe cdgba cbgef")
  ("egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg "
   " gbdfcae bgc cg cgb")
  ("gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc "
   " fgae cfgab fg bagce")))
;
(check-equal? (solution1 example-input) 26)
(check-equal? (solution2 example-input) 61229)

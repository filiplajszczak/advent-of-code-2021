#lang racket
(require algorithms)

(define input
  (map (λ (x) (string-split x "|")) (file->lines "day08.in")))

(define (input1 input)
  (append* (map string-split (map cadr input))))

(define (solution1 input)
  (count (λ (x) (member (string-length x) '(2 3 4 7))) (input1 input)))

(define string->set
  (compose list->set string->list))

(define (input2 input)
  (map (λ (line) (map (λ (part) (map string->set part)) (map string-split line))) input))

(define (set-count-equal? st n)
  (equal? (set-count st) n))

(define (fits? base mask reminder-count)
  (set-count-equal? (set-subtract base mask) reminder-count))

(define (get-six patterns seven)
  (findf (λ (pat) (and (set-count-equal? pat 6)
                       (fits? pat seven 4))) patterns))

(define (get-nine patterns six four)
  (findf (λ (pat) (and (set-count-equal? pat 6)
                       (not (set=? six pat))
                       (fits? pat four 2))) patterns))

(define (get-zero patterns nine six)
  (findf (λ (pat) (and (set-count-equal? pat 6)
                       (not (set=? nine pat))
                       (not (set=? six pat)))) patterns))

(define (get-five patterns six)
  (findf (λ (pat) (and (set-count-equal? pat 5) (fits? pat six 0))) patterns))

(define (get-three patterns seven)
  (findf (λ (pat) (and (set-count-equal? pat 5) (fits? pat seven 2))) patterns))

(define (get-two patterns nine)
  (findf (λ (pat) (and (set-count-equal? pat 5) (fits? pat nine 1))) patterns))

(define (line->number line)
  (let* ([patterns (car line)]
         [output (cadr line)]
         [digit-from-length
           (λ (len)
              (findf (λ (pat)
                        (set-count-equal? pat len))
                     patterns))]
         [eight (digit-from-length 7)]
         [one (digit-from-length 2)]
         [seven (digit-from-length 3)]
         [four (digit-from-length 4)]
         [six (get-six patterns seven)]
         [nine (get-nine patterns six four)]
         [zero (get-zero patterns six nine)]
         [five (get-five patterns six)]
         [three (get-three patterns seven)]
         [two (get-two patterns nine)]
         [result (λ (pat)
                    (match pat 
                      [(== one) #\1]
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

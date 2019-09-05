#lang racket
(require rackunit)
(require kw-utils/partial)

;---------------------
(define (filter-n predicate . lists)
  (let ([filtered
         (filter
          (lambda (cross-section) (apply predicate cross-section))
          (apply map list lists))])
    `(
     ,(map first filtered)
     ,(map second filtered))))
    
(check-equal?
 (filter-n eq? `(3 4 2 6) `(3 4 5 7))
 `((3 4) (3 4)))

(check-equal?
 (filter-n (compose not eq?) `(3 4 2 6) `(3 4 5 7))
 `((2 6) (5 7)))

;-------------------------------
(define (filter-out-bulls answer-digits guess-digits)
  (filter-n (compose not eq?) answer-digits guess-digits))

(check-equal?
 (filter-out-bulls `(1 2 4 5) `(1 4 3 2))
 `((2 4 5) (4 3 2)))

;----------------------
(define (contains? list item)
  (ormap (partial eq? item) list))

(check-equal? (contains? `(2 3 4 1) 1) #t)
(check-equal? (contains? `(2 3 4 1) 7) #f)

;-------------------------------

(define (count-cows  answer-digits guess-digits)
  (let ([without-bulls (filter-out-bulls answer-digits guess-digits)])
    (count (partial contains? (second without-bulls)) (first without-bulls))))

(check-equal? (count-cows `(1 2 4 5) `(1 4 3 2)) 2)
(check-equal? (count-cows `(1 2 4 5) `(2 4 5 1)) 4)
(check-equal? (count-cows `(1 2 3 4) `(5 6 7 8)) 0)

;-------------------------------
(define (count-bulls answer-digits guess-digits)
  (count eq? answer-digits guess-digits))

(check-equal? (count-bulls `(1 2 4 5) `(1 2 5 4)) 2)

;-----------------------------
(define (create-game answer-digits)
  (lambda (guess-digits)
    (if (eq? guess-digits `answer)
        answer-digits
        (let ([count-bull (count-bulls answer-digits guess-digits)]
              [count-cow (count-cows answer-digits guess-digits)])
          `(,count-bull bulls ,count-cow cows)))))
    
(define test-game (create-game `(1 2 3 4)))
(check-equal? (test-game `(1 2 4 5)) `(2 bulls 1 cows))
(check-equal? (test-game `(1 4 7 8)) `(1 bulls 1 cows))
(check-equal? (test-game `(1 2 3 4)) `(4 bulls 0 cows))
(check-equal? (test-game `(4 3 2 1)) `(0 bulls 4 cows))
(check-equal? (test-game `(5 6 7 8)) `(0 bulls 0 cows))

;-----------------------------
(define (shuffle list)
  (if (eq? 1 (length list))
      list
       (let* ([random-index (random 0 (length list))]
             [item-at-index (list-ref list random-index)])
         (cons
          item-at-index
          (shuffle (remq item-at-index list))))))

(define game
  (create-game (take (shuffle (range 0 10)) 4))) 

;example game creation
(define answer `(5 2 9 1))
(define my-game (create-game answer))
;example guessing
(my-game `(1 1 1 1))
;`(1 bulls 0 cows)
(my-game `(2 2 2 2))
;'(1 bulls 0 cows)
(my-game `(3 3 3 3))
;'(0 bulls 0 cows)
(my-game `(1 3 3 3))
;'(0 bulls 1 cows)
(my-game `(3 1 3 3))
;'(0 bulls 1 cows)
(my-game `(3 3 1 3))
;'(0 bulls 1 cows)
(my-game `(2 2 3 1))
;'(2 bulls 0 cows)
(my-game `(4 2 3 1))
;'(2 bulls 0 cows)
(my-game `(5 2 6 1))
;'(3 bulls 0 cows)
(my-game `(5 2 7 1))
;'(3 bulls 0 cows)
(my-game `(5 2 8 1))
;'(3 bulls 0 cows)
(my-game `(5 2 9 1))
;'(4 bulls 0 cows)


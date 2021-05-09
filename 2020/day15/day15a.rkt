#lang racket

(module+ main
  (let* ([target-a 2020]
         [target-b 30000000]
         [input "6,4,12,1,20,0,16"]
         [numbers (parse-numbers input)])
    ;; 475
    (println "Solving Day15A...")
    (printf "~a\n" (solve numbers target-a))
    (println "Solving Day15B...")
    (printf "~a\n" (solve numbers target-b))))

;; Replacing immutable hash with mutable and mutable vectors inside is still slow
(define/contract (solve input target)
  ((listof exact-integer?) exact-integer? . -> . exact-integer?)
  (let ([memory (init-memory input)]
        [start (add1 (length input))]
        [num (last input)])
    (simulate memory target start num)))

(define/contract (init-memory input)
  ((listof exact-integer?) . -> . (hash/c exact-integer? (listof exact-integer?)))
  (for/fold ([i 1]
             [mem (hash)]
             #:result mem)
            ([num (in-list input)])
    (values (add1 i) (hash-set mem num (list i)))))

(define/contract (simulate memory target start num)
  ((hash/c exact-integer? (listof exact-integer?)) exact-integer? exact-integer? exact-integer? . -> . exact-integer?)
  ;; (printf "~a ~a\n" start target)
  (for/fold ([mem memory]
             [res num]
             #:result res)
            ([i (in-range start (add1 target))])
    (let ([next (calculate-next mem res i)])
      ;; (printf "~a: ~a ~a spoken: ~a\n" i mem res next)
      (when (= 0 (modulo i 10000))
        (displayln i))
      (values (update-memory mem next i) next))))

(define/contract (calculate-next memory prevSpokenN index)
  ((hash/c exact-integer? (listof exact-integer?)) exact-integer? exact-integer? . -> . exact-integer?)
  (match (hash-ref memory prevSpokenN '())
    ;; not seen yet
    [(list) 0]
    ;; seen only once
    [(list _) 0]
    ;; difference between indices
    [(list latest older) (- latest older)]))

(define/contract (update-memory memory n index)
  ((hash/c exact-integer? (listof exact-integer?)) exact-integer? exact-integer? . -> . (hash/c exact-integer? (listof exact-integer?)))
  (let* ([indices (hash-ref memory n '())]
         [newIndices (for/list ([_ '(0 1)]
                                [i (cons index indices)]) i)])
    (hash-set memory n newIndices)))

(define/contract (parse-numbers input)
  (string? . -> . (listof exact-integer?))
  (map string->number (string-split input ",")))

(module+ test
  (require rackunit)
  (check-equal? (parse-numbers "0,3,6")
                '(0 3 6)
                "parse-numbers failed")
  (check-= (solve '(0 3 6) 10)
           0
           0
           "0 3 6; 10")
  (check-= (solve '(0 3 6) 2020)
           436
           0
           "0 3 6; 2020")
  (check-= (solve '(1 3 2) 2020)
           1
           0
           "1 3 2; 2020")
  (check-= (solve '(2 1 3) 2020)
           10
           0
           "2 1 3; 2020")
  (check-= (solve '(1 2 3) 2020)
           27
           0
           "1 2 3; 2020")
  (check-= (solve '(2 3 1) 2020)
           78
           0
           "2 3 1; 2020")
  (check-= (solve '(3 2 1) 2020)
           438
           0
           "3 2 1; 2020")
  (check-= (solve '(3 1 2) 2020)
           1836
           0
           "3 1 2; 2020"))

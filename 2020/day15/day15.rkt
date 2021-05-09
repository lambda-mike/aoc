#lang racket

(module+ main
  (let* ([target-a 2020]
         [target-b 30000000]
         [input "6,4,12,1,20,0,16"]
         [numbers (parse-numbers input)])
    ;; 475
    (println "Solving Day15A...")
    (printf "~a\n" (solve numbers target-a))
    ;; 11261
    (println "Solving Day15B...")
    (printf "~a\n" (solve numbers target-b))))

(define/contract (solve input target)
  ((listof exact-integer?) exact-integer? . -> . exact-integer?)
  (let ([memory (for/vector ([i target]) (make-vector 2 0))]
        ;; start index
        [start (add1 (length input))]
        ;; last spoken number
        [num (last input)])
    (for ([i (in-naturals 1)]
          [n (in-list input)])
      (let ([indices (vector-ref memory n)])
        (vector-set! indices 0 i)))
    (for ([i (in-range start (add1 target))])
      ;; (when (= 0 (modulo i 100000))
      ;;   (displayln i))
      (let* ([indices (vector-ref memory num)]
             [latest (vector-ref indices 0)]
             [oldest (vector-ref indices 1)])
        (cond [(or (= 0 latest) (= 0 oldest)) (set! num 0)]
              [else (set! num (- latest oldest))]))
      (update-indices! (vector-ref memory num) i))
    num))

(define (update-indices! indices index)
  (let ([latest (vector-ref indices 0)])
    (vector-set! indices 1 latest)
    (vector-set! indices 0 index))
  )

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

#lang racket

(define (simplify expr)
 (cond
   ;failed recursion check
   ;[(and (and (not(number? (second expr))) (not(symbol? (second expr)))) (and (not(number? (last expr))) (not(symbol? (last expr)))))
   ;  (define child1 (second expr))
   ;  (define child2 (last expr))
   ;  (cond
   ;    [(and (symbol? (second child1)) (or (not(equal? (second child1) (second child2))) (not(equal? (second child1) (last child2)))))
   ;     expr]
   ;    [(and (symbol? (last child1)) (or (not(equal? (last child1) (second child2)))) (not(equal? (last child1) (last child2))))
   ;     expr]
   ;    [else (simplify(list (first expr) simplify(child1) simplify(child2)))]
   ;    )
   ;  ]
   ;
   ;addtion
   [(equal? (first expr) '+)
    (cond
      ;checking for simplying multiplication (3x + 5x = 8x)
      [(and (and (not(number? (second expr))) (not(symbol? (second expr)))) (and (not(number? (last expr))) (not(symbol? (last expr)))))
        (define a (first (second expr)))
        (define b (second(second expr)))
        (if (and (and (equal? a (first(last expr))) (equal? a '*)) (equal? b (second(last expr))))
            (list a b (+(last (last expr)) (last (second expr))))
          (simplify(list (first expr) (simplify (second expr)) (simplify(last expr)))))
        ]
      ;simplifying inner expressions
      [(and (not(number? (second expr))) (not(symbol? (second expr))))
        (simplify(list (first expr) (simplify (second expr)) (last expr)))]
      [(and (not(number? (last expr))) (not(symbol? (last expr))))
        (simplify(list (first expr) (second expr) (simplify (last expr))))]
      ;removing 0's
      [(symbol? (second expr))
        (if (equal? (last expr) 0)
            (second expr)
        expr)]
      [(symbol? (last expr))
       (if (equal? (second expr) 0)
            (last expr)
        (list (first expr) (last expr) (second expr)))]
      ;standard addition
      [else(+ (second expr) (last expr))]
      )
    ]
   ;subtraction
  [(equal? (first expr) '-)
    (cond
      ;simplifying inner expressions
      [(and (not(number? (second expr))) (not(symbol? (second expr))))
        ;checking for double negative
        (define child (last expr))
        (if (and(equal? (length expr) 2) (equal? child (list (first expr) (second child))))
            (second child)
        (simplify(list (first expr) (simplify (second expr)) (last expr))))]
      [(and (not(number? (last expr))) (not(symbol? (last expr))))
        (simplify(list (first expr) (second expr) (simplify (last expr))))]
      ;removing 0
      [(symbol? (second expr))
        (if (equal? (last expr) 0)
            (second expr)
        expr)]
      [(symbol? (last expr))
        (if (equal? (second expr) 0)
            (list (first expr) (last expr))
        expr)]
      ;standard subtraction
      [else(- (second expr) (last expr))]
      )
    ]
  ;multiplication
  [(equal? (first expr) '*)
    (cond
      ;simplifying inner expressions
      [(and (not(number? (second expr))) (not(symbol? (second expr))))
        (simplify(list (first expr) (simplify (second expr)) (last expr)))]
      [(and (not(number? (last expr))) (not(symbol? (last expr))))
        (simplify(list (first expr) (second expr) (simplify (last expr))))]
      ;setting to 0 or just itself times 1
      [(symbol? (second expr))
        (cond
          [(equal? (last expr) 0)
            0]
          [(equal? (last expr) 1)
            (second expr)]
          [else expr])]
      [(symbol? (last expr))
        (cond
          [(equal? (second expr) 0)
            0]
          [(equal? (second expr) 1)
            (last expr)]
          [(number? (second expr))
            (list (first expr) (last expr) (second expr))]
          [else expr])]
      ;standard multiplication
      [else(* (second expr) (last expr))]
      )
    ]
  ;division
  [(equal? (first expr) '/)
    (cond
      ;simplifying inner expresions
      [(and (not(number? (second expr))) (not(symbol? (second expr))))
        (simplify(list (first expr) (simplify (second expr)) (last expr)))]
      [(and (not(number? (last expr))) (not(symbol? (last expr))))
        (simplify(list (first expr) (second expr) (simplify (last expr))))]
      ;preventing division with symbols
      [(or(symbol? (second expr)) (symbol? (last expr)))
        expr]
      ;standard subtraction
      [else(/ (second expr) (last expr))])]
  ;emergency catch
  [else expr]
  )
  )

(simplify '(+ x 0))
(simplify '(+ 5 (+ 10 2)))
(simplify '(+ (* 1 x) (* y 0)) )
(simplify '(+ 0 (+ 0 (+ 0 z))) )
(simplify '(* (+ 1 0) (+ x 0)) )
(simplify '(+ (* 1 x) (+ 0 y)))
(simplify '(- 20 (/ (+ (* 4 5)(* 2 5))(- 8 2))))
(simplify '(+ (* 2 x) (* 3 x)) )
;(simplify '(+ (* 2 x) (* 3 y)) )
#lang racket
(provide expr-compare)

;; Part 1: expr-compare

; helper function to find lambda symbol
(define (lambda? x) (member x '(lambda λ)))

(define (expr-compare x y)
  (cond
    ; trivial case - no difference
    [(equal? x y) x]

    ; boolean values
    [(and (boolean? x) (boolean? y)) (if x '% '(not %))]

    ; if one of them is not a list OR they are 2 lists with diff lengths
    [(or
      (or (not (list? x)) (not (list? y)))
      (not (equal? (length x) (length y))))
        (list 'if '% x y)]

    ; two lists of equal length
    [else (expr-compare-list x y)]))

; at this point, we know the lists are not equal but they have the same length
(define (expr-compare-list x y)
  (cond
    ; if either expr starts with 'quote'
    [(or (equal? (car x) 'quote) (equal? (car y) 'quote)) (list 'if '% x y)]

    ; both are some form of lambda (text/sym)
    [(and (lambda? (car x)) (lambda? (car y))) (expr-compare-lambda x y '() '())]

    ; only one of the lists starts with lambda
    [(or (lambda? (car x)) (lambda? (car y))) (list 'if '% x y)]

    ; only one list starts with if
    [(not (equal? (equal? (car x) 'if) (equal? (car y) 'if))) (list 'if '% x y)]

    ; all other cases
    [else (expr-compare-normal x y)]))

; basic expr compare case 
(define (expr-compare-normal x y)
  (cond
    ; base case - end of list
    [(equal? x '()) '()]

    ; current element is equal
    [(equal? (car x) (car y)) (cons (car x) (expr-compare-normal (cdr x) (cdr y)))]

    ; else, recursively call expr-compare
    [else (cons (expr-compare (car x) (car y)) (expr-compare-normal (cdr x) (cdr y)))]
  ))

; compare matching lambda functions
(define (expr-compare-lambda x y xdicts ydicts)
  (cond
    ; length is not equal
    [(not (equal? (length (car (cdr x))) (length (car (cdr y))))) (list 'if '% x y)]

    ; at least one is ASCII lambda
    [(or (equal? (car x) 'λ) (equal? (car y) 'λ)) (handle-compare-lambda (cdr x) (cdr y) 'λ xdicts ydicts)]

    ; both are text lambda
    [else (handle-compare-lambda (cdr x) (cdr y) 'lambda xdicts ydicts)]
  ))

; handle matching lambda functions w/ specified version (ASCII/text)
;   car x/y is the args list
(define (handle-compare-lambda x y lam xdicts ydicts)
  (list
    lam
    (build-lambda-args (car x) (car y))
    (build-lambda-func (car (cdr x)) (car (cdr y))
                       (cons (create-dict (car x) (car y) #t) xdicts)
                       (cons (create-dict (car x) (car y) #f) ydicts))
  ))

; builds new args for combined lambda function
(define (build-lambda-args x y)
  (cond
    ; base case
    [(equal? x '()) '()]

    ; different - use option
    [(not (equal? (car x) (car y))) (cons (create-option (car x) (car y)) (build-lambda-args (cdr x) (cdr y)))]

    ; same
    [else (cons (car x) (build-lambda-args (cdr x) (cdr y)))]
  ))

; builds new func body for combined lambda function
(define (build-lambda-func x y xdicts ydicts)
  (let
    ([newx (if (equal? (lookup xdicts x) "failure") x (lookup xdicts x))]
     [newy (if (equal? (lookup ydicts y) "failure") y (lookup ydicts y))])

    (cond
      ; if the original/mapped values are the same, we're done
      [(equal? newx newy) newx]

      ; both x and y are lists
      [(and (list? x) (list? y)) (expr-compare (apply-dicts xdicts x #t) (apply-dicts ydicts y #t))]

      [(list? x) (expr-compare (apply-dicts xdicts x #t) newy)]

      [(list? y) (expr-compare x (apply-dicts ydicts y #t))]

      [else (expr-compare newx newy)]
    )
  ))
  

; builds dict mapping x->(x!y) or y->(x!y) depending on forx?
(define (create-dict x y forx?)
  (cond
    ; base case
    [(equal? x '()) (hash)]

    ; different - add mapping to dict
    [(not (equal? (car x) (car y)))
      (if forx? 
        (hash-set (create-dict (cdr x) (cdr y) forx?) (car x) (create-option (car x) (car y)))
        (hash-set (create-dict (cdr x) (cdr y) forx?) (car y) (create-option (car x) (car y)))
      )]
     
    ; equal - no mapping needed
    [else (create-dict (cdr x) (cdr y) forx?)]
  ))

; build reflexive dict (maps to itself)
(define (create-reflexive-dict x)
  (cond
    ; base case
    [(equal? x '()) (hash)]

    ; equal - no mapping needed
    [else (hash-set (create-reflexive-dict (cdr x)) (car x) (car x))]
  ))

; lookup value in dicts, prioritized by most recent definition
(define (lookup dicts k)
  (cond
    ; reached end of dicts without finding match
    [(empty? dicts) "failure"]

    ; if current dict fails, check the next
    [(equal? (hash-ref (car dicts) k "failure") "failure") (lookup (cdr dicts) k)]

    ; found in current dict
    [else (hash-ref (car dicts) k)]
  ))

; apply dictionary mappings to a list
(define (apply-dicts dicts x first?)
  (cond
    ; base case
    [(equal? x '()) '()]

    ; don't replace booleans
    [(boolean? (car x)) (cons (car x) (apply-dicts dicts (cdr x) #f))]

    ; if first element is 'quote, don't apply
    [(and first? (equal? (car x) 'quote)) x]

    ; first element is if
    [(and first? (equal? (car x) 'if)) (cons 'if (apply-dicts dicts (cdr x) #f))]

    ; first element is lambda - we prioritize the dict for the new lambda and apply all mappings
    [(and first? (lambda? (car x)))
       (cons (car x)
             (cons (car (cdr x))
             (apply-dicts (cons (create-reflexive-dict (car (cdr x))) dicts) (cdr (cdr x)) #t)))]

    ; recursively apply dict inside lists
    [(list? (car x)) (cons (apply-dicts dicts (car x) #t) (apply-dicts dicts (cdr x) #f))]
    
    ; lookup and replace
    [else (cons (if (equal? (lookup dicts (car x)) "failure") (car x) (lookup dicts (car x)))
                (apply-dicts dicts (cdr x) #f))]
  ))

; helper function to build a!b
(define (create-option a b)
  (string->symbol (string-append (symbol->string a) "!" (symbol->string b))))
  

;; Part 2: test-expr-compare

; compare and see if the (expr-compare x y) result is the same with x when % = #t
;                                                 and the same with y when % = #f
(define (test-expr-compare x y) 
  (and (equal? (eval x)
               (eval `(let ([% #t]) ,(expr-compare x y))))
       (equal? (eval y)
               (eval `(let ([% #f]) ,(expr-compare x y))))))

;; Part 3: test-expr-x and test-expr-y
(define test-expr-x '(lambda (a b) (lambda (a d) (if d #t (quote a)))))
(define test-expr-y '(lambda (b a) (lambda (c d) (if d #f (quote b)))))

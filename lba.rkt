#lang racket

(provide (all-defined-out))

(struct Exception (msg))
(struct Rule (start cur-sym action end))

; return n-th element of list l
(define index
  (lambda (l n)
    (cond
      [(null? l) (error "indexing an empty list")]
      [else (if (zero? n)
                (car l)
                (index (cdr l) (sub1 n)))])))

; replace nth position of list a with symbol s
(define update
  (lambda (l n s)
    (cond
      [(null? l) (error "indexing an empty list")]
      [else (if (zero? n)
                (cons s (cdr l))
                (cons (car l) (update (cdr l) (sub1 n) s)))])))

; preprocess raw input by > and ∎ (\qed)
; input string should be a list of symbols
(define (prep input)
  (append (cons '> input) (list '∎)))

(define (initialize-cfg str start)
  `(,start 0 ,(prep str)))

; gives a list of pairs (act . dest) with matching start state and read symbol
(define (lookup rules st sym)
  (match rules
    ['() '()]
    [`((,start ,s ,act ,end) . ,rs)
     (if (and (eqv? start st)
              (eqv? s sym))
         (cons `(,act . ,end) (lookup (cdr rules) st sym))
         (lookup (cdr rules) st sym))]))

; lba actions shift cursor to r/l or replace the symbol at cursor pos
(define (shift-r cfg d)
  (match cfg
    [`(,st ,pos ,str)
     `(,d ,(add1 pos) ,str)]))

(define (shift-l cfg d)
  (match cfg
    [`(,st ,pos ,str)
     `(,d ,(sub1 pos) ,str)]))

(define (replace cfg a d)
  (match cfg
    [`(,st ,pos ,str)
     `(,d ,pos ,(update str pos a))]))


; Step from one configuartion to another
; (operational semantics of the machine)
(define (yield rules cfg goal)
  (begin (void)
         (match cfg
           [`(,st ,pos ,str) (let* ([s (index str pos)]
                                    [l (lookup rules st s)]
                                    [p1 (if (null? l)
                                            (error "no transitions found")
                                            (car l))]
                                    [act (car p1)]
                                    [dest (cdr p1)])
                               (if (eqv? dest goal)
                                   "==> accept!"
                                   (match act
                                     ['+ (shift-r cfg dest)]
                                     ['- (shift-l cfg dest)]
                                     [`,a (replace cfg a dest)])))])))

; Recur on yield
(define (drive auto cfg goal)
  (begin (displayln cfg)
         (let ([next (yield auto cfg goal)])
                (if (string? next)
                    (displayln next)
                    (drive auto next goal)))))


; Example-01: an LBA that recognizes L = { wwʳ | w ∈ {a, b}*}

; Extended alphabet Γ = {a, b} ∪ {>, ∎, γ}
; γ used to cross off a symbol
(define lba-wwr `((S > + L)
                  (L ∎ + A)
                  (L γ + L)
                  (L a γ La)
                  (L b γ Lb)
                  (La γ + Ra)
                  (Lb γ + Rb)

                  (Ra a + Ra)
                  (Ra b + Ra)
                  (Ra γ - Ua)
                  (Ra ∎ - Ua)

                  (Rb a + Rb)
                  (Rb b + Rb)
                  (Rb γ - Ub)
                  (Rb ∎ - Ub)

                  (Ua a γ U)
                  (Ub b γ U)
                  (U γ - V)

                  (V a - V)
                  (V b - V)
                  (V > + L)
                  (V γ + L)
                  ))



(define (example-01)
  (let* ([raw-input `(a b b a)]
         [init-cfg (initialize-cfg raw-input 'S)]
         [goal 'A])
    (drive lba-wwr init-cfg goal)))


(define (example-02)
  (let* ([raw-input `(a b a b)]
         [init-cfg (initialize-cfg raw-input 'S)]
         [goal 'A])
    (drive lba-wwr init-cfg goal)))

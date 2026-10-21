#lang racket
(require "ch4.rkt")

(define (mem x l)
  (cond
    ((null? l) #f)
    ((equal? (car l) x) l)
    (#t (mem x (cdr l)))))

(defrel (memo x l out)
  (conde
   ((caro l x) (== l out))
   ((fresh (d)
      (cdro l d)
      (memo x d out)))))

(define (rember x l)
  (cond
    ((null? l) '())
    ((equal? (car l) x) (cdr l))
    (#t (cons (car l)
             (rember x (cdr l))))))

(defrel (rembero x l out)
  (conde
    ((nullo l) (== '() out))
    ((conso x out l))
    ((fresh (a d res)
          (conso a d l)
          (conso a res out)
          (rembero x d res)))))
;; Frame 26:
;; What is the value of
;; (run* out (rembero 'pea '(pea) out))
;; --> '( () (pea) )
;; The first value for out comes from the 2nd line of the conde:
;;   Because we can cons 'pea onto '() to get '(pea)
;; The second value for out comes from the 3rd line of the conde:
;;   We know l is '(pea), we also know fresh d becomes '(),
;;   because that's the cdr of l, so fresh a becomes 'pea,
;;   then the other conso goal says the cons of 'pea onto some fresh
;;   res is our fresh out, but we know conso will succeed like
;;   (conso 'pea _1 '(pea . _1)) where _1 is the fresh res. So
;;   all that remains to get a complete value for out is to see
;;   how/if (rembero 'pea '() _1) succeeds. It does, from the first line
;;   of its conde, because l will be '(), so _1 is associated to '(),
;;   and that means out is associated to '(pea . ()), which is just '(pea).
;;   so the value of the run* is '(() (pea)).
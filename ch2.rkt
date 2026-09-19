#lang racket
;; doesn't include disj2 and conj2 which we want for early chapters
;; before learning about conde
;;(require minikanren)
(include "trs2-impl.scm")

(provide (all-defined-out))

(defrel (caro p a)
  (fresh (d)
         (conso a d p)))

(defrel (cdro p d)
  (fresh (a)
         (conso a d p)))

(defrel (conso a d p)
  (== `(,a . ,d) p))

(defrel (nullo p)
  (== p '()))

(defrel (pairo p)
  (fresh (a d)
         (conso a d p)))

(define (singleton? l)
  (cond
    ((pair? l) (null? (cdr l)))
    (else #f)))

(define (singletono l)
  (fresh (d)
         (cdro l d)
         (nullo d)))

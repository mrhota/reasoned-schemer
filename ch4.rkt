#lang racket
(require "ch3.rkt")

(provide (all-from-out "ch3.rkt")
         (all-defined-out))

(define (append l t)
  (cond
    ((null? l) t)
    (#t (cons (car l) (append (cdr l) t)))))

(defrel (appendo l t out)
  (conde
   ((nullo l) (== t out))
   ((fresh (a d res)
           (conso a d l)
           (conso a res out)
           (appendo d t res)))))

(defrel (swappendo l t out)
  (conde
   ((fresh (a d res)
           (conso a d l)
           (conso a res out)
           (swappendo d t res)))
   ((nullo l) (== t out))))

(define (unwrap x)
  (cond
    ((pair? x) (unwrap (car x)))
    (#t x)))

(defrel (unwrapo x out)
  (conde
    ((== x out))
    ((fresh (a res)
            (caro x a)
            (unwrapo a out)))))
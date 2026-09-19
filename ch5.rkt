#lang racket
(require "ch4.rkt")

(define (mem x l)
  (cond
    ((null? l) #f)
    ((equal? (car l) x) l)
    (#t (mem x (cdr l)))))

(defrel (memo x l out)
  (conde
   ((fresh (a)
           (caro l a)
           (== a x)
           (== l out)))
   (succeed (fresh (d)
                   (cdro l d)
                   (memo x d out)))))
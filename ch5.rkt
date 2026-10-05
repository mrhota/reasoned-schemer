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

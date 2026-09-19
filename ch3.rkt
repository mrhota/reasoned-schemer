#lang racket

;; a racket module, with all trs2-impl forms exported
(require "ch2.rkt")

(provide (all-from-out "ch2.rkt")
         (all-defined-out))

(define (list? l)
  (cond
    ((null? l) #t)
    ((pair? l) (list? (cdr l)))
    (#t #f)))

(defrel (listo l)
  (conde
   ((nullo l))
   ((fresh (d)
           (cdro l d)
           (listo d)))))

(define (lol? l)
  (cond
    ((null? l) #t)
    ((list? (car l)) (lol? (cdr l)))
    (#t #f)))

(defrel (lolo l)
  (conde
   ((nullo l))
   ((fresh (a d)
           (conso a d l)
           (listo a)
           (lolo d)))))

(defrel (singletono l)
  (fresh (a)
         (== `(,a) l)))

(defrel (loso l)
  (conde
   ((nullo l))
   ((fresh (a d)
           (conso a d l)
           (singletono a)
           (loso d)))))

(define (member? x l)
  (cond
    ((null? l) #f)
    ((equal? (car l) x) #t)
    (#t (member? x (cdr l)))))

(defrel (membero x l)
  (conde
   ((caro l x))
   ((fresh (a d)
          (conso a d l)
          (membero x d)))))

(defrel (proper-membero x l)
  (conde
   ((fresh (d)
           (conso x d l)
           (listo d)))
   ((fresh (a d)
           (conso a d l)
           (proper-membero x d)))))

(define (proper-member? x l)
  (cond
    ((null? l) #f)
    ((equal? (car l) x) (list? (cdr l)))
    (#t (proper-member? x (cdr l)))))

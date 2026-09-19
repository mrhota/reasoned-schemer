#lang racket
;; (require minikanren)
(include "trs2-impl.scm")

(defrel (teacupo t)
  (disj2 (== 'tea t) (== 'cup t)))

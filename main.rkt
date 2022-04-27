#lang racket/base

(module+ test
  (require rackunit))

(require "universe.rkt")
(require "cell.rkt")

;; Code here
(define blinker (list (cell 1 0) (cell 1 1) (cell 1 2)))


(module+ test
  ;; Any code in this `test` submodule runs when this file is run using DrRacket
  ;; or with `raco test`. The code here does not run when this file is
  ;; required by another module.

  (check-equal? (+ 2 2) 4))

(module+ main
  (define u (universe 80 25))

  (let loop ([live-cells blinker]
             [n 0])
    (printf "~a~n" live-cells)
    (cond 
          [(< n 2) (loop (step u live-cells) (+ n 1))])))
  

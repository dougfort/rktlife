#lang racket/base

(module+ test
  (require rackunit))

(require "cell.rkt")

(struct universe (width height)
  #:transparent)
(provide universe)

(define (neighbors u c)
  (define width  (universe-width u))
  (define height (universe-height u))
  (define x (cell-x c))
  (define y (cell-y c))
  (for/list (
             [delta-x '(-1 -1 -1  0 0  1 1 1)]
             [delta-y '(-1  0  1 -1 1 -1 0 1)])
    
    (cell (compute-new-x width x delta-x) (compute-new-y height y delta-y))))
(provide neighbors)

(define (compute-new-x w x delta)
  (define left-edge 0)
  (define right-edge (- w 1))
  (cond
    [(and (= delta -1) (= x left-edge))  right-edge]
    [(and (= delta  1) (= x right-edge)) left-edge]
    [else (+ x delta)]))

(module+ test
  (check-equal? (compute-new-x 80  0 -1) 79)
  (check-equal? (compute-new-x 80  0  1)  1)
  (check-equal? (compute-new-x 80 79 -1) 78)
  (check-equal? (compute-new-x 80 79  1)  0)
  (check-equal? (compute-new-x 80 78  1) 79)
  (check-equal? (compute-new-x 80 79  0) 79)
  (check-equal? (compute-new-x 80  0  0)  0))

(define (compute-new-y h y delta)
  (define top-edge 0)
  (define bottom-edge (- h 1))
  (cond
   [(and (= delta -1) (= y top-edge)) bottom-edge]
   [(and (= delta  1) (= y bottom-edge)) top-edge]
   [else (+ y delta)]))

(module+ test
  (check-equal? (compute-new-y 25  0 -1) 24)
  (check-equal? (compute-new-y 24  0  1)  1)
  (check-equal? (compute-new-y 25 24 -1) 23)
  (check-equal? (compute-new-y 25 24  1)  0)
  (check-equal? (compute-new-y 25 23  1) 24)
  (check-equal? (compute-new-y 25 24  0) 24)
  (check-equal? (compute-new-y 25  0  0)  0))


(define u (universe 4 4))

(define c (cell 4 4))
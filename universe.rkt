#lang racket/base

; the universe where life takes place

(provide
 universe
 universe-width
 universe-height
 step)

(module+ test
  (require rackunit))

(require racket/set)
(require "cell.rkt")

(struct universe (width height)
  #:transparent)

; identify the neighbors of a cell

(define (neighbors u c)
  (define width  (universe-width u))
  (define height (universe-height u))
  (define x (cell-x c))
  (define y (cell-y c))
  (for*/list (
              [delta-x (in-range -1 2)]
              [delta-y (in-range -1 2)]
              #:unless (and (= 0 delta-x) (= 0 delta-y)))    
    (cell (compute-new-x width x delta-x) (compute-new-y height y delta-y))))

; transform current live cells, returning the next generation
(define (step u l)
  ; load a hash-set of live cells so we can identify them later
  (define live-cells (list->set l))
  
  (define ht (make-hash))
  ; load the hash table with counts of the neighbors of each cell in the list l
  (for ([c l])
    (for ([n (neighbors u c)])
      (let ([count (hash-ref ht n 0)])
        (hash-set! ht n (add1 count)))))
  (for/list ([(k v) (in-hash ht)]
             #:when (will-live? (set-member? live-cells k) v))    
    k))

; from wikipedia https://en.wikipedia.org/wiki/Conway%27s_Game_of_Life
;
; Any live cell with fewer than two live neighbours dies, as if by underpopulation.
; Any live cell with two or three live neighbours lives on to the next generation.
; Any live cell with more than three live neighbours dies, as if by overpopulation.
; Any dead cell with exactly three live neighbours becomes a live cell, as if by reproduction.
(define (will-live? is-live count)
  (cond
    [(and is-live (< count 2)) #f]
    [(and is-live (or (= count 2) (= count 3))) #t]
    [(and is-live (> count 3)) #f]
    [(and (not is-live) (= count 3)) #t]
    [else #f]))

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

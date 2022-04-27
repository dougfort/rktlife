#lang racket/base

; a single cell at position x y

(provide cell cell-x cell-y)

(struct cell (x y)
  #:transparent)

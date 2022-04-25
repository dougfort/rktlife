#lang racket/base

;; a single cell at position x y
(struct cell (x y)
  #:transparent)
(provide cell cell-x cell-y)
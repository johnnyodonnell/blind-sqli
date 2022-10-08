#lang racket

(provide dump-table)


(define dump-table
  (lambda (request-generator database table)
    (displayln "Dumping table")))


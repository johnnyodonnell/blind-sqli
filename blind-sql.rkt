#lang racket

(require racket/cmdline)
(require "lib/actions/dump-table.rkt")


(define run
  (lambda (action url http-method database table)
    (cond
      [(equal? action "dump-table")
       (dump-table
         (lambda () #t))]
      [else
        (displayln "The requested action is not supported at this time.")])))

(define main
  (lambda ()
    (define database (make-parameter #f))
    (define table (make-parameter #f))
    (command-line
      #:program "blind-sql"
      #:once-each
      [("-d" "--database") db 
                            "Specify database"
                            (database db)]
      [("-t" "--table") tbl
                         "Specify table"
                         (table tbl)]
      #:args (action
               url
               http-method)
      (run action url http-method (database) (table)))))

(main)


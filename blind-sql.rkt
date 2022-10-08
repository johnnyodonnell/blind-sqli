#lang racket

(require racket/cmdline)
(require "lib/actions/dump-table.rkt")
(require "lib/request-generator-factory.rkt")


(define run
  (lambda (action
            http-method
            host
            path
            data-format-string
            success-regex
            database
            table)
    (displayln (format "Action: ~a" action))
    (displayln (format "HTTP Method: ~a" http-method))
    (displayln (format "Host: ~a" host))
    (displayln (format "Path: ~a" path))
    (displayln (format "Data format string: ~a" data-format-string))
    (displayln (format "Success regex: ~a" success-regex))
    (displayln (format "Database: ~a" database))
    (displayln (format "Table: ~a" table))
    (displayln "")
    (displayln "--- Executing Action ---")
    (flush-output)
    (let ([request-generator
            (create-request-generator
              http-method
              host
              path
              data-format-string
              success-regex)])
      (cond
        [(equal? (string-downcase action) "dump-table")
         (dump-table request-generator database table)]
        [else
          (begin
            (displayln
              "The requested action is not supported at this time.")
            (exit))]))))

(define main
  (lambda ()
    (define post-data (make-parameter #f))
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
               http-method
               host
               path
               data-format-string
               success-regex)
      (run
        action
        http-method
        host
        path
        data-format-string
        success-regex
        (database)
        (table)))))

(main)


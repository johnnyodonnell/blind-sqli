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
            fail-regex
            database
            table)
    (displayln (format "Action: ~a" action))
    (displayln (format "HTTP Method: ~a" http-method))
    (displayln (format "Host: ~a" host))
    (displayln (format "Path: ~a" path))
    (displayln (format "Data format string: ~a" data-format-string))
    (displayln (format "Success regex: ~a" success-regex))
    (displayln (format "Fail regex: ~a" fail-regex))
    (displayln (format "Database: ~a" database))
    (displayln (format "Table: ~a" table))
    (displayln "")

    (cond
      [(and success-regex fail-regex)
       (begin
         (displayln "Success regex and fail regex cannot both be defined")
         (exit))]
      [(not (or success-regex fail-regex))
       (begin
         (display "Either success regex or fail regex must be defined")
         (exit))]
      [else
        (begin
          (displayln "--- Executing Action ---")
          (flush-output)
          (let ([request-generator
                  (create-request-generator
                    http-method
                    host
                    path
                    data-format-string
                    success-regex
                    fail-regex)])
            (cond
              [(equal? (string-downcase action) "dump-table")
               (dump-table request-generator database table)]
              [else
                (begin
                  (displayln
                    "The requested action is not supported at this time.")
                  (exit))])))])))

(define main
  (lambda ()
    (define success-regex (make-parameter #f))
    (define fail-regex (make-parameter #f))
    (define database (make-parameter #f))
    (define table (make-parameter #f))
    (command-line
      #:program "blind-sql"
      #:once-each
      [("-s" "--success-regex") sr
                                "Specify success regex"
                                (success-regex sr)]
      [("-f" "--fail-regex") fr
                             "Specify fail regex"
                             (fail-regex fr)]
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
               data-format-string)
      (run
        action
        http-method
        host
        path
        data-format-string
        (success-regex)
        (fail-regex)
        (database)
        (table)))))

(main)


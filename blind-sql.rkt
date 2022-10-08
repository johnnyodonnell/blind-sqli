#lang racket

(require racket/cmdline)
(require "lib/actions/dump-table.rkt")
(require "lib/request-generator-factory.rkt")


(define run
  (lambda (action http-method host path success-regex post-data database table)
    (let ([request-generator
            (create-request-generator
              http-method
              host
              path
              success-regex
              post-data)])
      (cond
        [(equal? (string-downcase action) "dump-table")
         (dump-table request-generator database table)]
        [else
          (displayln
            "The requested action is not supported at this time.")]))))

(define main
  (lambda ()
    (define post-data (make-parameter #f))
    (define database (make-parameter #f))
    (define table (make-parameter #f))
    (command-line
      #:program "blind-sql"
      #:once-each
      [("-p" "--formatted-post-data") pd
                            "Specify formatted post data"
                            (post-data pd)]
      [("-d" "--database") db 
                            "Specify database"
                            (database db)]
      [("-t" "--table") tbl
                         "Specify table"
                         (table tbl)]
      #:args (action
               http-method
               host
               formatted-path
               success-regex)
      (run
        action
        http-method
        host
        formatted-path
        success-regex
        (post-data)
        (database)
        (table)))))

(main)


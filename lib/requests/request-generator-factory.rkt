#lang racket

(require net/http-client)
(require net/uri-codec)
(require "dynamic-scripts.rkt")

(provide create-request-generator)


(define data-format-string-getter (make-parameter #f))

(define read-response
  (lambda (response [first-iter #t])
    (let ([line (read-line response 'any)])
      (cond
        [(and first-iter (eof-object? line)) ""]
        [(eof-object? line) '()]
        [first-iter
          (string-join
            (cons line
                  (read-response response #f)))]
        [else (cons line (read-response response #f))]))))

(define encode-query
  (lambda (query [print #f])
    (let ([enc-query
            (uri-path-segment-encode
              (string-replace query #px"\\s+" "/**/"))])
      (if print
        (displayln enc-query)
        (void))
      enc-query)))

(define did-query-succeed?
  (lambda (str-response success-regex fail-regex [print #f])
    (cond
      [success-regex
        (not
          (not
            (regexp-match
              (regexp success-regex)
              str-response)))]
      [fail-regex
        (not
          (regexp-match
            (regexp fail-regex)
            str-response))]
      [else
        (begin
          (display "Either success regex or fail regex must be defined")
          (exit))])))

(define process-data-format-string
  (lambda (data-format-string host)
    (cond
      [(equal?
         (data-format-string-getter)
         'not-used)
       data-format-string]
      [(procedure?
         (data-format-string-getter))
       ((data-format-string-getter) host)]
      [(is-dynamic-script? data-format-string)
       (begin
         (data-format-string-getter
           (get-dynamic-script data-format-string))
         (process-data-format-string
           data-format-string
           host))]
      [else
        (begin
          (data-format-string-getter 'not-used)
          (process-data-format-string
            data-format-string
            host))])))

(define create-request-generator
  (lambda (http-method
            host
            path
            data-format-string
            headers
            success-regex
            fail-regex
            [print #f])
     (cond
       [(equal? (string-downcase http-method) "get")
        (lambda (query)
          (let-values ([(status headers response)
                        (http-sendrecv
                          host
                          (string-append
                            path
                            "?"
                            (encode-query
                              (format
                                (process-data-format-string
                                  data-format-string
                                  host)
                                query)
                              print))
                          #:headers headers)])
            (did-query-succeed?
              (read-response response)
              success-regex
              fail-regex
              print)))]
       [(equal? (string-downcase http-method) "post")
        (lambda (query)
          (let-values ([(status headers response)
                        (http-sendrecv
                          host
                          path
                          #:headers headers
                          #:method "POST"
                          #:data (encode-query
                                   (format
                                     (process-data-format-string
                                       data-format-string
                                       host)
                                     query)
                                   print))])
            (did-query-succeed?
              (read-response response)
              success-regex
              fail-regex
              print)))]
       [else
         (begin
           (displayln "HTTP method not supported at this time")
           (exit))])))


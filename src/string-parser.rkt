#lang typed/racket

(require "assert-equal.rkt")
(require "basic-types.rkt")
(require "define-forall.rkt")
(provide StringParser
         split-at-first
         split-at-last
         find-first
         find-last
         matching-char
         matching-chars
         exact-string
         exact-char
         string-parser-map
         match-no-strings
         string-parser-or)

(define-type (StringParser A)
  (All [X] (-> (-> Integer (Pair String X))
               (Maybe (Pair A X)))))

(define-forall [A]
               (split-at-first parser str)
             : (-> (StringParser A)
                   String
                   (Maybe (Triple String A String)))
  (let ([n (string-length str)])
    (let loop ([start 0])
      (: f (-> Integer (Pair String Integer)))
      (define (f len)
        (let* ([end (+ start len)]
               [middle (substring str start end)])
          (Pair middle end)))
      (match (parser f)
        [(Just (Pair a end))
         (let* ([prefix (substring str 0 start)]
                [suffix (substring str end)])
           (Just (Triple prefix a suffix)))]
        [(Nothing)
         (if (>= start n)
             (Nothing)
             (loop (+ start 1)))]))))

(define-forall [A]
               (split-at-last parser str)
             : (-> (StringParser A)
                    String
                    (Maybe (Triple String A String)))
  (let ([n (string-length str)])
    (let loop ([end n])
      (: f (-> Integer (Pair String Integer)))
      (define (f len)
        (let* ([start (- end len)]
               [middle (substring str start end)])
          (Pair middle start)))
      (match (parser f)
        [(Just (Pair a start))
         (let* ([prefix (substring str 0 start)]
                [suffix (substring str end)])
           (Just (Triple prefix a suffix)))]
        [(Nothing)
         (if (<= end 0)
             (Nothing)
             (loop (- end 1)))]))))

(define-forall [A]
               (find-first [parser : (StringParser A)] str)
             : (-> (StringParser A)
                    String
                    (Maybe A))
  (match (split-at-first [A] parser str)
    [(Just (Triple _ a _))
     (Just a)]
    [(Nothing)
     (Nothing)]))

(define-forall [A]
               (find-last parser str)
             : (-> (StringParser A)
                   String
                   (Maybe A))
  (match (split-at-last [A] parser str)
    [(Just (Triple _ a _))
     (Just a)]
    [(Nothing)
     (Nothing)]))

(define-forall [A]
               (matching-char pred)
             : (-> (-> Char (Maybe A))
                   (StringParser A))
  (lambda (f)
    (match (f 1)
      [(Pair str x)
       (let [(actual (string-ref str 0))]
         (match (pred actual)
           [(Just a)
            (Just (Pair a x))]
           [(Nothing)
            (Nothing)]))])))
(assert-equal
  (split-at-first [Char]
                  (matching-char [Char]
                                 (pred->maybe char-numeric?))
                  "abc123def")
  (Just (Triple "abc" #\1 "23def")))

(define-forall [A]
               (matching-chars pred)
             : (-> (-> Char (Maybe A))
                    (StringParser (Listof A)))
  (lambda #:forall [X] ([f : (-> Integer (Pair String X))])
    (let loop ([reversed-as : (Listof A)
                (list)]
               [n 0]
               [r : (-> (Maybe (Pair (Listof A) X)))
                (lambda () (Nothing))])
      (let ([next-n (+ n 1)])
        (match (f (+ n 1))
          [(Pair str x)
           (let [(char (string-ref str n))]
             (match (pred char)
               [(Just next-a)
                (let ([next-reversed-as
                       (cons next-a reversed-as)])
                  (loop next-reversed-as
                        next-n
                        (lambda ()
                          (Just (Pair (reverse next-reversed-as)
                                      x)))))]
               [Nothing
                (r)]))])))))
(assert-equal
  (split-at-first [(Listof Char)]
                  (matching-chars [Char]
                                  (pred->maybe char-numeric?))
                  "abc123def")
  (Just (Triple "abc" (list #\1 #\2 #\3) "def")))

(define-forall [A]
               (exact-string expected a)
             : (-> String A (StringParser A))
  (lambda (f)
    (match (f (string-length expected))
      [(Pair actual x)
       (if (string=? actual expected)
           (Just (Pair a x))
           (Nothing))])))
(assert-equal
  (split-at-first [Unit]
                  (exact-string [Unit] "-" (Unit))
                  "abc-def-ghi")
  (Just (Triple "abc" (Unit) "def-ghi")))
(assert-equal
  (split-at-last [Unit]
                 (exact-string [Unit] "-" (Unit))
                 "abc-def-ghi")
  (Just (Triple "abc-def" (Unit) "ghi")))

(define-forall [A]
               (exact-char expected a)
             : (-> Char A (StringParser A))
  (let ([pred : (-> Char (Maybe A))
        (lambda (actual)
          (if (char=? actual expected)
              (Just a)
              (Nothing)))])
    (matching-char [A] pred)))
(assert-equal
  (split-at-first [Unit]
                  (exact-char [Unit] #\- (Unit))
                  "abc-def-ghi")
  (Just (Triple "abc" (Unit) "def-ghi")))

(define-forall [A B]
               (string-parser-map a2b parser)
             : (-> (-> A B)
                    (StringParser A)
                    (StringParser B))
  (lambda (f)
    (match (parser f)
      [(Just (Pair a x))
       (Just (Pair (a2b a) x))]
      [(Nothing)
       (Nothing)])))
(assert-equal
  (split-at-first [String]
                  (string-parser-map [(Listof Char) String]
                                     list->string
                                     (matching-chars [Char]
                                       (pred->maybe char-numeric?)))
                  "abc123def")
  (Just (Triple "abc" "123" "def")))

(define-forall [A]
               (match-no-strings)
             : (-> (StringParser A))
  (lambda (f)
    (Nothing)))

(define-forall [A]
               (string-parser-or parsers)
             : (-> (Listof (StringParser A))
                    (StringParser A))
  (lambda (f)
    (let loop ([parsers parsers])
      (match parsers
        [(list)
          (Nothing)]
        [(cons parser rest)
          (match (parser f)
            [(Just result)
            (Just result)]
            [(Nothing)
            (loop rest)])]))))
(assert-equal
  (map (lambda ([s : String])
         (split-at-first [Char]
                         (string-parser-or [Char]
                                           (list (exact-char [Char] #\- #\-)
                                                 (exact-char [Char] #\: #\:)))
                         s))
       (list "abc-123:def" "abc:123-def"))
  (list (Just (Triple "abc" #\- "123:def"))
        (Just (Triple "abc" #\: "123-def"))))
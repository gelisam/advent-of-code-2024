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
         string-parser-value
         match-no-strings
         string-parser-or)

(define-type (StringParser A)
  (All [X] (-> (-> Integer (Maybe (Pair String X)))
               (Maybe (Pair A X)))))

(define-forall [A]
               (split-at-first parser str)
             : (-> (StringParser A)
                   String
                   (Maybe (Triple String A String)))
  (let ([n (string-length str)])
    (let loop ([start 0])
      (: f (-> Integer (Maybe (Pair String Integer))))
      (define (f len)
        (let ([end (+ start len)])
          (if (<= end n)
              (Just (Pair (substring str start end) end))
              (Nothing))))
      (match (parser f)
        [(Just (Pair a end))
         (let ([prefix (substring str 0 start)]
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
      (: f (-> Integer (Maybe (Pair String Integer))))
      (define (f len)
        (let ([start (- end len)])
          (if (>= start 0)
              (Just (Pair (substring str start end) start))
              (Nothing))))
      (match (parser f)
        [(Just (Pair a start))
         (let ([prefix (substring str 0 start)]
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
      [(Just (Pair str x))
       (let [(actual (string-ref str 0))]
         (match (pred actual)
           [(Just a)
            (Just (Pair a x))]
           [(Nothing)
            (Nothing)]))]
      [(Nothing)
       (Nothing)])))
(assert-equal
  (split-at-first [Char]
                  (matching-char [Char]
                                 (pred->maybe char-numeric?))
                  "abc123def")
  (Just (Triple "abc" #\1 "23def")))

(: matching-chars (-> (-> Char Boolean)
                      (StringParser String)))
(define (matching-chars pred)
  (lambda #:forall [X] ([f : (-> Integer (Maybe (Pair String X)))])
    (let loop ([n 0]
               [r : (-> (Maybe (Pair String X)))
                (lambda () (Nothing))])
      (let ([next-n (+ n 1)])
        (match (f next-n)
          [(Just (Pair str x))
           ; we don't want to check the entire string, as this would make the
           ; search quadratic. but we don't know whether we're moving forwards or
           ; backwards, so we need to check both ends of the string.
           (let ([first-char (string-ref str 0)]
                 [last-char (string-ref str n)])
             (if (and (pred first-char)
                      (pred last-char))
                 (loop next-n
                       (lambda ()
                         (Just (Pair str x))))
                 (r)))]
          [(Nothing)
           (r)])))))
(assert-equal
  (map (lambda ([s : String])
         (split-at-first [String]
                         (matching-chars char-numeric?)
                         s))
       (list "abc-def" "abc123-456def"))
  (list (Nothing)
        (Just (Triple "abc" "123" "-456def"))))
(assert-equal
  (map (lambda ([s : String])
         (split-at-last [String]
                        (matching-chars char-numeric?)
                        s))
       (list "abc-def" "abc123-456def"))
  (list (Nothing)
        (Just (Triple "abc123-" "456" "def"))))
(assert-equal
  (map (lambda ([s : String])
         (find-first [String]
                     (matching-chars char-numeric?)
                     s))
       (list "abcdef" "abc123-456def"))
  (list (Nothing)
        (Just "123")))
(assert-equal
  (map (lambda ([s : String])
         (find-last [String]
                    (matching-chars char-numeric?)
                    s))
       (list "abcdef" "abc123-456def"))
  (list (Nothing)
        (Just "456")))

(: exact-string (-> String (StringParser String)))
(define (exact-string expected)
  (lambda (f)
    (match (f (string-length expected))
      [(Just (Pair actual x))
       (if (string=? actual expected)
           (Just (Pair actual x))
           (Nothing))]
      [(Nothing)
       (Nothing)])))
(assert-equal
  (split-at-first [String]
                  (exact-string "-")
                  "abc-def-ghi")
  (Just (Triple "abc" "-" "def-ghi")))
(assert-equal
  (split-at-last [String]
                 (exact-string "-")
                 "abc-def-ghi")
  (Just (Triple "abc-def" "-" "ghi")))

(: exact-char (-> Char (StringParser Char)))
(define (exact-char expected)
  (let ([pred : (-> Char (Maybe Char))
        (lambda (actual)
          (if (char=? actual expected)
              (Just actual)
              (Nothing)))])
    (matching-char [Char] pred)))
(assert-equal
  (split-at-first [Char]
                  (exact-char #\-)
                  "abc-def-ghi")
  (Just (Triple "abc" #\- "def-ghi")))

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
  (split-at-first [Integer]
                  (string-parser-map [String Integer]
                                     string-length
                                     (matching-chars char-numeric?))
                  "abc123def")
  (Just (Triple "abc" 3 "def")))

(define-forall [A B]
               (string-parser-value b parser)
             : (-> B
                   (StringParser A)
                   (StringParser B))
  (string-parser-map [A B]
                     (lambda (a) b)
                     parser))
(assert-equal
  (split-at-first [String]
                  (string-parser-value [String String]
                                       "-"
                                       (matching-chars char-numeric?))
                  "abc123def")
  (Just (Triple "abc" "-" "def")))

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
                                           (list (exact-char #\-)
                                                 (exact-char #\:)))
                         s))
       (list "abc-123:def" "abc:123-def"))
  (list (Just (Triple "abc" #\- "123:def"))
        (Just (Triple "abc" #\: "123-def"))))

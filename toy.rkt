#lang typed/racket

(define-syntax (assert-equal stx)
  (syntax-case stx ()
    [(_ x y)
     #`(unless (equal? x y)
         (error 'assert
           (format "line ~a: expected\n~a\ngot\n~a"
             #,(syntax-line stx)
             y
             x)))]))

;;;;;;

(struct Unit () #:transparent)
(struct [A] Identity ([first : A]) #:transparent)
(struct [A B] Pair ([first : A] [second : B]) #:transparent)
(struct [A B C] Triple ([first : A] [second : B] [third : C]) #:transparent)

(define-type (Maybe A) (U Nothing (Just A)))
(struct Nothing () #:transparent)
(struct [A] Just ([value : A]) #:transparent)

(: pred->maybe
   (All [A] (-> (-> A Boolean)
                (-> A (Maybe A)))))
(define (pred->maybe pred)
  (lambda (a)
    (if (pred a)
        (Just a)
        (Nothing))))

;;;;;;

(define-syntax (define-forall stx)
  (syntax-case stx ()
    [(_ [A ...] T (name args ...) body ...)
     #`(begin
         (: tmp-name (All [A ...] T))
         (define (tmp-name args ...)
           body ...)
         (define-syntax (name stx2)
           (syntax-case stx2 ()
             [(_ [A2 (... ...)] args2 (... ...))
              #`((inst tmp-name A2 (... ...)) args2 (... ...))])))]))

;;;;;;

(define-type (StringParser A)
  (All [X] (-> (-> Integer (Pair String X))
               (Maybe (Pair A X)))))

(define-forall [A] (-> (StringParser A)
                       String
                       (Maybe (Triple String A String)))
               (split-at-first parser str)
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

(define-forall [A] (-> (StringParser A)
                        String
                        (Maybe (Triple String A String)))
               (split-at-last parser str)
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

(define-forall [A] (-> (StringParser A)
                        String
                        (Maybe A))
               (find-first (parser : (StringParser A)) str)
  (match (split-at-first [A] parser str)
    [(Just (Triple _ a _))
     (Just a)]
    [(Nothing)
     (Nothing)]))

(define-forall [A] (-> (StringParser A)
                       String
                       (Maybe A))
               (find-last parser str)
  (match (split-at-last [A] parser str)
    [(Just (Triple _ a _))
     (Just a)]
    [(Nothing)
     (Nothing)]))

(define-forall [A] (-> (-> Char (Maybe A))
                       (StringParser A))
               (matching-char pred)
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

(define-forall [A] (-> (-> Char (Maybe A))
                        (StringParser (Listof A)))
               (matching-chars pred)
  (lambda #:forall [X] ((f : (-> Integer (Pair String X))))
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

(define-forall [A] (-> String A (StringParser A))
               (exact-string expected a)
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

(define-forall [A] (-> Char A (StringParser A))
               (exact-char expected a)
  (let ([pred : (-> Char (Maybe A))
        (lambda (actual)
          (if (char=? actual expected)
              (Just a)
              (Nothing)))])
    (lambda (f)
      ((matching-char [A] pred) f))))
(assert-equal
  (split-at-first [Unit]
                  (exact-char [Unit] #\- (Unit))
                  "abc-def-ghi")
  (Just (Triple "abc" (Unit) "def-ghi")))

(define-forall [A B] (-> (-> A B)
                          (StringParser A)
                          (StringParser B))
               (string-matcher-map a2b parser)
  (lambda (f)
    (match (parser f)
      [(Just (Pair a x))
       (Just (Pair (a2b a) x))]
      [(Nothing)
       (Nothing)])))
(assert-equal
  (split-at-first [String]
                  (string-matcher-map [(Listof Char) String]
                                      list->string
                                      (matching-chars [Char]
                                        (pred->maybe char-numeric?)))
                  "abc123def")
  (Just (Triple "abc" "123" "def")))

(define-forall [A] (-> (StringParser A))
               (match-no-strings)
  (lambda (f)
    (Nothing)))

(define-forall [A] (-> (Listof (StringParser A))
                        (StringParser A))
               (string-matcher-or matchers)
  (lambda (f)
    (let loop ([matchers matchers])
      (match matchers
        [(list)
          (Nothing)]
        [(cons matcher rest)
          (match (matcher f)
            [(Just result)
            (Just result)]
            [(Nothing)
            (loop rest)])]))))
(assert-equal
  (split-at-first [Char]
                  (string-matcher-or [Char]
                                     (list (exact-char [Char] #\- #\-)
                                           (exact-char [Char] #\: #\:)))
                  "abc-123:def")
  (Just (Triple "abc" #\- "123:def")))
(assert-equal
  (split-at-first [Char]
                  (string-matcher-or [Char]
                                     (list (exact-char [Char] #\- #\-)
                                           (exact-char [Char] #\: #\:)))
                  "abc:123-def")
  (Just (Triple "abc" #\: "123-def")))

;;;;;;;

(: input (Listof String))
(define input (file->lines "example-input.txt"))

(: string-first-char (-> String Char))
(define (string-first-char str)
  (string-ref str 0))

(: string-last-char (-> String Char))
(define (string-last-char str)
  (string-ref str (sub1 (string-length str))))

(: string-drop-from-beginning (-> String Integer String))
(define (string-drop-from-beginning str n)
  (substring str n))

(: string-drop-from-end (-> String Integer String))
(define (string-drop-from-end str n)
  (substring str 0 (- (string-length str) n)))

(: string-drop-prefix (-> String String String))
(define (string-drop-prefix str prefix)
  (string-drop-from-beginning str (string-length prefix)))

(: string-drop-suffix (-> String String String))
(define (string-drop-suffix str suffix)
  (string-drop-from-end str (string-length suffix)))

(define-syntax (find-numeric-char stx)
  (syntax-case stx ()
    [(_ recur str next-equal? current-char drop-current-char)
     (with-syntax ([(cases ...)
                    (for/list ([num (in-list '("one" "two" "three" "four" "five" "six" "seven" "eight" "nine"))]
                               [char (in-list '(#\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))])
                      #`[(next-equal? str #,num) #,char])])
       #'(cond
           cases ...
           [(char-numeric? current-char) current-char]
           [else (recur (drop-current-char str 1))]))]))

(: first-numeric-char (-> String Char))
(define (first-numeric-char str)
  (find-numeric-char
    first-numeric-char
    str
    string-prefix?
    (string-first-char str)
    string-drop-from-beginning))

(: last-numeric-char (-> String Char))
(define (last-numeric-char str)
  (find-numeric-char
    last-numeric-char
    str
    string-suffix?
    (string-last-char str)
    string-drop-from-end))

(: get-integer (-> String Integer))
(define (get-integer str)
  (: relevant-digits (Listof Char))
  (define relevant-digits
    (list (first-numeric-char str)
          (last-numeric-char str)))
  
  (define number
    (string->number
      (list->string
        relevant-digits)))
  
  (assert number exact-integer?))

(: sum (-> (Listof Integer) Integer))
(define (sum lst)
  (apply + lst))

(: output Integer)
(define output (sum (map get-integer input)))

(displayln output)
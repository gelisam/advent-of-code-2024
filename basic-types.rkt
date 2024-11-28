#lang typed/racket

(provide Unit
         Identity
         Pair
         Triple
         Maybe Nothing Just
         pred->maybe)

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
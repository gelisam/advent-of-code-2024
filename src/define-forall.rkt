#lang typed/racket

(provide define-forall)

(define-syntax (define-forall stx)
  (syntax-case stx (:)
    [(_ (name [A ...] args ...) : T body ...)
     #`(begin
         (: tmp-name (All [A ...] T))
         (define (tmp-name args ...)
           body ...)
         (define-syntax (name stx2)
           (syntax-case stx2 ()
             [(_ [A2 (... ...)] args2 (... ...))
              #`((inst tmp-name A2 (... ...)) args2 (... ...))])))]))
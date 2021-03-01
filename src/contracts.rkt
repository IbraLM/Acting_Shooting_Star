#lang racket
(require rackunit)
(require rackunit/text-ui)
(require racket/contract)
(require "structures.rkt")
(require "actor.rkt")
(require "runtime.rkt")

(define (location? p)
  (list? p))

(define (state? s)
  (string? s))

(define (mailbox? m)
  (list? m))

(define (actor? a)
  (and (and (state? (actor-state a)) (mailbox? (actor-mailbox a))) (location? (actor-location a))))

(define (etiquette? e)
  (string? e))

(define (coord? c)
  (list? c))

(define (msg? m)
  (and (string? (msg-etiquette m)) (coord? (msg-coord m))))

(define (actors-list? l)
  (if (empty? l)
      #t
      (and (actor? (car l)) (actors-list? (cdr l)))))

(define (world? w)
  (and (and (actors-list? (world-actors)))))

(define (actors? a)
  (list? a))


(provide (contract-out
          ;[actor? (-> string?)]
          ;[world? (-> list?)]
          [actor-state (-> actor state?)]
          [actor-location (-> actor? location?)]
          [actor-send (-> actor? msg? actor?)]
          [actor-update (-> actor? actor?)]
          [create-missile (-> actor? actor?)]
          [actor-collision? (-> actor? actor? boolean?)]
          ;[play-tick (-> msg? world? world?)]
          ;[update-tick (-> world? world?)]
          ;[act (-> world? actor?)]
          [remove-first (-> actor? list? list?)]
          ;[remove-list (-> list? list? list?)]
          [collision? (-> actor? actor? boolean?)]
          [collision1 (-> actors? actor? actor? actors?)]
          [collision-world (-> actors? actor? actors?)]
          ;[intersection (-> list? list? list?)]
          [collision2 (-> actors? actors?)]))

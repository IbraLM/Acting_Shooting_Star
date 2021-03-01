#lang racket

(require rackunit)
(require rackunit/text-ui)
(require "structures.rkt")
(require "actor.rkt")
(require "runtime.rkt")
(require
  (prefix-in ct: charterm)
  (prefix-in lux: lux)
  (prefix-in raart: raart))



(define (start-application)
  (lux:call-with-chaos
   (raart:make-raart)
   (lambda () (lux:fiat-lux menu)))
  (void))

(start-application)

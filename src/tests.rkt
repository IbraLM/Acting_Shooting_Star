#lang racket
(require rackunit)
(require rackunit/text-ui)
(require "structures.rkt")
;(require "runtime.rkt")
(require "actor.rkt")

(struct world (actors states depth tick))

(define actora (actor "a" '() '(0 0)))
(define actorb (actor "b" '() '(0 0)))
(define actorc (actor "c" '() '(0 0)))
(define actord (actor "d" '() '(1 2)))
(define actore (actor "e" '() '(-1 0)))
(define actorf (actor "f" '() '(3 7)))
(define msg1 (msg "move" '(1 3) "everyone")) 
(define msg2 (msg "move" '(0 0) "everyone"))
(define msg3 (msg "move" '(-1 -1) "everyone"))
(define world1 (world (list actora actorb actorc) '() 0 1))

(define (egaliteListes L1 L2)
  (if (pair? L1)
      (if (= (car L1) (car L2))
      (egaliteListes (cdr L1) (cdr L2))
      #f)
  #t))

(define (collision? actor1 actor2)
  (and (or (eq? (car (actor-location actor1)) (+ 1 (car (actor-location actor2)))) (eq? (car (actor-location actor1)) (+ 2 (car (actor-location actor2)))))
  (eq? (cadr (actor-location actor1)) (cadr (actor-location actor2)))
  (or (or (equal? (actor-state actor1) "missile") (equal? (actor-state actor2) "missile")) (and (equal? (actor-state actor2) "missile-wall") (equal? (actor-state actor1) "wall")) (and (equal? (actor-state actor1) "shooter") (or (equal? (actor-state actor2) "wall") (equal? (actor-state actor2) "enemy") (equal? (actor-state actor2) "missile-wall"))))))

(define (remove-first s L)
  (cond
    [(null? L) '()]
    [(equal? (car L) s) (cdr L)]
    [else (cons (car L) (remove-first s (cdr L)))])) 

(define (collision1 actors actor1 actor2)
  (cond
    [(null? actors) actors]
    [(and (collision? actor1 actor2) )
    (remove-first actor2 actors)]
    [else actors]))

(define all-tests
  (test-suite
   "Test_for_structures_implementations"
   (test-case
    "Test_send&update1"
    (let* ([actor1 (actor "shooter" '() '(3 2))])
      (check-true (egaliteListes (actor-location (caar (actor-update (actor-send actor1 msg1)))) '(4 5)))))
   (test-case
    "Test_send&update2"
    (let* ([actor2 (actor "shooter" '() '(1 0))])
      (check-true (egaliteListes (actor-location (caar (actor-update (actor-send actor2 msg2)))) '(1 0)))))
   (test-case
    "Test_send&update3"
    (let* ([actor3 (actor "shooter" '() '(0 0))])
      (check-true (egaliteListes (actor-location (caar (actor-update (actor-send actor3 msg3)))) '(-1 -1)))))
   (test-case
    "Test_create-missile"
    (let* ([actor4 (actor "shooter" '() '(4 1))])
      (check-true (and (string=? (actor-state (create-missile actor4)) "missile") (egaliteListes (actor-mailbox (create-missile actor4)) '()) )
                  (egaliteListes (actor-location (create-missile actor4)) (actor-location actor4)) )))
   (test-case
    "Test_create-missile2"
    (let* ([msg_missile msg1] [actor5 (actor "shooter" '(msg_missile) '(4 1))] )
      (check-true (and (string=? (actor-state (create-missile actor5)) "missile") (egaliteListes (actor-mailbox (create-missile actor5)) '()))
                         (egaliteListes (actor-location (create-missile actor5)) (actor-location actor5)))))
   (test-case
    "Test_create-missile3"
    (let* ([msg_missile msg1] [actor5 (actor "shooter" '(msg_missile) '(-1 -3))] )
      (check-true (and (string=? (actor-state (create-missile actor5)) "missile") (egaliteListes (actor-mailbox (create-missile actor5)) '()))
                         (egaliteListes (actor-location (create-missile actor5)) (actor-location actor5)))))
   (test-case
    "Test_actor-collision?1"
    (let* ([actor6 (actor "shooter" '() '(0 0))] [actor7 (actor "shooter" '() '(0 0))])
    (check-true (actor-collision? actor6 actor7))))
   (test-case
    "Test_actor-collision?2"
    (let* ([actor6 (actor "shooter" '() '(-1 3))] [actor7 (actor "shooter" '() '(-1 3))])
    (check-true (actor-collision? actor6 actor7))))
   (test-case
    "Test_actor-collision?3"
    (let* ([actor8 (actor "shooter" '() '(3 2))] [actor9 (actor "shooter" '() '(1 0))])
    (check-false (actor-collision? actor8 actor9))))
   (test-case
    "Remove-First1"
    (check-true (egaliteListes (remove-first 1 '(1)) '())))
   (test-case
    "Remove-First2"
    (check-true (egaliteListes (remove-first 4 '(1 2 2 4 5)) '(1 2 2 5))))
   (test-case
    "Remove-First3"
    (check-true (egaliteListes (remove-first 1 '(2)) '(2))))
   (test-case
    "Remove-First4"
    (check-true (egaliteListes (remove-first 1 '()) '())))
   (test-case
    "collision1.1"
    (check-true (egaliteListes(car (collision1 (list actora actorb actorc) actora actorb)) (list actorc))))
   (test-case
    "collision1.2"
    (check-true (egaliteListes(car (collision1 (list actora actorb actora) actora actorb)) '())))
   (test-case
    "collision1.3"
    (check-true (egaliteListes(car (collision1 (list actord actore actorf) actore actorf)) (list actord actore actorf))))))
   (printf "Running_tests\n")
   (run-tests all-tests)

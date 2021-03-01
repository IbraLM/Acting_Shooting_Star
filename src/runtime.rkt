#lang racket

(provide world-send world-sendmsgs update-world remove-first collision? collision1 collision-world collision2 new-actors wrd world menu world-actors world-states world-tick world-depth)
(require "structures.rkt")
(require "actor.rkt")
(require 2htdp/universe)
(require
  (prefix-in ct: charterm)
  (prefix-in lux: lux)
  (prefix-in raart: raart))



;;;;;;;;;;;;;;FONCTIONS POUR MANIPULER LES ACTEURS DANS UN WORLD;;;;;;;;;;;;;;


;cette fonction envoie un msg à tous les acteurs de wrd
(define (world-send msg wrd)
  (letrec([new-world(lambda(msg l)
                      (cond
                        [(null? l) '()]
                        [else (cons (actor-send (car l) msg)
                                    (new-world msg (cdr l)))]))])
    (world (new-world msg (world-actors wrd)) (world-states wrd) (world-depth wrd) (world-tick wrd))))


;cette fonction renvoie une liste des messages 'mssgs' aux acteurs de world
(define (world-sendmsgs msgs wrd)
(cond
  [(empty? msgs) wrd]
  [else (foldl world-send wrd msgs)]))


;cette fonction met à jour les acteurs de wrd
(define (update-world wrd)
  (letrec([new-world(lambda(l)
                      (cond
                        [(null? l) '()]
                        [else (append (car(actor-update (car l)))
                                    (new-world (cdr l)))]))])
    (world (new-world (world-actors wrd)) (world-states wrd) (world-depth wrd) (world-tick wrd))))




;;;;;;;;;;;;;;FONCTIONS POUR GERER LA COLLISION ENTRE LES ACTEURS;;;;;;;;;;;;;;


; supprime un élément s de la liste L (fonction auxiliaire)
(define (remove-first s L)
  (cond
    [(null? L) '()] ; si la liste est vide, c’est la liste vide.
    [(equal? (car L) s) (cdr L)] ;sinon, si c’est l’élément qu’on cherche alors c’est la suite de la liste
    [else (cons (car L) (remove-first s (cdr L)))])) ; si ce n’est pas l’élément qu’on cherche, on continue



;cette fonction permet de savoir si une missile est en collision avec un enemi
(define (collision? actor1 actor2)
  (and (or (eq? (car (actor-location actor1)) (+ 1 (car (actor-location actor2)))) (eq? (car (actor-location actor1)) (+ 2 (car (actor-location actor2)))))
  (eq? (cadr (actor-location actor1)) (cadr (actor-location actor2)))
  (or (or (equal? (actor-state actor1) "missile") (equal? (actor-state actor2) "missile")) (and (equal? (actor-state actor2) "missile-wall") (equal? (actor-state actor1) "wall")) (and (equal? (actor-state actor1) "shooter") (or (equal? (actor-state actor2) "wall") (equal? (actor-state actor2) "enemy") (equal? (actor-state actor2) "missile-wall"))))))


; supprime actor1 et actor2 de world en cas de collision
(define (collision1 actors actor1 actor2)
  (cond
    [(null? actors) actors]
    [(and (collision? actor1 actor2) )
    (remove-first actor2 actors)]
    [else actors]))


; supprime tous les éléments qui sont en collision avec 'actor'
(define (collision-world actors actor)
  (cond
    [(empty? actors)  actors]
    [(and (empty? (cdr actors)) (collision? actor (car actors))) (list)]
    [else (append (list (car (collision1 actors actor (car actors)))) (collision-world (cdr (collision1 actors actor (car actors))) actor))]))


;cette fonction prends une liste des acteurs et supprime toutes les acteurs qui
;sont en collisions, c'est la fonction principale pour la gestion de collision
(define (collision2 actors)
  (cond
    [(null? actors) null]
    [(and (equal? (length (collision-world (cdr actors) (car actors))) (length (cdr actors))) (equal? (length (cdr actors)) 1)) actors]
    [(and (equal? (length (collision-world (cdr actors) (car actors))) (length (cdr actors))) (> (length (cdr actors)) 1)) (append (list (car actors)) (collision2 (cdr actors)))]
    [(empty? (collision-world (cdr actors) (car actors))) (list)]
    [else (collision2 (collision-world (cdr actors) (car actors)))]))




;;;;;;;;;;;;;;FONCTIONS POUR GERER LA COLLISION AVEC LES BORDURES;;;;;;;;;;;;;;


(define (bordures1 actor)
  (cond
  [(or (equal? (car (actor-location actor)) 0) (equal? (car (actor-location actor)) 1) (equal? (car (actor-location actor)) 90))
    (list)]
  [else (list actor)]))

(define (bordures actors)
  (cond
  [(empty? actors) null]
  [else (append (bordures1 (car actors)) (bordures (cdr actors)))]))




;;;;;;;;;;;;;;(ACTEURS MODIFIES + NOUVEAUX ACTEURS) APRES 1 TICK;;;;;;;;;;;;;;


;cette fonctions renvoie les ecteurs modifies + les nouveaux crees
;apres une tick
(define (final-update w)
  (collision2 (bordures (append (list (caar (actor-update (car(world-actors w)))))
             (append (cdar (actor-update (car(world-actors w))))
              (world-actors (update-world (world-sendmsgs (append (list move-left) (cadr (actor-update (car(world-actors w)))))  (world (cdr (world-actors w)) (world-states w) (world-depth w) (world-tick w))))))))))




;;;;;;;;;;;;;;FONCTION POUR ANNONCER LA FIN DU JEU;;;;;;;;;;;;;;


;cette fonction declare la fin du jeu si notre joueur principale est mort
(define (game-over actors)
  (cond
  [(not (equal? (actor-state (car actors)) "shooter")) (list (actor "game-over" '() (list 35 10)))]
  [else actors]))




;;;;;;;;;;;;;;FONCTIONS POUR GERER L'AFFICHAGE;;;;;;;;;;;;;;


;avoir les dimensions
(define-values (term-cols term-rows)
 (ct:with-charterm (ct:charterm-screen-size)))

;cette fonction affiche tous les acteurs dur world 'wrd'
(define (display-world wrd)
  (display-position (world-actors wrd) wrd))

;cette fonction affiche les acteurs de notre world
;selon leurs positions et leurs types
(define (display-position actors wrd [width 90] [height 20])
  (raart:matte term-cols term-rows
     (foldr (lambda (l back) (raart:place-at back (car l) (cadr l) (caddr l)))
            (raart:frame (raart:matte width height #:halign 'left #:valign 'top (raart:blank)))
	    ;(raart:matte 50 10 (raart:text "1")))
            (map (lambda (act)
                  (list (cadr (actor-location act))
                      (car (actor-location act))
                      (cond
                      [(equal? (actor-state act) "shooter") (raart:fg'blue (raart:text ">"))]
                      [(equal? (actor-state act) "wall") (raart:fg'red (raart:text "||"))]
                      [(equal? (actor-state act) "enemy") (raart:fg'red (raart:text "<"))]
                      [(equal? (actor-state act) "missile") (raart:fg 'yellow (raart:text "-"))]
                      [(equal? (actor-state act) "missile-wall") (raart:fg 'yellow (raart:text "*"))]
                      [(equal? (actor-state act) "score") (raart:frame(raart:text(~a "score" (world-tick wrd))))]
                      [(equal? (actor-state act) "title") (raart:fg 'yellow (raart:text  "*--* SHOOT'EM UP *--*" ))]
                      [(equal? (actor-state act) "travel") (raart:frame(raart:fg 'yellow (raart:text  "TIME TRAVEL" )))]
                      [(equal? (actor-state act) "game-over") (raart:frame (raart:vappend(raart:fg 'red (raart:text "GAME OVER !")) (raart:fg 'red (raart:text(~a "Your score is:  " (world-tick wrd)))) #:halign 'center))]
                      [(equal? (actor-state act) "menu") (raart:frame (raart:vappend(raart:fg 'red (raart:text "                     ~/~/~/~/MENU/~/~/~/~"))
                                                                        (raart:text "")
                                                                        (raart:fg 'yellow (raart:text "                     $$ Commandes du jeu: $$ "))
                                                                        (raart:text "      ** q, s, d, z : mouvements (gauche, bas, droite, haut)  ") 
                                                                        (raart:text "      ** a : pause et remonter dans le temps")
                                                                        (raart:text "      ** e : continuer le jeu ")
                                                                        (raart:text "      ** 0 : lancer des missiles ")
                                                                        (raart:text "      ** x : quitter le jeu")
                                                                        (raart:text "")
                                                                        (raart:fg 'red (raart:text "                      TAPEZ L POUR COMMENCER")) #:halign 'left))])))
                actors))))




;;;;;;;;;;;;;;AFFICHAGE AVEC LUX;;;;;;;;;;;;;;


;la structure world (elle contient une liste des acteurs une liste des 8 états précédents et la profondeur à laquelle on est revenu dans le passé et un tick)
(struct world (actors states depth tick)
  #:methods lux:gen:word
  [(define (word-fps w) ;;FPS desired rate
     7.0)

   (define (word-label s ft)
     "Shoot'em up")

   (define (word-event w e)
     (match e
       ["x" #f];;quit the game
       ["q" (world (cons (actor-send  (car (world-actors w)) move-left) (cdr (world-actors w))) (world-states w) (world-depth w) (world-tick w))]
       ["d" (world (cons (actor-send  (car (world-actors w)) move-right) (cdr (world-actors w))) (world-states w) (world-depth w) (world-tick w))]
       ["z" (world (cons (actor-send  (car (world-actors w)) move-up) (cdr (world-actors w))) (world-states w) (world-depth w) (world-tick w))]
       ["s" (world (cons (actor-send  (car (world-actors w)) move-down) (cdr (world-actors w))) (world-states w) (world-depth w) (world-tick w))]
       ["0" (world (cons (actor-send  (car (world-actors w)) new-actors) (cdr (world-actors w))) (world-states w) (world-depth w) (world-tick w))]
       ["a" (world (append (world-actors (n-ieme (world-states w) (world-depth w))) (list travel)) (world-states w) (sub1 (world-depth w)) (world-tick  w))]
       ["e" (world (remove-first travel (world-actors w)) (world-states w) 8 (world-tick w))]
       ["l" (world (world-actors (car (world-states w))) (world-states (car (world-states w))) (world-depth (car (world-states w))) (world-tick (car (world-states w))))]
       [_ w]
       ))

   (define (word-output w)
     (display-world w))


  ;dans cette fonction on met à jour nos acteurs séparément
  ;l'acteur principale(1er acteur) et les autres acteurs du world
  ;apres cette mise à jour en supprime les acteurs en collision
  ;et on construit un nouveau world avec des acteurs mis à jour et un tick
  ;incrémenté de 1
   (define (word-tick w)
    (cond
    [(equal? (world-tick w) 0) w]
    [(equal? (actor-state (car (world-actors w))) "game-over") (world (world-actors w) (append (cdr (world-states w)) (list w)) (world-depth w) (world-tick w))]
    [(and (> (world-depth w) 0) (<= (world-depth w) 7)) (world (world-actors w) (world-states w) (world-depth w) (+ 1 (world-tick w)))]
    [(and (equal? (world-depth w) 0) (world (world-actors w) (world-states w) 8 (+ 1 (world-tick w))))]
    [(equal? (modulo (world-tick w) 29) 0) (world (game-over (append (final-update w) actors1)) (append (cdr (world-states w)) (list w)) (world-depth w) (+ 1 (world-tick w)))]
    [(equal? (modulo (world-tick w) 53) 0) (world (game-over (append (final-update w) actors21)) (append (cdr (world-states w)) (list w)) (world-depth w) (+ 1 (world-tick w)))]
    [(equal? (modulo (world-tick w) 23) 0) (world (game-over (append (final-update w) actors2)) (append (cdr (world-states w)) (list w)) (world-depth w) (+ 1 (world-tick w)))]
    [(equal? (modulo (world-tick w) 74) 0) (world (game-over (append (final-update w) actors22)) (append (cdr (world-states w)) (list w)) (world-depth w) (+ 1 (world-tick w)))]
    [(equal? (modulo (world-tick w) 100) 0) (world (game-over (append (final-update w) actors3)) (append (cdr (world-states w)) (list w)) (world-depth w) (+ 1 (world-tick w)))]
    [(equal? (modulo (world-tick w) 49) 0) (world (game-over (append (final-update w) actors33)) (append (cdr (world-states w)) (list w)) (world-depth w) (+ 1 (world-tick w)))]
    [(equal? (modulo (world-tick w) 25) 0) (world-send new-actors2 (world (game-over (final-update w)) (append (cdr (world-states w)) (list w)) (world-depth w) (+ 1 (world-tick w))))]
    [else (world (game-over (final-update w)) (append (cdr (world-states w)) (list w)) (world-depth w) 
      (+ 1 (world-tick w)))]))
   ])




;;;;;;;;;;;;;;FONCTIONS AUXILIAIRES ET VARIABLES GLOBALES UTILES;;;;;;;;;;;;;;


;ces fonctions vont etre utilisées comme evenements
;pour controler notre acteur principale "chef d'orchestre"
(define move-left
  (msg "move" '(-1 0) "everyone"))
(define move-right
  (msg "move" '(1 0) "everyone"))
(define move-up
  (msg "move" '(0 -1) "everyone"))
(define move-down
  (msg "move" '(0 1) "everyone"))
(define new-actors
  (msg "new actors" '() "shooter"))

(define new-actors2
  (msg "new actors" '() "enemy"))

(define (n-ieme l n)
  (cond
    [(equal? 1 n) (car l)]
    [else (n-ieme (cdr l) (sub1 n))]))


(define actor1 (actor "shooter" '() '(27 12)))
(define actor3 (actor "wall" '() '(89 9)))
(define actor4 (actor "wall" '() '(89 10)))
(define actor5 (actor "wall" '() '(89 11)))
(define actor6 (actor "wall" '() '(89 8)))
(define actor7 (actor "enemy" '() '(89 7)))
(define actor31 (actor "wall" '() '(85 9)))
(define actor41 (actor "wall" '() '(87 10)))
(define actor51 (actor "enemy" '() '(89 11)))
(define actor61 (actor "enemy" '() '(87 8)))
(define actor231 (actor "enemy" '() '(85 9)))
(define actor241 (actor "wall" '() '(87 10)))
(define actor251 (actor "wall" '() '(89 11)))
(define actor261 (actor "wall" '() '(87 8)))
(define actor271 (actor "enemy" '() '(89 7)))
(define actor71 (actor "wall" '() '(89 7)))
(define actor32 (actor "wall" '() '(89 2)))
(define actor42 (actor "enemy" '() '(89 3)))
(define actor52 (actor "enemy" '() '(89 4)))
(define actor62 (actor "wall" '() '(89 5)))
(define actor232 (actor "enemy" '() '(89 2)))
(define actor242 (actor "wall" '() '(89 3)))
(define actor252 (actor "wall" '() '(89 4)))
(define actor262 (actor "enemy" '() '(89 5)))
(define actor33 (actor "wall" '() '(89 16)))
(define actor43 (actor "enemy" '() '(89 17)))
(define actor53 (actor "wall" '() '(89 18)))
(define actor63 (actor "enemy" '() '(89 19)))
(define actor333 (actor "enemy" '() '(89 16)))
(define actor343 (actor "wall" '() '(89 17)))
(define actor353 (actor "enemy" '() '(89 18)))
(define actor363 (actor "wall" '() '(89 19)))
(define actorm (actor "menu" '() (list 15 5)))
(define score (actor "score" '() (list 5 22)))
(define title (actor "title" '() (list 35 22)))
(define travel (actor "travel" '() (list 65 22)))
(define init-world (world (list actor1  actor4 actor5 actor6 score title) '() 0 1))
(define wrd (world (list actor1  actor4 actor5 actor6 score title) (list init-world init-world init-world init-world init-world init-world init-world init-world ) 8 1))
(define menu (world (list actorm) (list wrd) 0 0))

(define actors1 (list actor31 actor41 actor51 actor61 actor71))
(define actors21 (list actor231 actor241 actor251 actor261 actor271))
(define actors2 (list actor32 actor42 actor52 actor62))
(define actors22 (list actor232 actor242 actor252 actor262))
(define actors33 (list actor333 actor343 actor353 actor363))
(define actors3 (list actor33 actor43 actor53 actor63))

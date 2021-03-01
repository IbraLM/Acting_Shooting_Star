#lang racket

(provide actor msg actor-state actor-mailbox actor-location msg-etiquette msg-coord )
(provide (struct-out actor))
(provide (struct-out msg))
;(provide (struct-out world))

(struct actor (state mailbox location))

(struct msg (etiquette coord dest))

;(struct world (actors states depth tick))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;+
;   NAME:
;      RANDOMCHI
; PURPOSE:
;      GENERATE CHI-SQUARE DISTRIBUTED RANDOM VARIABLES.
;
; AUTHOR : BRANDON C. KELLY, STEWARD OBS., SEP 2005
;
; INPUTS :
;
;   SEED - THE SEED FOR THE RANDOM NUMBER GENERATOR, CAN BE UNDEFINED.
;   DOF - THE DEGREES OF FREEDOM FOR THE CHI-SQUARED DISTRIBUTION.
;
; OPTIONAL INPUTS :
;
;   NRAND - THE NUMBER OF RANDOM NUMBERS TO DRAW
;-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

function randomchi, seed, dof, nrand

if n_params() lt 2 then begin
    print, 'Syntax- result = randomchi( seed, dof[, nrand] )'
    return, -1
endif

if n_elements(nrand) eq 0 then nrand = 1

alpha = dof / 2.0
beta = 0.5

chisqr = randomgam( seed, alpha, beta, nrand )

return, chisqr
end

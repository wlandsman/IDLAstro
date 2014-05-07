;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;+
;  NAME:
;     RANDOMDIR
; PURPOSE:
;     GENERATE DIRICHLET-DISTRIBUTED RANDOM VARIABLES.
;
; AUTHOR : BRANDON C. KELLY, STEWARD OBS., APRIL 2006
;
; INPUTS :
;
;   SEED - THE SEED FOR THE RANDOM NUMBER GENERATOR, CAN BE UNDEFINED.
;   ALPHA - THE SHAPE PARAMETERS FOR THE DIRICHLET DISTRIBUTION. THIS
;           SHOULD BE A K-ELEMENT VECTOR.
;
; OPTIONAL INPUTS :
;
;   NRAND - THE NUMBER OF RANDOM NUMBERS TO DRAW
;
; CALLED ROUTINES :
;
;   RANDOMGAM
;-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

function randomdir, seed, alpha, nrand

if n_params() lt 2 then begin
    print, 'Syntax- theta = randomdir( seed, alpha[, nrand] )'
    return, 0
endif

if n_elements(alpha) lt 2 then begin
    print, 'Alpha must have at least 2 elements.'
    return, 0
endif

K = n_elements(alpha)

bad = where(alpha le 0, nbad)
if nbad ne 0 then begin
    print, 'All elements of ALPHA must be greater than 0.'
    return, 0
endif

if n_elements(nrand) eq 0 then nrand = 1

gamma = dblarr(nrand, K)

for j = 0, K - 1 do $
  gamma[0,j] = randomgam(seed, alpha[j], 1.0, nrand)

theta = gamma / transpose(total(gamma,2) ## replicate(1, K))

return, theta
end

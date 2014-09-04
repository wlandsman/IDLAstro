;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;+
; NAME:
;     RANDOMGAM
; PURPOSE:
;     GENERATE GAMMA-DISTRIBUTED RANDOM VARIABLES.
;
; AUTHOR : BRANDON C. KELLY, STEWARD OBS., APRIL 2006
;
; INPUTS :
;
;   SEED - THE SEED FOR THE RANDOM NUMBER GENERATOR, CAN BE UNDEFINED.
;   ALPHA, BETA - THE SHAPE PARAMETERS FOR THE GAMMA DISTRIBUTION.
;
; OPTIONAL INPUTS :
;
;   NRAND - THE NUMBER OF RANDOM NUMBERS TO DRAW
;-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

function randomgam, seed, alpha, beta, nrand

if n_params() lt 3 then begin
    print, 'Syntax- X = randomgam( seed, alpha, beta[, nrand] )'
    return, 0
endif

if alpha le 0 or beta le 0 then begin
    print, 'ALPHA and BETA must both be greater than zero.'
    return, 0
endif

if n_elements(nrand) eq 0 then nrand = 1

if alpha le 1 then begin

    alpha = alpha + 1
    alfshift = 1

endif else alfshift = 0

d = alpha - 1d / 3
c = 1 / sqrt(9 * d)

gamma = dblarr(nrand)

nempty = nrand
empty = lindgen(nrand)

repeat begin
    
    x = randomn(seed, nempty)
    v = 1 + c * x
    
    bad = where(v le 0, nbad)
    while nbad gt 0 do begin
        
        x2 = randomn(seed, nbad)
        x[bad] = x2
        v[bad] = 1 + c * x2
        bad2 = where(v[bad] le 0, nbad2)
        if nbad2 gt 0 then bad = bad[bad2]
        nbad = bad2
        
    endwhile
    
    v = v^3

    unif = randomu(seed, nempty)
    factor = 0.5 * x^2 + d - d * v + d * alog(v)
    u = where( alog(unif) lt factor, nu, comp=empty1 )

    if nu gt 0 then gamma[empty[u]] = d * v[u]
    nempty = nempty - nu

    if nempty ne 0 then empty = empty[empty1]

endrep until nempty eq 0

if alfshift then begin
    alpha = alpha - 1
    gamma = gamma * (randomu(seed, nrand))^(1d / alpha)
endif

gamma = gamma / beta

return, gamma
end

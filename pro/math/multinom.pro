;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;+
; NAME:
;   MULTINOM
; PURPOSE:
; SIMULATE MULTINOMIAL RANDOM VARIABLES
;
; AUTHOR : BRANDON C. KELLY, STEWARD OBS., APR 2006
;
; INPUTS :
;
;   N - THE NUMBER OF TRIALS
;   P - A K-ELEMENT VECTOR CONTAINING THE PROBABILITIES FOR EACH
;       CLASS.
;
; OPTIONAL INPUTS :
;
;   NRAND - THE NUMBER OF RANDOM VARIABLES TO DRAW
;   SEED - THE SEED FOR THE RANDOM NUMBER GENERATOR
;
; OUTPUT :
;   NRAND RANDOM DRAWS FROM A MULTINOMIAL DISTRIBUTION WITH PARAMETERS
;   N AND P.
;-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

function multinom, n, p, nrand, seed=seed

if n_params() lt 2 then begin
    print, 'Syntax- theta = multinom( n, p,[ nrand, seed=seed] )'
    return, 0
endif

k = n_elements(p)

bad = where(p lt 0 or p gt 1, nbad)
if nbad gt 0 then begin
    print, 'All element of p must be 0 <= p <= 1.'
    return, 0
endif

if n lt 1 then begin
    print, 'N must be at least 1.'
    return, 0
endif

if n_elements(nrand) eq 0 then nrand = 1

                                ;check if binomial
if k eq 2 then begin

    binom = randomu(seed, nrand, binomial=[n, p[0]], /double)
    multi = [[binom], [n - binom]]

    return, transpose(multi)

endif

multi = lonarr(k, nrand)

for i = 0L, nrand - 1 do begin
    
    multi[0,i] = randomu(seed, 1, binomial=[n, p[0]], /double)
    j = 1L
    nj = n - total(multi[0:j-1,i])

    while nj gt 0 do begin
        
        pj = p[j] / total(p[j:*])

        multi[j,i] = randomu(seed, 1, binomial=[nj,pj], /double)

        j = j + 1
        nj = n - total(multi[0:j-1,i])

    endwhile

endfor

return, multi
end

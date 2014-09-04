;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;+
; NAME:
;    RANDOMWISH
; PURUPOSE:
; ROUTINE TO DRAW RANDOM MATRICES FROM A WISHART DISTRIBUTION WITH DOF
; DEGREES OF FREEDOM AND SCALE MATRIX S.
;
; AUTHOR : BRANDON C. KELLY, STEWARD OBS., JULY 2006
;
; INPUTS :
;
;   SEED - THE SEED FOR THE RANDOM NUMBER GENERATOR, CAN BE UNDEFINED.
;   DOF - THE DEGREES OF FREEDOM FOR THE WISHART DISTRIBUTION.
;   S - THE SCALE MATRIX. THE DIMENSION OF S CANNOT BE GREATER THAN
;       DOF.
;
; OPTIONAL INPUTS :
;
;   NRAND - THE NUMBER OF RANDOM MATRICES TO DRAW
;
; CALLED ROUTINES :
;
;   MRANDOMN
;-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

function randomwish, seed, dof, S, nrand

if n_params() lt 3 then begin
    print, 'Syntax- W = randomwish( seed, dof, S[, nrand] )'
    return, 0
endif

dim = (size(S, /dim))[0]

if dim gt dof then begin

    print, 'Dimension of S cannot be larger than DOF.'
    return, 0

endif

if n_elements(nrand) eq 0 then nrand = 1

wish = dblarr(dim, dim, nrand)

for i = 0, nrand - 1 do begin

    x = mrandomn(seed, S, dof)
    wish[*,*,i] = x ## transpose(x)

endfor

return, reform(wish)
end

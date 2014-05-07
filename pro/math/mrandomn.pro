function mrandomn, seed, covar, nrand, STATUS = status

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;+
;  NAME:
;     MRANDOMN
; PURPOSE:
; Function to draw NRAND random deviates from a multivariate normal
; distribution with zero mean and covariance matrix COVAR.
; 
; AUTHOR : Brandon C. Kelly, Steward Obs., Sept. 2004
;
; INPUTS : 
;
;    SEED - The random number generator seed, the default is IDL's
;           default in RANDOMN()
;    COVAR - The covariance matrix of the multivariate normal
;            distribution.    
; OPTIONAL INPUTS :
;
;    NRAND - The number of randomn deviates to draw. The default is
;            one.
; OUTPUT :
;
;    The random deviates, an [NRAND, NP] array where NP is the
;    dimension of the covariance matrix, i.e., the number of
;    parameters.
;
; OPTIONAL OUTPUT:
;     STATUS - status of the Cholesky decomposition.   If STATUS = 0 then 
;         the computation was successful.  If STATUS > 0 then the 
;         input covariance matrix is not positive definite  (see LA_CHOLDC),
;         and MRANDOMN
;         Note that if a STATUS keyword is supplied then no error message 
;         will be printed.
; REVISION HISTORY:
;     Oct. 2013  -- Use LA_CHOLDC instead of CHOLDC to enable use of STATUS
;           keyword.    W. Landsman
;-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

if n_params() lt 2 then begin
    print, 'Syntax- Result = mrandomn( seed, covar, [nrand] , STATUS = )'
    return, 0
endif

printerr = ~arg_present(errmsg)
errmsg = '' 


;check inputs and set up defaults
if n_elements(nrand) eq 0 then nrand = 1
if size(covar, /n_dim) ne 2 then begin
    print, 'COVAR must be a matrix.'
    return, 0
endif

np = (size(covar))[1]
if (size(covar))[2] ne np then begin
    print, 'COVAR must be a square matrix.'
    return, 0
endif

epsilon = randomn(seed, nrand, np) ;standard normal random deviates (NP x NRAND matrix)

A = covar  ;store covariance into dummy variable for input into TRIRED

  la_choldc, A, /double, status=status        ;do Cholesky decomposition
  if status NE 0 then begin
     message,'Array is not positive definite, STATUS = ' + strtrim(status,2),/CON 
     return,-1
  endif   

for i = 0, np - 2  do A[i+1:*,i] = 0d        ;Zero out upper triangular portion

;transform standard normal deviates so they have covariance matrix COVAR
epsilon = A ## epsilon

return, epsilon
end

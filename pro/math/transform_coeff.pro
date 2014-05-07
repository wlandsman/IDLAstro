
function transform_coeff, coeff, alpha, beta
;+
; NAME:
;    TRANSFORM_COEFF()
; PURPOSE:
;    Compute new polynomial coefficients under a linear transformation 
; EXPLANATION:
;     Suppose one has a (nonlinear) polynomial (similar to the POLY() function)
;          y = C[0] + C[1]*x  + C[2]*x^2 + C[3]*x^3 + ...
;
;      and one has a linear transformation in X
;
;          x = alpha*x' + beta
;     This function computes the new polynomial coefficients under the linear
;     transformation. 
;
; CALLING SEQUENCE:
;     newcoeff = TRANSFORM_COEFF( coeff, alpha, beta)
; INPUTS:
;     Coeff  -  vector of polynomial coefficients (as with POLY()).    The 
;         degree of the polynomial is N_elements(coeff) - 1
;     Alpha, Beta - numeric scalars defining the linear transformation in X 
; OUTPUTS:
;    NewCoeff - Vector (same size as Coeff) giving the new polynomial 
;               coefficients
; EXAMPLE:
;     Suppose one has polynomial mapping a nonlinear distortion in the X 
;     direction of a spectrum
;
;     y = 0.2 + 1.1*x + 0.1*x^2
;
;     if one rebins the spectrum to half the size then the linear transformation
;     is  x = 2.*x'
;     so alpha = 2 and beta = 0
;     The new coefficients are
;     IDL> print, transform_coeff([0.2,1.1,0.1],2.,0) 
;     ==> [0.2, 2.2, 0.4] 
; METHOD:
;    Performs a binomial expansion of the polynomial and collect like terms
;    groups.google.com/group/comp.lang.idl-pvwave/msg/11132d96d9c0f93d?hl=en&
; REVISION HISTORY:
;   Written   W. Landsman          December 2007
;-
compile_opt idl2
if N_Params() LT 3 then begin
    print,'Syntax - newcoeff = TRANSFORM_COEFF( coeff, alpha, beta) '
    if N_elements(coeff) GT 0 then return,coeff else return,-1
endif    
degree=n_elements(coeff)-1

newarray=coeff*0

FOR i=0,degree DO BEGIN
    FOR j=0,i DO BEGIN
       newarray[j] = newarray[j] +  $
          coeff[i]*factorial(i)*alpha^j*beta^(i-j)/factorial(j)/factorial(i-j)
    ENDFOR
ENDFOR 

return, newarray
end

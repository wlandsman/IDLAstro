pro prob_kuiper, D, N_eff, probks
;+
; NAME:
;       PROB_KUIPER
; PURPOSE:
;       Return the significance of the Kuiper statistic
; EXPLANATION:
;       Returns the significance level of an observed value of the
;       Kuiper statistic D for an effective number of data points
;       N_eff.   Called by KUIPERONE
;
; CALLING SEQUENCE:
;       prob_kuiper, D, N_eff, probks
;
; INPUT PARAMETERS:
;       D -  Kuiper statistic, floating scalar, always non-negative
;       N_eff - Effective number of data points, scalar.   For a 2 sided test
;               this is given by (N1*N2)/(N1+N2) where N1 and N2 are the number
;               of points in each data set.
;
; OUTPUT PARAMETERS:
;       probks - floating scalar between 0 and 1 giving the significance level of
;               the Kuiper statistic.   Small values of PROB suggest that the
;               distribution being tested are not the same
;
; REVISION HISTORY:
;       Written     W. Landsman                August, 1992
;       Corrected typo (termbv for termbf)    H. Ebeling/W.Landsman  March 1996
;       Probably did not affect numeric result, but iteration went longer
;       than necessary
;       Converted to IDL V5.0   W. Landsman   September 1997
;       Adapted from PROB_KS    J. Ballet     July 2003
;-
 On_error,2

 if N_params() LT 3 then begin
     print,'Syntax - prob_kuiper, D, N_eff, prob'
     print,'  D - Kuiper statistic, input'
     print,'  N_eff - effective number of data points, input'
     print,'  prob - Significance level of D, output'
     return
 endif

 eps1 = 0.001    ;Stop if current term less than EPS1 times previous term
 eps2 = 1.e-8    ;Stop if current term changes output by factor less than EPS2

 en = sqrt( N_eff )
 lambda = (en + 0.155 + 0.24/en)*D

; No iteration if lambda is smaller than 0.4
 if lambda le 0.4 then begin
 	probks = 1.0
 	return
 endif

 a2 = -2.*lambda^2
 probks = 0.
 termbf = 0.

 for j = 1,100 do begin

     a2j2 = a2 * j^2
     term = 2 * (-2*a2j2-1) * exp(a2j2)
     probks = probks + term

     if ( abs(term) LE eps1*termbf ) or $
        ( abs(term) LE eps2*probks ) then return

     termbf = abs(term)

 endfor

 probks = 1.          ;Sum did not converge after 100 iterations
 return

 end

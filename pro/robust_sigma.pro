FUNCTION  ROBUST_SIGMA,Y, ZERO=REF, GOODVEC = Q
;
;+
; NAME:
;	ROBUST_SIGMA  
;
; PURPOSE:
;	Calculate a resistant estimate of the dispersion of a distribution.
; EXPLANATION:
;	For an uncontaminated distribution, this is identical to the standard
;	deviation.
;
; CALLING SEQUENCE:
;	result = ROBUST_SIGMA( Y, [ /ZERO, GOODVEC = ] )
;
; INPUT: 
;	Y = Vector of quantity for which the dispersion is to be calculated
;
; OPTIONAL INPUT KEYWORD:
;	/ZERO - if set, the dispersion is calculated w.r.t. 0.0 rather than the
;		central value of the vector. If Y is a vector of residuals, this
;		should be set.
;
; OPTIONAL OUPTUT KEYWORD:
;       GOODVEC = Vector of non-trimmed indices of the input vector
; OUTPUT:
;	ROBUST_SIGMA returns the dispersion. In case of failure, returns 
;	value of -1.0
;
; PROCEDURE:
;	Use the median absolute deviation as the initial estimate, then weight 
;	points using Tukey's Biweight. See, for example, "Understanding Robust
;	and Exploratory Data Analysis," by Hoaglin, Mosteller and Tukey, John
;	Wiley & Sons, 1983, or equation 9 in Beers et al. (1990, AJ, 100, 32)
;
; REVSION HISTORY: 
;	H. Freudenreich, STX, 8/90
;       Replace MED() call with MEDIAN(/EVEN)  W. Landsman   December 2001
;       Don't count NaN values  W.Landsman  June 2010
;
;-
 On_error,2
 compile_opt idl2
 
 EPS = 1.0E-20
 IF KEYWORD_SET(REF) THEN Y0=0. ELSE Y0  = MEDIAN(Y,/EVEN)

; First, the median absolute deviation MAD about the median:

 MAD = MEDIAN( ABS(Y-Y0), /EVEN )/0.6745

; If the MAD=0, try the MEAN absolute deviation:
 IF MAD LT EPS THEN MAD = MEAN( ABS(Y-Y0) )/.80
 IF MAD LT EPS THEN RETURN, 0.0

; Now the biweighted value:
 U   = (Y-Y0)/(6.*MAD)
 UU  = U*U
 Q   = WHERE(UU LE 1.0, COUNT)
 IF COUNT LT 3 THEN BEGIN
   PRINT,'ROBUST_SIGMA: This distribution is TOO WEIRD! Returning -1'
   SIGGMA = -1.
   RETURN,SIGGMA
 ENDIF

 N = TOTAL(FINITE(Y),/INT)      ;In case Y has NaN values          ;
 NUMERATOR = TOTAL( (Y[Q]-Y0)^2 * (1-UU[Q])^4 )
 DEN1  = TOTAL( (1.-UU[Q])*(1.-5.*UU[Q]) )
 SIGGMA = N*NUMERATOR/(DEN1*(DEN1-1.))
 
 IF SIGGMA GT 0. THEN RETURN, SQRT(SIGGMA) ELSE RETURN, 0.

 END

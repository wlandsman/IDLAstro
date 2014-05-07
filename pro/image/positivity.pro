function positivity, x, DERIVATIVE=deriv, EPSILON=epsilon
;+
; NAME:
;	POSITIVITY
; PURPOSE:
;	Map an image uniquely and smoothly into all positive values.
; EXPLANATION:
;	Take unconstrained x (usually an image), and map it uniquely and 
;	smoothly into positive values.   Negative values of x get mapped to 
;	interval ( 0, sqrt( epsilon )/2 ], positive values go to 
;	( sqrt( epsilon )/2, oo ) with deriv approaching 1.  Derivative is 
;	always 1/2 at x=0.   Derivative is used by the MRL deconvolution 
;	algorithm.
;
; CALLING SEQUENCE:
;	result = POSITIVITY( x, [ /DERIVATIVE, EPSILON = )
;
; INPUTS:
;	x - input array, unconstrained
;
; OUTPUT:
;	result =  output array = ((x + sqrt(x^2 + epsilon))/2
;		if the /DERIV keyword is set then instead the derivative of
;		the above expression with respect to X is returned
;
; OPTIONAL INPUT KEYWORDS:
;	DERIV -  if this keyword set, then the derivative of the positivity
;		mapping is returned, rather than the mapping itself
;	EPSILON - real scalar specifying the interval into which to map
;		negative values.    If EPSILON EQ 0 then the mapping reduces to 
;		positive truncation.   If EPSILON LT then the mapping reduces to
;		an identity (no change).  Default is EPSILON = 1e-9 
;
; REVISION HISTORY:
;	 F.Varosi NASA/GSFC 1992, as suggested by R.Pina UCSD.
;	Converted to IDL V5.0   W. Landsman   September 1997
;-

	if N_elements( epsilon ) NE 1 then epsilon = 1.e-9

	if keyword_set( deriv ) then begin
		if (epsilon GT 0) then return,(1 + x/sqrt( x^2 + epsilon ))/2 $
				  else if (epsilon LT 0) then return,(1)      $
				  else return,( x GT 0 )
	  endif else begin
		if (epsilon GT 0) then return,( x + sqrt( x^2 + epsilon ) )/2 $
				  else if (epsilon LT 0) then return, x       $
				  else return,( x > 0 )
	   endelse
end

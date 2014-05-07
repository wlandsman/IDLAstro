pro trapzd, func, a, b, s, step, _EXTRA = _EXTRA
;+
; NAME:
;       TRAPZD
; PURPOSE:
;       Compute the nth stage of refinement of an extended trapezoidal rule.
; EXPLANATION:
;       This procedure is called by QSIMP and QTRAP.   Algorithm from Numerical
;       Recipes, Section 4.2.   TRAPZD is meant to be called iteratively from
;       a higher level procedure.
;
; CALLING SEQUENCE:
;       TRAPZD, func, A, B, S, step, [ _EXTRA = ]
;
; INPUTS:
;       func - scalar string giving name of function to be integrated.   This
;               must be a function of one variable.
;       A,B -  scalars giving the limits of the integration
;
; INPUT-OUTPUT:
;       S -    scalar giving the total sum from the previous iterations on 
;               input and the refined sum after the current iteration on output.
;
;       step - LONG scalar giving the number of points at which to compute the
;               function for the current iteration.   If step is not defined on
;               input, then S is intialized using the average of the endpoints
;               of limits of integration.
;
; OPTIONAL INPUT KEYWORDS:
;       Any supplied keywords will be passed to the user function via the 
;       _EXTRA facility. 
;
; NOTES:
;       (1) TRAPZD will check for math errors (except for underflow) when 
;       computing the function at the endpoints, but not on subsequent 
;       iterations.
;
;       (2) TRAPZD always uses double precision to sum the function values
;       but the call to the user-supplied function is double precision only if 
;       one of the limits A or B is double precision.
; REVISION HISTORY:
;       Written         W. Landsman                 August, 1991
;       Always use double precision for TOTAL       March, 1996
;       Pass keyword to function via _EXTRA facility  W. Landsman July 1999
;       Don't check for floating underflow  W.Landsman  April 2008
;-
 On_error,2
 compile_opt idl2

 kpresent = keyword_set(_EXTRA)
 if N_elements(step) EQ 0 then begin          ;Initialize?

;If a math error occurs, it is likely to occur at the endpoints
     junk = check_math()                    ;
     if kpresent then s1 = CALL_FUNCTION(func,A, _EXTRA= _EXTRA) $
                 else s1 = CALL_FUNCTION(func,A)
     if check_math(mask=211) NE 0 then $
        message,'ERROR - Illegal lower bound of '+strtrim(A,2)+ $
                ' to function ' + strupcase(func)
     if kpresent then s2 = CALL_FUNCTION(func,B, _EXTRA = _EXTRA) $
                 else s2 = CALL_FUNCTION(func,B)
     if check_math(mask=211) NE 0 then $
        message,'ERROR - Illegal upper bound of '+strtrim(B,2) + $
                ' to function ' + strupcase(func)
     junk= check_math()		
     s = 0.5d * ( double(B)-A ) * ( s1+s2 )    ;First approx is average of endpoints
     step = 1l

 endif else begin

     tnm = float( step )               
     del = ( B - A ) / tnm                    ;Spacing of the points to add
     x = A + 0.5*del + findgen( step ) * del  ;Grid of points @ compute function
     if kpresent then sum = CALL_FUNCTION( func, x, _EXTRA = _EXTRA) $
                 else sum = CALL_FUNCTION( func, x)
     S = 0.5d * ( S + (double(B)-A) * total( sum, /DOUBLE )/tnm )     
     step = 2*step

 endelse

 return
 end 

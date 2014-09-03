function ZBRENT, x1, x2, FUNC_NAME=func_name, _EXTRA = _extra,   $
                         MAX_ITERATIONS=maxit, TOLERANCE=TOL
;+
; NAME:
;     ZBRENT
; PURPOSE:
;     Find the zero of a 1-D function up to specified tolerance.
; EXPLANTION:
;     This routine assumes that the function is known to have a zero.
;     Adapted from procedure of the same name in "Numerical Recipes" by
;     Press et al. (1992), Section 9.3
;
; CALLING:
;       x_zero = ZBRENT( x1, x2, FUNC_NAME="name", MaX_Iter=, Tolerance=, 
;                                 _EXTRA =  )
;
; INPUTS:
;       x1, x2 = scalars, 2 points which bracket location of function zero,
;                                               that is, F(x1) < 0 < F(x2).
;       Note: computations are performed with
;       same precision (single/double) as the inputs and user supplied function.
;
; REQUIRED INPUT KEYWORD:
;       FUNC_NAME = function name (string)
;               Calling mechanism should be:  F = func_name( px )
;               where:  px = scalar independent variable, input.
;                       F = scalar value of function at px,
;                           should be same precision (single/double) as input.
;
; OPTIONAL INPUT KEYWORDS:
;       MAX_ITER = maximum allowed number iterations, default=100.
;       TOLERANCE = desired accuracy of minimum location, default = 1.e-3.
;
;       Any other keywords are passed directly to the user-supplied function
;       via the _EXTRA facility.
; OUTPUTS:
;       Returns the location of zero, with accuracy of specified tolerance.
;
; PROCEDURE:
;       Brent's method to find zero of a function by using bracketing,
;       bisection, and inverse quadratic interpolation,
;
; EXAMPLE:
;       Find the root of the COSINE function between 1. and 2.  radians
;
;        IDL> print, zbrent( 1, 2, FUNC = 'COS')
;
;       and the result will be !PI/2 within the specified tolerance
; MODIFICATION HISTORY:
;       Written, Frank Varosi NASA/GSFC 1992.
;       FV.1994, mod to check for single/double prec. and set zeps accordingly.
;       Use MACHAR() to define machine precision   W. Landsman September 2002
;       Added _EXTRA keyword  W. Landsman  December 2011
;       Need to check whether user function accepts keywords W.L. Jan 2012
;-
        compile_opt idl2
        if N_params() LT 2 then begin
             print,'Syntax - result = ZBRENT( x1, x2, FUNC_NAME = ,'
             print,'                  [ MAX_ITER = , TOLERANCE = , _EXTRA=])'
             return, -1
        endif

        kpresent = keyword_set(_EXTRA)
        if N_elements( TOL ) NE 1 then TOL = 1.e-3
        if N_elements( maxit ) NE 1 then maxit = 100

        if size(x1,/TNAME) EQ 'DOUBLE' OR size(x2,/TNAME) EQ 'DOUBLE' then begin
                xa = double( x1 )
                xb = double( x2 )
                zeps = (machar(/DOUBLE)).eps   ;machine epsilon in double.
          endif else begin
                xa = x1
                xb = x2
                zeps = (machar(/DOUBLE)).eps   ;machine epsilon, in single 
           endelse
        
	if kpresent then begin 
          fa = call_function( func_name, xa, _EXTRA = _extra )
          fb = call_function( func_name, xb, _EXTRA = _extra )
        endif else begin 
          fa = call_function( func_name, xa )
          fb = call_function( func_name, xb )
	endelse
        fc = fb

        if (fb*fa GT 0) then begin
                message,"root must be bracketed by the 2 inputs",/INFO
                return,xa
           endif

        for iter = 1,maxit do begin

                if (fb*fc GT 0) then begin
                        xc = xa
                        fc = fa
                        Din = xb - xa
                        Dold = Din
                   endif

                if (abs( fc ) LT abs( fb )) then begin
                        xa = xb   &   xb = xc   &   xc = xa
                        fa = fb   &   fb = fc   &   fc = fa
                   endif

                TOL1 = 0.5*TOL + 2*abs( xb ) * zeps     ;Convergence check
                xm = (xc - xb)/2.

                if (abs( xm ) LE TOL1) || (fb EQ 0) then return,xb

                if (abs( Dold ) GE TOL1) && (abs( fa ) GT abs( fb )) then begin

                        S = fb/fa       ;attempt inverse quadratic interpolation

                        if (xa EQ xc) then begin
                                p = 2 * xm * S
                                q = 1-S
                          endif else begin
                                T = fa/fc
                                R = fb/fc
                                p = S * (2*xm*T*(T-R) - (xb-xa)*(R-1) )
                                q = (T-1)*(R-1)*(S-1)
                           endelse

                        if (p GT 0) then q = -q
                        p = abs( p )
                        test = ( 3*xm*q - abs( q*TOL1 ) ) < abs( Dold*q )

                        if (2*p LT test)  then begin
                                Dold = Din              ;accept interpolation
                                Din = p/q
                          endif else begin
                                Din = xm                ;use bisection instead
                                Dold = xm
                           endelse

                  endif else begin

                        Din = xm    ;Bounds decreasing to slowly, use bisection
                        Dold = xm
                   endelse

                xa = xb
                fa = fb         ;evaluate new trial root.

                if (abs( Din ) GT TOL1) then xb = xb + Din $
                                        else xb = xb + TOL1 * (1-2*(xm LT 0))

                if kpresent then $
                   fb = call_function( func_name, xb, _EXTRA = _extra ) else $
                   fb = call_function( func_name, xb )
		   
          endfor

        message,"exceeded maximum number of iterations: "+strtrim(iter,2),/INFO

return, xb
end

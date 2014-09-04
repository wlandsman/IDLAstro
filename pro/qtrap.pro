pro  qtrap, func, A, B, S, EPS=eps, MAX_ITER = max_iter, _EXTRA = _Extra
;+
; NAME:
;       QTRAP
; PURPOSE:
;       Integrate using trapezoidal rule to specified accuracy.
; EXPLANATION:
;       Integrate a function to specified accuracy using the extended 
;       trapezoidal rule.   Adapted from Numerical Recipes (1992, 2nd edition),
;       Section 4.2. 
;
; CALLING SEQUENCE:
;       QTRAP, func, A, B, S, [EPS = , MAX_ITER =, _EXTRA = ]
;
; INPUTS:
;       func - scalar string giving name of function of one variable to 
;               be integrated
;       A,B  - numeric scalars giving the lower and upper bound of the 
;               integration
;
; OUTPUTS:
;       S - Scalar giving the approximation to the integral of the specified
;               function between A and B.
;
; OPTIONAL KEYWORD PARAMETERS:
;       EPS - scalar specify the fractional accuracy before ending the 
;             iteration.    Default = 1E-6
;       MAX_ITER - Integer specifying the total number iterations at which 
;               QTRAP will terminate even if the specified accuracy has not yet
;               been met.    The maximum number of function evaluations will 
;               be 2^(MAX_ITER).   Default value is MAX_ITER = 20
;
;       Any other keywords are passed directly to the user-supplied function
;       via the _EXTRA facility.
; NOTES:
;       QTRAP is robust way of doing integrals that are not very smooth.  If the
;       function has a continuous 3rd derivative then the function QSIMP will 
;          likely be more efficient at performing the integral.
; EXAMPLE:
;       Compute the integral of sin(x) from 0 to !PI/3.
;    
;       IDL> QTRAP, 'sin', 0, !PI/3, S   & print,S
;   
;       The value obtained should be cos(!PI/3) = 0.5
;
; PROCEDURES CALLED:
;       TRAPZD, ZPARCHECK
; REVISION HISTORY:
;       W. Landsman         ST Systems Co.         August, 1991
;       Continue after Max Iter warning message, W. Landsman  March 1996
;       Converted to IDL V5.0   W. Landsman   September 1997
;       Pass keyword to function via _EXTRA facility  W. Landsman July 1999
;-
 On_error,2                                      ;Return to caller
 compile_opt idl2

 if N_params() LT 4 then begin
    print,'Syntax - QTRAP, func, A, B, S, [ Eps = , MAX_ITER = ]
    print,' func - scalar string giving function name
    print,' A,B - endpoints of integration, S - output sum'
    return
 endif

 zparcheck, 'QTRAP', func, 1, 7, 0, 'Function name'          ;Valid inputs?
 zparcheck, 'QTRAP', A, 2, [1,2,3,4,5], 0, 'Lower limit of Integral'
 zparcheck, 'QTRAP', B, 3, [1,2,3,4,5], 0, 'Upper limit of Integral'

 if ~keyword_set( EPS ) then eps = 1.e-6
 if ~keyword_set( MAX_ITER ) then max_iter = 20
 olds = -1.e30

 for i = 0, max_iter-1 do begin

    trapzd, func, A, B, S, it, _EXTRA = _EXTRA
    if ( abs(S-oldS) LT eps*abs(oldS) ) then return
    olds = s

 endfor

 message,/CON, $
        'WARNING - Sum did not converge after '+ strtrim(max_iter,2) + ' steps'

 return
 end

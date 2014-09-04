pro qsimp, func, A, B, S, EPS=eps, MAX_ITER = max_iter, _EXTRA = _EXTRA
;+
; NAME:
;       QSIMP
; PURPOSE:
;       Integrate using Simpson's rule to specified accuracy.
; EXPLANATION:
;       Integrate a function to specified accuracy using the extended 
;       trapezoidal rule.   Adapted from algorithm in Numerical Recipes, 
;       by Press et al. (1992, 2nd edition), Section 4.2.     This procedure
;       has been partly obsolete since IDL V3.5 with the introduction of the 
;       intrinsic function QSIMP(), but see notes below.
;
; CALLING SEQUENCE:
;       QSIMP, func, A, B, S, [ EPS = , MAX_ITER =, _EXTRA =  ]
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
;       EPS - scalar specifying the fractional accuracy before ending the 
;               iteration.  Default = 1E-6
;       MAX_ITER - Integer specifying the total number iterations at which 
;               QSIMP will terminate even if the specified accuracy has not yet
;               been met.   The maximum number of function evaluations will be
;               2^(MAX_ITER).    Default value is MAX_ITER = 20
;
;       Any other keywords are passed directly to the user-supplied function
;       via the _EXTRA facility.
; NOTES:
;       (1) The function QTRAP is robust way of doing integrals that are not 
;       very smooth.  However, if the function has a continuous 3rd derivative
;       then QSIMP will likely be more efficient at performing the integral.
;
;       (2) QSIMP can be *much* faster than the intrinsic QSIMP() function (as
;       of IDL V8.2.3).   This is because the intrinsic QSIMP() function only 
;       requires that the user supplied function accept a *scalar* variable.
;       Thus on the the 16th iteration, the intrinsic QSIMP() makes 32,767
;       calls to the user function, whereas this procedure makes one call 
;       with a  32,767 element vector.  Also, unlike the intrinsic QSIMP(), this
;       procedure allows keywords in the user-supplied function.
;
;       (3) Since the intrinsic QSIMP() is a function, and this file contains a 
;       procedure, there should be no name conflict.
; EXAMPLE:
;       Compute the integral of sin(x) from 0 to !PI/3.
;    
;       IDL> QSIMP, 'sin', 0, !PI/3, S   & print, S
;   
;       The value obtained should be cos(!PI/3) = 0.5
;
; PROCEDURES CALLED:
;       SETDEFAULTVALUE, TRAPZD, ZPARCHECK
;
; REVISION HISTORY:
;       W. Landsman         ST Systems Co.         August, 1991
;       Continue after max iter warning message   W. Landsman   March, 1996
;       Pass keyword to function via _EXTRA facility  W. Landsman July 1999
;       Use SETDEFAULTVALUE    W. Landsman  Aug 2013
;-

 On_error,2                                  ;Return to caller
 compile_opt idl2

 if N_params() LT 4 then begin
    print,'Syntax - QSIMP, func, A, B, S, [ MAX_ITER = , EPS = ]'
    print,' func - scalar string giving function name'
    print,' A,B - endpoints of integration, S - output sum'
    return
 endif

 zparcheck, 'QSIMP', func, 1, 7, 0, 'Function name'       ;Valid inputs?
 zparcheck, 'QSIMP', A, 2, [1,2,3,4,5], 0, 'Lower limit of Integral'
 zparcheck, 'QSIMP', B, 3, [1,2,3,4,5], 0, 'Upper limit of Integral'

 setdefaultvalue,eps,1.e-6               ;Typo fixed Oct 2013
 setdefaultvalue,max_iter,20

 ost = (oS = -1.e30)
 for i = 0,max_iter - 1 do begin
    trapzd, func, A,B, st, it, _EXTRA = _EXTRA
    S = (4.*st - ost)/3.
    if ( abs(S-oS) LT eps*abs(oS) ) then return
    os = s
    ost = st
 endfor
 
 message,/CON, $
        'WARNING - Sum did not converge after '+ strtrim(max_iter,2) + ' steps'
 
 return
 end

pro minF_conj_grad, p_min, f_min, conv_factor, FUNC_NAME=func_name, $
                                        TOLERANCE=tol, USE_DERIV=use, $
                                        INITIALIZE=initialize, QUADRATIC=quad
;+
; NAME:
;        MINF_CONJ_GRAD
; PURPOSE:
;       Find the local minimum of a scalar function using conjugate gradient
; EXPLANATION:
;       Find the local minimum of a scalar function of several variables using 
;       the Conjugate Gradient method (Fletcher-Reeves-Polak-Ribiere algorithm).
;       Function may be anything with computable partial derivatives.
;       Each call to minF_conj_grad performs one iteration of algorithm,
;       and returns an N-dim point closer to the local minimum of function.
; CALLING EXAMPLE:
;       p_min = replicate( 1, N_dim )
;       minF_conj_grad, p_min, f_min, conv_factor, FUNC_NAME="name",/INITIALIZE
;
;       while (conv_factor GT 0) do begin
;               minF_conj_grad, p_min, f_min, conv_factor, FUNC_NAME="name"
;       endwhile
; INPUTS:
;       p_min = vector of independent variables, location of minimum point
;               obtained from previous call to minF_conj_grad, (or first guess).
; KEYWORDS:
;       FUNC_NAME = function name (string)
;               Calling mechanism should be:  F = func_name( px, gradient )
;         where:
;               F = scalar value of function at px.
;               px = vector of independent variables, input.
;               gradient = vector of partial derivatives of the function
;                       with respect to independent variables, evaluated at px.
;                       This is an optional output parameter:
;                       gradient should not be calculated if parameter is not
;                       supplied in call (Unless you want to waste some time).
;      /INIT must be specified on first call (whenever p_min is a guess),
;                       to initialize the iteration scheme of algorithm.
;      /USE_DERIV causes the directional derivative of function to be used
;                       in the 1-D minimization part of algorithm
;                       (default is not to use directional derivative).
;       TOLERANCE = desired accuracy of minimum location, default=sqrt(1.e-7).
;      /QUADRATIC runs simpler version which works only for quadratic function.
; OUTPUTS:
;       p_min = vector giving improved solution for location of minimum point.
;       f_min = value of function at p_min.
;       conv_factor = gives the current rate of convergence (change in value),
;                       iteration should be stopped when rate gets near zero.
; EXTERNAL CALLS:
;       pro minF_bracket,  to find 3 points which bracket the minimum in 1-D.
;       pro minF_parabolic,  to find minimum point in 1-D.
;       pro minF_parabol_D,  to find minimum point in 1-D, using derivatives.
; COMMON BLOCKS:
;       common minf_conj_grad, grad_conj, grad_save, gs_norm
;       (to keep conjugate gradient, gradient and norm from previous iteration)
; PROCEDURE:
;       Algorithm adapted from Numerical Recipes, sec.10.6 (p.305).
;       Conjugate gradient is computed from gradient, which then gives
;       the best direction (in N-dim space) in which to proceed to find
;       the minimum point. The function is then minimized along
;       this direction of conjugate gradient (a 1-D minimization).
;       The algorithm is repeated starting at the new point by calling again.
; MODIFICATION HISTORY:
;       Written, Frank Varosi NASA/GSFC 1992.
;       Converted to IDL V5.0   W. Landsman   September 1997
;-
 On_error,2 
 
 if N_params() LT 3 then begin  
   print,'Syntax - minF_conj_grad, p_min, f_min, conv_factor, FUNC_NAME = 
   print,'         [ TOLERANCE=, USE_DERIV=, INITIALIZE= , QUADRATIC= ]
   return
 endif

  common minf_conj_grad, grad_conj, grad_save, gs_norm

        fp = call_function( func_name, p_min, gradient )

;Compute conjugate gradient direction:

        if keyword_set( initialize ) then begin

                grad_conj = -gradient
                gs_norm = total( gradient * gradient )
                if NOT keyword_set( quad ) then grad_save = gradient

          endif else begin

                grad_norm = total( gradient * gradient )

                if (grad_norm EQ 0) then begin
                        f_min = fp
                        conv_factor = 0
                        return
                   endif

                if keyword_set( quad ) then gamma = grad_norm/gs_norm else begin

                    gamma = ( grad_norm - total( grad_save*gradient ) )/gs_norm
                        grad_save = gradient
                  endelse

                grad_conj = gamma * grad_conj - gradient
                gs_norm = grad_norm
           endelse

;Now find mininum along direction of conjugate gradient:

        xa = 0
        xb = 1/sqrt( gs_norm )

        minF_bracket, xa,xb,xc, fa,fb,fc, FUNC_NAME=func_name, POINT=p_min, $
                                                        DIRECTION=grad_conj
        if keyword_set( use ) then begin

                minF_parabol_D, xa,xb,xc, x_min, f_min, FUN=func_name, TOL=tol,$
                                                POINT=p_min, DIRECTION=grad_conj
          endif else begin

                minF_parabolic, xa,xb,xc, x_min, f_min, FUN=func_name, TOL=tol,$
                                                POINT=p_min, DIRECTION=grad_conj
           endelse

        conv_factor = 2*abs( f_min - fp )/( (abs(f_min) + abs(fp)) > 1.e-9 )

        p_min = p_min + x_min * grad_conj
return
end

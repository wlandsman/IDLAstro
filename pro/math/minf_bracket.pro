pro minF_bracket, xa,xb,xc, fa,fb,fc, FUNC_NAME=func_name, $
                                                POINT_NDIM=pn, DIRECTION=dirn
;+
; NAME:
;       MINF_BRACKET
; PURPOSE:
;       Bracket a local minimum of a 1-D function with 3 points,
; EXPLANATION:
;       Brackets a local minimum of a 1-d function with 3 points,
;       thus ensuring that a minimum exists somewhere in the interval.
;       This routine assumes that the function has a minimum somewhere....
;       Routine can also be applied to a scalar function of many variables,
;       for such case the local minimum in a specified direction is bracketed,
;       This routine is called by minF_conj_grad, to bracket minimum in the 
;       direction of the conjugate gradient of function of many variables
; CALLING EXAMPLE:
;       xa=0  & xb=1                                    
;       minF_bracket, xa,xb,xc, fa,fb,fc, FUNC_NAME="name"      ;for 1-D func.
;  or:
;       minF_bracket, xa,xb,xc, fa,fb,fc, FUNC="name",     $
;                                         POINT=[0,1,1],   $
;                                         DIRECTION=[2,1,1]     ;for 3-D func.
; INPUTS:
;       xa = scalar, guess for point bracketing location of minimum.
;       xb = scalar, second guess for point bracketing location of minimum.
; KEYWORDS:
;       FUNC_NAME = function name (string)
;               Calling mechanism should be:  F = func_name( px )
;               where:
;                       px = scalar or vector of independent variables, input.
;                       F = scalar value of function at px.
;       POINT_NDIM = when working with function of N variables,
;               use this keyword to specify the starting point in N-dim space.
;               Default = 0, which assumes function is 1-D.
;       DIRECTION = when working with function of N variables,
;               use this keyword to specify the direction in N-dim space
;               along which to bracket the local minimum, (default=1 for 1-D).
;               (xa,xb,xc) are then relative distances from POINT_NDIM.
; OUTPUTS:
;       xa,xb,xc = scalars, 3 points which bracket location of minimum,
;               that is, f(xb) < f(xa) and f(xb) < f(xc), so minimum exists.
;               When working with function of N variables
;               (xa,xb,xc) are then relative distances from POINT_NDIM,
;               in the direction specified by keyword DIRECTION,
;               with scale factor given by magnitude of DIRECTION.
; OPTIONAL OUTPUT:
;       fa,fb,fc = value of function at 3 points which bracket the minimum,
;                       again note that fb < fa and fb < fc if minimum exists.
; PROCEDURE:
;       algorithm from Numerical Recipes (by Press, et al.), sec.10.1 (p.281).
; MODIFICATION HISTORY:
;       Written, Frank Varosi NASA/GSFC 1992.
;       Converted to IDL V5.0   W. Landsman   September 1997
;-
        goldm = (sqrt(5)+1)/2           ;golden mean factor to march with.
        glimit = 100                    ;maximum factor to try.
        tiny = 1.e-19                   ;a tiny number to avoid divide by zero.

        if N_elements( pn ) LE 0 then begin
                pn = 0
                dirn = 1
           endif

        if (xa EQ xb) then xb = xa + 1
        fa = call_function( func_name, pn + xa * dirn )
        fb = call_function( func_name, pn + xb * dirn )

        if (fb GT fa) then begin
                x = xa  &  xa = xb  &  xb = x
                f = fa  &  fa = fb  &  fb = f
           endif

        xc = xb + goldm * (xb-xa)
        fc = call_function( func_name, pn + xc * dirn )

        while (fb GE fc) do begin

                zba = xb-xa
                zbc = xb-xc
                r = zba * (fb-fc)
                q = zbc * (fb-fa)
                delta = q-r
                sign = 1 - 2 * (delta LT 0)
                xu = xb - (zbc * q - zba * r)/(2* sign * (abs( delta ) > tiny) )
                ulim = xb + glimit * (xc-xb)

                if ( (xb-xu)*(xu-xc) GT 0 ) then begin

                        fu = call_function( func_name, pn + xu * dirn )

                        if (fu LT fc) then begin
                                xa = xb  &  xb = xu
                                fa = fb  &  fb = fu
                                return
                          endif else if (fu GT fb) then begin
                                xc = xu
                                fc = fu
                                return
                           endif

                        xu = xc - goldm * zbc
                        fu = call_function( func_name, pn + xu * dirn )

                 endif else if ( (xc-xu)*(xu-ulim) GT 0 ) then begin

                        fu = call_function( func_name, pn + xu * dirn )

                        if (fu LT fc) then begin
                                xb = xc  &  fb = fc
                                xc = xu  &  fc = fu
                                xu = xc + goldm * (xc-xb)
                                fu = call_function( func_name, pn + xu * dirn )
                           endif

                  endif else if ( (ulim-xc)*(xu-ulim) GE 0 ) then begin

                        xu = ulim
                        fu = call_function( func_name, pn + xu * dirn )

                   endif else begin

                        xu = xc + goldm * (xc-xb)
                        fu = call_function( func_name, pn + xu * dirn )
                    endelse
        
                xa = xb  &  xb = xc  &  xc = xu
                fa = fb  &  fb = fc  &  fc = fu
          endwhile
return
end

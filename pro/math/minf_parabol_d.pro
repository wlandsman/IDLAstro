; Procedure minF_parabol_D,
; first, a utility function which gets derivative in 1-D:
;------------------------------------------------------------------------------
function call_func_deriv, func_name, x, deriv, POINT_NDIM=pn, DIRECTION=dirn

        f = call_function( func_name, pn + x * dirn, grad )

        deriv = total( [grad * dirn] )

return, f
end
;------------------------------------------------------------------------------
pro minF_parabol_D, xa,xb,xc, xmin, fmin, FUNC_NAME=func_name,    $
                                          MAX_ITERATIONS=maxit,   $
                                          TOLERANCE=TOL,          $
                                          POINT_NDIM=pn, DIRECTION=dirn
;+
; NAME:
;       MINF_PARABOL_D
; PURPOSE:
;       Minimize a function using a modified  Brent's method with derivatives
; EXPLANATION:
;       Based on the procedure DBRENT in Numerical Recipes by Press et al.
;       Finds a local minimum of a 1-D function up to specified tolerance,
;       using the first derivative of function in the algorithm.
;       This routine assumes that the function has a minimum nearby.
;       (recommend first calling minF_bracket, xa,xb,xc, to bracket minimum).
;       Routine can also be applied to a scalar function of many variables,
;       for such case the local minimum in a specified direction is found,
;       This routine is called by minF_conj_grad, to locate minimum in the 
;       direction of the conjugate gradient of function of many variables.
;
; CALLING EXAMPLES:
;       minF_parabol_D, xa,xb,xc, xmin, fmin, FUNC_NAME="name"  ;for 1-D func.
;  or:
;       minF_parabol_D, xa,xb,xc, xmin, fmin, FUNC="name", $
;                                         POINT=[0,1,1],   $
;                                         DIRECTION=[2,1,1]     ;for 3-D func.
; INPUTS:
;       xa,xb,xc = scalars, 3 points which bracket location of minimum,
;               that is, f(xb) < f(xa) and f(xb) < f(xc), so minimum exists.
;               When working with function of N variables
;               (xa,xb,xc) are then relative distances from POINT_NDIM,
;               in the direction specified by keyword DIRECTION,
;               with scale factor given by magnitude of DIRECTION.
; KEYWORDS:
;       FUNC_NAME = function name (string)
;               Calling mechanism should be:  F = func_name( px, gradient )
;               where:
;                       px = scalar or vector of independent variables, input.
;                       F = scalar value of function at px.
;                       gradient = derivative of function, a scalar if 1-D,
;                               a gradient vector if N-D,
;                               (should only be computed if arg. is present).
;
;       POINT_NDIM = when working with function of N variables,
;               use this keyword to specify the starting point in N-dim space.
;               Default = 0, which assumes function is 1-D.
;       DIRECTION = when working with function of N variables,
;               use this keyword to specify the direction in N-dim space
;               along which to bracket the local minimum, (default=1 for 1-D).
;               (xa, xb, xc, x_min are then relative distances from POINT_NDIM)
;       MAX_ITER = maximum allowed number iterations, default=100.
;       TOLERANCE = desired accuracy of minimum location, default=sqrt(1.e-7).
;
; OUTPUTS:
;       xmin = estimated location of minimum.
;               When working with function of N variables,
;               xmin is the relative distance from POINT_NDIM,
;               in the direction specified by keyword DIRECTION,
;               with scale factor given by magnitude of DIRECTION,
;               so that min. Loc. Pmin = Point_Ndim + xmin * Direction.
;       fmin = value of function at xmin (or Pmin).
; PROCEDURE:
;       Brent's method to minimize a function by using parabolic interpolation
;       and using first derivative of function,
;       from Numerical Recipes (by Press, et al.), sec.10.3 (p.287),
; MODIFICATION HISTORY:
;       Written, Frank Varosi NASA/GSFC 1992.
;-
        zeps = 1.e-7    ;machine epsilon, smallest addition.
        if N_elements( TOL ) NE 1 then TOL = sqrt( zeps )
        if N_elements( maxit ) NE 1 then maxit = 100

        if N_elements( pn ) LE 0 then begin
                pn = 0
                dirn = 1
           endif

        xLo = xa < xc
        xHi = xa > xc
        xmin = xb
        fmin = call_func_deriv( func_name, xmin, dx, POINT=pn, DIR=dirn )
        xv = xmin  &  xw = xmin
        fv = fmin  &  fw = fmin
        dv = dx  &  dw = dx
        es = 0.

        for iter = 1,maxit do begin

                xm = (xLo + xHi)/2.
                TOL1 = TOL * abs(xmin) + zeps
                TOL2 = 2*TOL1

                if ( abs( xmin - xm ) LE ( TOL2 - (xHi-xLo)/2. ) ) then return

                if (abs( es ) GT TOL1) then begin

                        d1 = 2*(xHi-xLo)
                        d2 = d1
                        if (dw NE dx) then d1 = (xw-xmin)*dx/(dx-dw)
                        if (dv NE dx) then d2 = (xv-xmin)*dx/(dx-dv)
                        u1 = xmin + d1
                        u2 = xmin + d2
                        ok1 = ((xLo-u1)*(u1-xHi) GT 0) AND (dx*d1 LE 0)
                        ok2 = ((xLo-u2)*(u2-xHi) GT 0) AND (dx*d2 LE 0)
                        olde = es
                        es = ds

                        if NOT (ok1 OR ok2) then goto,BISECT

                        if (ok1 AND ok2) then begin

                            if (abs( d1 ) LT abs( d2 )) then ds=d1 else ds=d2

                         endif else if (ok1) then ds=d1 else ds=d2

                        if (abs( ds ) LE abs( olde/2 )) then begin

                                xu = xmin + ds

                                if ((xu-xLo) LT TOL2) OR $
                                   ((xHi-xu) LT TOL2) then $
                                             ds = TOL1 * (1-2*((xm-xmin) LT 0))
                                goto,STEP
                          endif
                   endif

        BISECT: if (dx GE 0) then  es = xLo-xmin  else  es = xHi-xmin
                ds = es/2

        STEP:   sign = 1 - 2*(ds LT 0)
                xu = xmin + sign * ( abs( ds ) > TOL1 )
                fu = call_func_deriv( func_name, xu, du, POINT=pn, DIR=dirn )

                if (fu GT fmin) AND (abs( ds ) LT TOL1) then return

                if (fu LE fmin) then begin

                        if (xu GE xmin) then xLo=xmin else xHi=xmin
                        xv = xw  &  fv = fw  &  dv = dw
                        xw = xmin  &  fw = fmin  &  dw = dx
                        xmin = xu  &  fmin = fu  &  dx = du
                
                  endif else begin

                        if (xu LT xmin) then xLo=xu else xHi=xu

                        if (fu LE fw) OR (xw EQ xmin) then begin

                                xv = xw  &  fv = fw  &  dv = dw
                                xw = xu  &  fw = fu  &  dw = du

                          endif else if (fu LE fv) OR (xv EQ xmin) $
                                                   OR (xv EQ xw) then begin
                                xv = xu  &  fv = fu  &  dv = du
                           endif
                   endelse
          endfor

        message,"exceeded maximum number of iterations: "+strtrim(iter,2),/INFO
return
end

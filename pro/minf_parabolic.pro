pro minF_parabolic, xa,xb,xc, xmin, fmin, FUNC_NAME=func_name,    $
                                          MAX_ITERATIONS=maxit,   $
                                          TOLERANCE=TOL,          $
                                          POINT_NDIM=pn, DIRECTION=dirn
;+
; NAME:
;       MINF_PARABOLIC
; PURPOSE:
;       Minimize a function using Brent's method with parabolic interpolation
; EXPLANATION:
;       Find a local minimum of a 1-D function up to specified tolerance.
;       This routine assumes that the function has a minimum nearby.
;       (recommend first calling minF_bracket, xa,xb,xc, to bracket minimum).
;       Routine can also be applied to a scalar function of many variables,
;       for such case the local minimum in a specified direction is found,
;       This routine is called by minF_conj_grad, to locate minimum in the 
;       direction of the conjugate gradient of function of many variables.
;
; CALLING EXAMPLES:
;       minF_parabolic, xa,xb,xc, xmin, fmin, FUNC_NAME="name"  ;for 1-D func.
;  or:
;       minF_parabolic, xa,xb,xc, xmin, fmin, FUNC="name", $
;                                         POINT=[0,1,1],   $
;                                         DIRECTION=[2,1,1]     ;for 3-D func.
; INPUTS:
;       xa,xb,xc = scalars, 3 points which bracket location of minimum,
;               that is, f(xb) < f(xa) and f(xb) < f(xc), so minimum exists.
;               When working with function of N variables
;               (xa,xb,xc) are then relative distances from POINT_NDIM,
;               in the direction specified by keyword DIRECTION,
;               with scale factor given by magnitude of DIRECTION.
; INPUT KEYWORDS:
;      FUNC_NAME = function name (string)
;               Calling mechanism should be:  F = func_name( px )
;               where:
;                       px = scalar or vector of independent variables, input.
;                       F = scalar value of function at px.
;
;      POINT_NDIM = when working with function of N variables,
;               use this keyword to specify the starting point in N-dim space.
;               Default = 0, which assumes function is 1-D.
;      DIRECTION = when working with function of N variables,
;               use this keyword to specify the direction in N-dim space
;               along which to bracket the local minimum, (default=1 for 1-D).
;               (xa, xb, xc, x_min are then relative distances from POINT_NDIM)
;      MAX_ITER = maximum allowed number iterations, default=100.
;      TOLERANCE = desired accuracy of minimum location, default=sqrt(1.e-7).
; OUTPUTS:
;       xmin = estimated location of minimum.
;               When working with function of N variables,
;               xmin is the relative distance from POINT_NDIM,
;               in the direction specified by keyword DIRECTION,
;               with scale factor given by magnitude of DIRECTION,
;               so that min. Loc. Pmin = Point_Ndim + xmin * Direction.
;       fmin = value of function at xmin (or Pmin).
; PROCEDURE:
;       Brent's method to minimize a function by using parabolic interpolation.
;       Based on function BRENT in Numerical Recipes in FORTRAN (Press et al. 
;       1992),  sec.10.2 (p. 397).
; MODIFICATION HISTORY:
;       Written, Frank Varosi NASA/GSFC 1992.
;       Converted to IDL V5.0   W. Landsman   September 1997
;-
        zeps = 1.e-7                    ;machine epsilon, smallest addition.
        goldc = 1 - (sqrt(5)-1)/2       ;complement of golden mean.

        if N_elements( TOL ) NE 1 then TOL = sqrt( zeps )
        if N_elements( maxit ) NE 1 then maxit = 100

        if N_elements( pn ) LE 0 then begin
                pn = 0
                dirn = 1
           endif

        xLo = xa < xc
        xHi = xa > xc
        xmin = xb
        fmin = call_function( func_name, pn + xmin * dirn )
        xv = xmin  &  xw = xmin
        fv = fmin  &  fw = fmin
        es = 0.

        for iter = 1,maxit do begin

                goldstep = 1
                xm = (xLo + xHi)/2.
                TOL1 = TOL * abs(xmin) + zeps
                TOL2 = 2*TOL1

                if ( abs( xmin - xm ) LE ( TOL2 - (xHi-xLo)/2. ) ) then return

                if (abs( es ) GT TOL1) then begin

                        r = (xmin-xw) * (fmin-fv)
                        q = (xmin-xv) * (fmin-fw)
                        p = (xmin-xv) * q + (xmin-xw) * r
                        q = 2 * (q-r)
                        if (q GT 0) then p = -p
                        q = abs( q )
                        etemp = es
                        es = ds

                        if (p GT q*(xLo-xmin)) AND $
                           (p LT q*(xHi-xmin)) AND $
                           (abs( p ) LT abs( q*etemp/2 )) then begin
                                ds = p/q
                                xu = xmin + ds
                                if (xu-xLo LT TOL2) OR (xHi-xu LT TOL2) then $
                                        ds = TOL1 * (1-2*((xm-xmin) LT 0))
                                goldstep = 0
                           endif
                   endif

                if (goldstep) then begin
                        if (xmin GE xm) then  es = xLo-xmin  else  es = xHi-xmin
                        ds = goldc * es
                   endif

                xu = xmin + (1-2*(ds LT 0)) * ( abs( ds ) > TOL1 )
                fu = call_function( func_name, pn + xu * dirn )

                if (fu LE fmin) then begin

                        if (xu GE xmin) then xLo=xmin else xHi=xmin
                        xv = xw  &  fv = fw
                        xw = xmin  &  fw = fmin
                        xmin = xu  &  fmin = fu
                
                  endif else begin

                        if (xu LT xmin) then xLo=xu else xHi=xu

                        if (fu LE fw) OR (xw EQ xmin) then begin

                                xv = xw  &  fv = fw
                                xw = xu  &  fw = fu

                          endif else if (fu LE fv) OR (xv EQ xmin) $
                                                   OR (xv EQ xw) then begin
                                xv = xu  &  fv = fu
                           endif
                   endelse
          endfor

        message,"exceeded maximum number of iterations: "+strtrim(iter,2),/INFO
return
end

PRO quadterp, xtab, ytab, xint, yint, MISSING = MISSING
;+
; NAME:
;       QUADTERP     
; PURPOSE:
;       Quadratic interpolation of X,Y vectors onto a new X grid
; EXPLANATION:
;       Interpolate a function Y = f(X) at specified grid points using an
;       average of two neighboring 3 point quadratic (Lagrangian) interpolants.
;       Use LINTERP for linear interpolation
;
; CALLING SEQUENCE:
;       QUADTERP, Xtab, Ytab, Xint, Yint, [ MISSING = ]
;
; INPUT: 
;       Xtab - Vector (X TABle) containing the current independent variable 
;               Must be either monotonic increasing or decreasing
;       Ytab - Vector (Y TABle) containing the dependent variable defined
;               at each of the points of XTAB.
;       Xint - Scalar or vector giving the values of X for which interpolated 
;               Y values are sought
;
; OUTPUT: 
;       Yint - Interpolated value(s) of Y, same number of points as Xint
;
; OPTIONAL INPUT KEYWORD:
;       MISSING - Scalar specifying Yint value(s) to be assigned, when Xint
;               value(s) are outside of the range of Xtab.     Default is to
;               truncate the out of range Yint value(s) to the nearest value 
;               of Ytab.   See the help for the INTERPOLATE function.
; METHOD:
;       3-point Lagrangian interpolation.  The average of the two quadratics 
;       derived from the four nearest  points is returned in YTAB.   A single
;       quadratic is used near the end points.   VALUE_LOCATE is used 
;       to locate center point of the interpolation.
;
; NOTES:
;       QUADTERP provides one method of high-order interpolation.   The 
;           RSI interpol.pro function includes the following alternatives:
;
;       interpol(/LSQUADRATIC) - least squares quadratic fit to a 4 pt 
;               neighborhood
;       interpol(/QUADRATIC) - quadratic fit to a 3 pt neighborhood
;       interpol(/SPLINE) - cubic spline fit to a 4 pt neighborhood
;
;       Also, the IDL Astro function HERMITE fits a cubic polynomial and its
;             derivative to the two nearest points. 
; RESTRICTIONS:
;       Unless MISSING keyword is set, points outside the range of Xtab in 
;       which valid quadratics can be computed are returned at the value 
;       of the nearest end point of Ytab (i.e. Ytab[0] and Ytab[NPTS-1] ).
;
; EXAMPLE:
;       A spectrum has been defined using a wavelength vector WAVE and a
;       flux vector FLUX.  Interpolate onto a new wavelength grid, e.g. 
;
;       IDL> wgrid = [1540.,1541.,1542.,1543.,1544.,1545.]
;       IDL> quadterp, wave, flux, wgrid, fgrid 
;     
;       FGRID will be a 5 element vector containing the quadratically
;       interpolated values of FLUX at the wavelengths given in WGRID.
;
;  EXTERNAL ROUTINES:
;       ZPARCHECK
;  REVISION HISTORY:
;       31 October 1986 by B. Boothman, adapted from the IUE RDAF
;       12 December 1988 J. Murthy, corrected error in Xint
;       September 1992, W. Landsman, fixed problem with double precision
;       August 1993, W. Landsman, added MISSING keyword
;       June, 1995, W. Landsman, use single quadratic near end points
;       Converted to IDL V5.0   W. Landsman   September 1997
;       Fix occasional problem with integer X table,  
;       YINT is a scalar if XINT is a scalar   W. Landsman Dec 1999
;       Use VALUE_LOCATE instead of TABINV W. Landsman  Feb. 2000
;-
 On_error,2
 compile_opt idl2

 if N_params() LT 4 then begin
     print,'Syntax - QUADTERP, xtab, ytab, xint, yint, [ MISSING = ]'
     return
 endif

 zparcheck,'QUADTERP',xtab,1,[1,2,3,4,5],1,'Independent (X) vector'
 zparcheck,'QUADTERP',ytab,2,[1,2,3,4,5],1,'Dependent (Y) vector'

 npts = min( [N_elements(xtab), N_elements(ytab) ] )
 m = n_elements(xint)

 if size(xtab,/TNAME) NE 'DOUBLE' then xt = float(xtab) else xt = xtab
 
 Xmin = min( [ Xtab[0],Xtab[npts-1] ], max = Xmax)
 u = xint > Xmin < Xmax 

 if npts LT 3 then  $
     message,' ERROR - At least 3 points required for quadratic interpolation'

; Determine index of data-points from which interpolation is made 

        index = value_locate(xtab,xint) > 0L < (npts-2)

; First quadratic   
 
        i0 = (index-1) > 0   & i1 = i0+1 & i2 = (i1 +1)
        x0  = xt[i0]  & x1 = xt[i1] & x2 = xt[i2]
        p1 = ytab[i0] * (u-x1) * (u-x2) / ((x0-x1) * (x0-x2)) + $
             ytab[i1] * (u-x0) * (u-x2) / ((x1-x0) * (x1-x2)) + $
             ytab[i2] * (u-x0) * (u-x1) / ((x2-x0) * (x2-x1))

; Second Quadratic

        i2 = (index+2) < (npts-1) & i1 = i2-1 & i0 = (i1-1)
        x0  = xt[i0]  & x1 = xt[i1] & x2 = xt[i2]
        p2 =  ytab[i0] * (u-x1) * (u-x2) / ((x0-x1) * (x0-x2)) + $
              ytab[i1] * (u-x0) * (u-x2) / ((x1-x0) * (x1-x2)) + $
              ytab[i2] * (u-x0) * (u-x1) / ((x2-x0) * (x2-x1))
  
  
      yint = (p1 + p2) / 2.    ;Average of two quadratics

  if N_elements(missing) EQ 1 then begin
        bad = where( (Xint LT Xmin) or (Xint GT Xmax ), Nbad)
        if Nbad GT 0 then Yint[bad] = missing
  endif
 

 return
 end

FUNCTION TSUM,X,Y,IMIN,IMAX, NAN=NAN              ;Trapezoidal summation
;+
; NAME:
;       TSUM
; PURPOSE:
;       Trapezoidal summation of the area under a curve. 
; EXPLANATION:
;       Adapted from the procedure INTEG in the IUE procedure library.  
;
; CALLING SEQUENCE:
;       Result = TSUM(y)
;              or
;       Result = TSUM( x, y, [ imin, imax, /nan ] )  
; INPUTS:
;       x = array containing monotonic independent variable.  If omitted, then
;               x is assumed to contain the index of the y variable.
;               x = lindgen( N_elements(y) ).
;       y = array containing dependent variable y = f(x)
;
; OPTIONAL INPUTS:
;       imin = scalar index of x array at which to begin the integration
;               If omitted, then summation starts at x[0].
;       imax = scalar index of x value at which to end the integration 
;               If omitted then the integration ends at x[npts-1].
;       nan: If set cause the routine to check for occurrences of the IEEE 
;                 floating-point values NaN or Infinity in the input data. 
;                 Elements with the value NaN or Infinity are treated as missing data
;
; OUTPUTS:
;       result = area under the curve y=f(x) between x[imin] and x[imax].
;
; EXAMPLE:
;       IDL> x = [0.0,0.1,0.14,0.3] 
;       IDL> y = sin(x)
;       IDL> print,tsum(x,y)    ===>  0.0445843
;       
;       In this example, the exact curve can be computed analytically as 
;       1.0 - cos(0.3) = 0.0446635     
; PROCEDURE:
;       The area is determined of individual trapezoids defined by x[i],
;       x[i+1], y[i] and y[i+1].
;
;       If the data is known to be at all smooth, then a more accurate
;       integration can be found by interpolation prior to the trapezoidal
;       sums, for example, by the standard IDL User Library int_tabulated.pro.
; MODIFICATION HISTORY:
;       Written, W.B. Landsman, STI Corp. May 1986
;       Modified so X is not altered in a one parameter call Jan 1990
;       Converted to IDL V5.0   W. Landsman   September 1997
;       Allow non-integer values of imin and imax  W. Landsman April 2001
;       Fix problem if only 1 parameter supplied W. Landsman June 2002
;       Added /nan keyword. Julio Castro/WL May 2014
;-
; Set default parameters
 On_error,2
 npar = N_params()
   
 if npar EQ 1 then begin
    npts = N_elements(x)
    yy = x
    xx = lindgen(npts)
    ilo = 0   & imin = ilo
    ihi = npts-1 & imax = ihi
 endif else begin

   if ( npar LT 3 ) then imin = 0
   npts = min( [N_elements(x), N_elements(y)] )
   if ( npar LT 4 ) then imax = npts-1
   ilo = long(imin)
   ihi = long(imax)
   xx = x[ilo:ihi]
   yy = y[ilo:ihi]
   npts = ihi - ilo + 1
 endelse   
; 
;  Remove NaN values
;
   if keyword_set(NaN) then begin 
   g = where(finite(yy),npts)
   yy = yy[g]
   xx = xx[g]
  endif          
;   
; Compute areas of trapezoids and sum result
;
  xdif = xx[1:*] - xx
  yavg =  ( yy[0:npts-2] + yy[1:npts-1] ) / 2.  
  sum = total( xdif*yavg ) 

; Now account for edge effects if IMIN or IMAX parameter are not integers

  hi = imax - ihi
  lo = imin - ilo
  if (ihi LT imax) then sum +=  (x[ihi+1]-x[ihi])*hi* $
              (y[ihi] + (hi/2.) *(y[ihi+1] - y[ihi]) )
  if (ilo LT imin) then sum -=  (x[ilo+1]-x[ilo])*lo* $
              (y[ilo] + (lo/2.) *(y[ilo+1] - y[ilo]) )
  return, sum

  end     

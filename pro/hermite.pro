function hermite,xx,ff,x, FDERIV = fderiv
;+
; NAME:
;       HERMITE
; PURPOSE:
;       To compute Hermite spline interpolation of a tabulated function.
; EXPLANATION:
;       Hermite interpolation computes the cubic polynomial that agrees with 
;       the tabulated function and its derivative at the two nearest 
;       tabulated points.   It may be preferable to Lagrangian interpolation 
;       (QUADTERP) when either (1) the first derivatives are known, or (2)
;       one desires continuity of the first derivative of the interpolated
;       values.    HERMITE() will numerically compute the necessary
;       derivatives, if they are not supplied.
;       
; CALLING SEQUENCE:
;       F = HERMITE( XX, FF, X, [ FDERIV = ])
;
; INPUT PARAMETERS:
;       XX - Vector giving tabulated X values of function to be interpolated
;               Must be either monotonic increasing or decreasing   
;       FF - Tabulated values of function, same number of elements as X
;       X -  Scalar or vector giving the X values at which to interpolate
;
; OPTIONAL INPUT KEYWORD:
;       FDERIV - function derivative values computed at XX.    If not supplied,
;               then HERMITE() will compute the derivatives numerically.
;               The FDERIV keyword is useful either when (1) the derivative
;               values are (somehow) known to better accuracy than can be 
;               computed numerically, or (2) when HERMITE() is called repeatedly
;               with the same tabulated function, so that the derivatives
;               need be computed only once.
;
; OUTPUT PARAMETER:
;       F - Interpolated values of function, same number of points as X
;
; EXAMPLE:
;       Interpolate the function 1/x at x = 0.45 using tabulated values
;       with a spacing of 0.1
;
;       IDL> x = findgen(20)*0.1 + 0.1
;       IDL> y = 1/x
;       IDL> print,hermite(x,y,0.45)         
;               This gives 2.2188 compared to the true value 1/0.45 = 2.2222
;
;       IDL> yprime = -1/x^2      ;But in this case we know the first derivatives
;       IDL> print,hermite(x,y,0.45,fderiv = yprime)
;             == 2.2219            ;and so can get a more accurate interpolation
; NOTES:
;       The algorithm here is based on the FORTRAN code discussed by 
;       Hill, G. 1982, Publ Dom. Astrophys. Obs., 16, 67.   The original 
;       FORTRAN source is U.S. Airforce. Surveys in Geophysics No 272. 
;
;       HERMITE() will return an error if one tries to interpolate any values 
;       outside of the range of the input table XX
; PROCEDURES CALLED:
;       None
; REVISION HISTORY:
;       Written,    B. Dorman (GSFC) Oct 1993, revised April 1996
;       Added FDERIV keyword,  W. Landsman (HSTX)  April 1996
;       Test for out of range values  W. Landsman (HSTX) May 1996
;       Converted to IDL V5.0   W. Landsman   September 1997
;       Use VALUE_LOCATE instead of TABINV   W. Landsman   February 2001
;-
   On_error,2 

   if N_Params() LT 3 then begin
        print,'Syntax:  f = HERMITE( xx, ff, x, [FDERIV = ] )'
        return,0
   endif

   n = N_elements(xx)           ;Number of knot points
   m = N_elements(x)            ;Number of points at which to interpolate

   l = value_locate(xx,x)       ;Integer index of interpolation points 

   bad = where( (l LT 0) or (l EQ n-1), Nbad)
        if Nbad GT 0 then message, 'ERROR - Valid interpolation range is ' + $
        strtrim(xx[0],2) + ' to ' + strtrim(xx[n-1],2)

   n1 = n - 1
   n2 = n - 2

   l1  = l + 1  
   l2 = l1 + 1
   lm1 = l - 1
   h1 = double(1./(xx[l] - xx[l1]))
   h2 = - h1

; If derivatives were not supplied, then compute numeric derivatives at the 
; two closest knot points

 if N_elements(fderiv) NE 0 then begin
        f2 = fderiv[l1]
        f1 = fderiv[l]

 endif else begin

   f1 = dblarr(m)
   f2 = dblarr(m)
   for i = 0,m-1 do begin
        if l[i] ne 0 then begin
           if l[i] lt n2 then begin
              f2[i] = (ff[l2[i]] - ff[l[i]])/(xx[l2[i]]-xx[l[i]])
           endif else begin
              f2[i] = (ff[n1] - ff[n2])/(xx[n1] - xx[n2])
           endelse
           f1[i] = ( ff[l1[i]] - ff[lm1[i]] )/( xx[l1[i]] - xx[lm1[i]] )
        endif else begin
           f1[i] = (ff[1] - ff[0])/(xx[1] - xx[0])
           f2[i] = (ff[2] - ff[0])/(xx[2] - xx[0])
        endelse
   endfor
 endelse
       
    xl1 = x - xx[l1]
    xl  = x - xx[l]
    s1  = xl1*h1
    s2  = xl*h2

; Now finally the Hermite interpolation formula

    f   = (ff[l]*(1.-2.*h1*xl) + f1*xl)*s1*s1 + $                       
                                        (ff[l1]*(1.-2.*h2*xl1) + f2*xl1)*s2*s2

    if m eq 1 then return,f[0] else return,f

    end


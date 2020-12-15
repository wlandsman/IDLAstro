pro linterp, Xtab, Ytab, Xint, Yint, MISSING = missing, NoInterp = NoInterp
;+
; NAME:   
;       LINTERP  
; PURPOSE: 
;       Linearly interpolate tabulated 1-d data from one grid to a new one.
; EXPLANATION:
;       The results of LINTERP are numerically equivalent to the IDL intrinsic
;       INTERPOL() function, but note the following:
;         (1) LINTERP is a procedure rather than a function
;         (2) INTERPOL() extrapolates beyond the end points whereas LINTERP
;             truncates to the endpoints (or uses the MISSING keyword)
;         (3) LINTERP (unlike INTERPOL) uses the intrinsic INTERPOLATE function
;                 and thus may have a speed advantage
;         (4) Prior to V8.2.3 LINTERP converted the new grid vector to floating point 
;                (because INTERPOLATE does this) whereas INTERPOL() and post-V8.2.3 
;                LINTERP will keep double precision if supplied.
;
;       Use QUADTERP for quadratic interpolation.
;
; CALLING SEQUENCE:
;       LINTERP, Xtab, Ytab, Xint, Yint, [MISSING =, /NoInterp ]   
;
; INPUT PARAMETERS: 
;       Xtab -  Vector containing the current independent variable grid.
;               Must be monotonic increasing or decreasing
;       Ytab -  Vector containing the current dependent variable values at 
;               the XTAB grid points.
;       Xint -  Scalar or vector containing the new independent variable grid 
;               points for which interpolated value(s) of the dependent 
;               variable are sought.    Note that -- due to a limitation of the
;               intrinsic INTERPOLATE() function -- Xint is always converted to
;               floating point internally.
;
; OUTPUT PARAMETERS:
;       Yint  -  Scalar or vector with the interpolated value(s) of the 
;               dependent variable at the XINT grid points.
;               YINT is double precision if XTAB or YTAB are double,
;               otherwise YINT is float
;
; OPTIONAL INPUT KEYWORD:
;       MISSING - Scalar specifying YINT value(s) to be assigned, when Xint
;               value(s) are outside of the range of Xtab.     Default is to
;               truncate the out of range YINT value(s) to the nearest value 
;               of YTAB.   See the help for the INTERPOLATE function.
;       /NoINTERP - If supplied then LINTERP returns the YTAB value(s) 
;               associated with the closest XTAB value(s)rather than 
;               interpolating.
;
; EXAMPLE:
;       To linearly interpolate from a spectrum wavelength-flux pair
;       Wave, Flux to another wavelength grid defined as:
;       WGrid = [1540., 1541., 1542., 1543., 1544, 1545.]
;   
;       IDL>  LINTERP, Wave, Flux, WGrid, FGrid  
;
;       FGRID will be a 6 element vector containing the values of Flux
;       linearly interpolated onto the WGrid wavelength scale
;
; PROCEDURE: 
;       Uses TABINV to calculate the effective index of the values
;       in Xint in the table Xtab.  The resulting index is used
;       with the intrinsic INTERPOLATE function to find the corresponding 
;       Yint value in Ytab.  Unless the MISSING keyword is supplied, out
;       of range Yint values are truncated to the nearest value of Ytab.
;
; PROCEDURES CALLED:
;       TABINV, ZPARCHECK
; MODIFICATION HISTORY:
;       Adapted from the IUE RDAF,  W. Landsman      October, 1988
;       Modified to use the new INTERPOLATE function        June, 1992
;       Modified to always return REAL*4             October, 1992
;       Added MISSING keyword                        August, 1993
;       Converted to IDL V5.0   W. Landsman   September 1997
;       Added NoInterp keyword  W. Landsman      July 1999
;       Work for unsigned, 64 bit integers  W. Landsman  October 2001
;       Call INTERPOLATE with /DOUBLE if V8.2.3 W. Landsman Feb 2015
;-
 On_error,2
 compile_opt idl2

 if N_params() LT 4 then begin
   print,'Syntax - LINTERP, Xtab, Ytab, Xint, Yint, [ MISSING = ]' 
   print,'    Xtab, Ytab - Input X and Y vectors'
   print,'    Xint - Input X value (scalar or vector) at which to interpolate'
   print,'    Yint - Output interpolated Y value(s)'
   return
 endif

 numeric = [indgen(5)+1,12,13,14,15]      ;Numeric datatypes
 zparcheck, 'LINTERP', Xtab, 1, numeric, 1, 'Current X Vector' 
 zparcheck, 'LINTERP', Ytab, 2, numeric, 1, 'Current Y Vector' 
 zparcheck, 'LINTERP', Xint, 3, numeric, [0,1], 'New X Vector or Scalar'

; Determine index of data-points from which interpolation is made

 npts = min( [ N_elements(Xtab), N_elements(Ytab) ] )
 tabinv, Xtab, Xint, r                                    
 if keyword_set(NoInterp) then Yint = Ytab[round(r)] else begin
 ytype = size( Ytab, /TYPE)

; Perform linear interpolation

 if (ytype LE 3) || (ytype GE 12) then  $             ;Integer or byte input?
     Yint = interpolate( float(Ytab), r) else $
     if !VERSION.RELEASE GE '8.2.3' then $
     Yint = interpolate( Ytab, r, DOUBLE = (ytype EQ 5) ) else $
     Yint = interpolate( Ytab, r)

 endelse 

 if N_elements(missing) EQ 1 then begin
        Xmin = min( [ Xtab[0],Xtab[npts-1] ], max = Xmax)
        bad = where( (Xint LT Xmin) or (Xint GT Xmax ), Nbad)
        if Nbad GT 0 then Yint[bad] = missing
 endif
        
 return
 end                                        

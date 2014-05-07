PRO TABINV, XARR, X, IEFF, FAST = fast
;+ 
; NAME:
;       TABINV     
; PURPOSE:  
;       To find the effective index of a function value in an ordered vector.
;       
; CALLING SEQUENCE:
;       TABINV, XARR, X, IEFF, [/FAST]
; INPUTS:
;       XARR - the vector array to be searched, must be monotonic
;               increasing or decreasing
;       X    - the function value(s) whose effective
;               index is sought (scalar or vector)
;
; OUTPUT:
;       IEFF - the effective index or indices of X in XARR
;              always floating point, same # of elements as X
;
; OPTIONAL KEYWORD INPUT:
;       /FAST - If this keyword is set, then the input vector is not checked
;               for monotonicity, in order to improve the program speed.
; RESTRICTIONS:
;       TABINV will abort if XARR is not monotonic.  (Equality of 
;       neighboring values in XARR is allowed but results may not be
;       unique.)  This requirement may mean that input vectors with padded
;       zeroes could cause routine to abort.
;
; PROCEDURE:
;       VALUE_LOCATE() is used to find the values XARR[I]
;       and XARR[I+1] where XARR[I] < X < XARR[I+1].
;       IEFF is then computed using linear interpolation 
;       between I and I+1.
;               IEFF = I + (X-XARR[I]) / (XARR[I+1]-XARR[I])
;       Let N = number of elements in XARR
;               if x < XARR[0] then IEFF is set to 0
;               if x > XARR[N-1] then IEFF is set to N-1
;
; EXAMPLE:
;       Set all flux values of a spectrum (WAVE vs FLUX) to zero
;       for wavelengths less than 1150 Angstroms.
;         
;       IDL> tabinv, wave, 1150.0, I
;       IDL> flux[ 0:fix(I) ] = 0.                         
;
; FUNCTIONS CALLED:
;       None
; REVISION HISTORY:
;       Adapted from the IUE RDAF                     January, 1988         
;       More elegant code  W. Landsman                August, 1989
;       Mod to work on 2 element decreasing vector    August, 1992
;       Updated for V5.3 to use VALUE_LOCATE()     W. Landsman January 2000
;       Work when both X and Xarr are integers     W. Landsman August 2001
;       Use ARRAY_EQUAL, always internal double precision W.L.  July 2009
;       Allow Double precision output, faster test for monotonicity.
;                    WL, January 2012
;-               
 On_error,2
 compile_opt idl2

 if N_params() LT 3 then begin
     print,'Syntax- TABINV, XARR, X, I, [/FAST]'
     return
 endif

 Npoints = N_elements(xarr) & npt= npoints - 1
 if ( Npoints LE 1 ) then message, /TRACE, $
   'Search vector (first parameter) must contain at least 2 elements'

 do_double= (size(xarr,/tname) EQ 'DOUBLE') || (size(x,/TNAME) EQ 'DOUBLE')

 if ~keyword_set(fast) then begin

 ; Test for monotonicity (everywhere increasing or decreasing vector)

  i = xarr[1:*] GE xarr
   test = array_equal( i, 1b) || array_equal(i, 0b) 
     if ~test then  message, $
       'ERROR - First parameter must be a monotonic vector' 
 endif

 if do_double then ieff = double( VALUE_LOCATE(xarr,x)) else $
                   ieff = float(  VALUE_LOCATE(xarr,x))
 g = where( (ieff LT npt) and (ieff GE 0), Ngood)
 if Ngood GT 0 then begin
      neff = ieff[g]
      x0 = double(xarr[neff])
      diff =  x[g] - x0
      ieff[g] = neff +  diff / (xarr[neff+1] - x0 ) 
 endif
     
 ieff = ieff > 0.0

 return
 end

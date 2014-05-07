FUNCTION CONS_DEC,DEC,X,ASTR,ALPHA        ;Find line of constant Dec
;+
; NAME:
;       CONS_DEC
; PURPOSE:
;       Obtain the X and Y coordinates of a line of constant declination
; EXPLANATION:
;       Returns a set of Y pixels values, given an image with astrometry, and 
;            either
;       (1)  A set of X pixel values, and a scalar declination value, or
;       (2)  A set of declination values, and a scalar X value
;
;       Form (1) can be used to find the (X,Y) values of a line of constant
;       declination.  Form (2) can be used to find the Y positions of a set
;       declinations, along a line of constant X.
;
; CALLING SEQUENCE:
;       Y = CONS_DEC( DEC, X, ASTR, [ ALPHA ])
;
; INPUTS:
;       DEC - Declination value(s) in DEGREES (-!PI/2 < DEC < !PI/2).  
;               If X is a vector, then DEC must be a scalar.
;       X -   Specified X pixel value(s) for line of constant declination 
;               If DEC is a vector, then X must be a scalar.
;       ASTR - Astrometry structure, as extracted from a FITS header by the
;               procedure EXTAST
; OUTPUT:
;       Y   - Computed set of Y pixel values.  The number of Y values is the
;               same as either DEC or X, whichever is greater.
;
; OPTIONAL OUTPUT:
;       ALPHA - the right ascensions (DEGREES) associated with the (X,Y) points
;
; RESTRICTIONS:
;       Implemented only for the TANgent, SIN and CAR projections
;
; NOTES:
;       The algorithm (and notation) is based on AIPS Memo 27 by Eric Greisen,
;       with modifications for a coordinate description (CD) matrix as 
;       described in Paper II of Greisen & Calabretta (2002, A&A, 395, 1077).
;       These documents are available from 
;       http://www.cv.nrao.edu/fits/documents/wcs/wcs.html
;
; REVISION HISTORY:
;       Written, Wayne Landsman  STX Co.                          April 1988
;       Use new astrometry structure,     W. Landsman    HSTX     Jan. 1994
;       Use CD matrix, add SIN projection   W. Landsman  HSTX     April, 1996
;       Converted to IDL V5.0   W. Landsman   September 1997
;       Fix case where DEC is scalar, X is vector   W. Landsman RITSS Feb. 2000
;       Fix possible sign error introduced Jan. 2000   W. Landsman  May 2000
;       Work for the CARee' projection W. Landsman   May 2003
;-
  On_error,2

  if N_params() lt 3 then begin
        print,'Syntax - Y = CONS_DEC( DEC, X, ASTR, [ALPHA] )'
        return, 0
  endif

  RADEG = 180.0D/!DPI
 
  n = N_elements(x)
  Ndec = N_elements(dec)
  crpix = astr.crpix -1.
  crval = astr.crval/RADEG
  cd =  astr.cd/RADEG
  cdelt = astr.cdelt

  A = -cd[0,0]*cdelt[0] 
  B = -cd[0,1]*cdelt[0] 
  C =  cd[1,0]*cdelt[1]
  D =  cd[1,1]*cdelt[1] 

  xx = x - crpix[0]          ;New coordinate origin
  sdel0 = sin(crval[1]) & cdel0 = cos(crval[1])

  ctype = strupcase( strmid(astr.ctype[0], 5,3))
  case ctype of 

'TAN': begin
  aa = d
  bb = (b*c-d*a)*xx*cdel0 + sdel0*b
  sign = 2*( aa GT 0 ) - 1 
  alpha = crval[0] + atan(bb/aa) + $ 
      sign * asin( tan(dec/RADEG)* ( (B*C-D*A)*xx*sdel0 - B*cdel0)/ $
        sqrt(aa^2+bb^2))
  end

'SIN': begin

  aa = d
  bb = b*sdel0
  sign = 2*( aa GT 0 ) - 1 

  denom =  cos(dec/RADEG)*sqrt(aa^2+bb^2)
  alpha = crval[0] + atan(bb/aa) + $ 
     sign * asin( ( (b*c-a*d)*xx - sin(dec/RADEG)*cdel0*b ) / denom )
  end

'CAR': begin
  alpha = crval[0] + (b*c -a*d)*xx   
  if (N_elements(alpha) EQ 1) and (Ndec GT 1) then $
        alpha = replicate(alpha[0],Ndec)
end

ELSE: message,'ERROR - Program only works for TAN, SIN and CAR projections'
  endcase

   alpha = alpha * RADEG

   if (N_elements(dec) EQ 1) and (n GT 1) then $
   ad2xy, alpha, replicate(dec, n) , astr, x1, y else $
   ad2xy, alpha, dec, astr, x1, y

  return,y
  end

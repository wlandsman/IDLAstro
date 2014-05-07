pro polint, xa, ya, x, y, dy
;+
; NAME:
;     POLINT
; PURPOSE:
;     Interpolate a set of N points by fitting a polynomial of degree N-1
; EXPLANATION:
;     Adapted from algorithm in Numerical Recipes, Press et al. (1992), 
;     Section 3.1.
;
; CALLING SEQUENCE
;     POLINT, xa, ya, x, y, [ dy ]
; INPUTS:
;     XA - X Numeric vector, all values must be distinct.   The number of
;          values in XA should rarely exceed 10 (i.e. a 9th order polynomial)
;     YA - Y Numeric vector, same number of elements
;     X - Numeric scalar specifying value to be interpolated 
;
; OUTPUT:
;     Y - Scalar, interpolated value in (XA,YA) corresponding to X
;
; OPTIONAL OUTPUT
;     DY - Error estimate on Y, scalar
;
; EXAMPLE:
;     Find sin(2.5) by polynomial interpolation on sin(indgen(10))
;
;               IDL> xa = indgen(10)
;               IDL> ya = sin( xa )
;               IDL> polint, xa, ya, 2.5, y ,dy
;             
;     The above method gives  y = .5988 & dy = 3.1e-4  a close
;     approximation to the actual sin(2.5) = .5985
;
; METHOD:
;     Uses Neville's algorithm to iteratively build up the correct
;     polynomial, with each iteration containing one higher order.
;
; REVISION HISTORY:
;     Written W. Landsman                 January, 1992
;     Converted to IDL V5.0   W. Landsman   September 1997
;-
 On_error,2

 if N_params() LT 4 then begin
     print,'Syntax - polint, xa, ya, x, y, [ dy ]'
     print,'     xa,ya - Input vectors to be interpolated'
     print,'     x - Scalar specifying point at which to interpolate'
     print,'     y - Output interpolated scalar value'
     print,'     dy - Optional error estimate on y'
     return
 endif

 N = N_elements( xa ) 
 if N_elements( ya ) NE N then message, $
      'ERROR - Input X and Y vectors must have same number of elements'

; Find the index of XA which is closest to X

 dif = min( abs(x-xa), ns )

 c = ya   &   d = ya
 y = ya[ns]
 ns = ns - 1

 for m = 1,n-1 do begin

    ho = xa[0:n-m-1] - x
    hp = xa[m:n-1] - x
    w = c[1:n-m] - d[0:n-m-1]
    den = ho - hp
    if min( abs(den) ) EQ 0 then message, $
           'ERROR - All input X vector values must be distinct'
    den = w / den
    d = hp * den
    c = ho * den
    if ( 2*ns LT n-m-1 ) then dy = c[ns+1] else begin
                  dy = d[ns]
                  ns = ns - 1
    endelse
    y = y + dy
 endfor

 return
 end

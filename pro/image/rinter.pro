FUNCTION RINTER, P, X, Y, DFDX, DFDY, INITIALIZE = initialize
;+
; NAME:
;      RINTER
; PURPOSE:
;      Cubic interpolation of an image at a set of reference points.
; EXPLANATION:
;      This interpolation program is equivalent to using the intrinsic 
;      INTERPOLATE() function with CUBIC = -0.5.   However,
;      RINTER() has two advantages: (1) one can optionally obtain the 
;      X and Y derivatives at the reference points, and (2) if repeated
;      interpolation is to be applied to an array, then some values can
;      be pre-computed and stored in Common.   RINTER() was originally  
;      for use with the DAOPHOT procedures, but can also be used for 
;      general cubic interpolation.
;
; CALLING SEQUENCE:
;      Z = RINTER( P, X, Y, [ DFDX, DFDY ] )
;               or
;      Z = RINTER(P, /INIT)
;
; INPUTS:                 
;      P  - Two dimensional data array, 
;      X  - Either an N element vector or an N x M element array,
;               containing X subscripts where cubic interpolation is desired.
;      Y -  Either an N element vector or an N x M element array, 
;               containing Y subscripts where cubic interpolation is desired.
;
; OUTPUT:
;      Z -  Result = interpolated vector or array.  If X and Y are vectors,
;              then so is Z, but if X and Y are arrays then Z will be also.
;              If P is DOUBLE precision, then so is Z, otherwise Z is REAL.
;
; OPTIONAL OUTPUT:
;      DFDX - Vector or Array, (same size and type as Z), containing the 
;               derivatives with respect to X
;      DFDY - Array containing derivatives with respect to Y
;
; OPTIONAL KEYWORD INPUT:
;     /INIT - Perform computations associated only with the input array (i.e. 
;             not with X and Y) and store in common.    This can save time if
;             repeated calls to RINTER are made using the same array.  
;        
; EXAMPLE:
;      suppose P is a 256 x 256 element array and X = FINDGEN(50)/2. + 100.
;      and Y = X.  Then Z will be a 50 element array, containing the
;      cubic interpolated points.
;
; SIDE EFFECTS:
;      can be time consuming.
;
; RESTRICTION:
;      Interpolation is not possible at positions outside the range of 
;       the array (including all negative subscripts), or within 2 pixel
;       units of the edge.  No error message is given but values of the 
;       output array are meaningless at these positions.
;
; PROCEDURE:
;       invokes CUBIC interpolation algorithm to evaluate each element
;       in Z at virtual coordinates contained in X and Y with the data
;       in P.                                                          
;
; COMMON BLOCKS:
;       If repeated interpolation of the same array is to occur, then
;       one can save time by initializing the common block RINTER.    
;
; REVISION HISTORY:
;       March 1988 written W. Landsman STX Co.
;       Checked for IDL Version 2, J. Isensee, September, 1990
;       Corrected call to HISTOGRAM, W. Landsman November 1990
;       Converted to IDL V5.0   W. Landsman   September 1997
;       Fix output derivatives for 2-d inputs, added /INIT W. Landsman May 2000
;       
;-
 On_error, 2     
 common rinter, c1, c2, c3, init         

 if (N_params() LT 3) and (NOT keyword_set(INIT)) then begin     
        print, 'Syntax:  Z = RINTER( P, X, Y, [ DFDX, DFDY] )  '
        print, '   or    Z = RINTER( P, /INIT)    to initialize common block
        print,'P -  Array to be interpolated' 
        print,'X -  Vector or array of X positions' 
        print,'Y -  Vector or array of Y Positions'
        print,'DFDX, DFDY - Optional output derivatives '
        return,0
 endif 

 c = size(p)
 if c[0] NE 2 then $    
        message,'Input array (first parameter) must be 2 dimensional'

 if keyword_set(initialize) then begin

; Don't use SHIFT function to avoid wraparound at the end points

      nx = c[1]
      p_1 = p & p1 = p & p2 = p
      p_1[1,0] = p[0:nx-2,*]
      p1[0,0] = p[1:*,*]
      p2[0,0] = p[2:*,*]
      c1 = 0.5*(p1 - p_1)
      c2 = 2.*p1 + p_1 - 0.5*(5.*p + p2)
      c3 = 0.5*(3.*(p-p1) + p2 - p_1)
      init = 1
      if N_params() LT 3 then return,0
 endif
 
 sx = size(x)
 npts = sx[sx[0]+2]
 c[3] = c[3] > 4                        ;Make sure output array at least REAL

 i = long( x[*] ) 
 j = long( y[*] )
 xdist = x[*] - i  
 ydist = y[*] - j
 x_1 = c[1]*(j-1) + i
 x0 = x_1 + c[1] 
 x1 = x0 + c[1] 
 x2 = x1 + c[1]

 if N_elements(init) EQ 0 then init = 0    ;Has COMMON block been initialized?

 if init EQ 0 then begin 

   xgood = [ x_1,x0,x1,x2 ]
   num = histogram( xgood, MIN=0)
   xgood = where( num GE 1 ) 
   p_1 = p[xgood-1] & p0 = p[xgood] & p1 = p[xgood+1] & p2 = p[xgood+2]
   c1 = p*0.  & c2 = c1 & c3 = c1
   c1[xgood] = 0.5*( p1 - p_1)
   c2[xgood] = 2.*p1 + p_1 - 0.5*(5.*p0 + p2)
   c3[xgood] = 0.5*(3.*(p0 - p1) + p2 - p_1)
 endif

 y_1 = xdist*( xdist*( xdist*c3[x_1] +c2[x_1]) + c1[x_1]) + p[x_1]
 y0 =  xdist*( xdist*( xdist*c3[x0] +c2[x0]) + c1[x0]) + p[x0]
 y1 =  xdist*( xdist*( xdist*c3[x1] +c2[x1]) + c1[x1]) + p[x1]
 y2 =  xdist*( xdist*( xdist*c3[x2] +c2[x2]) + c1[x2]) + p[x2]

 if N_params() GT 3 then begin
 
    dy_1 = xdist*(xdist*c3[x_1]*3. + 2.*c2[x_1]) + c1[x_1]
    dy0  = xdist*(xdist*c3[x0 ]*3. + 2.*c2[x0]) + c1[x0]
    dy1  = xdist*(xdist*c3[x1 ]*3. + 2.*c2[x1]) + c1[x1]
    dy2  = xdist*(xdist*c3[x2 ]*3. + 2.*c2[x2]) + c1[x2]
    d1 = 0.5*(dy1 - dy_1)
    d2 = 2.*dy1 + dy_1 - 0.5*(5.*dy0 +dy2)
    d3 = 0.5*( 3.*( dy0-dy1 ) + dy2 - dy_1)
    dfdx =  ydist*( ydist*( ydist*d3 + d2 ) + d1 ) + dy0

 endif

 d1 = 0.5*(y1 - y_1)
 d2 = 2.*y1 + y_1 - 0.5*(5.*y0 +y2)
 d3 = 0.5*(3.*(y0-y1) + y2 - y_1)
 z =  ydist*(ydist*(ydist*d3 + d2) + d1) + y0  
 if N_params() GT 3 then dfdy = ydist*(ydist*d3*3.+2*d2) + d1   
 
 if ( sx[0] EQ 2 ) then begin        ;Convert results to 2-D if desired

    z = reform(z,sx[1],sx[2] ) 
    if N_params() GT 3 then  begin      ;Create output derivative arrays?
          dfdx = reform(dfdx,sx[1],sx[2])
          dfdy = reform(dfdy,sx[1],sx[2])          
     endif

 endif

 return,z
 end                    

pro Bprecess, ra, dec, ra_1950, dec_1950, MU_RADEC = mu_radec,  $
                  PARALLAX = parallax,  RAD_VEL = rad_vel, EPOCH = epoch
;+
; NAME:
;       BPRECESS
; PURPOSE:
;       Precess positions from J2000.0 (FK5) to B1950.0 (FK4)
; EXPLANATION:
;       Calculates the mean place of a star at B1950.0 on the FK4 system from
;       the mean place at J2000.0 on the FK5 system.    
;
; CALLING SEQUENCE:
;       bprecess, ra, dec, ra_1950, dec_1950, [ MU_RADEC = , PARALLAX = 
;                                       RAD_VEL =, EPOCH =   ]
;
; INPUTS:
;       RA,DEC - Input J2000 right ascension and declination in *degrees*.
;               Scalar or N element vector
;
; OUTPUTS:
;       RA_1950, DEC_1950 - The corresponding B1950 right ascension and 
;               declination in *degrees*.    Same number of elements as
;               RA,DEC but always double precision.
;
; OPTIONAL INPUT-OUTPUT KEYWORDS
;       MU_RADEC - 2xN element double precision vector containing the proper 
;                  motion in seconds of arc per tropical *century* in right 
;                  ascension and declination.
;       PARALLAX - N_element vector giving stellar parallax (seconds of arc)
;       RAD_VEL  - N_element vector giving radial velocity in km/s
;
;       The values of MU_RADEC, PARALLAX, and RADVEL will all be modified
;       upon output to contain the values of these quantities in the
;       B1950 system.  The parallax and radial velocity will have a very 
;       minor influence on the B1950 position.   
;
;       EPOCH - scalar giving epoch of original observations, default 2000.0d
;           This keyword value is only used if the MU_RADEC keyword is not set.
; NOTES:
;       The algorithm is taken from the Explanatory Supplement to the 
;       Astronomical Almanac 1992, page 186.
;       Also see Aoki et al (1983), A&A, 128,263
;
;       BPRECESS distinguishes between the following two cases:
;       (1) The proper motion is known and non-zero
;       (2) the proper motion is unknown or known to be exactly zero (i.e.
;               extragalactic radio sources).   In this case, the reverse of 
;               the algorithm in Appendix 2 of Aoki et al. (1983) is used to 
;               ensure that the output proper motion is  exactly zero. Better 
;               precision can be achieved in this case by inputting the EPOCH 
;               of the original observations.
;
;       The error in using the IDL procedure PRECESS for converting between
;       B1950 and J1950 can be up to 12", mainly in right ascension.   If
;       better accuracy than this is needed then BPRECESS should be used.
;
;       An unsystematic comparison of BPRECESS with the IPAC precession 
;       routine (http://nedwww.ipac.caltech.edu/forms/calculator.html) always 
;       gives differences less than 0.15".
; EXAMPLE:
;       The SAO2000 catalogue gives the J2000 position and proper motion for
;       the star HD 119288.   Find the B1950 position. 
;
;       RA(2000) = 13h 42m 12.740s      Dec(2000) = 8d 23' 17.69''  
;       Mu(RA) = -.0257 s/yr      Mu(Dec) = -.090 ''/yr
;
;       IDL> mu_radec = 100D* [ -15D*.0257, -0.090 ]
;       IDL> ra = ten(13, 42, 12.740)*15.D 
;       IDL> dec = ten(8, 23, 17.69)
;       IDL> bprecess, ra, dec, ra1950, dec1950, mu_radec = mu_radec
;       IDL> print, adstring(ra1950, dec1950,2)
;               ===> 13h 39m 44.526s    +08d 38' 28.63"
;
; REVISION HISTORY:
;       Written,    W. Landsman                October, 1992
;       Vectorized, W. Landsman                February, 1994
;       Treat case where proper motion not known or exactly zero  November 1994
;       Handling of arrays larger than 32767   Lars L. Christensen, march, 1995
;       Fixed bug where A term not initialized for vector input 
;            W. Landsman        February 2000
;       Use V6.0 notation  W. Landsman Mar 2011
;       
;-   
  On_error,2
  compile_opt idl2

  if N_params() LT 4 then begin
     print,'Syntax - BPRECESS, ra,dec, ra_1950, dec_1950, [MU_RADEC =' 
     print,'                            PARALLAX = , RAD_VEL = ]'
     print,'  Input RA and Dec should be given in DEGREES for J2000'
     print,'  Proper motion, MU_RADEC, (optional) in arc seconds per *century*'
     print,'  Parallax (optional) in arc seconds'      
     print,'  Radial Velocity (optional) in km/s'
     return

  endif

  N = N_elements( ra )
  if N EQ 0 then message,'ERROR - First parameter (RA vector) is undefined'

  if ~keyword_set( RAD_VEL) then rad_vel = dblarr(N) else begin
        rad_vel = rad_vel*1.
        if N_elements( RAD_VEL) NE N then message, $
        'ERROR - RAD_VEL keyword vector must contain ' + strtrim(N,2) +' values'
  endelse

  if keyword_set( MU_RADEC) then begin
         if (N_elements( mu_radec) NE 2*N ) then message, $
    'ERROR - MU_RADEC keyword (proper motion) be dimensioned (2,' + $
                 strtrim(N,2) + ')'
        mu_radec = mu_radec*1.
  endif

  if ~keyword_set( Parallax) then parallax = dblarr(N) else $
        parallax = parallax*1.

  if ~keyword_set(Epoch) then epoch = 2000.0d0

  radeg = 180.D/!DPI
  sec_to_radian = 1.d0/radeg/3600.d0

 M =  [ [+0.9999256795D, -0.0111814828D, -0.0048590040D,  $
         -0.000551D,  -0.238560D,     +0.435730D     ], $
       [ +0.0111814828D, +0.9999374849D, -0.0000271557D,  $ 
         +0.238509D,     -0.002667D,      -0.008541D     ], $
       [ +0.0048590039D, -0.0000271771D, +0.9999881946D , $
         -0.435614D,      +0.012254D,      +0.002117D      ], $
       [ -0.00000242389840D, +0.00000002710544D, +0.00000001177742D, $
         +0.99990432D,    -0.01118145D,    -0.00485852D    ], $
       [ -0.00000002710544D, -0.00000242392702D, +0.00000000006585D, $
         +0.01118145D,     +0.99991613D,    -0.00002716D    ], $
       [ -0.00000001177742D, +0.00000000006585D,-0.00000242404995D, $
         +0.00485852D,   -0.00002717D,    +0.99996684D] ] 

 A_dot = 1D-3*[1.244D, -1.579D, -0.660D ]           ;in arc seconds per century

 ra_rad = ra/radeg       &      dec_rad = dec/radeg
 cosra =  cos( ra_rad )  &       sinra = sin( ra_rad )
 cosdec = cos( dec_rad ) &      sindec = sin( dec_rad )

 dec_1950 = dec*0.
 ra_1950 = ra*0.

 for i = 0L, N-1 do begin

; Following statement moved inside loop in Feb 2000.
 A = 1D-6*[ -1.62557D, -0.31919D, -0.13843D]        ;in radians

 r0 = [ cosra[i]*cosdec[i], sinra[i]*cosdec[i], sindec[i] ]

 if keyword_set(mu_radec) then begin

 mu_a = mu_radec[ 0, i ]
 mu_d = mu_radec[ 1, i ]
 r0_dot = [ -mu_a*sinra[i]*cosdec[i] - mu_d*cosra[i]*sindec[i] , $ ;Velocity vector
             mu_a*cosra[i]*cosdec[i] - mu_d*sinra[i]*sindec[i] , $
             mu_d*cosdec[i] ] + 21.095d * rad_vel[i] * parallax[i] * r0

 endif else r0_dot = [0.0d0, 0.0d0, 0.0d0]

  R_0 = [ r0, r0_dot ]
  R_1 =  M # R_0

 ; Include the effects of the E-terms of aberration to form r and r_dot.

 r1 = R_1[0:2]  
 r1_dot = R_1[3:5] 

 if ~keyword_set(Mu_radec) then begin
        r1 = r1 + sec_to_radian * r1_dot * (epoch - 1950.0d)/100.
        A = A + sec_to_radian * A_dot * (epoch - 1950.0d)/100.
 endif

 x1 = R_1[0]   &   y1 = R_1[1]    &  z1 = R_1[2]
 rmag = sqrt( x1^2 + y1^2 + z1^2 )


 s1 = r1/rmag    & s1_dot = r1_dot/rmag

 s = s1
 for j = 0,2 do begin
    r = s1 + A - (total(s * A))*s
    s = r/rmag
 endfor 
 x = r[0]          & y = r[1]     &  z = r[2]  
 r2 = x^2 + y^2 + z^2
 rmag = sqrt( r2 )
 
 if keyword_set(Mu_radec) then begin
         r_dot = s1_dot + A_dot - ( total( s * A_dot))*s
         x_dot = r_dot[0]  & y_dot= r_dot[1]  &  z_dot = r_dot[2]
         mu_radec[0,i] = ( x*y_dot - y*x_dot) / ( x^2 + y^2)
         mu_radec[1,i] = ( z_dot* (x^2 + y^2) - z*(x*x_dot + y*y_dot) ) /  $
                     ( r2*sqrt( x^2 + y^2) )
 endif

 dec_1950[i] = asin( z / rmag)
 ra_1950[i] = atan( y, x)

  if parallax[i] GT 0. then begin
      rad_vel[i] = ( x*x_dot + y*y_dot + z*z_dot )/ (21.095*Parallax[i]*rmag)
      parallax[i] = parallax[i] / rmag
  endif
 endfor

 neg = where( ra_1950 LT 0, NNeg )
 if Nneg GT 0 then ra_1950[neg] = ra_1950[neg] + 2.D*!DPI

 ra_1950 = ra_1950*radeg & dec_1950 = dec_1950*radeg

; Make output scalar if input was scalar

 sz = size(ra)
 if sz[0] EQ 0 then begin
        ra_1950 = ra_1950[0]     &      dec_1950 = dec_1950[0]
 endif

 return
 end

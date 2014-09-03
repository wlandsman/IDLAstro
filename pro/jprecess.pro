pro jprecess, ra, dec, ra_2000, dec_2000, MU_RADEC = mu_radec,  $
                  PARALLAX = parallax,  RAD_VEL = rad_vel, EPOCH = epoch
;+
; NAME:
;      JPRECESS
; PURPOSE:
;      Precess astronomical coordinates from B1950 to J2000
; EXPLANATION:
;      Calculate the mean place of a star at J2000.0 on the FK5 system from the
;      mean place at B1950.0 on the FK4 system.
;
;      Use BPRECESS for the reverse direction J2000 ==> B1950
; CALLING SEQUENCE:
;      jprecess, ra, dec, ra_2000, dec_2000, [ MU_RADEC = , PARALLAX = 
;               RAD_VEL =, EPOCH =   ]
;
; INPUTS:
;      RA,DEC - input B1950 right ascension and declination in *degrees*.
;               Scalar or vector
;
; OUTPUTS:
;      RA_2000, DEC_2000 - the corresponding J2000 right ascension and 
;               declination in *degrees*.   Same number of elements as RA,DEC
;               but always double precision. 
;
; OPTIONAL INPUT-OUTPUT KEYWORDS
;      MU_RADEC - 2xN element double precision vector containing the proper 
;                  motion in seconds of arc per tropical *century* in right 
;                  ascension and declination.
;      PARALLAX - N_element vector giving stellar parallax (seconds of arc)
;      RAD_VEL  - N_element vector giving radial velocity in km/s
;
;       The values of MU_RADEC, PARALLAX, and RADVEL will all be modified
;       upon output to contain the values of these quantities in the
;       J2000 system.    Values will also be converted to double precision.  
;       The parallax and radial velocity will have a very minor influence on 
;       the J2000 position.
;
;       EPOCH - scalar giving epoch of original observations, default 1950.0d
;           This keyword value is only used if the MU_RADEC keyword is not set.
;  NOTES:
;       The algorithm is taken from the Explanatory Supplement to the 
;       Astronomical Almanac 1992, page 184.
;       Also see Aoki et al (1983), A&A, 128,263
;
;       JPRECESS distinguishes between the following two cases:
;       (1) The proper motion is known and non-zero
;       (2) the proper motion is unknown or known to be exactly zero (i.e.
;               extragalactic radio sources).   In this case, the algorithm
;               in Appendix 2 of Aoki et al. (1983) is used to ensure that
;               the output proper motion is  exactly zero.    Better precision
;               can be achieved in this case by inputting the EPOCH of the
;               original observations.
;
;       The error in using the IDL procedure PRECESS for converting between
;       B1950 and J2000 can be up to 12", mainly in right ascension.   If
;       better accuracy than this is needed then JPRECESS should be used.
;
; EXAMPLE:
;       The SAO catalogue gives the B1950 position and proper motion for the 
;       star HD 119288.   Find the J2000 position. 
;
;          RA(1950) = 13h 39m 44.526s      Dec(1950) = 8d 38' 28.63''  
;          Mu(RA) = -.0259 s/yr      Mu(Dec) = -.093 ''/yr
;
;       IDL> mu_radec = 100D* [ -15D*.0259, -0.093 ]
;       IDL> ra = ten(13,39,44.526)*15.D 
;       IDL> dec = ten(8,38,28.63)
;       IDL> jprecess, ra, dec, ra2000, dec2000, mu_radec = mu_radec
;       IDL> print, adstring(ra2000, dec2000,2)
;               ===> 13h 42m 12.740s    +08d 23' 17.69"
;
; RESTRICTIONS:
;      "When transferring individual observations, as opposed to catalog mean
;       place, the safest method is to tranform the observations back to the
;       epoch of the observation, on the FK4 system (or in the system that was
;       used to to produce the observed mean place), convert to the FK5 system,
;       and transform to the the epoch and equinox of J2000.0" -- from the
;       Explanatory Supplement (1992), p. 180
;
; REVISION HISTORY:
;       Written,    W. Landsman                September, 1992
;       Corrected a couple of typos in M matrix   October, 1992
;       Vectorized, W. Landsman                   February, 1994
;       Implement Appendix 2 of Aoki et al. (1983) for case where proper
;       motion unknown or exactly zero     W. Landsman    November, 1994
;       Converted to IDL V5.0   W. Landsman   September 1997
;       Fixed typo in updating proper motion   W. Landsman   April 1999
;       Make sure proper motion is floating point  W. Landsman December 2000
;       Use V6.0 notation  W. Landsman Mar 2011
;-   
  On_error,2
  compile_opt idl2

  if N_params() LT 4 then begin
       print,'Syntax - JPRECESS, ra,dec, ra_2000, dec_2000, [MU_RADEC =' 
       print,'                            PARALLAX = , RAD_VEL = ]'
       print,'Input RA and Dec should be given in DEGREES for B1950'
       print,'Proper motion, MU_RADEC, (optional) in arc seconds per *century*'
       print,'Parallax (optional) in arc seconds'      
       print,'Radial Velocity (optional) in km/s'
       return

  endif

  N = N_elements( ra )
  if N EQ 0 then message,'ERROR - first parameter (RA vector) is undefined'

  if ~keyword_set( RAD_VEL) then rad_vel = dblarr(N) else begin
        rad_vel = rad_vel*1.
        if N_elements( RAD_VEL ) NE N then message, $
        'ERROR - RAD_VEL keyword vector must contain ' + strtrim(N,2) + ' values'
  endelse

  if N_elements( MU_RADEC) GT 0 then begin
         if (N_elements( mu_radec) NE 2*N ) then message, $
    'ERROR - MU_RADEC keyword (proper motion) be dimensioned (2,' + $
                 strtrim(N,2) + ')'
        mu_radec = mu_radec*1.      ;Make sure at least float
  endif

  if N_elements(epoch) EQ 0 then epoch = 1950.0d0

  if N_elements( Parallax) EQ 0 then parallax = dblarr(N) else $
        parallax = parallax*1.

  radeg = 180.D/!DPI
  sec_to_radian = 1./radeg/3600.0d0

 M =  [ [+0.9999256782D, +0.0111820610D, +0.0048579479D,  $
         -0.000551D,     +0.238514D,     -0.435623D     ], $
       [ -0.0111820611D, +0.9999374784D, -0.0000271474D,  $ 
         -0.238565D,     -0.002667D,      +0.012254D      ], $
       [ -0.0048579477D, -0.0000271765D, +0.9999881997D , $
         +0.435739D,      -0.008541D,      +0.002117D      ], $
       [ +0.00000242395018D, +0.00000002710663D, +0.00000001177656D, $
         +0.99994704D,    +0.01118251D,    +0.00485767D    ], $
       [ -0.00000002710663D, +0.00000242397878D, -0.00000000006582D, $
         -0.01118251D,     +0.99995883D,    -0.00002714D    ], $
       [ -0.00000001177656D, -0.00000000006587D, 0.00000242410173D, $
         -0.00485767D,   -0.00002718D,     1.00000956D] ] 

 A = 1D-6*[ -1.62557D, -0.31919D, -0.13843D]        ;in radians
 A_dot = 1D-3*[1.244D, -1.579D, -0.660D ]           ;in arc seconds per century

 if epoch NE 1950.0d then $
        A = A + sec_to_radian * A_dot * (epoch - 1950.0D)/100.0d

 ra_rad = ra/radeg       &      dec_rad = dec/radeg
 cosra =  cos( ra_rad )  &       sinra = sin( ra_rad )
 cosdec = cos( dec_rad ) &      sindec = sin( dec_rad )

 ra_2000 = ra*0.
 dec_2000 = dec*0.

 for i = 0l, N-1 do begin

 r0 = [ cosra[i]*cosdec[i], sinra[i]*cosdec[i], sindec[i] ]

  if ~keyword_set( MU_RADEC) then begin
        mu_a = 0.0d0
        mu_d = 0.0d0
  endif else begin
         if (N_elements( mu_radec) NE 2*N ) then message, $
    'ERROR - MU_RADEC keyword (proper motion) must be dimensioned (2,' + $
                strtrim(N,2) + ')'
         mu_a = mu_radec[ 0, i]
         mu_d = mu_radec[ 1, i ]
  endelse

 r0_dot = [ -mu_a*sinra[i]*cosdec[i] - mu_d*cosra[i]*sindec[i], $ ;Velocity vector
             mu_a*cosra[i]*cosdec[i] - mu_d*sinra[i]*sindec[i] , $
             mu_d*cosdec[i] ]  + 21.095 * rad_vel[i] * parallax[i] * r0

 ; Remove the effects of the E-terms of aberration to form r1 and r1_dot.

 r1 = r0 - A + (total(r0 * A))*r0
 r1_dot = r0_dot - A_dot + ( total( r0 * A_dot))*r0

 R_1 = [r1, r1_dot]         
 
 R = M # R_1

 if ~keyword_set(mu_RADEC) then begin
         rr = [ R[0], R[1], R[2]]
         v =  [ R[3],R[4],R[5] ]
         t = ((epoch - 1950.0d0) - 50.00021d)/100.0d0
         rr1 = rr + sec_to_radian*v*t
         x = rr1[0]  & y = rr1[1]  & Z = rr1[2]  
 endif else begin
         x = R[0]  & y = R[1]  & Z = R[2]  
         x_dot = R[3]  & y_dot= R[4]  &  z_dot = R[5]
 endelse

 r2 = x^2 + y^2 + z^2
 rmag = sqrt( r2 )
 dec_2000[i] = asin( z / rmag)
 ra_2000[i] = atan( y, x)

 if keyword_set(mu_RADEC) then begin
         mu_radec[0, i] = ( x*y_dot - y*x_dot) / ( x^2 + y^2)
         mu_radec[1, i] = ( z_dot* (x^2 + y^2) - z*(x*x_dot + y*y_dot) ) /  $
                     ( r2*sqrt( x^2 + y^2) )
 endif
 
  if parallax[i] GT 0. then begin
      rad_vel[i] = ( x*x_dot + y*y_dot + z*z_dot )/ (21.095*Parallax[i]*rmag)
      parallax[i] = parallax[i] / rmag

  endif
 endfor 

 neg = where( ra_2000 LT 0, NNeg )
 if Nneg GT 0 then ra_2000[neg] = ra_2000[neg] + 2.D*!DPI

 ra_2000 = ra_2000*radeg & dec_2000 = dec_2000*radeg

; Make output scalar if input was scalar

 sz = size(ra)
 if sz[0] EQ 0 then begin
        ra_2000 = ra_2000[0]     & dec_2000 = dec_2000[0]
 endif

 return
 end

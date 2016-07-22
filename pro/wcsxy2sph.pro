;+
; NAME:
;      WCSXY2SPH
;
; PURPOSE:
;      Convert x and y (map) coordinates to spherical coordinates
; EXPLANATION:
;      To convert x and y (map) coordinates to spherical (longitude and
;      latitude or sky) coordinates.    This procedure is the inverse of
;      WCSSPH2XY.
;
;     This is a lower level procedure -- given a FITS header, the user will
;     usually use XYAD which will then call WCSXY2SPH with the appropriate
;     parameters.
; CATEGORY:
;      Mapping and Auxilliary FITS Routine
;
; CALLING SEQUENCE:
;
;      wcsxy2sph, x, y, longitude, latitude, [map_type], [ CTYPE = ,$
;             FACE = , PV1 =, PV2 = ,CRVAL =, CRXY =, LONGPOLE=, LATPOLE=]
;
; INPUT PARAMETERS:
;
;       x - x coordinate of data, scalar or vector, in degrees, NOTE: x
;               increases to the left, not the right
;       y - y coordinate of data, same number of elements as x, in degrees
;       map_type - optional positional parameter, scalar corresponding to a
;               particular map projection.  This is not a FITS standard, it is
;               simply put in to allow function similar to that of less general
;               map projection procedures (eg AITOFF).  The following list gives
;               the map projection types and their respective numbers.
;
;  FITS  Number  Name                       Comments
;  code   code
;  ----  ------  -----------------------    -----------------------------------
;   DEF     0    Default = Plate Carree
;   AZP     1    Zenithal perspective       pv2_1 required
;   TAN     2    Gnomic                     AZP w/ pv2_1 = 0
;   SIN     3    Orthographic               pv2_1, pv2_2 optional
;   STG     4    Stereographic              AZP w/ pv2_1 = 1
;   ARC     5    Zenithal Equidistant
;   ZPN     6    Zenithal polynomial        PV2_0, PV2_1....PV2_20 possible
;   ZEA     7    Zenithal equal area
;   AIR     8    Airy                       pv2_1 required
;   CYP     9    Cylindrical perspective    pv2_1 and pv2_2 required
;   CAR    10    Plate Carree
;   MER    11    Mercator
;   CEA    12    Cylindrical equal area     pv2_1 required
;   COP    13    Conical perspective        pv2_1 and pv2_2 required
;   COD    14    Conical equidistant        pv2_1 and pv2_2 required
;   COE    15    Conical equal area         pv2_1 and pv2_2 required
;   COO    16    Conical orthomorphic       pv2_1 and pv2_2 required
;   BON    17    Bonne's equal area         pv2_1 required
;   PCO    18    Polyconic
;   SFL    19    Sanson-Flamsteed (GLS is allowed as a synonym for SFL)
;   PAR    20    Parabolic
;   AIT    21    Hammer-Aitoff
;   MOL    22    Mollweide
;   CSC    23    Cobe Quadrilateralized     inverse converges poorly
;                Spherical Cube
;   QCS    24    Quadrilateralized
;                Spherical Cube
;   TSC    25    Tangential Spherical Cube
;   SZP    26    Slant Zenithal perspective  PV2_1,PV2_2, PV2_3 optional
;   HPX    27    HEALPix projection          pv2_1 and pv2_2 optional
;   HCT    28    HealCart (Cartesian approximation of Healpix)
;   XPH    29    HEALPix butterfly projection (centred on a pole)
;
; OPTIONAL KEYWORD PARAMETERS:
;
;       CTYPE - One, two, or three element vector containing 8 character
;               strings corresponding to the CTYPE1, CTYPE2, and CTYPE3
;               FITS keywords:
;
;               CTYPE[0] - first four characters specify standard system
;               ('RA--','GLON' or 'ELON' for right ascension, galactic
;               longitude or ecliptic longitude respectively), second four
;               letters specify the type of map projection (eg '-AIT' for
;               Aitoff projection)
;               CTYPE[1] - first four characters specify standard system
;               ('DEC-','GLAT' or 'ELAT' for declination, galactic latitude
;               or ecliptic latitude respectively; these must match
;               the appropriate system of ctype1), second four letters of
;               ctype2 must match second four letters of ctype1.
;               CTYPE[2] - if present must be the 8 character string,'CUBEFACE',
;               only used for spherical cube projections to identify an axis
;               as containing the face on which each x and y pair of
;               coordinates lie.
;       FACE - a input variable used for spherical cube projections to
;               designate the face of the cube on which the x and y
;               coordinates lie.   Must contain the same number of elements
;               as X and Y.
;       CRVAL - 2 element vector containing standard system coordinates (the
;               longitude and latitude) of the reference point
;       CRXY - 2 element vector giving the x and y coordinates of the
;               reference point, if this is not set the offset of the x
;               coordinate is assumed to be 0. 
;               See Calabretta & Griesen Sec 2.5.
;       PV2  - Vector of projection parameter associated with latitude axis
;             PV2 will have up to 21 elements for the ZPN projection, up to 3
;             for the SIN projection and no more than 2 for any other
;             projection.   The first element corresponds to PV2_1, the
;             second to PV2_2, etc.

;    Parameters simply passed to WCS_ROTATE:
;  
;     CRVAL - 2 element vector containing standard system coordinates (the
;               longitude and latitude) of the reference point
;     PV1   - Vector of projection parameters associated with longitude
;     LONGPOLE -  native longitude of standard system's North Pole
;     LATPOLE  -  "target" native latitude of the standard system's North Pole
;
; OUTPUT PARAMETERS:
;
;       longitude - longitude of data, same number of elements as x, in degrees
;       latitude - latitude of data, same number of elements as x, in degrees
;
;       Longitude and latitude will be set to NaN, wherever elements of X,Y
;       have no corresponding longitude, latitude values.
; NOTES:
;       The conventions followed here are described in more detail in the paper
;      "Representations of Celestial Coordinates in FITS" by Calabretta &
;       Greisen (2002, A&A, 395, 1077, also see
;       http://fits.gsfc.nasa.gov/fits_wcs.html).   The general scheme
;       outlined in that article is to convert x and y coordinates into a
;       "native" longitude and latitude and then rotate the system into one of
;       three generally recognized systems (celestial, galactic or ecliptic).
;
;       This procedure necessitates two basic sections.  The first converts
;       x and y coordinates to "native" coordinates while the second converts
;       "native" to "standard" coordinates.  The first section contains the
;       guts of the code in which all of the map projection is done.  The
;       second step is performed by WCS_ROTATE and only involves rotation of
;       coordinate systems.  WCSXY2SPH can be called in a form similar to
;       AITOFF, EQPOLE, or QDCB by calling wcsxy2sph with a fifth parameter
;       specifying the map projection by number and by not using any of the
;       keywords related to the map projection type (eg ctype1 and ctyp2).
;
; PROCEDURE:
;       The first task of the procedure is to do general error-checking to
;       make sure the procedure was called correctly and none of the
;       parameters or keywords conflict.  This is particularly important
;       because the procedure can be called in two ways (either using
;       FITS-type keywords or using a number corresponding a map projection
;       type).  All variables are converted into double precision values.
;
;       The second task of the procedure is to take x and y coordinates and
;       convert them into "native" latitude and longitude coordinates.
;       Map-specific error-checking is done at this time.  All of the
;       equations were obtained from "Representations of Celestial
;       Coordinates in FITS" and cases needing special attention are handled
;       appropriately (see the comments with individual map projections for
;       more information on special cases).     WCS_ROTATE is then called to
;       convert the "native" coordinates to "standard" coordinates by rotating
;       the coordinate system.  This rotation is governed by the keywords
;       CRVAL, and LONGPOLE.  The transformation is a straightforward
;       application of euler angles.  Finally, longitude values are converted
;       into the range from 0 to 360 degrees.
;
; COMMON BLOCKS:
;       none
; PROCEDURES CALLED:
;       WCS_ROTATE
;
; ORIGINAL AUTHOR:
;
;       Rick Balsano  LANL    8/31/93   V1.1
;
; MODIFICATIONS/REVISION LEVEL:
;
; 1.2    9/12/93   W. Landsman Vectorized CRXY, CRVAL, CTYPE
; 1.3    29/12/93  I. Freedman Eliminated LU decomposition
; 1.4    22/09/94  W. Landsman If scalar input, then scalar output
; 1.5    02/03/05  W. Landsman Change variable name BETA for V4.0 compatibility
; 1.6    06/07/05  W. Landsman Change loop index from integer to long
; 1.7    02/18/99  W. Landsman Fixed implementation of ARC algorithm
; 1.8    June 2003 W. Landsman Update conic projections, add LATPOLE keyword
; 1.81   Sep 2003 W. Landsman Avoid divide by zero
; 1.82   Sep 2003 W. Landsman CTYPE keywords need not be 8 characters
; 1.83   Sep 2003 W. Landsman Preserve input array sizes
; 1.9    Jan 2004 W. Landsman don't modify scalars, fix PARabolic code
; 2.0    Feb 2004 W. Landsman Fix AIR and AZP projections
; 2.1    Feb 2004 W. Landsman Fix tangent projection for matrix input
; 3.0    May 2004  W. Landsman Support extended SIN (=NCP), slant zenithal
;                  (SZP), and zenithal polynomial (ZPN) projections, use
;                   PV2 keyword vector instead of PROJP1, PROJP2
; 3.1    May 2004 W. Landsman/J. Ballet Handle NaN values, flag invalid output
;                   for AITOFF projection
; 3.1.1  Dec 2005 W. Landsman/W. Thompson Fixed problem with Airy projection
;                   centered on 90 degree latitude
; 3.1.2  May 2006 W. Landsman/Y.Sato Fix problem selecting the correct root
;                    for the ZPN projection
; 3.2    Aug 2007  W. Landsman Correct treatment of PVi_j parameters
; 3.3    Oct 2007  Sergey Koposov Support HEALPIX projection
; 3.4    May 2012  Benjamin Alan Weaver, Add nonstandard HEALCART 
;                        projection, Allow map_index to be > 25
; 3.4.1  May 2013  W. Landsman Allow GLS as a synonym for SFL 
; 3.5    Jul 2013  J. P. Leahy: Add nonstandard XPH projection and 
;                        improved HEALPix support; changed sign of CRXY
;                        for consistency with WCSSPH2XY; introduced PV1
; 3.5.1  Dec 2013  W. Landsman Return scalar for scalar input for ZPN proj.
;-
PRO wcsxy2sph, x, y, longitude, latitude, map_type, ctype = ctype, $
              face=face, pv2 = pv2, pv1 = pv1, $
              crval=crval, crxy = crxy, longpole = longpole, Latpole = latpole

 compile_opt idl2, hidden
 
; Define angle constants
 pi = !DPI
 radeg = 180d0/!dpi
 pi2 = pi/2.0d
 ;
 ; Please keep the following list up to date in extast as well as here
 ; and in wcssph2xy:
 ;
 map_types=['DEF','AZP','TAN','SIN','STG','ARC','ZPN','ZEA','AIR','CYP',$
            'CAR','MER','CEA','COP','COD','COE','COO','BON','PCO','SFL',$
            'PAR','AIT','MOL','CSC','QSC','TSC','SZP','HPX','HCT','XPH']

; check to see that enough parameters (at least 4) were sent
 if ( N_params() lt 4 ) then begin
    print,'Syntax - WCSXY2SPH, x, y, longitude, latitude,[ map_type,  '
    print,'             CTYPE= , FACE=, PV2= , CRVAL= , CRXY= , '
    print,'             LATPOLE = , LONGPOLE = ]'
    return
 endif else if (n_params() eq 5) then begin
    if keyword_set(ctype) then message,$
  'Use either the MAP_TYPE positional parameter or set the projection type' + $
    'with CTYPE, but not both.'

; set projection_type string using map_type parameter (a number)
  ntypes = n_elements(map_types)
  if (N_ELEMENTS(map_type) eq 1 && map_type ge 0 && $
      map_type lt ntypes) then begin
         projection_type = map_types[map_type]
  endif else message,'MAP_TYPE must be a scalar >= 0 and < '+$
            strtrim(string(ntypes),2)+'; it was set to '+$
            strtrim(string(map_type),2)

endif else if (n_params() eq 4) then wcs_check_ctype, ctype, projection_type 
    ; checks CTYPE format and extract projection type

; GENERAL ERROR CHECKING

; find the number of elements in each of the data arrays
 n_x = n_elements(x)
 n_y = n_elements(y)
 sz_x = size(x)
 sz_y = size(y)
; check to see that the data arrays have the same size
 if (n_x ne n_y) then $
        message,'The arrays X and Y must have the same number of elements.'

; this sets the default map projection type for the cases when map_type or
; projection_type is set to 'DEF' or if projection_type is not set at this
; point.  As suggested in 'Representations of Celestial Coordinates in FITS'
; the default type is set to CAR (Plate Carree) the simplest of all projections.
if (n_elements(projection_type) eq 0) || (projection_type eq 'DEF') then $
   projection_type='CAR'

; Check to make sure all the correct parameters and keywords are set for
; spherical projections.
 if ((N_elements(ctype) EQ 3) || keyword_set(face) ||  $
    (projection_type eq 'CSC') || $
    (projection_type eq 'QSC') || (projection_type eq 'TSC')) then begin

  noface =  ~keyword_set(face)

endif

; check to see if the x and y offsets are set properly.  If not, break out
; of program.  If so, apply offsets.  If the x and y offsets are not set,
; then assume they are zero.

 if ( (keyword_set(crxy)) && N_elements(crxy) NE 2) then $
     message,'Offset keyword CRXY must contain 2 elements'

 if keyword_set(crxy) && ~array_equal(crxy,[0d0,0d0]) then begin
        xx = double(x + crxy[0] )
        yy = double(y + crxy[1] )
 endif else begin
        xx = double(x)
        yy = double(y)
 endelse

 if  ( N_elements(crval) eq 1 ) then $
        message,'CRVAL keyword must be a 2 element vector'

; BRANCH BY MAP PROJECTION TYPE
case strupcase(projection_type) of
  'AZP':begin
  PV2_1 = N_elements(PV2) GE 1 ? PV2[0] : 0.0  ; PV2_1 =mu (spherical radii)
  PV2_2 = N_elements(PV2) GE 2 ? PV2[1] : 0.0  ; PV2_2 = gamma (degrees)

    if (pv2_1 lt 0) then message,$
      'AZP map projection requires the keyword pv2_1 >= 0'
    gamma = pv2_2/radeg
    mu = pv2_1
    r = sqrt(xx^2 + yy^2*cos(gamma)^2)
    rho = r/(radeg*(mu+1) + yy*sin(gamma) )
    omega = asin( rho*mu/ sqrt( rho^2 + 1.d0) )
    xsi = atan(1.d0, rho)
    phi = atan(xx, -yy*cos(gamma) )
    theta1 = xsi - omega
    theta2 = xsi + omega + !dpi
    theta = theta1*0.0
    if abs(mu) LT 1 then begin
          g = where(abs(theta1) LT pi2, Ng)
          if Ng GT 0 then theta[g] = theta1[g]
          g = where(abs(theta2) LT pi2, Ng)
          if Ng GT 0 then theta[g] = theta2[g]
    endif else begin
          diff1 = abs(pi2 - theta1)
          diff2 = abs(pi2 - theta2)
          g = where((diff1 le diff2), Ng)
          if Ng GT 0 then theta[g] = theta1[g]
          g = where( (diff2 LT diff1) , Ng)
          if Ng GT 0 then theta[g] = theta2[g]
    endelse

  end
  'SZP': begin

       mu = N_elements(PV2) GT 0 ? PV2[0] : 0
       phi_c = N_elements(PV2) GT 1 ? PV2[1] : 0
       theta_c = N_elements(PV2) GT 2 ? PV2[2] : 90.0

       phi_c = phi_c/radeg & theta_c = theta_c/radeg
       xp = -mu*cos(theta_c)*sin(phi_c)
       yp =  mu*cos(theta_c)*cos(phi_c)
       zp =  mu*sin(theta_c) + 1.

       xx = xx/radeg  &  yy = yy/radeg
       xb = (xx - xp)/zp & yb = (yy - yp)/zp
       a = xb^2 + yb^2 + 1
       b = xb*(xx - xb) + yb*(yy - yb)
       c = (xx - xb)^2 + (yy - yb)^2 - 1.
       rad = sqrt(b^2 - a*c)
       rad1 = (-b + rad)/a
       rad2 = (-b - rad)/a
       arad1 = abs(rad1)
       arad2 = abs(rad2)
       rad = rad*0.
       g = where((arad1 LE pi2) and (arad2 GT pi2), Ng )
       if Ng GT 0 then rad[g] = rad1[g]
       g = where((arad2 LE pi2) and (arad1 GT pi2), Ng )
       if Ng GT 0 then rad[g] = rad2[g]
       g = where((arad2 LE pi2) and (arad1 LE pi2), Ng )
       if Ng GT 0 then rad[g] = rad2[g] > rad1[g]
       theta = asin(rad)
       phi = atan( xx - xb*(1-sin(theta)), -(yy - yb*(1-sin(theta))) )
        end

  'TAN':begin
    sz_x = size(xx,/dimen)
    if sz_x[0] EQ 0 then theta = pi2 else $
        theta = make_array(value=pi2,dimen = sz_x)     ;Default is 90 degrees
    r = sqrt(xx^2 + yy^2)
    g = where(r GT 0, Ng)
    if Ng GT 0 then theta[g] = atan(radeg/r[g])
    phi = atan(xx,-yy)
  end

  'SIN':begin

    PV2_1 = N_elements(PV2) GE 1 ? PV2[0] : 0.0
    PV2_2 = N_elements(PV2) GE 2 ? PV2[1] : 0.0

    if (pv2_1 EQ 0) && (pv2_2 EQ 0) then begin
       theta = acos(sqrt(xx^2 + yy^2)/radeg)
       phi = atan(xx,-yy)
    endif else begin
       x = xx/radeg & y = yy/radeg
       a = pv2_1^2 + pv2_2^2 + 1
       b = pv2_1*(x - pv2_1) + pv2_2*(y - pv2_2)
       c = (x - pv2_1)^2 + (y - pv2_2)^2 - 1.
       rad = sqrt(b^2 - a*c)
       rad1 = (-b + rad)/a
       rad2 = (-b - rad)/a
       arad1 = abs(rad1)
       arad2 = abs(rad2)
       rad = rad*0.
       g = where((arad1 LE pi2) and (arad2 GT pi2), Ng )
       if Ng GT 0 then rad[g] = rad1[g]
       g = where((arad2 LE pi2) and (arad1 GT pi2), Ng )
       if Ng GT 0 then rad[g] = rad2[g]
       g = where((arad2 LE pi2) and (arad1 LE pi2), Ng )
       if Ng GT 0 then rad[g] = rad2[g] > rad1[g]
       theta = asin(rad)
       phi = atan( x - pv2_1*(1-sin(theta)), -(y - pv2_2*(1-sin(theta))) )
   endelse
  end

  'STG':begin
    theta = pi2 - 2*atan(sqrt(xx^2 + yy^2)/(2.d0*radeg))
    phi = atan(xx, -yy)
  end

  'ARC':begin
    theta = pi2 - sqrt(xx^2 + yy^2)/radeg
    phi = atan(xx, -yy)
  end

  'ZPN':  begin
   rtheta = sqrt(xx^2 + yy^2)/radeg
   phi = atan(xx, -yy)
   g = where(pv2 NE 0, Ng)
   if Ng GT 0 then np = max(g) else np =0
   pv2 = pv2[0:np]
   n = N_elements(xx)
   theta = dblarr(n)
   for i=0, n-1 do begin
      pv = pv2
      pv[0] = pv[0] - rtheta[i]
      gamma = fz_roots(pv)
; Want only the real roots
   good = where( imaginary(gamma) EQ 0, Ng)
   if Ng EQ 0 then message,'ERROR in ZPN computation: no real roots found'
   gamma = double( gamma[good])

; If multiple real roots are found, then we seek the value closest to the
; approximate linear solution

   if Ng GT 1 then begin
        gamma1 = -pv[0]/pv[1]
        dgmin = min(abs(gamma - gamma1), dgmin_index)
        gamma = gamma[dgmin_index]
      good = where( (gamma GE -pi2) and (gamma LE pi2), Ng)
      if Ng EQ 0 then gamma = gamma[0] else gamma = gamma[good[0]]
   endif
   theta[i] = pi2 - gamma
   if size(yy,/N_dimen) EQ 0 then theta = theta[0]    ;Make scalar again
   endfor
  end


  'ZEA':begin
    theta = pi2 - 2.d0*asin(sqrt(xx^2 + yy^2)/(2.d0*radeg))
    phi = atan(xx,-yy)
  end

  'AIR':begin

    if N_elements(PV2) LT 1 then begin
      message,/informational,$
          'pv2_1 not set, using default of pv2_1 = 90 for AIR map projection'
      pv2_1 = 9.d1
    endif else pv2_1 = pv2[0]

; Numerically solve the equation for xi, by iterating the equation for xi.
; The default initial value for xi is 30 degrees, but for some values of
; x and y, this causes an imaginary angle to result for the next iteration of
; xi.  Unfortunately, this causes the value of xi to converge to an incorrect
; value, so the initial xi is adjusted to avoid this problem.
    theta_b = pv2_1/radeg
    xi = theta_b
    zeta_b = (pi2-theta_b)/2.d0
    if (cos(zeta_b) NE 1) then $
      a = alog(cos(zeta_b))/(tan(zeta_b))^2 $
    else a = -0.5d0
    rtheta = sqrt(xx^2 + yy^2)/(2.0d*radeg)

    repeat begin
      bad=where( abs(exp((-rtheta - a*tan(xi))*tan(xi))) gt 1)
      if (bad[0] ne -1) then xi[bad] = xi[bad]/2.d0
    endrep until (bad[0] eq -1)

    tolerance = 1.d-12
    repeat begin

      xi_old = xi
      xi = acos(exp( (-rtheta - a*tan(xi) )*tan(xi)))

    endrep until (max(abs(xi_old - xi)) lt tolerance)

;    print,rtheta,alog(cos(xi))/tan(xi) + a*tan(xi)
    theta = pi2 - 2.d0*xi
    phi = atan(xx,-yy)
  end

  'CYP':begin
    if n_elements(pv2 eq 0) then begin
      message,/informational,$
            'PV2_1 not set, using default of pv2_1 = 0 for CYP map projection'
      pv2_1 = 0.d0
    endif else pv2_1 = pv2[0]
    if N_elements(pv2) LT 2 then begin
      message,/informational,$
            'PV2_2 not set, using default of pv2_2 = 1 for CYP map projection'
      pv2_2 = 1.d0
    endif else pv2_2 = pv2[1]
    if (pv2_1 eq -pv2_2) then message,$
      'PV2_1 = -PV2_2 is not allowed for CYP map projection.'

    eta = yy/((pv2_1 + pv2_2)*radeg)
    theta = atan(eta,1) + asin(eta*pv2_1/sqrt(eta^2 + 1.d0))
    phi = xx/(pv2_2*radeg)
  end

  'CAR':begin
    phi = xx/radeg
    theta = yy/radeg
  end

  'MER':begin
    phi = xx/radeg
    theta = 2*atan(exp(yy/radeg)) - pi2
  end

  'CEA':begin
    if N_elements(PV2) LT 1 then message,$
      'CEA map projection requires that PV2_1 keyword be set.'
    pv2_1 = pv2[0]
    if ((pv2_1 le 0) || (pv2_1 gt 1)) then message,$
      'CEA map projection requires 0 < PV2_1 <= 1'
    phi = xx/radeg
    theta = asin(yy*pv2_1/radeg)
  end

  'COP':begin
    if N_elements(PV2) LT 1 then message,$
      'COP map projection requires that PV2_1 keyword be set.'
    pv2_1 =  pv2[0]
    if N_elements(PV2) LT 2 then begin
      message,/informational,$
      'PV2_2 not set, using default of PV2_2 = 0 for COP map projection'
      pv2_2=0
    endif else pv2_2 = pv2[1]
    if ((pv2_1 lt -90) || (pv2_2 gt 90) || (pv2_1 gt 90)) then message,$
 'pv2_1 and pv2_2 must satisfy -90<=PV2_1<=90, PV2_2<=90 for COP projection'
    if (pv2_1 eq -pv2_2) then message,$
 'COP projection with PV2_1=-PV2_2 is better done as a cylindrical projection'

    theta_a = pv2_1/radeg
    alpha = pv2_2/radeg
    y_0 = radeg*cos(alpha)/tan(theta_a)
    R_theta = sqrt(xx^2+(y_0-yy)^2)
    if pv2_1 LT 0 then R_theta = -R_theta
    theta = theta_a + atan(1.d0/tan(theta_a) - R_theta/$
          (radeg*cos(alpha)))
     phi = atan( xx/R_theta,(y_0-yy)/R_theta )/sin(theta_a)
  end

  'COD':begin
    if N_elements(pv2) LT 1 then message,$
      'COD map projection requires that PV2_1 keyword be set.'
    pv2_1 = pv2[0]
    if N_elements(pv2) LT 2 then begin
      message,/informational,$
     'PV2_2 not set, using default of PV2_2 = 0 for COD map projection'
      pv2_2 = 0
    endif else pv2_2 = pv2[1]
    if ((pv2_1 lt -90) || (pv2_2 gt 90) || (pv2_1 gt 90)) then message,$
 'pv2_1 and pv2_2 must satisfy -90<=pv2_1<=90,pv2_2<=90 for COD projection'

; use general set of equations for pv2_1 not = pv2_2
    theta_a = pv2_1/radeg

    if (pv2_2 NE 0) then begin
      alpha = pv2_2/radeg
      C = sin(theta_a)*sin(alpha)/alpha
      Y_0 = radeg*alpha/tan(alpha)/tan(theta_a)
      R_theta = sqrt(xx^2+(y_0-yy)^2)
      if pv2_1 LT 0 then R_theta = -R_theta
       theta = theta_a + alpha/(tan(alpha)*tan(theta_a))-  R_theta/radeg
; use special set of equations for pv2_1 = pv2_2
    endif else begin
      C = sin(theta_a)
      y_0 = radeg/tan(theta_a)
     R_theta = sqrt(xx^2+(y_0-yy)^2)
     if pv2_1 LT 0 then R_theta = -R_theta
      theta = theta_a + 1.0d/tan(theta_a) - R_theta/radeg
   endelse
    phi = atan( xx/R_theta,(y_0-yy)/R_theta )/C
   end

  'COE':begin
    if N_elements(pv2) LT 1 then message,$
      'COE map projection requires that pv2_1 keyword be set.'
    pv2_1 = pv2[0]
    if N_elements(pv2) LT 2 then begin
      message,/informational,$
      'pv2_2 not set, using default of pv2_2 = 0 for COE map projection'
      pv2_2 = 0
    endif else pv2_2 = pv2[1]
    if ((pv2_1 lt -90) || (pv2_2 gt 90) || (pv2_1 gt 90)) then message,$
 'pv2_1 and pv2_2 must satisfy -90<=pv2_1<=90,pv2_2<=90 for COE projection'
    theta_a = pv2_1/radeg
    eta = pv2_2/radeg
    theta1 = (theta_a - eta)
     theta2 = (theta_a + eta)
    s_1 = sin( theta1)
    s_2 = sin( theta2)
    stheta_a = sin(theta_a)
    gamma = s_1 + s_2
    C = gamma/2
    y_0 = radeg*2.d0*sqrt(1.d0 + s_1*s_2 - gamma*stheta_a)/gamma
    R_theta = (xx^2+(y_0-yy)^2)
    if pv2_1 LT 0 then R_theta = -R_theta
    phi = 2*atan(xx/R_theta,(y_0 - yy)/R_theta)/gamma
    theta = asin((1.d0 + s_1*s_2-(xx^2+(y_0-yy)^2)*(gamma/(2.d0*radeg))^2)/gamma)

  end

  'COO':begin
    if N_elements(pv2) LT 1 then message,$
      'COO map projection requires that pv2_1 keyword be set.'
    pv2_1 = pv2[0]
    if N_elements(pv2) LT 2 then begin
      message,/informational,$
      'pv2_2 not set, using default of pv2_2 = 0 for COO map projection'
      pv2_2 = 0
    endif else  pv2_2 = pv2[1]
    if ((pv2_1 lt -90) || (pv2_2 gt 90) || (pv2_1 gt 90)) then message,$
 'pv2_1 and pv2_2 must satisfy -90<=pv2_1<=90,pv2_2<=90 for COO projection'
    theta_1 = (pv2_1 - pv2_2)/radeg
    theta_2 = (pv2_1 + pv2_2)/radeg
    theta_a = pv2_1/radeg


; calculate value of c in simpler fashion if pv2_1 = pv2_2
    if (theta_1 eq theta_2) then c = sin(theta_1) else $
    c = alog(cos(theta_2)/cos(theta_1))/alog(tan((pi2-theta_2)/2.d0)/$
    tan((pi2-theta_1)/2.d0))

    alpha = radeg*cos(theta_1)/(c*(tan((pi2-theta_1)/2.d0))^c)
    Y_0 = alpha*(tan((pi2-theta_a)/2.d0)^c)
    R_theta = sqrt(xx^2+(y_0-yy)^2)
    if pv2_1 LT 0 then R_theta = -R_theta
     phi = atan( xx/R_theta,(y_0-yy)/R_theta )/C
    theta = pi2 - 2*atan((R_theta/alpha)^(1.d0/c))
  end

  'BON':begin
    if (N_elements(pv2) LT 1) then message,$
      'BON map projection requires that PV2_1 keyword be set.'
    pv2_1 = pv2[0]
    if ((pv2_1 lt -90) || (pv2_1 gt 90)) then message,$
      'pv2_1 must satisfy -90 <= pv2_1 <= 90 for BON map projection'
    if (pv2_1 eq 0) then message,$
      'pv2_1 = 0 for BON map projection is better done with SFL map projection'
    theta_1 = pv2_1/radeg
    y_0 = 1.d0/tan(theta_1) + theta_1
    s = theta_1/abs(theta_1)
    theta = y_0 - s*sqrt(xx^2 + (y_0*radeg - yy)^2)/radeg
    phi = s*(y_0 - theta)*atan(s*xx/(y_0*radeg - theta),$
                               (y_0*radeg - yy)/(y_0*radeg - theta))/cos(theta)
  end

  'PCO':begin
; Determine where y = 0 and assign theta to 0 for these points.  The reason
; for doing this separately is that the initial condition for theta in the
; numerical solution is sign(y)*45 which only works for y not = 0.
    bad = where(yy eq 0)
    good = where(yy ne 0)
    theta = double(xx - xx)
    if (bad[0] ne -1) then theta[bad] = 0.d0

; Find theta numerically.
    tolerance = 1.d-11
    tolerance_2 = 1.d-11
    if (good[0] ne -1) then begin
      theta_p = double(xx - xx)
      theta_p[good] = pi2*yy[good]/abs(yy[good])
      theta_n = double(xx - xx)
      f_p = double(xx - xx)
      f_p[good] = xx[good]^2 - 2.d0*radeg*(yy[good] - radeg*theta_p[good])/$
                  tan(theta_p[good]) + (yy[good] - radeg*theta_p[good])^2
      f_n = double(xx - xx) - 999.d0
      lambda = double(xx - xx)
      f = double(xx - xx)
      repeat begin
        case_1 = where((yy ne 0.d0) and (f_n lt (-1.d2)))
        case_2 = where((yy ne 0.d0) and (f_n ge (-1.d2)))
        if (case_1[0] ne -1) then lambda[case_1] = 0.5d0
        if (case_2[0] ne -1) then $
          lambda[case_2] = f_p[case_2]/(f_p[case_2] - f_n[case_2])
        lambda[good] = 1.d-1 > (9.d-1 < lambda[good])
        theta[good] = (1.d0 - lambda[good])*theta_p[good] + $
                      lambda[good]*theta_n[good]
        f[good] = xx[good]^2 - 2.d0*radeg*(yy[good] - radeg*theta[good])/$
                  tan(theta[good]) + (yy[good] - radeg*theta[good])^2
        neg = where((yy ne 0.d0) and (f lt 0.d0))
        pos = where((yy ne 0.d0) and (f gt 0.d0))
        if (neg[0] ne -1) then begin
          f_n[neg] = f[neg]
          theta_n[neg] = theta[neg]
        end
        if (pos[0] ne -1) then begin
          f_p[pos] = f[pos]
          theta_p[pos] = theta[pos]
        end
      endrep until ((max(abs(theta_p - theta_n)) lt tolerance) || $
                    (max(abs(f)) lt tolerance_2))
    endif

; Determine phi differently depending on whether theta = 0 or not.
    bad = where(theta eq 0.d0)
    good = where(theta ne 0.d0)
    phi = double(x - x)
    if (bad[0] ne -1) then phi[bad] = xx[bad]/radeg
    phi[good] = atan(xx[good]/radeg*tan(theta[good]),$
       1.d0 - (yy[good]/radeg - theta[good])*tan(theta[good]))/sin(theta[good])
  end

  'SFL':begin
    phi = xx/(radeg*cos(yy/radeg))
    theta = yy/radeg
  end

  'GLS':begin
    phi = xx/(radeg*cos(yy/radeg))
    theta = yy/radeg
  end


  'PAR':begin

    theta = 3.d0*asin(yy/pi/radeg)
    phi = xx/(1.d0 - 4.d0*(yy/pi/radeg)^2)/radeg
  end

  'AIT':begin
  z2 = 1.d0 - (xx/(4.d0*radeg))^2 - (yy/(2.d0*radeg))^2
  bad = where(z2 lt 0.5d0,nbad)
  z = sqrt(z2)
  phi = 2.d0*atan(z*xx/(2.d0*radeg),2.d0*z^2 - 1.d0)
  theta = asin(yy*z/radeg)
  if nbad gt 0 then begin
       phi[bad] = !values.d_nan
      theta[bad] = !values.d_nan
   endif

  end

  'MOL':begin
    phi = pi*xx/(radeg*2.d0*sqrt(2.d0 - (yy/radeg)^2))
    arg = 2.d0*asin(yy/(sqrt(2.d0)*radeg))/pi + $
                 yy*sqrt(2.d0 - (yy/radeg)^2)/1.8d2

   theta = asin(2.d0*asin(yy/(sqrt(2.d0)*radeg))/pi + $
                 yy*sqrt(2.d0 - (yy/radeg)^2)/1.8d2)

  end

  'CSC':begin
    xx = xx/4.5d1
    yy = yy/4.5d1

;
;   If the faces are not defined, assume that the faces need to be defined
;   and the whole sky is displayed as a "sideways T".
;
        if noface eq 1 then begin

                face=intarr(n_elements(xx))

                face1 = where((xx le 1.0) and (yy le 1.0) and (yy ge -1.0),nf1)
                if nf1 gt 0 then begin
                        face[face1]=1
                endif

                face4 = where((xx gt 5.0),nf4)
                if nf4 gt 0 then begin
                        face[face4]=4
                        xx[face4]=xx[face4]-6.0d0
                endif

                face3 = where((xx le 5.0) and (xx gt 3.0),nf3)
                if nf3 gt 0 then begin
                        face[face3]=3
                        xx[face3]=xx[face3]-4.0d0
                endif

                face2 = where((xx le 3.0) and (xx gt 1.0),nf2)
                if nf2 gt 0 then begin
                        face[face2]=2
                        xx[face2]=xx[face2]-2.0d0
                endif

                face0 = where((xx le 1.0) and (yy gt 1.0),nf0)
                if nf0 gt 0 then begin
                        face[face0]=0
                        yy[face0]=yy[face0] - 2.0
                endif

                face5 = where((xx le 1.0) and (yy lt -1.0),nf5)
                if nf5 gt 0 then begin
                        face[face5]=5
                        yy[face5]=yy[face5] + 2.0
                endif

        endif

; Define array of numerical constants used in determining alpha and beta1.
    p = dblarr(7,7)
    p[0,0] = -0.27292696d0
    p[1,0] = -0.07629969d0
    p[0,1] = -0.02819452d0
    p[2,0] = -0.22797056d0
    p[1,1] = -0.01471565d0
    p[0,2] = 0.27058160d0
    p[3,0] = 0.54852384d0
    p[2,1] = 0.48051509d0
    p[1,2] = -0.56800938d0
    p[0,3] = -0.60441560d0
    p[4,0] = -0.62930065d0
    p[3,1] = -1.74114454d0
    p[2,2] = 0.30803317d0
    p[1,3] = 1.50880086d0
    p[0,4] = 0.93412077d0
    p[5,0] = 0.25795794d0
    p[4,1] = 1.71547508d0
    p[3,2] = 0.98938102d0
    p[2,3] = -0.93678576d0
    p[1,4] = -1.41601920d0
    p[0,5] = -0.63915306d0
    p[6,0] = 0.02584375d0
    p[5,1] = -0.53022337d0
    p[4,2] = -0.83180469d0
    p[3,3] = 0.08693841d0
    p[2,4] = 0.33887446d0
    p[1,5] = 0.52032238d0
    p[0,6] = 0.14381585d0

; Calculate alpha and beta1 using numerical constants
    sum = double(x - x)
    for j = 0,6 do for i = 0,6 - j do sum = sum + p[i,j]*xx^(2*i)*yy^(2*j)
    alpha = xx + xx*(1 - xx^2)*sum

    sum = double(x - x)
    for j = 0,6 do for i = 0,6 - j do sum = sum + p[i,j]*yy^(2*i)*xx^(2*j)
    beta1 = yy + yy*(1 - yy^2)*sum

; Calculate theta and phi from alpha and beta1; the method depends on which
; face the point lies on
    phi = double(x - x)
    theta = double(x - x)
    for i = 0l, n_x - 1 do begin
      case face[i] of
        0:begin
          if (beta1[i] eq 0.d0) then begin
            if (alpha[i] eq 0.d0) then begin
              theta[i] = pi2
; uh-oh lost information if this happens
              phi[i] = 0.d0
            endif else begin
              phi[i] = alpha[i]/abs(alpha[i])*pi2
              theta[i] = atan(abs(1.d0/alpha[i]))
            endelse
          endif else begin
            phi[i] = atan(alpha[i],-beta1[i])
            theta[i] = atan(-cos(phi[i])/beta1[i])
          endelse
; ensure that the latitudes are positive
          theta[i] = abs(theta[i])
        end
        1:begin
          phi[i] = atan(alpha[i])
          theta[i] = atan(beta1[i]*cos(phi[i]))
        end
        2:begin
          if (alpha[i] eq 0.d0) then phi[i] = pi2 else $
            phi[i] = atan(-1.d0/alpha[i])
          if (phi[i] lt 0.d0) then phi[i] = phi[i] + pi
          theta[i] = atan(beta1[i]*sin(phi[i]))
        end
        3:begin
          phi[i] = atan(alpha[i])
          if (phi[i] gt 0.d0) then phi[i] = phi[i] - pi else $
          if (phi[i] lt 0.d0) then phi[i] = phi[i] + pi
          theta[i] = atan(-beta1[i]*cos(phi[i]))
        end
        4:begin
          if (alpha[i] eq 0.d0) then phi[i] = -pi2 else $
            phi[i] = atan(-1.d0/alpha[i])
          if (phi[i] gt 0.d0) then phi[i] = phi[i] - pi
          theta[i] = atan(-beta1[i]*sin(phi[i]))
        end
        5:begin
          if (beta1[i] eq 0.d0) then begin
            if (alpha[i] eq 0.d0) then begin
              theta[i] = -pi2
; uh-oh lost information if this happens
              phi[i] = 0.d0
            endif else begin
              phi[i] = -alpha[i]/abs(alpha[i])*pi2
              theta[i] = -atan(abs(1.d0/alpha[i]))
            endelse
          endif else begin
            phi[i] = atan(alpha[i],beta1[i])
            theta[i] = atan(-cos(phi[i])/beta1[i])
          endelse
; ensure that the latitudes are negative
          theta[i] = -abs(theta[i])
        end

      endcase
    endfor
  end

  'QSC':begin

    xx=xx/45.0d0
    yy=yy/45.0d0
;
;   If the faces are not defined, assume that the faces need to be defined
;   and the whole sky is displayed as a "sideways T".
;
        if noface eq 1 then begin

                face=intarr(n_elements(xx))

                face1 = where((xx le 1.0) and (yy le 1.0) and (yy ge -1.0),nf1)
                if nf1 gt 0 then begin
                        face[face1]=1
                endif

                face4 = where((xx gt 5.0),nf4)
                if nf4 gt 0 then begin
                        face[face4]=4
                        xx[face4]=xx[face4]-6.0d0
                endif

                face3 = where((xx le 5.0) and (xx gt 3.0),nf3)
                if nf3 gt 0 then begin
                        face[face3]=3
                        xx[face3]=xx[face3]-4.0d0
                endif

                face2 = where((xx le 3.0) and (xx gt 1.0),nf2)
                if nf2 gt 0 then begin
                        face[face2]=2
                        xx[face2]=xx[face2]-2.0d0
                endif

                face0 = where((xx le 1.0) and (yy gt 1.0),nf0)
                if nf0 gt 0 then begin
                        face[face0]=0
                        yy[face0]=yy[face0] - 2.0
                endif

                face5 = where((xx le 1.0) and (yy lt -1.0),nf5)
                if nf5 gt 0 then begin
                        face[face5]=5
                        yy[face5]=yy[face5] + 2.0
                endif

        endif


; First determine the quadrant in which each points lies.  Calculate the
; ratio (alpha/beta1) for each point depending on the quadrant.  Finally,
; use this information and the face on which the point lies to calculate
; phi and theta.
    theta = double(x - x)
    phi = double(x - x)
    rho = double(x - x)
    ratio = double(x - x)
    larger = double(x - x)
    smaller = double(x - x)

    temp = where(abs(yy) ge abs(xx), Ntemp)
    if Ntemp GT 0 then larger[temp] = yy[temp]
    temp = where(abs(xx) gt abs(yy), Ntemp )
    if Ntemp GT 0 then larger[temp] = xx[temp]
    temp = where(abs(yy) lt abs(xx), Ntemp )
    if Ntemp GT 0 then smaller[temp] = yy[temp]
    temp = where(abs(xx) le abs(yy), Ntemp)
    if Ntemp GT 0 then smaller[temp] = xx[temp]

    temp = where(larger ne 0.d0, Ntemp)
    if Ntemp GT 0 then ratio[temp] = sin(pi/1.2d1*smaller[temp]/larger[temp])/$
                      (cos(pi/1.2d1*smaller[temp]/larger[temp]) - sqrt(0.5d0))

    temp = where(larger eq 0.d0, Ntemp)
    if Ntemp GT 0 then ratio[temp] = 1.d0
    rho = 1.d0 - (larger)^2*(1.d0 - 1.d0/sqrt(2.d0 + ratio^2))

    temp = where((abs(xx) gt abs(yy)) and (ratio ne 0.d0), Ntemp)
    if Ntemp GT 0 then ratio[temp] = 1.d0/ratio[temp]

    temp = where((abs(xx) gt abs(yy)) and (ratio eq 0.d0), Ntemp)
; use a kludge to produce the correct value for 1/0 without generating an error
    if Ntemp GT 0 then ratio[temp] = tan(pi2)

    for i = 0l, n_x-1 do begin
      case face[i] of
        0:begin
          if (xx[i] ne 0.d0) then phi[i] = atan(-ratio[i]) else $
          if (yy[i] le 0.d0) then phi[i] = 0.d0 else $
          if (yy[i] gt 0.d0) then phi[i] = pi

          if (yy[i] ne 0.d0) then theta[i] = asin(rho[i]) else $
          if (xx[i] le 0.d0) then theta[i] = -pi2 else $
          if (xx[i] gt 0.d0) then theta[i] = pi2

          if (yy[i] gt 0.d0) then begin
            if (xx[i] lt 0.d0) then phi[i] = phi[i] - pi $
            else if (xx[i] gt 0.d0) then phi[i] = phi[i] + pi
          endif
        end
        1:begin
          if (xx[i] ne 0.d0) then begin
            if (yy[i] ne 0.d0) then $
             phi[i] = xx[i]/abs(xx[i])*acos(sqrt(rho[i]^2*(1.d0 + ratio[i]^2)/$
                             (ratio[i]^2 + rho[i]^2))) $
            else phi[i] = xx[i]/abs(xx[i])*acos(rho[i])
          endif else phi[i] = 0.d0
          if (yy[i] ne 0.d0) then theta[i] = yy[i]/abs(yy[i])*acos(rho[i]/$
                                            cos(phi[i])) else theta[i] = 0.d0
        end
        2:begin
          if (yy[i] ne 0.d0) then begin
            if (xx[i] gt 0.d0) then $
               phi[i] = pi - asin(sqrt(rho[i]^2*(1.d0 + ratio[i]^2)/$
                             (ratio[i]^2 + rho[i]^2))) $
            else if (xx[i] lt 0.d0) then $
               phi[i] = asin(sqrt(rho[i]^2*(1.d0 + ratio[i]^2)/$
                             (ratio[i]^2 + rho[i]^2))) $
            else phi[i] = pi2
            theta[i] = yy[i]/abs(yy[i])*acos(rho[i]/abs(sin(phi[i])))
          endif else begin
            theta[i] = 0.d0
            if (xx[i] gt 0.d0) then phi[i] = pi - asin(rho[i]) $
            else if (xx[i] lt 0.d0) then phi[i] = asin(rho[i]) $
            else phi[i] = pi2
          endelse
        end
        3:begin
          if (yy[i] ne 0.d0) then begin
            if (xx[i] gt 0.d0) then $
              phi[i] = acos(sqrt(rho[i]^2*(1.d0 + ratio[i]^2)/$
                            (ratio[i]^2 + rho[i]^2))) - pi $
            else if (xx[i] lt 0.d0) then $
              phi[i] = -acos(sqrt(rho[i]^2*(1.d0 + ratio[i]^2)/$
                       (ratio[i]^2 + rho[i]^2))) + pi $
            else phi[i] = pi
            theta[i] = yy[i]/abs(yy[i])*acos(-rho[i]/cos(phi[i]))
          endif else begin
            theta[i] = 0.d0
            if (xx[i] gt 0.d0) then phi[i] = acos(rho[i]) - pi $
            else if (xx[i] lt 0.d0) then phi[i] = -acos(rho[i]) + pi $
            else phi[i] = pi
          endelse
        end
        4:begin
          if (yy[i] ne 0.d0) then begin
            if (xx[i] gt 0.d0) then $
               phi[i] = -asin(sqrt(rho[i]^2*(1.d0 + ratio[i]^2)/$
                             (ratio[i]^2 + rho[i]^2))) $
            else if (xx[i] lt 0.d0) then $
               phi[i] = asin(sqrt(rho[i]^2*(1.d0 + ratio[i]^2)/$
                             (ratio[i]^2 + rho[i]^2))) - pi $
            else phi[i] = -pi2
            theta[i] = yy[i]/abs(yy[i])*acos(-rho[i]/sin(phi[i]))
          endif else begin
            theta[i] = 0.d0
            if (xx[i] gt 0.d0) then phi[i] = -asin(rho[i]) $
            else if (xx[i] lt 0.d0) then phi[i] = asin(rho[i]) - pi $
            else phi[i] = -pi2
          endelse
        end
        5:begin
          if (xx[i] ne 0.d0) then phi[i] = atan(ratio[i]) $
          else if (yy[i] le 0.d0) then phi[i] = pi $
          else if (yy[i] gt 0.d0) then phi[i] = 0.d0

          if (yy[i] ne 0.d0) then theta[i] = asin(-rho[i]) $
          else if (xx[i] le 0.d0) then theta[i] = -pi2 $
          else if (xx[i] gt 0.d0) then theta[i] = pi2

          if (yy[i] lt 0.d0) then begin
            if (xx[i] lt 0.d0) then phi[i] = phi[i] - pi $
            else if (xx[i] gt 0.d0) then phi[i] = phi[i] + pi
          endif
        end
      endcase
    endfor
  end

  'TSC':begin

    xx=xx/45.0d0
    yy=yy/45.0d0
;
;   If the faces are not defined, assume that the faces need to be defined
;   and the whole sky is displayed as a "sideways T".
;
        if noface eq 1 then begin

                face=intarr(n_elements(xx))

                face1 = where((xx le 1.0) and (yy le 1.0) and (yy ge -1.0),nf1)
                if nf1 gt 0 then begin
                        face[face1]=1
                endif

                face4 = where((xx gt 5.0),nf4)
                if nf4 gt 0 then begin
                        face[face4]=4
                        xx[face4]=xx[face4]-6.0d0
                endif

                face3 = where((xx le 5.0) and (xx gt 3.0),nf3)
                if nf3 gt 0 then begin
                        face[face3]=3
                        xx[face3]=xx[face3]-4.0d0
                endif

                face2 = where((xx le 3.0) and (xx gt 1.0),nf2)
                if nf2 gt 0 then begin
                        face[face2]=2
                        xx[face2]=xx[face2]-2.0d0
                endif

                face0 = where((xx le 1.0) and (yy gt 1.0),nf0)
                if nf0 gt 0 then begin
                        face[face0]=0
                        yy[face0]=yy[face0] - 2.0
                endif

                face5 = where((xx le 1.0) and (yy lt -1.0),nf5)
                if nf5 gt 0 then begin
                        face[face5]=5
                        yy[face5]=yy[face5] + 2.0
                endif

        endif

    rho = sin(atan(1.0d0/sqrt(xx^2 + yy^2)))
    phi = double(x - x)
    theta = double(x - x)
    for i = 0l, n_x - 1 do begin
      case face[i] of
        0:begin
          phi[i] = atan(xx[i],-yy[i])
          theta[i] = asin(rho[i])
        end
        1:begin
          if (xx[i] ne 0.d0) then begin
            if (xx[i] ge 0.d0) then $
             phi[i] = atan(sqrt((1.d0/rho[i]^2- 1.d0)/(1 + (yy[i]/xx[i])^2))) $
            else phi[i] =atan(-sqrt((1.d0/rho[i]^2 - 1.d0)/$
                               (1 + (yy[i]/xx[i])^2)))
            theta[i] = atan(yy[i]/xx[i]*sin(phi[i]))
          endif else begin
            phi[i] = 0.d0
            if (yy[i] ge 0.d0) then theta[i] = acos(rho[i]) $
            else theta[i] = -acos(rho[i])
          endelse
        end
        2:begin
; The point theta = 0, phi = Pi/2 lies in this region, allowing
; rho = Cos[theta]*Sin[phi] to be 1, causing an infinite quantity in the
; equation for phi
          if (rho[i] eq 1.d0) then begin
            phi[i] = pi2
            theta[i] = 0.d0
          endif else if (xx[i] gt 1.d-14) then begin
           phi[i] = atan(-sqrt((1.d0 + (yy[i]/xx[i])^2)/$
                                (1.d0/rho[i]^2 - 1.d0)))+pi
            theta[i] = atan(-yy[i]/xx[i]*cos(phi[i]))
          endif else if (xx[i] lt -1.d-14) then begin
            phi[i]=atan(sqrt((1.d0+(yy[i]/xx[i])^2)/(1.d0/rho[i]^2 - 1.d0)))
            theta[i] = atan(-yy[i]/xx[i]*cos(phi[i]))
          endif else begin
             phi[i] = pi2
            if (yy[i] ge 0) then theta[i] = acos(rho[i]/sin(phi[i])) $
            else theta[i] = -acos(rho[i]/sin(phi[i]))
          endelse
        end
        3:begin
          if (abs(xx[i]) ge 1.d-5) then begin
            if (xx[i] gt 0.d0) then $
           phi[i] = atan(sqrt((1.d0/rho[i]^2 - 1.d0)/$
                          (1 + (yy[i]/xx[i])^2)))-pi $
        else phi[i] = atan(-sqrt((1.d0/rho[i]^2 - 1.d0)/$
                            (1 + (yy[i]/xx[i])^2)))+pi
            theta[i] = atan(-yy[i]/xx[i]*sin(phi[i]))
          endif else begin
            if (xx[i] ge 0.d0) then phi[i] = -pi $
            else phi[i] = pi
            if (yy[i] ge 0) then theta[i] = acos(rho[i]) $
            else theta[i] = -acos(rho[i])
          endelse
        end
        4:begin
          if (rho[i] eq 1.d0) then begin
            phi[i] = -pi2
            theta[i] = atan(yy[i]/xx[i])
          endif else if (xx[i] gt 1.d-14) then begin
           phi[i]=atan(-sqrt((1.d0 + (yy[i]/xx[i])^2)/(1.d0/rho[i]^2 - 1.d0)))
            theta[i] = atan(yy[i]/xx[i]*cos(phi[i]))
          endif else if (xx[i] lt -1.d-14) then begin
            phi[i]=atan(sqrt((1.d0+(yy[i]/xx[i])^2)/(1.d0/rho[i]^2 - 1.d0)))-pi
            theta[i] = atan(yy[i]/xx[i]*cos(phi[i]))
          endif else begin
             phi[i] = 1.5d0*!pi
            if (yy[i] ge 0) then theta[i] = acos(rho[i]) $
            else theta[i] = -acos(rho[i])
          endelse
        end
        5:begin
          phi[i] = atan(xx[i],yy[i])
          theta[i] = asin(-rho[i])
        end

      endcase
    endfor
  end

  'HPX':begin ; HEALPix projection
              ; See Calabretta & Roukema 2007, MNRAS, 381, 865

    pv2_1 = N_ELEMENTS(pv2) GE 1 ? pv2[0] : 4.d
    pv2_2 = N_ELEMENTS(pv2) GE 2 ? pv2[1] : 3.d
    hpx_k = pv2_2                  ; The main generalised HEALPIX parameters
    hpx_h = pv2_1                  ;
    ik = ROUND(hpx_k)
    ih = ROUND(hpx_h)
    IF (ik le 0 || ih le 0) THEN MESSAGE, $
      'Illegal PV2 array:' + STRCOMPRESS(STRJOIN(pv2,/single))+ $
      '; should be positive integers'

    phi   = xx ; Create theta & phi arrays in same shape as xx & yy.
    theta = yy ;

                            ; Is pixel is in an unoccupied facet ?
    invfd = ih / 360d0      ; inverse facet diagonal
    wdiag = ik / 2          ; Semi-width of occupied diagonal in facets

    IF ik THEN BEGIN               ; Odd K (including standard HEALPix):
      xoff = ih + wdiag + 1 ? 0.5d0 : 0d0
      ix   = ROUND( (xx + yy)*invfd + xoff ) ; Facet indices
      diag = ROUND( (yy - xx)*invfd - xoff ) + wdiag + TEMPORARY(ix)
    ENDIF ELSE BEGIN               ; Even K
                                   ; Row offset for (0,0) depends on
                                   ; whether h is odd or even:
      yoff = ih + (ik-1)/2 ? -0.25d0 : 0.25d0
      ioff = ih ? 2*((ik-2)/4) + 1 : 2*(ik/4)
      ix   = ROUND( (xx + yy)*invfd + yoff )
      diag = ROUND( (yy - xx)*invfd + yoff ) + ioff  + TEMPORARY(ix)
    ENDELSE
    hpx_good = WHERE(diag LT ik AND diag GE 0, COMPLEMENT = hpx_bad)
    if hpx_bad[0] ne -1 then begin ; Set coords of off-sky pixels to NaN:
      phi[hpx_bad]   = !VALUES.D_NAN
      theta[hpx_bad] = !VALUES.D_NAN
    endif
 
    ylim = 90*(ik-1)/hpx_h

    eqfaces = -1
    polfaces = -1
    IF hpx_good[0] NE -1 THEN BEGIN
        equas = where(abs(yy[hpx_good]) le ylim, complement=poles)
        IF equas[0] NE -1 THEN eqfaces  = hpx_good[TEMPORARY(equas)]
        IF poles[0] NE -1 THEN polfaces = hpx_good[TEMPORARY(poles)]
    ENDIF
    hpx_good = 0
 
                                ; equatorial region
    if eqfaces[0] ne -1 then begin
        phi[eqfaces]=xx[eqfaces]/radeg
        theta[eqfaces]=asin(yy[eqfaces]*(hpx_h/(ik*90)))
; Allow wrapped values of x, so following commented out:
;       hpx_bad = where(xx[eqfaces] lt -180.D or xx[eqfaces] gt 180.D)
;       if hpx_bad[0] ne -1 then begin
;           phi[eqfaces[hpx_bad]]=!VALUES.D_NAN
;           theta[eqfaces[hpx_bad]]=!VALUES.D_NAN
;       endif
    endif

                                ; polar regions
    if polfaces[0] ne -1 then begin
      hpx_sig = (ik+1)/2.D - abs(yy[polfaces])*(hpx_h/180d)
      hpx_omega = FIX((hpx_k mod 2 eq 1) or yy[polfaces] gt 0)
      hpx_xc = -180 + (2 * floor( (xx[polfaces]+180d0)*hpx_h/360d0 + $
                                (1-hpx_omega)/2d0 ) $
                       + hpx_omega) * 180d0/hpx_h

      poles = where(hpx_sig EQ 0.D) ; Avoid divide by zero at poles
      IF poles[0] NE -1 THEN hpx_sig[poles] = 1.D
      phi[polfaces] = ( hpx_xc + (xx[polfaces]-hpx_xc)/hpx_sig ) / radeg
      theta[polfaces] = ((yy[polfaces] gt 0)*2-1)*asin(1-hpx_sig^2/hpx_k)
      IF poles[0] NE -1 THEN BEGIN
          phi[polfaces[poles]] = hpx_xc[poles] / radeg
          theta[polfaces[poles]] = $
               ((yy[polfaces[poles]] gt 0)*2-1) * !dpi/2d0
      ENDIF
    endif
  end

  'HCT':begin
    phi = xx/radeg
    theta = dblarr(n_elements(yy))
    ylim = 90*(3-1)/4
    w_np = where(yy ge ylim, n_np)
    w_eq = where((yy lt ylim) and (yy gt -ylim), n_eq)
    w_sp = where(yy le -ylim, n_sp)

    if n_np gt 0 then theta[w_np] =  asin(1-(2-yy[w_np]/ylim)^2/3.d)
    if n_eq gt 0 then theta[w_eq] =  asin((yy[w_eq]/ylim)*2./3.d)
    if n_sp gt 0 then theta[w_sp] = -asin(1-(2+yy[w_sp]/ylim)^2/3.d)
  end

  'XPH':begin ; Butterfly re-arrangement of HPX, see Calabretta & Lowe (2013)
;                                
; xx, yy reference point is the pole which is in the centre of the
; grid. Diagonal length in IWC is 360deg/sqrt(2)
;
; identify on-sky & off-sky pixels:
     scale = 1d0/sqrt(2d0)
     halfwidth = 180d0*scale
     quarter   =  90d0*scale    
     dg1 = (xx + yy)
     dg2 = (xx - yy)
     good = WHERE( xx ge -halfwidth AND xx le halfwidth AND $
                   yy ge -halfwidth AND yy le halfwidth AND $
                   ((dg1 ge -quarter  AND dg1 le quarter)  OR $
                    (dg2 ge -quarter  AND dg2 le quarter)), ngood, $
                   COMPLEMENT = bad, NCOMPLEMENT = nbad)     

     scalar = sz_x[0] EQ 0
     dims = scalar ? 1 : sz_x[1:sz_x[0]]
     phi = DBLARR(dims,/NOZERO) & theta = DBLARR(dims,/NOZERO)
     
     IF nbad GT 0 THEN BEGIN
        phi[bad] = !values.D_NAN
        theta[bad] = !values.D_NAN
        bad = 0
     ENDIF
     IF ngood GT 0 THEN BEGIN
        dg1 = dg1[good]*scale
        dg2 = dg2[good]*scale
        
        xi  = DBLARR(ngood,/NOZERO)
        eta = DBLARR(ngood,/NOZERO)
        phig = DBLARR(ngood)
                              ; transform xy coords from XPH to HPX
        xq = xx[good] ge 0d0  ; true on LHS of projection
        yq = yy[good] ge 0d0  ; true in top half of projection
               
        quad = WHERE(~xq AND yq)  ; upper right
        IF quad[0] ne -1 THEN BEGIN
           xi[quad]   = -dg1[quad]
           eta[quad]  =  dg2[quad]
           phig[quad] = -180d0 
        ENDIF
        quad = WHERE(~xq AND ~yq) ; lower right
        IF quad[0] ne -1 THEN BEGIN
           xi[quad]   =  dg2[quad]
           eta[quad]  =  dg1[quad]
           phig[quad] =  -90d0
        ENDIF
        quad = WHERE(xq AND ~yq)  ; lower left
        IF quad[0] ne -1 THEN BEGIN
           xi[quad]   =  dg1[quad]
           eta[quad]  = -dg2[quad]
           phig[quad] =  0d0
        ENDIF
        quad = WHERE(xq AND yq)  ; upper left
        IF quad[0] ne -1 THEN BEGIN
           xi[quad]   = -dg2[quad]
           eta[quad]  = -dg1[quad]
           phig[quad] =  90d0
        ENDIF
        xq = 0 & yq = 0 & dg1 = 0 & dg2 = 0 & quad = 0
        eta += 90d0
        thetag = DBLARR(ngood,/NOZERO)
        
        poles = WHERE(ABS(eta) gt 45d0, COMPLEMENT=equas)
        IF equas[0] NE -1 THEN BEGIN
          phig[equas]  += xi[equas]
          thetag[equas] = ASIN(eta[equas]*(2d0/135d0))
        ENDIF
        equas = 0
        IF poles[0] NE -1 THEN BEGIN
          hpx_sigma     = (90d0 - ABS(eta[poles]))/45d0
          test = WHERE(hpx_sigma lt 1d-4,ntest)
          phig[poles]  += xi[poles]/hpx_sigma
          sgn           = 2*(eta[poles] gt 0d0) - 1
          thetag[poles] = TEMPORARY(sgn)*ASIN(1d0-hpx_sigma^2/3d0)
          IF ntest GT 0 THEN $
              thetag[poles[test]] = pi2 - SQRT(2d0/3d0)*hpx_sigma[test]
        ENDIF
        phig += 45d0
        poles = 0 & hpx_sigma = 0
        phi[good] = TEMPORARY(phig)/radeg
        theta[good] = TEMPORARY(thetag)
        good = 0
     ENDIF
     IF scalar THEN BEGIN
        phi = phi[0]
        theta = theta[0]
     ENDIF  
  end

  else:message,strupcase(projection_type) + $
               ' is not a valid projection type.  Reset CTYPE'

endcase

; Convert from "native" coordinate system to "standard" coordinate system
; if the CRVAL keyword is set.  Otherwise, assume the map projection is
; complete

 phi = phi*radeg
 theta = theta*radeg

 if ( N_elements(crval) GE 2 ) then begin

  if N_elements(map_type) EQ 0 then $
           map_type = where(projection_type EQ map_types)
   map_type = map_type[0]
   conic = (map_type GE 13) && (map_type LE 16)
   zenithal =  ((map_type GE 1) && (map_type LE 8)) || $
                (map_type EQ 26) || (map_type EQ 29)
   if conic then theta0 = pv2_1 else if zenithal then theta0 = 90 $
            else theta0 = 0

   wcs_rotate, longitude, latitude, phi, theta, crval, longpole=longpole, $
           theta0 = theta0, latpole = latpole, pv1=pv1, /REVERSE
 endif else begin    ;no rotation from standard to native coordinates

  latitude = theta
  longitude = phi

endelse

; CONVERT LONGITUDE FROM -180 TO 180 TO 0 TO 360

good = WHERE(FINITE(longitude), ngood)
IF ngood GT 0 THEN BEGIN
   lgood = longitude[good]
   temp = where(lgood lt 0.d0, Nneg)
   if (Nneg GT 0) then lgood[temp] = lgood[temp] + 3.6d2
   temp = where(lgood ge 3.6d2, Nneg)
   if (Nneg GT 0) then lgood[temp] = lgood[temp] - 3.6d2
   longitude[good] = lgood
ENDIF

 return
 end

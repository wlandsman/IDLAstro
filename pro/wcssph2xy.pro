;+
; NAME:
;     WCSSPH2XY
; PURPOSE:
;     Convert spherical coordinates to x and y (map) angular coordinates
; EXPLANATION:
;     Convert spherical (longitude and latitude -- sky) coordinates to x
;     and y intermediate world coordinates (still nominally in degrees) in
;     the projection plane of the map.  This procedure is the inverse of
;     WCSXY2SPH.    See WCS_DEMO for example of use.
;
;     This is a lower level procedure -- given a FITS header, the user will
;     usually use ADXY which will then call WCSSPH2XY with the appropriate
;     parameters.
; CATEGORY:
;     Mapping and Auxiliary FITS Routine
;
; CALLING SEQUENCE:
;      wcssph2xy, longitude, latitude, x, y, [ map_type , CTYPE = ,
;               FACE =, PV1= PV2= , CRVAL = , CRXY = , LONGPOLE = ,
;               LATPOLE = , PHI0 = , NORTH_OFFSET =, SOUTH_OFFSET =, BADINDEX =]
;
; INPUT PARAMETERS:
;     longitude - longitude of data, scalar or vector, in degrees
;     latitude  - latitude of data, same number of elements as longitude,
;                 in degrees
;     map_type  - optional positional parameter, numeric scalar (0-29)
;               corresponding to a particular map projection.  This is not a
;               FITS standard, it is simply put in to allow function similar
;               to that of less general map projection procedures (eg AITOFF).
;               The following list gives the map projection types and their
;               respective numbers.
;
;  FITS  Number  Name                       Comments
;  code   code
;  ----  ------  -----------------------    -----------------------------------
;   DEF     0    Default = Plate Carree
;   AZP     1    Zenithal perspective       PV2_1 required
;   TAN     2    Gnomic                     AZP w/ mu = 0
;   SIN     3    Orthographic               PV2_1,PV2_2 optional
;   STG     4    Stereographic              AZP w/ mu = 1
;   ARC     5    Zenithal Equidistant
;   ZPN     6    Zenithal polynomial        PV2_0, PV2_1....PV2_20 possible
;   ZEA     7    Zenithal equal area
;   AIR     8    Airy                       PV2_1 required
;   CYP     9    Cylindrical perspective    PV2_1 and PV2_2 required
;   CAR    10    Plate Carree
;   MER    11    Mercator
;   CEA    12    Cylindrical equal area     PV2_1 required
;   COP    13    Conical perspective        PV2_1 and PV2_2 required
;   COD    14    Conical equidistant        PV2_1 and PV2_2 required
;   COE    15    Conical equal area         PV2_1 and PV2_2 required
;   COO    16    Conical orthomorphic       PV2_1 and PV2_2 required
;   BON    17    Bonne's equal area         PV2_1 required
;   PCO    18    Polyconic
;   SFL    19    Sanson-Flamsteed  (GLS is allowed as a synonym for SFL)
;   PAR    20    Parabolic
;   AIT    21    Hammer-Aitoff
;   MOL    22    Mollweide
;   CSC    23    Cobe Quadrilateralized     convergence of inverse is poor
;                Spherical Cube
;   QSC    24    Quadrilateralized
;                Spherical Cube
;   TSC    25    Tangential Spherical Cube
;   SZP    26    Slant Zenithal Projection   PV2_1,PV2_2, PV2_3 optional
;   HPX    27    HealPix
;   HCT    28    HealCart (Cartesian approximation of Healpix)
;   XPH    29    HEALPix butterfly projection
;
; OPTIONAL INPUT KEYWORD PARAMETERS:
;
;     CTYPE - One, two, or three element vector containing 8 character
;              strings corresponding to the CTYPE1, CTYPE2, and CTYPE3
;              FITS keywords:
;
;               CTYPE[0] - first four characters specify standard system
;               ('RA--','GLON' or 'ELON' for right ascension, Galactic
;               longitude or ecliptic longitude respectively), second four
;               letters specify the type of map projection (eg '-AIT' for
;               Aitoff projection)
;               CTYPE[1] - first four characters specify standard system
;               ('DEC-','GLAT' or 'ELAT' for declination, galactic latitude
;               or ecliptic latitude respectively; these must match
;               the appropriate system of ctype1), second four letters of
;               ctype2 must match second four letters of ctype1.
;               CTYPE[2] - if present must be the 8 character string,'CUBEFACE',
;                only used for spherical cube projections to identify an axis
;               as containing the face on which each x and y pair of
;               coordinates lie.
;     PV2  -  Vector of projection parameter associated with latitude axis
;             PV2 will have up to 21 elements for the ZPN projection, up to 3
;             for the SIN projection and no more than 2 for any other
;             projection.   The first element corresponds to PV2_1, the
;             second to PV2_2, etc.
;     CRXY -    2 element vector giving the x and y coordinates of the
;               reference point. if this is not set the offset is [0,0].
;               Used to implement (x0,y0) in Sect 2.5 of Griesen & Calabretta 2002
;               Do not confuse with CRPIX.
;
;    Parameters simply passed to WCS_ROTATE:
;  
;     CRVAL - 2 element vector containing standard system coordinates (the
;               longitude and latitude) of the reference point
;     PV1   - Vector of projection parameters associated with longitude
;     LONGPOLE -  native longitude of standard system's North Pole
;     LATPOLE  -  "target" native latitude of the standard system's North Pole
;
;    Parameters intended to enhance invertability:
;
;     NORTH_OFFSET - offset (radians) added to input points near north pole.
;     SOUTH_OFFSET - offset (radians) added to input points near south pole.
;
; OUTPUT PARAMETERS:
;
;       x - x coordinate of data, same number of elements as longitude, in
;               degrees; if CRXY is set, then x will be returned offset by
;               crxy[0].  NOTE: x in all map projections increases to the
;               left, not the right.
;       y - y coordinate of data, same number of elements as longitude, in
;               degrees; if CRXY is set, y will be returned offset by crxy[1]
;
; OPTIONAL OUTPUT KEYWORD PARAMETERS:
;       FACE - a output variable used for spherical cube projections to
;               designate the face of the cube on which the x and y
;               coordinates lie.   Will contain the same number of elements as
;               X and Y.    Must contain at least 1 arbitrary element on input
;               If FACE is NOT defined on input, it is assumed that the
;               spherical cube projection is laid out over the whole sky
;               in the "sideways T" configuration.
;     BADINDEX - vector, list of transformed points too close to poles.
;
; NOTES:
;       The conventions followed here are described in more detail in
;       "Representations of Celestial Coordinates in FITS" by Calabretta
;       and  Greisen (2002, A&A, 395, 1077; also see
;       http://fits.gsfc.nasa.gov/fits_wcs.html).  The general
;       scheme outlined in that article is to first use WCS_ROTATE to convert
;       coordinates in one of three standard systems (celestial, galactic,
;       or ecliptic) into a "native system" of latitude and longitude.  The
;       latitude and longitude are then converted into x and y coordinates
;       which depend on the map projection which is performed.   The rotation
;       from standard to native coordinates can be skipped if one so desires.
;       This procedure necessitates two basic sections.  The first converts
;       "standard" coordinates to "native" coordinates while the second converts
;       "native" coordinates to x and y coordinates.  The first section is
;       simply a call to WCS_ROTATE, while the second contains the guts of
;       the code in which all of the map projection is done.  This procedure
;       can be called in a form similar to AITOFF, EQPOLE, or QDCB by calling
;       wcssph2xy with a fifth parameter specifying the map projection by
;       number and by not using any of the keywords related to the map
;       projection type (e.g. CTYPE).
;
; PROCEDURE:
;
;       The first task of the procedure is to do general error-checking to
;       make sure the procedure was called correctly and none of the
;       parameters or keywords conflict.  This is particularly important
;       because the procedure can be called in two ways (either using
;       FITS-type keywords or using a number corresponding to a map projection
;       type).  All variables are converted into double precision values and
;       angular measurements are converted from degrees into radians.
;       If necessary, longitude values are converted into the range -pi to pi.
;       Any latitude points close to the  of the poles are mapped to a specific
;       latitude of  from the pole so that the map transformations become
;       completely invertible.  The magnitude of this correction is given by
;       the keywords NORTH_OFFSET and SOUTH_OFFSET and a list of affected
;       points is optionally returned in the "badindex" output parameter.
;       The next task of the procedure is to convert the "standard"
;       coordinates to "native" coordinates by rotating the coordinate system.
;       This rotation is performed by the procedure WCS_ROTATE and is governed
;       by the keywords CRVAL and LONGPOLE.   The final task of the WCSSPH2XY
;       is to take "native" latitude and longitude coordinates and convert
;       them into x and y coordinates.  Any map specific error-checking is
;       done at this time.  All of the equations were obtained from
;       "Representations of Celestial Coordinates in FITS" and cases needing
;       special attention are handled appropriately (see the comments with
;       individual map projections for more information on special cases).
;
;       Note that a further transformation (using the CD matrix) is required
;       to convert the (x,y) coordinates to pixel coordinates.
; COMMON BLOCKS:
;
;       none
;
; PROCEDURES CALLED:
;       WCS_ROTATE
;
; ORIGINAL AUTHOR:
;
;       Rick Balsano   LANL   V1.1     8/31/93
;
; MODIFICATIONS/REVISION LEVEL:
;       2.3     9/15/93  W. Landsman (HSTX) Update quad cube coords, vectorize
;                        keywords
;       2.4     12/29/93 I. Freedman (HSTX) Eliminated LU decomposition
;       2.5     1/5/93   I. Freedman (HSTX) Offset keywords / bad point index
;       2.6     Dec 94   Compute pole for transformations where the reference
;                       pixel is at the native origin    W. Landsman (HSTX)
;       2.7     May 95  Change internal variable BETA for V4.0 compatibility
;       2.8     June 95 Change loop indices from integer to long
;       2.9     3/18/96 Change FACE usage for cube projections to match WCSLIB
;                       C/FORTRAN software library.
;       2.10    02/18/99 Fixed implementation of ARC algorithm
;       2.11    June 2003 Update conic projections, add LATPOLE keyword
;     	2.12  	Aug 2003, N.Rich - Fix pre-V5.5 bug from previous update
;       2.13    Sep 2003, W. Landsman CTYPE keywords need not be 8 characters
;       2.14    Jan 2004, W. Landsman don't modify scalars, fix PARabolic code
;       2.15    Feb 2004, W. Landsman Fix AZP and AIR algorithms
;       3.0    May 2004  W. Landsman Support extended SIN (=NCP), slant zenithal
;                  (SZP), and zenithal polynomial (ZPN) projections, use
;                   PV2 keyword vector instead of PROJP1, PROJP2
;       3.1     Jul 2005 W.Landsman/C. Markwardt Set unprojectable points in
;                   tangent projection to NaN
;       3.1.1   Jul 2005 Fixed 3.1 mod to work for scalars
;       3.2     Dec 2005 Fixed Airy projection for latitude centered at 90 deg
;       3.3     Aug 2007 R. Munoz, W.Landsman Correct treatment of PV1_2 and
;                        PV2_2 parameters
;       3.4    Oct 2007  Sergey Koposov Support HEALPIX projection
;       3.4.1  June 2009 Check for range of validity of ZPN polynomial W.L.
;       3.5    May 2012  Benjamin Alan Weaver, Add nonstandard HEALCART 
;                        projection, Allow map_index to be > 25
;       3.5.1  May 2013  W. Landsman Allow GLS as a synonym for SFL
;       3.6    Jul 2013  J. P. Leahy added XPH projection, apply polar offsets
;                        only for cylindrical & conic projections. 
;       3.6.1  Dec 2013  W. Landsman Polar offsets done in radians
;       3.6.2  Jan 2016  W. Landsman Lat and Long can have different size so long 
;                        as they have the same number of elements
;-

PRO wcssph2xy,longitude,latitude,x,y,map_type, ctype=ctype,$
              face = face, pv1 = pv1, pv2 = pv2, crval = crval, $
              crxy = crxy, longpole = longpole, latpole = latpole, $
              north_offset = north_offset, south_offset = south_offset, $
              badindex = badindex

compile_opt idl2, hidden


; DEFINE ANGLE CONSTANTS
 pi = !DPI
 pi2 = pi/2.d0
 radeg = 57.295779513082323d0
 map_types=['DEF','AZP','TAN','SIN','STG','ARC','ZPN','ZEA','AIR','CYP',$
            'CAR','MER','CEA','COP','COD','COE','COO','BON','PCO','SFL',$
            'PAR','AIT','MOL','CSC','QSC','TSC','SZP','HPX','HCT','XPH']

; check to see that enough parameters (at least 4) were sent
 if (N_params() lt 4) then begin
    print,'Syntax - WCSSPH2XY, longitude, latitude, x, y, [ map_type,'
    print,'           CTYPE= ,FACE=, PV1=, PV2=, CRVAL=, CRXY=, LATPOLE='
    print,'           LONGPOLE= ,NORTH_OFFSET=, SOUTH_OFFSET=, BADINDEX=]'
    return
 endif


; GENERAL ERROR CHECKING
; find the number of elements in each of the data arrays

 n_long = N_elements( longitude )
 n_lat  = N_elements( latitude )
 ; check to see that the data arrays have the same size
 if n_long NE n_lat then begin
     message,$
       'LONGITUDE and LATITUDE must have the same number of elements.'
 endif

 if (N_params() eq 5) then begin

  if keyword_set(ctype) then message,$
  'Use either the MAP_TYPE positional parameter or set the projection type' + $
  ' with CTYPE, but not both.'

; set projection_type string using map_type parameter (a number)
  ntypes = n_elements(map_types)
  if (N_ELEMENTS(map_type) eq 1 && map_type ge 0 && $
      map_type lt ntypes) then begin
         projection_type = map_types[map_type]
  endif else message,'MAP_TYPE must be a scalar >= 0 and < '+$
            strtrim(string(ntypes),2)+'; it was set to '+$
            strtrim(string(map_type),2)

endif else if (n_params() eq 4) then  wcs_check_ctype, ctype, projection_type 
    ; checks CTYPE format and extract projection type

; this sets the default map projection type for the cases when map_type or
; projection_type is set to 'DEF' or if projection_type is not set at this
; point.  As suggested in 'Representations of Celestial Coordinates in FITS'
; the default type is set to CAR (Plate Caree) the simplest of all projections.
 if ((n_elements(projection_type) eq 0) || $
     (projection_type eq 'DEF') ) then begin
           projection_type='CAR'
        message, /INFORMATIONAL, $
          'Projection type not supplied, set to default (Plate Caree)'
 endif

; Check to make sure all the correct parameters and keywords are set for
; spherical projections.
if (N_ELEMENTS(ctype) EQ 3 || keyword_set(face) || (projection_type eq 'CSC') || $
    (projection_type eq 'QSC') || (projection_type eq 'TSC')) then begin

  noface = n_elements(face) eq 0

endif

; check to see if the x and y offsets are set properly.  If not, break out
; of program.  If the x and y offsets are not set then assume they are zero.
if ((n_elements(crxy) ne 0) && (n_elements(crxy) ne 2)) then $
    message,'Offset keyword CRXY must contain 2 elements'

if ((n_elements(crval) ne 0) && (n_elements(crval) ne 2)) then $
    message,'CRVAL keyword must contain 2 elements'


; Convert all longitude values into the range -180 to 180 so that equations
; work properly.
  lng = double( longitude )   & lat = double( latitude )
  temp = where(lng ge 180d0, Ntemp)
  if Ntemp GT 0 then lng[temp] = lng[temp] - 360.0d0

; Convert from standard coordinate system to "native" coordinate system
; if the CRVAL keyword is set.  Otherwise, assume the latitude and longitude
; given are in "native" coordinates already (this is  essentially what is done
; in the procedure AITOFF).

 PV2_1 = N_elements(pv2) GT 0 ? pv2[0] : 0
 PV2_2 = N_elements(pv2) GT 1 ? pv2[1] : 0

 if N_elements(map_type) EQ 0 then begin
     wmt      = where(projection_type EQ map_types)
     map_type = wmt[0]
 endif

 conic = (map_type GE 13) && (map_type LE 16)
 zenithal = ((map_type GE 1) && (map_type LE 8)) || $
             (map_type EQ 26) || (map_type EQ 29)
 cylindrical = (map_type GE 9 && map_type LE 12) || $
             map_type EQ 27 || map_type EQ 28 
; Rotate from standard celestial coordinates into the native system.
        if conic then theta0 = PV2_1 else if zenithal then theta0 = 90 $
                 else theta0 = 0
 if N_elements(crval) GE 2 then begin
        wcs_rotate, lng, lat, phi, theta, crval, pv1 = pv1, $
                latpole = latpole, longpole=longpole, theta0 = theta0
	
        phi   /= radeg
        theta /= radeg
 endif else begin
     phi = lng/radeg
     theta = lat/radeg
 endelse

  IF cylindrical || conic  THEN BEGIN
; Make small offsets at poles to allow the transformations to be
; completely invertible. They are necessary in cylindrical & conic 
; projections since the pole is mapped to a line in the projection plane. 
; These introduce a small fractional error but only at the poles. 
;
     IF N_elements(north_offset) EQ 0 then north_offset = 1.d-7
     IF N_elements(south_offset) EQ 0 then south_offset = 1.d-7

     bad = where(abs(theta - pi2) lt north_offset, Nbad)
     IF (Nbad GT 0) THEN BEGIN
         MESSAGE,/INFORM,'Some input points are too close to the NORTH pole.'
         theta[bad] = pi2 - north_offset
         IF KEYWORD_SET(badindex) THEN badindex = bad
     ENDIF
     bad = where(abs(theta + pi2) lt south_offset, Nbad)
     IF (Nbad GT 0) THEN BEGIN
         MESSAGE,/INFORM,'Some input points are too close to the SOUTH pole.'
         lat[bad] = south_offset - pi2
         IF KEYWORD_SET(badindex) THEN BEGIN
             badindex = [badindex, bad]
             badindex = badindex[sort(badindex)]
         ENDIF
     ENDIF
 ENDIF
 
; BRANCH BY MAP PROJECTION TYPE
case strupcase(projection_type) of
  'AZP':begin
     if (PV2_1 lt 0) then message,$
      'AZP map projection requires the keyword PV2_1 >= 0'
    gamma = PV2_2/radeg
    mu = PV2_1

    r_theta = radeg*cos(theta)*(mu + 1.d0)/ $
             ( (mu + sin(theta)) + cos(theta)*cos(phi)*tan(gamma))
    x = r_theta*sin(phi)
    y = -r_theta*cos(phi)/cos(gamma)
  end
  'SZP': begin
     mu = N_elements(PV2) GT 0 ? PV2[0] : 0
     phi_c = N_elements(PV2) GT 1 ? PV2[1] : 0
     theta_c = N_elements(PV2) GT 1 ? PV2[2] : 90
     phi_c = phi_c/radeg & theta_c = theta_c/radeg
     xp = -mu*cos(theta_c)*sin(phi_c)
     yp =  mu*cos(theta_c)*cos(phi_c)
     zp =  mu*sin(theta_c) + 1.
     denom = zp - (1-sin(theta))
     x = radeg*( zp*cos(theta)*sin(phi) - xp*(1-sin(theta)) )/ denom
     y = -radeg*( zp*cos(theta)*cos(phi) + yp*(1-sin(theta)) )/ denom

     end
  'TAN':begin
    sz_theta = size(theta,/dimen)
    if sz_theta[0] EQ 0 then x = !Values.D_NAN else $
          x = make_array(value = !values.D_NAN, dimen=sz_theta)
    y = x
    g = where(theta GT 0, Ng)
    if Ng GT 0 then begin
        r_theta = radeg/tan(theta[g])
        x[g] = r_theta*sin(phi[g])
        y[g] = -r_theta*cos(phi[g])
    endif
  end

  'SIN':begin
    if N_elements(PV2_1) EQ 0 then PV2_1 = 0
    if N_elements(PV2_2) EQ 0 then PV2_2 = 0
    if (PV2_1 EQ 0) && (PV2_2 EQ 0) then begin
        r_theta = radeg*cos(theta)
        x = r_theta*sin(phi)
        y = -r_theta*cos(phi)
    endif else begin                   ;NCP projection
        x =  radeg*(cos(theta)*sin(phi) + PV2_1*(1-sin(theta)) )
        y = -radeg*(cos(theta)*cos(phi) - PV2_2*(1-sin(theta)) )
    endelse
  end

  'STG':begin
    r_theta = 2.d0*radeg*tan((pi2-theta)/2.d0)
    x = r_theta*sin(phi)
    y = -r_theta*cos(phi)
  end

  'ARC':begin
    r_theta = radeg*( pi2 - theta )
    x = r_theta*sin(phi)
    y = -r_theta*cos(phi)
  end

  'ZPN':begin
    z = pi2 - theta
    g = where(pv2 NE 0, Ng)
    np = Ng GT 0 ? max(g) : 0
    par = pv2[0:np]
    Nbad  = 0
;Check for range of validity for a nonlinear polynomial.    Set the derivative
; to zero and check for any real, positive roots.
    if np GT 2 then begin
          dpar = (indgen(np)+1) * par[1:*]     ;Polynomial derivative
	  zroots = fz_roots(dpar)               ;Find zeros
	  g = where(imaginary(zroots) EQ 0, Ng)      ;Any real roots?
          if Ng GT 0 then zroots = float(zroots[g])
	  g = where(zroots gt 0,Ng)
	  if Ng GT 0 then rlim = min(zroots[g])
	  bad = where(z GT rlim, Nbad)
    endif
    r_theta = radeg*poly(z, par)
    x = r_theta*sin(phi)
    y = -r_theta*cos(phi)
    if Nbad GT 0 then begin
        x[bad] = !VALUES.D_NAN
	y[bad] = !VALUES.D_NAN
	endif
    end


  'ZEA':begin
    r_theta = 2.d0*radeg*sin((pi2 - theta)/2.d0)
    x = r_theta*sin(phi)
    y = -r_theta*cos(phi)
  end

  'AIR':begin
    if ~keyword_set(PV2_1) then begin
      message,/informational,$
          'PV2_1 not set, using default of PV2_1 = 90 for AIR map projection'
      PV2_1 = 9.d1
    endif
    theta_b = PV2_1/radeg

    xi = (pi2 - theta)/2.d0

; When theta_b (aka PV2_1 in radians) is equal to pi/2 the normal equations
; for the AIR projection produce infinities.  To avoid the problem, values
; of theta_b equal to pi/2 cause a different set of equations to be used.
    if (theta_b eq pi2) then begin

; AIR produces the same radii for different latitudes, causing some overlap.  To
; avoid this problem, if latitudes which are far enough south to be a problem
; are included in the data, the routine will stop.

      if (min(theta) lt -36/radeg) then begin
        message,'AIR produces overlap of native latitudes south of ',/continue
        print,'-36 with the PV2_1 = 90'
        return
      endif

; points with xi too small are labelled as bad to prevent poor behavior of the
; equation for r_theta
      good = where(abs(xi) ge 1.d-10, Ngood)
      r_theta = lng*0
      if (Ngood GT 0) then $
        r_theta[good] = -2*radeg*(alog(cos(xi[good]))/tan(xi[good]) - $
	                 0.5*tan(xi[good]))

    endif else begin
      xi_b = (pi2 - theta_b)/2.d0
      a = alog(cos(xi_b))/tan(xi_b)/tan(xi_b)

; AIR produces the same radii for different latitudes, causing some overlap.  To
; avoid this problem, if latitudes which are far enough south to be a problem
; are included in the data, the routine will stop.

      xi_temp = (findgen(90) + 1)/radeg
      radius=-radeg*(alog(cos(xi_temp))/tan(xi_temp)+alog(cos(xi_b))/$
                                                      tan(xi_b)*tan(xi_temp))
      i = 0
      repeat i = i + 1 $
      until ((radius[i + 1] le radius[i]) || (i eq n_elements(radius) - 2))
      if (i lt (n_elements(radius)- 2)) then min_lat = 90 - 2*radeg*xi_temp[i] $
      else min_lat = -90
      if (min(theta) lt min_lat[0]/radeg) then begin
        message,'AIR produces overlap of native latitudes south of ',/continue
        print,format='(i3,a21,i3)',min_lat[0],' with the PV2_1 = ',PV2_1
        return
      endif

; points with xi too small are labelled as bad to prevent poor behavior of the
; equation for r_theta

      good = where(abs(xi) ge 1.d-10, Ngood)
      r_theta = lng*0
      if (Ngood GT 0) then r_theta[good] = -2*radeg*(alog(cos(xi[good]))/$
        tan(xi[good]) + a*tan(xi[good]))
    endelse
    x = r_theta*sin(phi)
    y = -r_theta*cos(phi)
  end

  'CYP':begin
    if (n_elements(PV2_1) eq 0) then begin
      message,/informational,$
           'PV2_1 not set, using default of PV2_1 = 0 for CYP map projection'
      PV2_1 = 0.d0
    endif
    if (n_elements(PV2_2) eq 0) then begin
      message,/informational,$
           'PV2_2 not set, using default of PV2_2 = 1 for CYP map projection'
      PV2_2 = 1.d0
    endif
    if (PV2_1 eq -PV2_2) then message,$
      'PV2_1 = -PV2_2 is not allowed for CYP map projection.'

    x = PV2_2*radeg*phi
    y = radeg*(PV2_1 + PV2_2)*sin(theta)/(PV2_1 + cos(theta))
  end

  'CAR':begin
    x = radeg*phi
    y = radeg*theta
  end

  'MER':begin
    x = radeg*phi
    y = radeg*alog(tan((pi2 + theta)/2.d0))
  end

  'CEA':begin
    if N_elements(PV2_1) EQ 0  then message,$
      'CEA map projection requires that PV2_1 keyword be set.'
    if ((PV2_1 le 0) || (PV2_1 gt 1)) then message,$
      'CEA map projection requires 0 < PV2_1 <= 1'
    x = radeg*phi
    y = radeg*sin(theta)/PV2_1
  end

  'COP':begin
    if ~keyword_set(PV2_1) then message,$
      'COP map projection requires that PV2_1 keyword be set.'
    if ~keyword_set(PV2_2) then begin
      message,/informational,$
      'PV2_2 not set, using default of PV2_2 = 0 for COP map projection'
      PV2_2= 0
    endif
    if ((PV2_1 lt -90) || (PV2_2 gt 90) || (PV2_1 gt 90)) then message,$
 'PV2_1 and PV2_2 must satisfy -90<=PV2_1<=90,0<=PV2_2<=90 for COP projection'
    if (PV2_1 eq -PV2_2) then message,$
 'COP projection with PV2_1=-PV2_2 is better done as a cylindrical projection'
    theta_a = PV2_1/radeg
    alpha = PV2_2/radeg
    bad = where((theta ge theta_a + pi2) or (theta le theta_a - pi2))
    if (bad[0] ne -1) then begin
      message,/continue,$
  'COP map projection diverges for native latitude = PV2_1 +- 90.'
      message,'Remove these points and try again.'
    endif

    r_theta = radeg*cos(alpha)*(1.d0/tan(theta_a)-tan(theta-theta_a))
    a_phi = phi*sin(theta_a)
    y_0 = radeg*cos(alpha)/tan(theta_a)
    x = r_theta*sin(a_phi)
    y = y_0 - r_theta*cos(a_phi)

  end

  'COD':begin
    if ~keyword_set(PV2_1) then message,$
      'COD map projection requires that PV2_1 keyword be set.'
    if ~keyword_set(PV2_2) then begin
      message,/informational,$
     'PV2_2 not set, using default of PV2_2 = 0 for COD map projection'
      PV2_2 = 0
    end
    if ((PV2_1 lt -90) || (PV2_2 gt 90) || (PV2_1 gt 90)) then message,$
 'PV2_1 and PV2_2 must satisfy -90<=PV2_1<=90,PV2_2<=90 for COD projection'
    if (PV2_1 eq -PV2_2) then message,$
    'COD gives divergent equations for PV2_1 = -PV2_2'
    theta_a = PV2_1/radeg

; when PV2_1 not = PV2_2 use regular equations
  if (PV2_2 NE 0) then begin
      alpha = PV2_2/radeg
      r_theta = theta_a - theta + alpha/(tan(alpha)*tan(theta_a))
      a_phi = sin(theta_a)*sin(alpha)*phi/alpha
      y_0 = radeg*alpha/(tan(alpha)*tan(theta_a))
; if the two parameters PV2_1 and PV2_2 are equal use the simpler set of
; equations
    endif else begin
      r_theta = theta_a - theta + 1.d0/tan(theta_a)
      a_phi = phi*sin(theta_a)
      y_0 = radeg/tan(theta_a)

    endelse
    x = radeg*r_theta*sin(a_phi)
    y = y_0 - radeg*r_theta*cos(a_phi)

  end

  'COE':begin
    if N_elements(PV2_1) EQ 0 then message,$
      'COE map projection requires that PV2_1 keyword be set.'
    if N_elements(PV2_2) EQ 0 then begin
      message,/informational,$
      'PV2_2 not set, using default of PV2_2 = 0 for COE map projection'
      PV2_2 = 0
    end
    if ((PV2_1 lt -90) || (PV2_2 gt 90) || (PV2_1 gt PV2_2)) then message,$
 'PV2_1 and PV2_2 must satisfy -90<=PV2_1<=PV2_2<=90 for COE map projection'
    if (PV2_1 eq -PV2_2) then message,$
    'COE gives divergent equations for PV2_1 = -PV2_2'

    theta_1 = (PV2_1 - PV2_2)/radeg
    theta_2 = (PV2_1 + PV2_2)/radeg
    s_1 = sin(theta_1)
    s_2 = sin(theta_2)
    stheta_a = sin(PV2_1/radeg)
    gamma = s_1 + s_2
    r_theta=radeg*2.d0*sqrt(1.d0+ s_1*s_2-gamma*sin(theta))/gamma

     a_phi = phi*gamma/2.d0
    y_0 = radeg*2.d0*sqrt(1.d0+ s_1*s_2-gamma*stheta_a)/gamma
    x = r_theta*sin(a_phi)
    y = y_0 - r_theta*cos(a_phi)
  end

  'COO':begin
    if ~keyword_set(PV2_1) then message,$
      'COO map projection requires that PV2_1 keyword be set.'
    if ~keyword_set(PV2_2) then begin
      message,/informational,$
      'PV2_2 not set, using default of PV2_2 = 0 for COO map projection'
      PV2_2 = 0
    end
    if ((PV2_1 lt -90) || (PV2_2 gt 90) || (PV2_1 gt 90)) then message,$
 'PV2_1 and PV2_2 must satisfy -90<=PV2_1<=90,PV2_2<=90 for COO projection'
    if (PV2_1 eq -PV2_2) then message,$
    'COO gives divergent equations for PV2_1 = -PV2_2'
    theta_1 = (PV2_1 - PV2_2)/radeg
    theta_2 = (PV2_1 + PV2_2)/radeg
    theta_a = PV2_1/radeg


; for cases where PV2_1 = 0, use a simpler formula to calculate c,
; otherwise use the regular formula
    if (PV2_2 eq 0) then c = sin(theta_1) else $
    c = alog(cos(theta_2)/cos(theta_1))/alog(tan((pi2-theta_2)/2.d0)/$
    tan((pi2-theta_1)/2.d0))

    alpha = radeg*cos(theta_1)/(c*(tan((pi2-theta_1)/2.d0))^c)
    r_theta = alpha*(tan((pi2-theta)/2.d0))^c
    y_0 = alpha*tan((pi2-theta_a)/2.)^c
    a_phi = c*phi
    x = r_theta*sin(a_phi)
    y = y_0 - r_theta*cos(a_phi)

  end

  'BON':begin
    if (N_elements(PV2) LT 1) then message,$
      'BON map projection requires that PV2_1 keyword be set.'
    pv2_1 = pv2[0]
    if ((PV2_1 lt -90) || (PV2_1 gt 90)) then message,$
      'PV2_1 must satisfy -90 <= PV2_1 <= 90 for BON map projection'
    if (PV2_1 eq 0) then message,$
      'PV2_1 = 0 for BON map projection is better done with SFL map projection'

    theta_1 = PV2_1/radeg
    s = theta_1/abs(theta_1)
    y_0 = 1.d0/tan(theta_1) + theta_1
    a = phi*cos(theta)/(y_0 - theta)
    x = radeg*(y_0 - theta)*sin(a)
    y = radeg*(y_0 - (y_0 - theta)*cos(a))
  end

  'PCO':begin
; The equations for x and y are poorly behaved for theta = 0.  Avoid this by
; explicitly assigning values for x and y when theta = 0.
    zero_ind = where(theta eq 0, Nzero)

; create x and y with same structure as longitude
    x = lng*0  & y = x
    if (Nzero GT 0) then begin
      x[zero_ind] = radeg*phi[zero_ind]
      y[zero_ind] = 0.d0
    endif
    good_ind = where(theta ne 0, Ngood)
    if Ngood GT 0 then begin
    x[good_ind] = radeg*sin(phi[good_ind]*sin(theta[good_ind]))/$
                  tan(theta[good_ind])
    y[good_ind] = radeg*(theta[good_ind]+$
        (1.d0 - cos(phi[good_ind]*sin(theta[good_ind])))/tan(theta[good_ind]))
    endif
  end

  'SFL':begin
    x = radeg*phi*cos(theta)
    y = radeg*theta
  end

  'GLS':begin        ;Alternative name for SFL projection
    x = radeg*phi*cos(theta)
    y = radeg*theta
  end

  'PAR':begin
    x = radeg*phi*(2.d0*cos(2.d0*theta/3.d0) - 1.d0)
    y = 180.0*sin(theta/3.d0)
  end

  'AIT':begin
    alpha = radeg*sqrt(2.d0/(1.d0 + cos(theta)*cos(0.5d0*phi)))
    x = 2.d0*alpha*cos(theta)*sin(0.5d0*phi)
    y = alpha*sin(theta)
  end

  'MOL':begin
; Use Newton's method to find a numerical solution to the equation:
;  alpha + 1/2*sin(2*alpha) - 1/2*pi*sin(theta) = 0
    tolerance = 1.0d-14
    alpha = lng*0
    repeat begin
    alpha_old = alpha
    alpha = alpha_old - (alpha_old + 0.5*sin(2.d0*alpha_old) - $
            0.5*pi*sin(theta))/(1.d0 + cos(2.d0*alpha_old))
    endrep until (max(abs(alpha - alpha_old)) lt tolerance)

    x = 2.d0^1.5*phi*radeg*cos(alpha)/pi
    y = sqrt(2.d0)*radeg*sin(alpha)
  end

  'CSC':begin
; calculate direction cosines
    l = cos(theta)*sin(phi)
    m = cos(theta)*cos(phi)
    n = sin(theta)

; determine the face on which the x and y coordinates will reside by setting
; rho equal to the maximum of n,m,l,-m,-l,-n which corresponds to faces 0
; through 5 respectively
    rho =  lng*0
    if size(lng,/N_dimen) EQ 0 then  face = 0 else face = lonarr(n_long)

; use an array to store a remapping of the direction cosines.  This way, faces
; 0 and 5 take points on their borders with faces 1-4.  The reason for this is
; that if the max function sees identical values in an array, it takes the
; index of the first occurrence of that value.
    remap = [0,5,2,1,4,3]

    for i = 0l, n_long-1 do begin
      dir_cos = float([n[i],-n[i],l[i],m[i],-l[i],-m[i]])
      rho[i] = max(dir_cos,temp)
      face[i] = remap[temp]
    endfor

; based on the face determined for each point, find the parameters alpha and
; beta1
    alpha = lng*0
    beta1 = alpha
    for i = 0l, n_long-1 do begin
      case face[i] of
        0:begin
          alpha[i] = l[i]/n[i]
          beta1[i] = -m[i]/n[i]
        end
        1:begin
          alpha[i] = l[i]/m[i]
          beta1[i] = n[i]/m[i]
        end
        2:begin
          alpha[i] = -m[i]/l[i]
          beta1[i] = n[i]/l[i]
        end
        3:begin
          alpha[i] = l[i]/m[i]
          beta1[i] = -n[i]/m[i]
        end
        4:begin
          alpha[i] = -m[i]/l[i]
          beta1[i] = -n[i]/l[i]
        end
        5:begin
          alpha[i] = -l[i]/n[i]
          beta1[i] = -m[i]/n[i]
        end
      endcase
    end

; define all of the numerical constants to use in determining x and y
    r_0 = 0.577350269
    gam_s = 1.37484847732
    em = 0.004869491981
    gam = -0.13161671474
    ome = -0.159596235474
    d_0 = 0.0759196200467
    d_1 = -0.0217762490699
    c_00 = 0.141189631152
    c_10 = 0.0809701286525
    c_01 = -0.281528535557
    c_20 = -0.178251207466
    c_11 = 0.15384112876
    c_02 = 0.106959469314
    fconst = 45.0d0

    x = fconst*(alpha*gam_s+alpha^3*(1-gam_s)+alpha*beta1^2*(1-alpha^2)*$
        (gam+(em-gam)*alpha^2+(1-beta1^2)*(c_00+c_10*alpha^2+c_01*beta1^2+$
        c_20*alpha^4+c_11*alpha^2*beta1^2+c_02*beta1^4))+alpha^3*(1-alpha^2)*$
        (ome-(1-alpha^2)*(d_0+d_1*alpha^2)))
    y = fconst*(beta1*gam_s+beta1^3*(1-gam_s)+beta1*alpha^2*(1-beta1^2)*$
        (gam+(em-gam)*beta1^2+(1-alpha^2)*(c_00+c_10*beta1^2+c_01*alpha^2+$
        c_20*beta1^4+c_11*beta1^2*alpha^2+c_02*alpha^4))+beta1^3*(1-beta1^2)*$
        (ome-(1-beta1^2)*(d_0+d_1*beta1^2)))


    if noface eq 1 then begin
        xf=fconst*[0.0d0,0.0d0,2.0d0,4.0d0,6.0d0,0.0d0]
        yf=fconst*[2.0d0,0.0d0,0.0d0,0.0d0,0.0d0,-2.0d0]
        x=x+xf[face]
        y=y+yf[face]
    endif
  end

  'QSC':begin
; calculate direction cosines
    l = cos(theta)*sin(phi)
    m = cos(theta)*cos(phi)
    n = sin(theta)

; determine the face on which the x and y coordinates will reside by setting
; rho equal to the maximum of n,m,l,-m,-l,-n which corresponds to faces 0
; through 5 respectively
    rho = lng*0
    if size(lng,/N_dimen) EQ 0 then face = 0 else face = lonarr(n_long)

; use an array to store a remapping of the direction cosines.  This way, faces
; 0 and 5 take points on their borders with faces 1-4.  The reason for this is
; that if the max function sees identical values in an array, it takes the
; index of the first occurrence of that value.
    remap = [0,5,2,1,4,3]

    for i = 0l, n_long-1 do begin
      dir_cos = float([n[i],-n[i],l[i],m[i],-l[i],-m[i]])
      rho[i] = max(dir_cos,temp)
      face[i] = remap[temp]
    endfor

; based on the face determined for each point, find the parameters alpha and
; beta1
    alpha = lng*0
    beta1 = alpha
    for i = 0l, n_long-1 do begin
      case face[i] of
        0:begin
          alpha[i] = l[i]/n[i]
          beta1[i] = -m[i]/n[i]
        end
        1:begin
          alpha[i] = l[i]/m[i]
          beta1[i] = n[i]/m[i]
        end
        2:begin
          alpha[i] = -m[i]/l[i]
          beta1[i] = n[i]/l[i]
        end
        3:begin
          alpha[i] = l[i]/m[i]
          beta1[i] = -n[i]/m[i]
        end
        4:begin
          alpha[i] = -m[i]/l[i]
          beta1[i] = -n[i]/l[i]
        end
        5:begin
          alpha[i] = -l[i]/n[i]
          beta1[i] = -m[i]/n[i]
        end
      endcase
    end

    x = lng*0
    y = x &  xi = y

    s = 2.d0*(((alpha gt abs(beta1)) or (beta1 ge abs(alpha))) - 0.5d0)

    case_1 = where(abs(alpha) gt abs(beta1))
    case_2 = where((abs(alpha) le abs(beta1)) and (beta1 ne 0.d0))
    case_3 = where((alpha eq 0.d0) and (beta1 eq 0.d0))
    if (case_1[0] ne -1) then xi[case_1] = beta1[case_1]/alpha[case_1]
    if (case_2[0] ne -1) then xi[case_2] = alpha[case_2]/beta1[case_2]
    if (case_3[0] ne -1) then xi[case_3] = 0.d0

    fconst=45.0d0
    u = fconst*s*sqrt((1.d0 - rho)/(1.d0 - 1.d0/sqrt(2.d0 + xi^2)))
    v = (u/1.5d1)*radeg*(atan(xi) - asin(xi/sqrt(2.d0*(1.d0 + xi^2))))
    if (case_1[0] ne -1) then begin
      x[case_1] = u[case_1]
      y[case_1] = v[case_1]
    endif
    if (case_2[0] ne -1) then begin
      x[case_2] = v[case_2]
      y[case_2] = u[case_2]
    endif
    if (case_3[0] ne -1) then begin
      x[case_3] = 0.d0
      y[case_3] = 0.d0
    endif

    if noface eq 1 then begin
        xf=fconst*[0.0d0,0.0d0,2.0d0,4.0d0,6.0d0,0.0d0]
        yf=fconst*[2.0d0,0.0d0,0.0d0,0.0d0,0.0d0,-2.0d0]
        x=(x+xf[face])
        y=(y+yf[face])
    endif
  end

  'TSC':begin
; calculate direction cosines
    l = cos(theta)*sin(phi)
    m = cos(theta)*cos(phi)
    n = sin(theta)

; determine the face on which the x and y coordinates will reside by setting
; rho equal to the maximum of n,m,l,-m,-l,-n which corresponds to faces 0
; through 5 respectively
    rho = lng*0
    if size(lng,/N_dimen) EQ 0 then face = 0 else face = lonarr(n_long)

; use an array to store a remapping of the direction cosines.  This way, faces
; 0 and 5 take points on their borders with faces 1-4.  The reason for this is
; that if the max function sees identical values in an array, it takes the
; index of the first occurrence of that value.
    remap = [0,5,2,1,4,3]

    for i = 0l, n_long-1 do begin
      dir_cos = float([n[i],-n[i],l[i],m[i],-l[i],-m[i]])
      rho[i] = max(dir_cos,temp)
      face[i] = remap[temp]
    endfor

; based on the face determined for each point, find the parameters eta and xi
    eta = lng*0
    xi = eta
    for i = 0l, n_long-1 do begin
      case face[i] of
        0:begin
          eta[i] = -m[i]
          xi[i] = l[i]
        end
        1:begin
          eta[i] = n[i]
          xi[i] = l[i]
        end
        2:begin
          eta[i] = n[i]
          xi[i] = -m[i]
        end
        3:begin
          eta[i] = n[i]
          xi[i] = -l[i]
        end
        4:begin
          eta[i] = n[i]
          xi[i] = m[i]
        end
        5:begin
          eta[i] = m[i]
          xi[i] = l[i]
        end
      endcase
    endfor
    fconst = 45.0d0
    r_theta = fconst/tan(asin(rho))
    a_phi = atan(xi,-eta)
    x = r_theta*sin(a_phi)
    y = -r_theta*cos(a_phi)
    if noface eq 1 then begin
        xf=fconst*[0.0d0,0.0d0,2.0d0,4.0d0,6.0d0,0.0d0]
        yf=fconst*[2.0d0,0.0d0,0.0d0,0.0d0,0.0d0,-2.0d0]
        x=(x+xf[face])
        y=(y+yf[face])
    endif
  end

  'HPX': begin
;
; See Calabretta & Roukema 2007, MNRAS, 381, 865
;
      pv2_1 = N_ELEMENTS(pv2) GE 1 ? pv2[0] : 4.d
      pv2_2 = N_ELEMENTS(pv2) GE 2 ? pv2[1] : 3.d
      hpx_k = pv2_2                  ; The main generalised HEALPIX parameters
      hpx_h = pv2_1                  ;
      ik = ROUND(hpx_k)
      ih = ROUND(hpx_h)

      thetalim = asin((hpx_k-1d0)/hpx_k)

      eqfaces = where( abs(theta) le thetalim, complement=polfaces)
      x = phi  ; make x & y arrays in same shape as phi/theta.  
      y = phi

; equatorial region  
      if eqfaces[0] ne -1 then begin
          x[eqfaces] = phi[eqfaces]*radeg
          y[eqfaces] = (90d * hpx_k / hpx_h) * sin( theta[eqfaces])
      endif

;polar regions
      if polfaces[0] ne -1 then begin
          hpx_sig = sqrt ( hpx_k * (1d0 - abs(sin(theta[polfaces]))))
          hpx_omega = ((hpx_k mod 2 eq 1) or theta[polfaces] gt 0)*1.D
          hpx_phic = -180d0 + (2*floor((phi[polfaces]*radeg+180d0)*hpx_h/360d0 + $
                                  (1-hpx_omega)/2.) + hpx_omega)*180d0/hpx_h
          x[polfaces] = hpx_phic + (phi[polfaces]*radeg-hpx_phic) * hpx_sig
          y[polfaces] = 180./hpx_h * ((theta[polfaces] gt 0)*2-1) * $
                                ((hpx_k+1)/2 - hpx_sig)
      endif
  end
  'HCT':begin
    x = phi*radeg
    y = DBLARR(N_ELEMENTS(theta))
    thetalim = ASIN(2.D/3.D)
    w_np = WHERE(theta GE thetalim, n_np)
    w_eq = WHERE((theta LT thetalim) AND (theta GT -thetalim), n_eq)
    w_sp = WHERE(theta LE -thetalim, n_sp)
    IF n_np GT 0 THEN y[w_np] = 45.D*(2.D - SQRT(3.D*(1.D - SIN(theta[w_np]))))
    IF n_eq GT 0 THEN y[w_eq] = 45.D*(3.D/2.D)*SIN(theta[w_eq])
    IF n_sp GT 0 THEN y[w_sp] = 45.D*(SQRT(3.D*(1.D + SIN(theta[w_sp])))-2.D)
  end

  'XPH':begin
;
; HEALPix butterfly projection: see Calabretta & Lowe (2013)
; 
    scale = 1d0/sqrt(2d0)
    thetalim = asin(2d0/3d0)
    out_of_range = WHERE(phi EQ !dpi, nout)
    IF nout GT 0 THEN phi[out_of_range] = -!dpi
    xi  = phi           ; get array of same shape
    eta = SIN(theta)
    test = 0*FIX(xi)
    psi = (phi*radeg + 180d0) mod 90d0
    eqfaces = where(abs(theta) le thetalim, complement=polfaces)
    IF eqfaces[0] NE -1 THEN BEGIN
        xi[eqfaces]  = psi[eqfaces]
        eta[eqfaces] *= 67.5d0
    ENDIF
    IF polfaces[0] NE -1 THEN BEGIN
        hpx_sigma = SQRT(3d0*(1d0 - ABS(eta[polfaces])))
        xi[polfaces] = 45d0 + (psi[polfaces] - 45d0)*hpx_sigma
        sgn = 2*(theta[polfaces] GT 0) - 1
        eta[polfaces] = TEMPORARY(sgn)*(90d0 - 45d0*hpx_sigma)
    ENDIF
    psi = 0
    xi  -= 45d0
    eta -= 90d0
    x = xi + eta
    y = TEMPORARY(xi) - TEMPORARY(eta)
    quad = WHERE((-!dpi LE phi) AND (phi LT -pi2))
    IF quad[0] NE -1 THEN BEGIN
        temp    = x[quad]
        x[quad] = -y[quad]
        y[quad] = -temp
        test[quad] = 1
    ENDIF
    quad = WHERE((-pi2 LE phi) AND (phi LT 0))
    IF quad[0] NE -1 THEN BEGIN
        y[quad] *= -1d0
        test[quad] = 1
    ENDIF
    quad = WHERE((0d0 LE phi) AND (phi LT pi2))
    IF quad[0] NE -1 THEN BEGIN
        temp    = x[quad] 
        x[quad] = y[quad]
        y[quad] = temp
        test[quad] = 1
    ENDIF
    quad = WHERE((pi2 LE phi) AND (phi LT !dpi))
    IF quad[0] NE -1 THEN BEGIN
        x[quad]  *= -1d0
        test[quad] = 1
    ENDIF
    quad = 0
    x *= scale
    y *= scale
  end
  else:message,strupcase(projection_type)+$
               ' is not a valid projection type.  Reset CTYPE'
endcase

if keyword_set(crxy) && ~array_equal(crxy, [0d0,0d0]) then begin
    x = x - crxy[0]
    y = y - crxy[1]
endif

END

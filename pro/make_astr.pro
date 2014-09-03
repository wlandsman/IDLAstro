pro make_astr,astr, CD=cd, DELTA = cdelt, CRPIX = crpix, CRVAL = crval, $
                    CTYPE = ctype, LATPOLE = LATPOLE, LONGPOLE = longpole, $ 
                    PV2 = pv2, NAXIS = naxis, AXES = axes, pv1 = pv1, $
                    RADECSYS = radecsys, EQUINOX = equinox, $
                    DATE_OBS = dateobs, MJD_OBS = mjdobs
;+
; NAME:
;       MAKE_ASTR
; PURPOSE:
;       Build an astrometry structure from input parameter values
; EXPLANATION:
;       This structure can be subsequently placed in a FITS header with 
;       PUTAST
;
; CALLING SEQUENCE:
;       MAKE_ASTR, astr, CRPIX =, CRVAL =, [CD = , DELT =,  CTYPE =,    $
;               LATPOLE = , LONGPOLE =, PV2 =, NAXIS =, AXES =, PV1 =,  $
;               RADECSYS =, EQUINOX =, DATEOBS =, MJDOBS =]
;
; OUTPUT PARAMETER:
;       ASTR - Anonymous structure containing astrometry info.  See the 
;              documentation for EXTAST for descriptions of the individual
;              tags
;
; REQUIRED INPUT KEYWORDS
;       CRPIX - 2 element vector giving X and Y coordinates of reference pixel
;               (def = NAXIS/2).  VALUES MUST BE IN FITS CONVENTION (first pixel
;               is [1,1]) AND NOT IDL CONVENTION (first pixel is [0,0]).
;       CRVAL - 2 element double precision vector giving R.A. and DEC of 
;               reference pixel in DEGREES
; OPTIONAL INPUT KEYWORDS
;       CD -  2 x 2 array containing the astrometry parameters CD1_1 CD1_2
;              in DEGREES/PIXEL                                CD2_1 CD2_2
;       DELT - 2 element vector giving physical increment at reference pixel
;              in DEGREES/PIXEL default = [-1.0D, 1.0D]/3600.  (1 arcsec/pixel)
;       CTYPE - 2 element string vector giving projection types, default
;              ['RA---TAN','DEC--TAN']
;       LATPOLE - Scalar latitude of the north pole, default = +90
;       LONGPOLE - scalar longitude of north pole
;       PV2 - Vector of projection parameters associated with latitude axis.   
;             Not required for some projections (e.g. TAN) and optional for 
;             others (e.g. SIN).
;             Usually a 2 element vector, but may contain up to 21 elements
;             for the Zenithal Polynomial (ZPN) projection.   Corresponds to 
;             the keywords PV2_1, PV2_2...  Defaults to 0.0
;
;      Added for version 2 astrometry structure:
;      AXES  - 2 element integer vector giving the FITS-convention axis 
;              numbers associated with astrometry, in ascending order. 
;              Default [1,2].
;      NAXIS - 2 element integer vector giving number of pixels on each axis
;      PV1 -  Vector of projection parameters associated with longitude axis
;             Elements 4 & 5 (if present) are equivalent to LONGPOLE & LATPOLE 
;             and take precedence if both are specified, i.e. LONGPOLE & LATPOLE
;             in the structure are forced to agree with PV1.
;      RADECSYS - String giving RA/Dec system e.g. 'FK4', 'ICRS' etc.
;      EQUINOX  - Double giving the epoch of the mean equator and equinox
;      DATEOBS  - Text string giving (start) date/time of observations
;      MJDOBS   - Modified julian date of start of observations.
;              (specify one or other of DATEOBS or MJDOBS)
;
; NOTES:
;       (1) An anonymous structure is created to avoid structure definition
;               conflicts.    This is needed because some projection systems
;               require additional dimensions (i.e. spherical cube
;               projections require a specification of the cube face).
;       (2) The name of the keyword for the CDELT parameter is DELT because
;               the IDL keyword CDELT would conflict with the CD keyword
;       (3) The astrometry structure definition was slightly modified in 
;               July 2003; all angles are now double precision, and the 
;               LATPOLE tag was added.   In April 2007 the CRPIX tag was also
;               changed to double precision.
; REVISION HISTORY:
;       Written by   W. Landsman              Mar. 1994
;       Added LATPOLE, all angles double precision  W. Landsman July 2003
;       Use PV2 keyword rather than PROJP1, PROJP2 W. Landsman May 2004
;       Make .CRPIX tag double precision, change CDELT default to 1"/pixel
;                      W. Landsman April 2007
;        Default plate scale is now 1"/pixel (not 1 deg/pix)  WL  Oct. 2010
;        Oct 2010 change should only apply when CD matrix not given 
;                     M. Cushing/W.L.  Aug 2011
;        added v2 parameters; more filling out of defaults; default 
;        LATPOLE changed to 90 (FITS standard) J. P. Leahy Jul 2013
;-
 On_error, 0
 compile_opt idl2

 if ( N_params() LT 1 ) then begin
	print,'Syntax - MAKE_ASTR, astr, CRPIX =, CRVAL =, [CD = , DELT =,  '
        print,'	       CTYPE =, LATPOLE= , LONGPOLE =, PV2=, NAXIS =, AXES=,'
        print,'        PV1=, RADECSYS= , EQUINOX=, DATEOBS=, MJDOBS= ]'
	return
 endif

;
; List of known map types copied from wcsxy2sph. Needs to be kept up
; to date!
;
 map_types=['DEF','AZP','TAN','SIN','STG','ARC','ZPN','ZEA','AIR','CYP',$
            'CAR','MER','CEA','COP','COD','COE','COO','BON','PCO','SFL',$
            'PAR','AIT','MOL','CSC','QSC','TSC','SZP','HPX','HCT','XPH']

; If neither CD nor CDELT keywords present then assume 1"/pixel
; If CD supplied but not CDELT then set CDELT = [1.0,1.0] 

 if N_elements( cd ) EQ 0 then begin 
     cd = [ [1.,0.], [0.,1.] ]
     if N_elements( cdelt) EQ 0 then cdelt = [-1.0D, 1.0D]/3600.0d
 endif else if N_elements( cdelt) EQ 0 then cdelt = [1.0D, 1.0D]    
     
 if N_elements( crpix) EQ 0 then message, $
	'ERROR - CRPIX is a required keyword for a new astrometry structure'
 
 if N_elements( crval) EQ 0 then message, $
	'ERROR - CRVAL is a required keyword for a new astrometry structure'

 if N_elements( ctype)  EQ 0 then ctype = ['RA---TAN','DEC--TAN']

 N_pv2 = N_elements(pv2) 
 IF N_pv2 EQ 0 then pv2 = 0.0D
 
 if N_elements(axes) EQ 0 then axes = [1,2]
 
 ; Search astrometric axes:
 lon0 = WHERE(STRMID(ctype,0,5) EQ 'RA---')
 lon1 = WHERE(STRMID(ctype,1,4) EQ  'LON-')
 lon2 = WHERE(STRMID(ctype,2,4) EQ   'LN-')
 lon = [lon0, lon1, lon2]
 form = [REPLICATE(0,N_ELEMENTS(lon0)),REPLICATE(1,N_ELEMENTS(lon1)), $
         REPLICATE(2,N_ELEMENTS(lon2))]
 good = WHERE(lon GE 0, ngood)
 IF ngood GT 1 THEN MESSAGE, 'Both axis types are longitude!'
 lon  = ngood EQ 1 ? lon[good] : -1
 lon_form = ngood EQ 1 ? form[good] : -1 

 lat0 = WHERE(STRMID(ctype,0,5) EQ 'DEC--')
 lat1 = WHERE(STRMID(ctype,1,4) EQ  'LAT-')
 lat2 = WHERE(STRMID(ctype,2,4) EQ   'LT-')
 lat = [lat0, lat1, lat2]
 form = [REPLICATE(0,N_ELEMENTS(lat0)),REPLICATE(1,N_ELEMENTS(lat1)), $
         REPLICATE(2,N_ELEMENTS(lat2))]
 good = WHERE(lat GE 0, ngood)
 IF ngood GT 1 THEN MESSAGE, 'Both axis types are latitude"
 lat  = ngood EQ 1 ? lat[good] : -1
 lat_form = ngood EQ 1 ? form[good] : -1 
 
 badco = lon_form NE lat_form 
 CASE lon_form OF
     -1: coord = 'X'  ; unknown type of coordinate
      0: coord = 'C'  ; celestial coords, i.e. RA/Dec
      1: BEGIN  ; longitude format is xLON where x = G, E, etc.
          coord = STRMID(ctype[0],0,1)
          badco = badco || coord NE STRMID(ctype[1],0,1)
      END
      2: BEGIN  ; longitude format is yzLN 
          coord = STRMID(ctype[0],0,2)
          badco = badco || coord NE STRMID(ctype[2],0,2)
      END
      ELSE: MESSAGE, 'Internal error: unexpected lon_form' 
 ENDCASE
 
 flip = lat[0] LT lon[0]
 
 proj = STRMID(ctype[0], 5, 3)
 badco = badco || proj NE STRMID(ctype[1], 5, 3)
 IF badco THEN MESSAGE, 'ERROR: longitude and latitude coordinate types must match:'
 
 test = WHERE(proj EQ map_types)
 known = test GE 0
 
 npv1 = N_ELEMENTS(pv1)
 IF npv1 EQ 5 THEN latpole  = pv1[4]
 IF npv1 GE 4 THEN longpole = pv1[3]
 IF npv1 GE 3 THEN theta0 = pv1[2] 
 IF npv1 GE 2 THEN phi0 = pv1[1] ELSE phi0 = 0
 IF npv1 GE 2 THEN xyoff = pv1[0] NE 0 ELSE xyoff = 0
 
 IF N_ELEMENTS(latpole) EQ 0 THEN latpole = 90
 
 conic = (proj EQ 'COP') || (proj EQ 'COE') || (proj EQ 'COD') || $
         (proj EQ 'COO')

 IF conic THEN BEGIN 
     IF N_pv2 EQ 0 THEN message, $
     'ERROR -- Specify PV2 for conic projections'
     theta_a = pv2[0]
 ENDIF ELSE BEGIN ; Is it a zenithal projection?
     if (proj EQ 'AZP') || (proj EQ 'SZP') || (proj EQ 'TAN') || $
        (proj EQ 'STG') || (proj EQ 'SIN') || (proj EQ 'ARC') || $
        (proj EQ 'ZPN') || (proj EQ 'ZEA') || (proj EQ 'AIR') || $
        (proj EQ 'XPH') then begin
         theta_a = 90d0
     endif else theta_a = 0d0
 ENDELSE
     
 IF N_ELEMENTS(theta0) EQ 0 THEN theta0 = theta_a 
 
 IF N_ELEMENTS(longpole) EQ 0 THEN BEGIN
     if crval[1] GE theta0 then longpole = 0d0 else longpole = 180d0
     longpole += phi0
 ENDIF
 
 pv1 = [xyoff, phi0, theta0, longpole, latpole]
 
 x0y0 = [0d0, 0d0]
 IF xyoff && (phi0 NE 0d0 || theta0 NE theta_a) THEN BEGIN 
 ; calculate IWC offsets x_0, y_0
     WCSSPH2XY, phi0, theta0, x0, y0, CTYPE = ctype, PV2 = pv2
     x0y0 = [x0, y0]
 ENDIF 
 
 N_rdsys = N_ELEMENTS(radecsys)
 IF N_rdsys EQ 0 THEN radecsys = '' ELSE $
     radecsys = STRUPCASE(STRTRIM(radecsys,2))
 N_mjd = N_ELEMENTS(mjdobs) 
 IF N_mjd EQ 0 THEN mjdobs   = !values.D_NAN
 N_date = N_ELEMENTS(dateobs)
 IF N_date EQ 0 THEN dateobs  = 'UNKNOWN' ELSE $
     dateobs = STRUPCASE(STRTRIM(dateobs,2))
 
 IF N_mjd GT 0 && N_date EQ 0 THEN dateobs = date_conv(mjdobs+2400000.5d0,'FITS')
 IF N_date GT 0 THEN BEGIN
     dateobs = date_conv(dateobs,'FITS', BAD_DATE=bad_date) ; try to convert to standard format
     IF ~bad_date THEN BEGIN
        mjdtest = date_conv(dateobs,'MODIFIED')
        IF N_mjd EQ 0 THEN mjdobs = mjdtest ELSE $
            IF ABS(mjdtest - mjdobs) GT 1 THEN MESSAGE, $
               'DATE-OBS and MJD-OBS are inconsistent'
     ENDIF ELSE dateobs = 'UNKNOWN'
 ENDIF

 N_Eq = N_ELEMENTS(equinox)
 IF N_Eq EQ 0 THEN equinox = !values.D_NAN
 IF (coord EQ 'C' || coord EQ 'E' || coord EQ 'H') THEN BEGIN
     IF N_rdsys EQ 0 THEN BEGIN
         IF N_eq EQ 0 THEN radecsys = 'ICRS' $
         ELSE radecsys = equinox GE 1984d0 ? 'FK5' : 'FK4'
     ENDIF ELSE IF N_eq EQ 0 THEN CASE STRMID(radecsys,0,3) OF
           'FK4': equinox = 1950d0
           'FK5': equinox = 2000d0
           'ICR': equinox = 2000d0
           ELSE: equinox = 0d0
     ENDCASE
 ENDIF
 
 IF N_ELEMENTS(naxis) NE 2 THEN naxis = [0,0]
 
 ASTR = {NAXIS:naxis, CD: cd, CDELT: cdelt, CRPIX: crpix, CRVAL: crval, $
         CTYPE: string(ctype), $
         LONGPOLE: double( longpole[0]),  LATPOLE: double(latpole[0]), $
         PV2: pv2, PV1: pv1, $
         AXES: axes, REVERSE: flip, $
         COORD_SYS: coord, PROJECTION: proj, KNOWN: known, $
         RADECSYS: radecsys, EQUINOX: DOUBLE(equinox), $
         DATEOBS: dateobs, MJDOBS: DOUBLE(mjdobs), X0Y0: x0y0}
 
  return
  end

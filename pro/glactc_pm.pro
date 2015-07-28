pro glactc_pm,ra,dec,mu_ra,mu_dec,year,gl,gb,mu_gl,mu_gb,j, $
	degree=degree, fk4 = fk4, SuperGalactic = superGalactic, mustar=mustar
;+
; NAME:  
;       GLACTC_PM
; PURPOSE:
;        Convert between celestial and Galactic (or Supergalactic) proper
;        motion (also converts coordinates).
; EXPLANATION:
;       Program to convert proper motion in equatorial coordinates (ra,dec)
;       to proper motion in Galactic coordinates (gl, gb) or Supergalacic 
;       Coordinates  (sgl,sgb)  or back to equatorial coordinates (j=2).
;       The proper motion unit is arbitrary, but be sure to set /MUSTAR if
;       units are the projection of the proper motion on the RA, Dec axis.
;       It does precession on the coordinates but does not
;       take care of precession of the proper motions which is usually a
;       very small effect.
;
; CALLING SEQUENCE: 
;       GLACTC_PM, ra, dec, mu_ra,mu_dec,year, gl, gb, mu_gl, mu_gb, j, 
;                  [ /DEGREE, /FK4, /SuperGalactic, /mustar ]
;
; INPUT PARAMETERS: 
;       year     equinox of ra and dec, scalar       (input)
;       j        direction of conversion     (input)
;		1:  ra,dec,mu_ra,mu_dec --> gl,gb,mu_gl,mu_gb
;		2:  gl,gb,mu_gl,mu_gb  --> ra,dec,mu_ra,mu_dec
;
; INPUTS OR OUTPUT PARAMETERS: ( depending on argument J )
;       ra       Right ascension, hours (or degrees if /DEGREES is set), 
;                         scalar or vector.  
;       dec      Declination, degrees,scalar or vector
;       mu_ra    right ascension proper motion any proper motion unit
;					(angle/time)
;       mu_dec    declination proper motion in any proper motion unit
;					(angle/time)
;       gl       Galactic longitude, degrees, scalar or vector
;       gb       Galactic latitude, degrees, scalar or vector
;       mu_gl    galactic longitude proper motion in any time unit
;       mu_gb    galactic latitude proper motion in any time unit
;       All results forced double precision floating.
;
; OPTIONAL INPUT KEYWORD PARAMETERS:
;       /DEGREE - If set, then the RA parameter (both input and output) is 
;                given in degrees rather than hours. 
;       /FK4 - If set, then the celestial (RA, Dec) coordinates are assumed
;              to be input/output in the FK4 system.    By default,  coordinates
;              are assumed to be in the FK5 system.    For B1950 coordinates,
;              set the /FK4 keyword *and* set the year to 1950.
;       /SuperGalactic - If set, the GLACTC returns SuperGalactic coordinates
;              as defined by deVaucouleurs et al. (1976) to account for the 
;              local supercluster. The North pole in SuperGalactic coordinates 
;              has Galactic coordinates l = 47.47, b = 6.32, and the origin is 
;              at Galactic coordinates l = 137.37, b= 0 
;	/mustar - if set then input and output of mu_ra and mu_dec are the 
;                projections of mu in the ra or dec direction rather than
;		the d(ra)/dt or d(mu)/dt.  So mu_ra becomes mu_ra*cos(dec)
;               and mu_gl becomes mu_gl*cos(gb).
;              
; EXAMPLES:
;       Find the SuperGalactic proper motion of M33 given its
;       equatorial proper motion mu* =(-29.3, 45.2) microas/yr.
;       Where the (*) indicates ra component is actual mu_ra*cos(dec) 
;		(Position: RA (J2000): 01 33 50.9, Dec (J2000): 30 39 36.8)
;
;       IDL> glactc_pm, ten(1,33,50.9),ten(30,39,36.8),-29.3,45.2, 2000,$
;				sgl,sgb,mu_sgl,mu_sgb,1,/Supergalactic,/mustar
;       ==> SGL = 328.46732 deg, SGB = -0.089896901 deg, 
;			mu_sgl = 33.732 muas/yr, mu_gb = 41.996 muas/yr.
;         And for the roundtrip:
;       IDL> glactc_pm, ra,dec,mu_ra,mu_dec,2000,$
;       IDL>  sgl,sgb,mu_sgl,mu_sgb,2,/Supergalactic,/mustar
;        ==> ra=1.5641376 hrs., dec= 30.660277 deg,
;            mu_ra= -29.300000 muas/yr, mu_dec=i 45.200000 muas/yr
;
; PROCEDURE CALLS:
;       BPRECESS, JPRECESS, PRECESS
; HISTORY: 
;       Written                Ed Shaya, U of MD,  Oct 2009.
;       Adapted from GLACTC  to make proper motion transformations, 
;       Correct occasional sign error in galactic longitude E. Shaya  Nov 2011
;       Correct occasional sign error for year not set to 1950  W. Landsman,F. Mazzi July 2015
;-
IF n_PARAMS() LT 6 THEN BEGIN
	PRINT,'Syntax - glactc_pm,ra,dec,mu_ra,mu_dec,year,gl,gb,mu_gl,mu_gb, j, [/DEGREE, /FK4, /mustar]'
	PRINT,'j = 1: ra,dec,mu_ra,mu_dec --> gl,gb,mu_gl,mu_gb'
	PRINT, 'j = 2:  gl,gb,mu_gl,mu_gb --> ra,dec,mu_ra,mu_dec'
	RETURN
ENDIF
Radeg = 180.0d/!DPI
;
; Galactic pole at ra 12 hrs 49 mins, dec 27.4 deg, equinox B1950.0
; position angle from Galactic center to equatorial pole = 123 degs.

IF KEYWORD_SET(SuperGalactic) THEN BEGIN
    rapol = 283.18940711d/15.0d & decpol = 15.64407736d
    dlon =  26.73153707 
ENDIF ELSE BEGIN 
   rapol = 12.0d0 + 49.0d0/60.0d0 
   decpol = 27.4d0
   dlon = 123.0d0
ENDELSE
sdp = SIN(decpol/radeg)
cdp = SQRT(1.0d0-sdp*sdp)
radhrs=radeg/15.0d0

; Branch to required type of conversion.   Convert coordinates to B1950 as 
; necessary
CASE j OF                   
    1: BEGIN
        IF ~KEYWORD_SET(degree) THEN  ras = ra*15.0d ELSE ras =ra
        decs = dec
        IF ~KEYWORD_SET(fk4) THEN BEGIN
        	IF year NE 2000 THEN precess,ras,decs,year,2000
        	bprecess,ras,decs,ra2,dec2
        	ras = ra2
        	decs = dec2
        ENDIF ELSE IF year NE 1950 THEN precess,ras,decs,year,1950,/fk4
	raIndeg = ras
        ras = ras/radeg - rapol/radhrs 
        sdec = SIN(decs/radeg)
        cdec = SQRT(1.0d0-sdec*sdec)
        sgb = sdec*sdp + cdec*cdp*COS(ras)
        gb = radeg * ASIN(sgb)
        cgb = SQRT(1.0d0-sgb*sgb)
        sine = cdec * SIN(ras) / cgb
        cose = (sdec-sdp*sgb) / (cdp*cgb)
        gl = dlon - radeg*ATAN(sine,cose)
        ltzero=WHERE(gl lt 0.0, Nltzero)
        IF Nltzero GE 1 THEN gl[ltzero]=gl[ltzero]+360.0d0

; 	Calculate proper motions transforms for j = 1
;       Take derivative of sgb line above:
        IF ~KEYWORD_SET(mustar) THEN mu_ra = mu_ra*cdec
	mu_gb =  mu_dec*(cdec*sdp-sdec*cdp*COS(ras))/cgb $
		  - mu_ra*cdp*SIN(ras)/cgb
;	Get mu_gl by using the known length of the vector.
	mu_gl = SQRT(mu_dec^2 + mu_ra^2 - mu_gb^2)
        IF ~KEYWORD_SET(mustar) THEN mu_gl = mu_gl/cgb

;       However, sqrt gives an ambiguous sign.
;	Determine the sign by seeing which direction it is going in gl.
	glactc,raIndeg,decs,year,gl0,gb0,1,/degree,Supergalactic=Supergalactic
	ra_delta = 1d-2*mu_ra/ABS(mu_ra)
	dec_delta = 1d-2*mu_dec/ABS(mu_ra)
	glactc, raIndeg+ra_delta, decs+dec_delta, year, gl2, gb2, 1,$
		/degree,Supergalactic=Supergalactic
	IF (gl2 LT gl0) THEN mu_gl = -ABS(mu_gl)


        RETURN
        END
    2: BEGIN
        sgb = SIN(gb/radeg)
        cgb = SQRT(1.0d0-sgb*sgb)
        sdec = sgb*sdp + cgb*cdp*COS((dlon-gl)/radeg)
        dec = radeg * ASIN(sdec)
        cdec = SQRT(1.0d0-sdec*sdec)
        sinf = cgb * SIN((dlon-gl)/radeg) / cdec
        cosf = (sgb-sdp*sdec) / (cdp*cdec)
        ra = rapol + radhrs*ATAN(sinf,cosf)
        ra = ra*15.0d

;	Calculate proper motions for j=2, see above (j=1 case)
        IF ~KEYWORD_SET(mustar) THEN mu_gl = mu_gl*cgb
	mu_dec =  mu_gb*(cgb*sdp-sgb*cdp*COS((dlon-gl)/radeg))/cdec $
		+ mu_gl*cdp*SIN((dlon-gl)/radeg)/cdec
	mu_ra = SQRT(mu_gl^2 + mu_gb^2 - mu_dec^2)
        IF ~KEYWORD_SET(mustar) THEN mu_ra = mu_ra/cdec

;       However, sqrt gives an ambiguous sign.
;	Determine the sign by seeing which direction it is going in gl.
	glactc,raIndeg,decs0,year,gl,gb,2,/degree,Supergalactic=Supergalactic
	mu_gl_delta = 1d-2*mu_gl/ABS(mu_gl)
	mu_gb_delta = 1d-2*mu_gb/ABS(mu_gl)
	glactc, ra2, dec2, year, gl+mu_gl_delta, gb+mu_gb_delta, 2,$
		/degree,Supergalactic=Supergalactic
	IF (ra2 LT raIndeg) THEN mu_ra = -ABS(mu_ra)

        IF ~KEYWORD_SET(fk4) THEN BEGIN
        	ras = ra & decs = dec
        	jprecess,ras,decs,ra,dec
        	IF year NE 2000 THEN precess,ra,dec,2000,year
        ENDIF ELSE BEGIN
		IF year NE 1950 THEN precess,ra,dec,1950,year,/fk4
	ENDELSE 
        gt36 = WHERE(ra GT 360.0, Ngt36)
        IF Ngt36 GE 1 THEN ra[gt36] = ra[gt36] - 360.0d0
        IF ~KEYWORD_SET(degree) THEN ra = ra/15.0D0
        RETURN
        END
ENDCASE
END

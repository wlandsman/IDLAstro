PRO wcs_check_ctype, ctype, projection_type, coord_type
;+
; NAME:
;     WCS_CHECK_CTYPE
; PURPOSE:
;     Checks that a pair of CTYPE parameters conform to WCS format and return
;     the projection type and coordinate type extracted from them.
;
; EXPLANATION:
;
;     Stops with an error message if CTYPE does not conform to standard,
;     unless one or both CTYPE strings is missing.
;
;     If only CTYPE[0] is present, and is valid, this counts as a
;     "pass".
;
;     If ctype is unset, returns silently, with coord_type = 'X' and
;     projection_type = 'DEF'.
;
;     Low-level procedure extracted from WCSXY2SPH & WCSSPH2XY to reduce code 
;     duplication. 
;
; CATEGORY:
;     Mapping and Auxiliary FITS Routine
;
; CALLING SEQUENCE:
;      wcs_check_ctype, ctype, projection_type, [coord_type]
;
; INPUT PARAMETERS:
;     ctype  - astrometry-related CTYPE strings extracted from the header.
;
; OUTPUT PARAMETERS:
;     projection_type - three-character code specifying map projection.
;                       If ctype is not specified returns 'DEF' for default.
;     coord_type -      one- or two-character code specifying the coordinate
;                       type, 'X' (unknown) if not specified. 'C' for RA & Dec. 
;
; NOTES:
;       The conventions followed here check consistency with
;       "Representations of Celestial Coordinates in FITS" by Calabretta
;       and  Greisen (2002, A&A, 395, 1077; also see
;       http://fits.gsfc.nasa.gov/fits_wcs.html).
;
; PROCEDURE:
;       Astrometry CTYPEs should come in longitude and latitude pairs in one
;       of three formats: 'RA---xxx' & 'DEC--xxx', 'yLON-xxx' & 'yLAT-xxx', or
;       'zzLN-xxx' & 'zzLT-xxx' where xxx is the projection code and y or zz
;       specify the type of the latitude & longitude axes, e.g. Galactic,
;       Ecliptic etc. If the CTYPE pair is in this format, xxx is returned as
;       the projection type.
;
; COMMON BLOCKS:
;       none
;
; PROCEDURES CALLED:
;       none
;
; AUTHOR:
;
;       J. P. Leahy
;
; MODIFICATIONS/REVISION LEVEL:
;
;       1.0     Jul 2013 Extracted from WCSXY2SPH & WCSSPH2XY
;       1.1     Aug 2013 Now does actually stop if error detected.
;       1.2     Jan 2014 Recognize when RA, DEC reversed, W. Landsman
;-
COMPILE_OPT IDL2, hidden
ON_ERROR, 1

projection_type = 'DEF'
coord_type = 'X'
coord_form1 = 0
IF N_elements( ctype ) GE 1 THEN BEGIN
    ctype1 = strtrim(ctype[0],2)
    if strlen(ctype1) LT 8 then $
      message,'ERROR - ' + strupcase(ctype1) + $
      ' is not a valid spherical projection type.'
    projection_type = STRUPCASE(STRMID(ctype1,5,3))
    coord = STRUPCASE(STRMID(ctype1,0,4))
    coord_tail = STRMID(coord,2,2)
    bad_coord = 0B
    CASE coord_tail OF
        '--': BEGIN
            coord_form1 = 1
            bad_coord = coord NE 'RA--'
            coord_type = 'C'
        END
        'ON': BEGIN
            coord_form1 = 2
            bad_coord = STRMID(coord,1,3) NE 'LON'
            coord_type = STRMID(coord,0,1)
        END
        'LN': BEGIN
            coord_form1 = 3
            coord_type = STRMID(coord,0,2)
        END
	'C-': BEGIN
	     coord_form1 = 1
	     bad_coord = coord NE 'DEC-'
	     coord_type = 'C'
	     END
        ELSE: bad_coord = 1B
    ENDCASE
   
    IF bad_coord THEN BEGIN
        MESSAGE, 'Unrecognised first coordinate type:' +  coord, /continue
        MESSAGE, 'Should be ''RA--'' or ''xLON'' or ''xxLN'''
    ENDIF

    IF N_elements( ctype ) GE 2 THEN BEGIN
        ctype2 = ctype[1]
        if (projection_type ne STRUPCASE(STRMID(ctype2,5,3))) then begin
          message,'The same map projection type must be in characters',/continue
          message,'    5-8 of both CTYPE1 and CTYPE2.'
        endif
        coord = STRUPCASE(STRMID(ctype2,0,4))
        coord_tail = STRMID(coord,2,2)
        CASE coord_tail OF
            'C-': BEGIN
                bad_coord = coord NE 'DEC-'
                coord_form2 = 1
                coord_head2='C'
            END
	    '--': BEGIN
            coord_form2 = 1
            bad_coord = coord NE 'RA--'
            coord_head2 = 'C'
             END

            'AT': BEGIN
                bad_coord = STRMID(coord,1,3) NE 'LAT'
                coord_head2 = STRMID(coord,0,1)
                coord_form2 = 2
            END
            'LT': BEGIN
                coord_head2 = STRMID(coord,0,2)
                coord_form2 = 3
            END  
            ELSE: bad_coord = 1B
        ENDCASE
        IF bad_coord THEN BEGIN
            MESSAGE, 'Unrecognised second coordinate type:' + coord, /CONTINUE
            MESSAGE, 'Should be ''DEC-'' or ''xLAT'' or ''xxLT'''
        ENDIF
        if (coord_form1 NE coord_form2 || coord_type NE coord_head2) then begin
           message,'The same standard system must be in the first 4', /continue
           message,'characters of both CTYPE1 and CTYPE2.'
        endif
    ENDIF
ENDIF
END


pro getrot, hdr, rot, cdelt, DEBUG = debug, SILENT = silent, ALT=alt      ;GET ROTation 
;+
; NAME:
;    GETROT
; PURPOSE:
;     Return the rotation and plate scale of an image from its FITS header
; EXPLANATION:
;     Derive the counterclockwise rotation angle, and the X and Y scale
;     factors of an image, from a FITS image header.   The input parameter 
;     may be either a FITS image header or an astrometry structure (as 
;     obtained by extast.pro)
;
; CALLING SEQUENCE:
;     GETROT, Hdr, [ Rot, CDelt, /SILENT, DEBUG =  ]   
;             or 
;     GETROT, Astr, Rot, CDelt, /SILENT, DEBUG = ]       
;
; INPUT PARAMETERS:
;     HDR - FITS Image header (string array).  Program will extract the 
;             astrometry structure
;              or
;     ASTR -  ASTROMETRY structure, of the type returned by EXTAST.
;             See the documentation for EXTAST.PRO for details.
;
; OPTIONAL OUTPUT PARAMETERS:
;       ROT - Scalar giving the counterclockwise rotation of NORTH in DEGREES 
;               from the +Y axis of the image.
;       CDELT- 2 element vector giving the scale factors in DEGREES/PIXEL in 
;               the X and Y directions.   CDELT[1] is always positive, whereas
;               CDELT[0] is negative for a normal left-handed coordinate system,
;               and positive for a right-handed system. 
;
;       If no output variables are supplied (or /DEBUG is set), then GETROT 
;       will display the rotation and plate scale at the terminal.
;
; OPTIONAL INPUT KEYWORD
; 
;       ALT - single character 'A' through 'Z' or ' ' specifying an alternate
;             astrometry system present in the FITS header.   See extast.pro
;             for more information on the ALT keyword.    Ignored if an
;             astrometry structure rather than FITS header is supplied.
;       DEBUG - if DEBUG is set, GETROT will print the rotation for both the 
;           X and Y axis when these values are unequal.  If DEBUG is set to 2, 
;           then the output parameter ROT will contain both X and Y rotations.
;
;       /SILENT - if set, then do not provide a warning about a right-handed
;           coordinate system
; PROCEDURE:
;       If the FITS header already contains CDELT (and CD or CROTA) keyword,
;       (as suggested by the Calabretta & Greisen (2002, A&A, 395, 1077) FITS 
;       standard) then this is used for the scale factor.
;       
;       If the header contains CD keywords but no CDELT keywords (as in IRAF
;       headers) then the scale factor is derived from the CD matrix.
;
;       In case of skew (different rotations of the X and Y axes), the rotations
;       are averaged together if they are less than 2 degrees.   Otherwise,
;       a warning is given and the X rotation is used.  
;
; PROCEDURES USED:
;       EXTAST, GSSS_EXTAST
; REVISION HISTORY:
;       Written W. Landsman STX January 1987 
;       Option to return both rotations added.  J. D. Offenberg, STX, Aug 1991
;       Use new astrometry structure   W. Landsman  Mar 1994
;       Recognize a GSSS header        W. Landsman  June 1994
;       Correct rotation determination with unequal CDELT values WL October 1998
;       Consistent conversion between CROTA and CD matrix  WL  October 2000
;       Correct CDELT computations for rotations near 90 deg WL November 2002
;       Preserve sign in the CDELT output  WL June 2003
;       Check if latitude/longitude reversed in CTYPE  WL  February 2004
;       Fix problem in latitude check  M.Lombardi/W.Landsman Sep 2004
;       Added ALT keyword W. Landsman May 2005
;       Account for any rotation of the native system by examining the value
;        of LONGPOLE       H. Taylor/W. Landsman
;       Account for case where X,Y rotations differ by 2*!pi WL. Aug 2011
;-
 Compile_opt IDL2
 On_error,2

 if N_params() EQ 0 then begin
        print,'Syntax: GETROT, Hdr, [ Rot, CDelt, DEBUG= , /SILENT, ALT=]'
        print,'    OR: GETROT, Astr, [ Rot, CDelt, DEBUG= , /SILENT]'
        return
 endif

 tpi = 2*!dpi
 if ~keyword_set(DEBUG) then debug = 0
 radeg = 180.0/!DPI
 sz = size(hdr,/STR)                ;Did user supply a FITS header or a CD matrix?

 if ((sz.N_dimensions eq 1) && $
     (sz.type_name EQ 'STRING')) then begin     ;FITS header?

        extast,hdr,astr,alt=alt             ;Extract astrometry from header,
        if strmid(astr.ctype[0],5,3) EQ 'GSS' then begin
                hdr1 = hdr
                gsss_stdast, hdr1
                extast, hdr1, astr
        endif
        cd = astr.cd/RADEG      ;then extract CD matrix from astrometry.
        if N_elements(cd) NE 4 then $
            message,'ERROR - Header is missing astrometry keywords CD or CDELT'

endif else $
 if ((sz.N_dimensions eq 1) and $
     (sz.type_name EQ 'STRUCT')) then begin     ;Astrometry Structure?
      astr = hdr
      cd = astr.cd/RADEG
 endif else message, $
        'ERROR - First parameter must be an image header or astrometry structure'

 if astr.cdelt[0] NE 1.0 then begin
        cdelt = astr.cdelt
        cd[0,0] *= cdelt[0] & cd[0,1] *= cdelt[0]
        cd[1,1] *= cdelt[1] & cd[1,0] *= cdelt[1]
 endif else  cd = astr.cd/RADEG                                        
 
 ctype = strmid(astr.ctype[0],0,4)
; Check if first coordinate in CTYPE is latitude
 if (ctype EQ 'DEC-') or (strmid(ctype, 1) EQ 'LAT')  then $
      cd = reverse(cd,1)
 det = cd[0,0]*cd[1,1] - cd[0,1]*cd[1,0]
 if det LT 0 then sgn = -1 else sgn = 1
 if ~keyword_set(SILENT) then if det GT 0 then $
   message,'WARNING - Astrometry is for a right-handed coordinate system',/INF
 cdelt = fltarr(2)
 if (cd[1,0] eq 0) && (cd[0,1] eq 0) then begin ;Unrotated coordinates?
   rot = 0.
   rot2 = 0.
   cdelt[0] = cd[0,0]
   cdelt[1] = cd[1,1]
 endif else begin
   rot  = atan(  sgn*cd[0,1],  sgn*cd[0,0] ) 
   rot2 = atan( -cd[1,0],  cd[1,1] )
  

 if rot NE rot2 then begin          ;Handle unequal rotations
      if keyword_set(debug) then $
        print,'X axis rotation:',rot*!RADEG, ' Y axis rotation:',rot2*!RADEG
      if debug eq 2 then rot = [rot,rot2] else begin
      
      if abs(rot - rot2)*!RADEG LT 2 then rot = (rot + rot2)/2. else $
      if abs(rot - rot2- tpi)*!radeg lt 2  then rot = (rot +rot2 -tpi)/2. else $
      if abs(rot - rot2 +tpi)*!radeg lt 2  then rot = (rot +rot2 +tpi)/2. else $
         message,/INF, $
	 'WARNING: X and Y axis rotations differ by more than 2 degrees'
      endelse
      
 endif

  cdelt[0] =   sgn*sqrt(cd[0,0]^2 + cd[0,1]^2)
  cdelt[1] =   sqrt(cd[1,1]^2 + cd[1,0]^2)
 endelse

 rot = rot*RADEG   
 if astr.longpole NE 180.0d then rot = rot + ( 180.0d - astr.longpole )
 rot = float(rot)
 cdelt = float(cdelt*RADEG)

 if N_params() EQ 1 || keyword_set(DEBUG) then begin
        if debug LT 2 then print,'Rotation (counterclockwise)',rot,' degrees'
        print,'Sampling interval X axis',cdelt[0]*3600.,' arc seconds/pixel'
        print,'                  Y axis',cdelt[1]*3600.,' arc seconds/pixel'
 endif

 return
 end

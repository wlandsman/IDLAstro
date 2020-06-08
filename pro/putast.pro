pro putast, hdr, astr, crpix, crval, ctype, EQUINOX=equinox, $
                  CD_TYPE = cd_type, ALT = alt, NAXIS = naxis
;+
; NAME:
;    PUTAST
; PURPOSE:
;    Put WCS astrometry parameters into a given FITS header.
;
; CALLING SEQUENCE:
;     putast, hdr              ;Prompt for all values
;               or
;     putast, hdr, astr, [EQUINOX =, CD_TYPE =, ALT= , NAXIS=]
;               or
;     putast, hdr, cd,[ crpix, crval, ctype], [ EQUINOX =, CD_TYPE =, ALT= ]    
;
; INPUTS:
;     HDR -  FITS header, string array.   HDR will be updated to contain
;             the supplied astrometry.
;     ASTR - IDL structure containing values of the astrometry parameters
;            CDELT, CRPIX, CRVAL, CTYPE, LONGPOLE, and PV2
;            See EXTAST.PRO for more info about the structure definition
;                            or
;     CD   - 2 x 2 array containing the astrometry parameters CD1_1 CD1_2
;                                                             CD2_1 CD2_2
;              in units of DEGREES/PIXEL
;     CRPIX - 2 element vector giving X and Y coord of reference pixel
;              BE SURE THE COORDINATES IN CRPIX ARE GIVEN IN FITS STANDARD
;              (e.g. first pixel in image is [1,1] ) AND NOT IDL STANDARD
;              (first pixel in image is [0,0]
;     CRVAL - 2 element vector giving R.A. and DEC of reference pixel 
;               in degrees
;     CTYPE - 2 element string vector giving projection types for the two axes.
;             For example, to specify a tangent projection one should set
;             ctype = ['RA---TAN','DEC--TAN'] 
;
;     Fields added for version 2:
;      .PV1 - Vector of projection parameters associated with longitude axis
;      .AXES  - 2 element integer vector giving the FITS-convention axis 
;               numbers associated with astrometry, in ascending order. 
;               Default [1,2].
;      .REVERSE - byte, true if first astrometry axis is Dec/latitude
;      .COORDSYS - 1 or 2 character code giving coordinate system, including
;                 'C' = RA/Dec, 'G' = Galactic, 'E' = Ecliptic, 'X' = unknown.
;      .RADECSYS - String giving RA/Dec system e.g. 'FK4', 'ICRS' etc.
;      .EQUINOX  - Double giving the epoch of the mean equator and equinox
;      .DATEOBS  - Text string giving (start) date/time of observations
;      .MJDOBS   - Modified julian date of start of observations.
;      .X0Y0     - Not written to header.
;
;
; OUTPUTS:
;      HDR - FITS header now contains the updated astrometry parameters
;               A brief HISTORY record is also added.
;
; OPTIONAL KEYWORD INPUTS:
;       ALT -  single character 'A' through 'Z' or ' ' specifying an alternate 
;              astrometry system to write in the FITS header.    The default is
;              to write primary astrometry or ALT = ' '.   If /ALT is set, 
;              then this is equivalent to ALT = 'A'.   See Section 3.3 of 
;              Greisen & Calabretta (2002, A&A, 395, 1061) for information about
;              alternate astrometry keywords.
;
;
;       CD_TYPE - Integer scalar, either 0, 1 or 2 specifying how the CD matrix
;                is to be written into the header
;               (0) write PCn_m values along with CDELT values
;               (1) convert to rotation and write as a CROTA2 value (+ CDELT)
;               (2) as CDn_m values (IRAF standard) 
;
;            All three forms are valid representations according to Greisen &
;            Calabretta (2002, A&A, 395, 1061), also available at 
;            http://fits.gsfc.nasa.gov/fits_wcs.html ) although form (0) is 
;            preferred.   Form (1) is the former AIPS standard and is now  
;            deprecated and cannot be used if any skew is present.
;            If CD_TYPE is not supplied, PUTAST will try to determine the 
;            type of astrometry already in the header.   If there is no 
;            astrometry in the header then the default is CD_TYPE = 2.
;
;      EQUINOX - numeric scalar giving the year of equinox  of the reference 
;                coordinates.  Keyword value takes precedence over value in
;                astrometry structure which takes precedence over value in 
;                header; if none of these present then default is 2000.
;
;      NAXIS - By default, PUTAST does not update the NAXIS keywords in the
;            FITS header.    If NAXIS is set, and an astrometry structure is
;            supplied then the NAXIS1 and NAXIS2 keywords in the FITS header 
;            will be updated with the .NAXIS structure tags values.     If an 
;            astrometry structure is not supplied, then one can set NAXIS to a 
;            two element vector to update the NAXIS1, NAXIS2 keywords. 
; NOTES:
;       The recommended use of this procedure is to supply an astrometry
;       structure. This can be produced with MAKE_ASTR.    
;
;       If parameters are supplied by keyword, the full range of
;       astrometry header info is not supported by PUTAST.
;
;       PUTAST does not delete astrometry parameters already present in the 
;       header, unless they are explicity overwritten.   
;
;       If present in the astrometry structure, PUTAST will add SIP 
;      ( http://fits.gsfc.nasa.gov/registry/sip.html ) or TPV 
;       ( http://fits.gsfc.nasa.gov/registry/tpvwcs.html ) distortion parameters
;       to a FITS header.  
; PROMPTS:
;       If only a header is supplied, the user will be prompted for a plate 
;       scale, the X and Y coordinates of a reference pixel, the RA and
;       DEC of the reference pixel, the equinox of the RA and Dec and a 
;       rotation angle.
;
; PROCEDURES USED:
;       ADD_DISTORT, GETOPT(), GET_COORDS, GET_EQUINOX(), SXADDPAR, SXPAR(), 
;       TAG_EXIST(), ZPARCHECK
; REVISION HISTORY:
;       Written by W. Landsman 9-3-87
;       Major rewrite, use new astrometry structure   March, 1994
;       Use both CD and CDELT to get plate scale for CD_TYPE=1   September 1995
;       Use lower case for FITS keyword Comments  W.L.    March 1997
;       Fixed for CD_TYPE=1 and CDELT = [1.0,1.0]   W.L   September 1997
;       Default value of CD_TYPE is now 2, Use GET_COORDS to read coordinates
;       to correct -0 problem           W.L.  September 1997
;       Update CROTA1 if it already exists  W.L. October 1997
;       Convert rotation to degrees for CD_TYPE = 1  W. L.   June 1998
;       Accept CD_TYPE = 0 keyword input   W.L   October 1998
;       Remove reference to obsolete !ERR  W.L.  February 2000
;       No longer support CD001001 format, write default tangent CTYPE value
;       consistent conversion between CROTA and CD matrix W.L. October 2000
;       Use GET_EQUINOX to get equinox value  W.L.  January 2001
;       Update CTYPE keyword if previous value is 'LINEAR'  W.L. July 2001
;       Use SIZE(/TNAME) instead of DATATYPE()  W.L.   November 2001
;       Allow direct specification of CTYPE W.L.        June 2002
;       Don't assume celestial coordinates W. Landsman  April 2003
;       Make default CD_TYPE = 2  W. Landsman   September 2003
;       Add projection parameters, e.g. PV2_1, PV2_2 if present in the 
;       input structure   W. Landsman    May 2004
;       Correct interactive computation of image center W. Landsman Feb. 2005
;       Don't use CROTA (CD_TYPE=1) if a skew exists W. Landsman  May 2005
;       Added NAXIS keyword  W. Landsman   January 2007
;       Update PC matrix, if CD_TYPE=0 and CD matrix supplied W.L. July 2007
;       Don't write PV2 keywords for WCS types that don't use it W.L. Aug 2011
;       Add SIP distortion parameters if present W.L. April 2012
;       Work if empty distortion structure present  W.L. November 2012
;       Spurious error message introduced April 2012 if CD matrix rather
;         than structure supplied  W.L.  January 2013 
;       Allow for version 2 astrometry structure J. P. Leahy July 2013
;       Bug fix in interactive use JPL Aug 2013.
;       Support IRAF TNX projection  M. Sullivan U. of Southamptom March 2014
;       PV1_3, PV1_4 keywords take precedence over LONPOLE, LATPOLE keywords
;                   WL, August 2014
;       Fix typo spelling RADECSYS, don't use LONPOLE, LATPOLE in PV keywords when
;          TPV projection   WL  December 2015
;	    Corrected for case when Equinox is NaN in structure. J. Murthy May 2016
;       Fix when no structure supplied W. Landsman Oct 2018
;-
 compile_opt idl2
 npar = N_params()

 if ( npar EQ 0 ) then begin    ;Was header supplied?
        print,'Syntax: PUTAST, Hdr, astr, [ EQUINOX= , CD_TYPE=, ALT= ,/NAXIS]'
        print,'       or'
        print,'Syntax: PUTAST, Hdr, [ cd, crpix, crval, ctype, EQUINOX = , CD_TYPE =]'   
        return
 endif
 
 RADEG = 180.0d/!DPI
 ax = ['1','2'] ; Default axis numbers
 astr2 = 0B     ; Assume input astronomy structure (if any) is version 1.
                ; will be updated if not.
 
 zparcheck, 'PUTAST', hdr, 1, 7, 1, 'FITS image header'
 if N_elements(alt) EQ 0 then alt = '' else if (alt EQ '1') then alt = 'a'

 if ( npar EQ 1 ) then begin            ;Prompt for astrometry parameters?
   ctype = strtrim(sxpar(hdr,'CTYPE*', Count = N_Ctype),2)
   if (N_Ctype NE 2) || (ctype[0] EQ 'PIXEL') || (ctype[0] EQ 'LINEAR') then $
                ctype = ['RA---TAN','DEC--TAN']
   read,'Enter plate scale in arc seconds/pixel: ',cdelt
   inp =''
   print,'Reference pixel position should be in FITS convention'
   print,'(First pixel has coordinate (1,1) )'

GETCRPIX: print, $
  'Enter X and Y position of a reference pixel ([RETURN] for plate center)'
   read, inp
   if ( inp EQ '' ) then $ 
          crpix = [ sxpar(hdr,'NAXIS1')+1, sxpar(hdr,'NAXIS2')+1] / 2. $
     else crpix = getopt( inp, 'F')

   if N_elements( crpix ) NE 2 then begin
      print,'PUTAST: INVALID INPUT - Enter 2 scalar values'
      goto, GETCRPIX     
   endif

RD_CEN:
   inp = ''
   read,'Enter RA (hrs) and Dec (degrees) of reference pixel:',inp
   GET_COORDS, crval,in=inp
   if crval[0] EQ -999 then goto, rd_cen

   crval[0] = crval[0]*15.
 
   inp = ''
   read,'Enter rotation angle in degrees, East of north [0.]: ',inp
   rotat = getopt(inp,'F')/RADEG
   cd = (cdelt / 3600.)*[[-cos(rotat),-sin(rotat)], [-sin(rotat), cos(rotat)]]
   npar = 4
 endif else begin

   if size(astr,/TNAME) EQ 'STRUCT' then begin      
                                             ;User supplied astrometry structure
        cd = astr.cd
        cdelt = astr.cdelt
        crval = astr.crval
        crpix = astr.crpix
        ctype = astr.ctype
        if keyword_set(naxis)  then if tag_exist(astr,'NAXIS') then $
            naxis = astr.naxis
        longpole = astr.longpole
        if tag_exist(astr,'latpole') then latpole = astr.latpole
        if tag_exist(astr,'pv2') then pv2 = astr.pv2
        astr2 = TAG_EXIST(astr,'AXES')
        IF astr2 THEN BEGIN ; version 2 astrometry structure
           ax = STRTRIM(STRING(astr.axes),2)
           IF N_ELEMENTS(equinox) EQ 0 THEN $
              if (finite(astr.equinox)) then equinox = astr.equinox
        ENDIF
   endif else  begin
        cd = astr
        zparcheck,'PUTAST', cd, 2, [4,5], 2, 'CD matrix'
   endelse
 endelse
 

 ;Write NAXIS values
   if N_elements(naxis) EQ 2 then begin 
	      sxaddpar,hdr,'NAXIS'+ax[0],naxis[0],/SaveC
	      sxaddpar,hdr,'NAXIS'+ax[1],naxis[1],/SaveC
   endif      
 
;   Add CTYPE to FITS header

 if N_elements( ctype ) GE 2 then begin

 sxaddpar,hdr,'CTYPE'+ax[0]+alt,ctype[0],' Coordinate Type','HISTORY',/SaveC
 sxaddpar,hdr,'CTYPE'+ax[1]+alt,ctype[1],' Coordinate Type','HISTORY',/SaveC

 endif

;   Add EQUINOX keyword and value to FITS header

 if N_elements( equinox ) EQ 0 then begin        ;Is EQUINOX already in header?
    equinox = get_equinox( hdr, code)   
    if  code LT 0 then $
       sxaddpar, hdr, 'EQUINOX'+alt, 2000.0, ' Equinox of Ref. Coord.',  $
                      'HISTORY',/SaveC
 
 endif else $
     sxaddpar,hdr, 'EQUINOX'+alt, equinox, 'Equinox of Ref. Coord.', 'HISTORY',/Sav

; Add coordinate description (CD) matrix to FITS header
; 0. PCn_m keywords  1. CROTA + CDELT     2: CD1_1 
  
 
 if (N_elements(cd_type) EQ 0) then begin
 cd_type = 2
 pc1_1 = sxpar( hdr, 'PC'+ax[0]+'_'+ax[0]+alt, Count = N_PC)
      if N_pc EQ 0 then begin 
      cd1_1 = sxpar( hdr, 'CD'+ax[0]+'_'+ax[0]+alt, Count = N_CD)
      if N_CD EQ 0 then begin               ; 
             CDELT1 = sxpar( hdr,'CDELT'+ax[0]+alt, COUNT = N_CDELT1)
             if N_CDELT1 GE 1 then cd_type = 1
      endif       
     endif else cd_type = 0
 endif

; If there is a skew then we can't use a simple CROTA representation

  if CD_TYPE EQ 1 then if abs(cd[1,0]) NE abs(cd[0,1]) then begin
         cd_type = 0
	 sxdelpar,hdr,['CROTA'+ax[0] + alt,'CROTA'+ax[1] + alt]
        message,/INF,'Astrometry incompatible with a CROTA2 representation'
        message,/INF,'Writing PC matrix instead'
  endif	 


  degpix  = ' Degrees / Pixel'
  
  if cd_type EQ 0 then begin


    sxaddpar, hdr, 'PC'+ax[0]+'_'+ax[0]+alt, cd[0,0], degpix, 'HISTORY',/SaveC
    sxaddpar, hdr, 'PC'+ax[1]+'_'+ax[0]+alt, cd[1,0], degpix, 'HISTORY',/SaveC
    sxaddpar, hdr, 'PC'+ax[0]+'_'+ax[1]+alt, cd[0,1], degpix, 'HISTORY',/SaveC
    sxaddpar, hdr, 'PC'+ax[1]+'_'+ax[1]+alt, cd[1,1], degpix, 'HISTORY',/SaveC

    if N_elements(cdelt) EQ 2 then begin 
    sxaddpar, hdr, 'CDELT'+ax[0]+alt, cdelt[0], degpix, 'HISTORY',/SaveC
    sxaddpar, hdr, 'CDELT'+ax[1]+alt, cdelt[1], degpix, 'HISTORY',/SaveC
    endif

  endif else if cd_type EQ 2 then begin

    if N_elements(CDELT) GE 2 then if (cdelt[0] NE 1.0) then begin
            cd[0,0] = cd[0,0]*cdelt[0] & cd[0,1] =  cd[0,1]*cdelt[0]
            cd[1,1] = cd[1,1]*cdelt[1] & cd[1,0] =  cd[1,0]*cdelt[1]
    endif


    sxaddpar, hdr, 'CD'+ax[0]+'_'+ax[0]+alt, cd[0,0], degpix, 'HISTORY',/SaveC
    sxaddpar, hdr, 'CD'+ax[1]+'_'+ax[0]+alt, cd[1,0], degpix, 'HISTORY',/SaveC
    sxaddpar, hdr, 'CD'+ax[0]+'_'+ax[1]+alt, cd[0,1], degpix, 'HISTORY',/SaveC
    sxaddpar, hdr, 'CD'+ax[1]+'_'+ax[1]+alt, cd[1,1], degpix, 'HISTORY',/SaveC

 endif else begin

  ; Programs should only look for CROTA2, but we also update CROTA1 if it 
  ; already exists.   Also keep existing comment field if it exists.

         if N_elements(CDELT) GE 2 then begin
                if cdelt[0] NE 1.0 then delt = cdelt
        endif 

        if N_elements(delt) EQ 0 then begin
                        det = cd[0,0]*cd[1,1] - cd[0,1]*cd[1,0]
                        if det LT 0 then sgn = -1 else sgn = 1
                        delt = [sgn*sqrt(cd[0,0]^2 + cd[0,1]^2), $
                                     sqrt(cd[1,0]^2 + cd[1,1]^2) ]
        endif 
        sxaddpar, hdr, 'CDELT'+ax[0]+alt, delt[0],degpix, 'HISTORY',/SaveC
        sxaddpar, hdr, 'CDELT'+ax[1]+alt, delt[1],degpix, 'HISTORY',/SaveC

        if (cd[1,0] eq 0) and (cd[0,1] eq 0) then rot = 0.0 else $ 
        rot = float(atan( -cd[1,0],cd[1,1])*RADEG) 

        crota2 = sxpar(hdr,'CROTA'+ax[1], Count = N_crota2)
        if N_crota2 GT 0 then sxaddpar, hdr, 'CROTA2'+alt, rot else $
             sxaddpar, hdr, 'CROTA'+ax[1]+alt, rot, ' Rotation Angle (Degrees)'
        crota1 = sxpar(hdr,'CROTA'+ax[0], Count = N_crota1)
        if N_crota1 GT 0 then $
             sxaddpar, hdr, 'CROTA'+ax[0]+alt, rot       
 

 endelse

 hist = ' CD Matrix Written'

; Add CRPIX keyword to FITS header

 if N_elements( crpix ) GE 2  then begin                ;Add CRPIX vector?

        zparcheck, 'PUTAST', crpix, 3, [1,2,4,3,5], 1, 'CRPIX vector'

        sxaddpar, hdr, 'CRPIX'+ax[0]+alt, crpix[0], ' Reference Pixel in X', $
                        'HISTORY', /SaveC
        sxaddpar, hdr, 'CRPIX'+ax[1]+alt ,crpix[1], ' Reference Pixel in Y', $
                        'HISTORY', /SaveC

        hist = ' CD and CRPIX parameters written'
 endif

;  Add CRVAL keyword and values to FITS header.   Convert CRVAL to double
;  precision to ensure enough significant figures

 if N_elements( crval ) GE 2 then begin
     comm = STRARR(2)
     astrcode = astr2 ? astr.coord_sys : STRMID(ctype[0],0,1)
     IF ~astr2 && STRMID(ctype[0],0,4) EQ 'RA--' THEN astrcode = 'C'
     CASE astrcode OF
        'C': BEGIN
              coord = 'Celestial'
              comm[0] = ' R.A. (degrees) of reference pixel'
              comm[1] = ' Declination of reference pixel'
        END
        'G': coord = 'Galactic'               
        'E': coord = 'Ecliptic'
        'S': coord = 'Supergalactic'               
        'H': coord = 'Helioecliptic'
        'T': coord = 'Terrestrial'
        'X': coord = ''  ; unknown system
        ELSE: coord = astrcode
     ENDCASE
     IF astrcode NE 'C' THEN $
         comm = ' '+coord+[' longitude',' latitude']+' of reference pixel'
     IF astr2 && astr.reverse THEN comm = REVERSE(comm)
     zparcheck, 'PUTAST', crval, 3, [2,4,3,5], 1, 'CRVAL vector'
     sxaddpar, hdr, 'CRVAL'+ax[0]+alt, double(crval[0]), comm[0], 'HISTORY'
     sxaddpar, hdr, 'CRVAL'+ax[1]+alt, double(crval[1]), comm[1], 'HISTORY'
     hist = ' World Coordinate System parameters written'
  endif
  
; We don't want to update PV keywords if they are being used for TPV projection
     if size(astr,/tname) EQ 'STRUCT' then begin
     pv_update = ~tag_exist(astr,'DISTORT') ||  $
                  (tag_exist(astr,'DISTORT') &&  astr.distort.name NE 'TPV')
    endif else pv_update = 0
    if N_elements(longpole) EQ 1 then begin
        if pv_update then astr.pv1[3] = longpole
        test = sxpar(hdr,'LONPOLE',count=N_lonpole)
        if N_lonpole EQ 1 then $
            sxaddpar, hdr, 'LONPOLE' +alt ,double(longpole), $
	 ' Native longitude of ' +coord + ' pole', 'HISTORY', /SaveC
    endif 
 
    if N_elements(latpole) EQ 1 then begin
       if pv_update then astr.pv1[4] = latpole
       test = sxpar(hdr,'LATPOLE',count=N_latpole)
        if N_latpole EQ 1 then $
         sxaddpar, hdr, 'LATPOLE' +alt ,double(latpole), $
          ' Native latitude of ' +coord + ' pole', 'HISTORY', /SaveC
    endif	 

    Npv2 = N_elements(pv2)
    if Npv2 GT 0 then begin
         ctyp = strmid(ctype[0],5,3)
; List of WCS types for which no PV2 values should be written	 
         no_pv2 = ['TPV','TNX','TAN','ARC','STG','CAR','MER','SFL','PAR','MOL','AIT', $
	           'PC0','TSC','CSC','QSC' ]
	 if total(no_pv2 EQ ctyp,/int) EQ 0 then begin
	       pv2str = 'PV2_' 	   
	       IF astr2 THEN $
	          pv2str = 'PV'+(astr.reverse ? ax[0] : ax[1])+'_' ; Latitude axis PV 
         case ctyp of 
        'ZPN': for i=0,npv2-1 do sxaddpar,hdr, pv2str + strtrim(i,2) + alt, $
             pv2[i],' Projection parameter ' + strtrim(i,2),'HISTORY',/SaveC 
          else: for i=0,npv2-1 do sxaddpar,hdr, pv2str + strtrim(i+1,2) + alt,$
             pv2[i],' Projection parameter ' + strtrim(i+1,2),'HISTORY',/SaveC 
          endcase
	 endif
    endif
     
    IF astr2 THEN BEGIN
       ctyp = strmid(ctype[0],5,3)
; List of WCS types for which no PV1 values should be written	 
       no_pv1 = ['TPV','TNX','TAN']
       if total(no_pv1 EQ ctyp,/int) EQ 0 then begin
          pv1str = 'PV'+(astr.reverse ? ax[1] : ax[0])+'_' ; Longitude axis PV 
          FOR i=0,4 DO SXADDPAR, hdr, pv1str + STRTRIM(i,2)+alt, $
            astr.pv1[i], ' Projection parameters', 'HISTORY', /SaveC
       ENDIF
        IF FINITE(astr.mjdobs) THEN SXADDPAR, hdr, 'MJD-OBS', astr.mjdobs, $
          ' Modified Julian day of observations', 'HISTORY', /SaveC
        IF astr.dateobs NE 'UNKNOWN' THEN SXADDPAR, hdr, 'DATE-OBS', $
          astr.dateobs, ' Date of observations', 'HISTORY', /SaveC
        IF astr.radecsys NE '' THEN SXADDPAR, hdr, 'RADECSYS'+alt, $
          astr.radecsys,' Reference frame', 'HISTORY', /SaveC
    ENDIF
    
;Add SIP distortion parameters if present  

    if size(astr,/tname) EQ 'STRUCT' && tag_exist(astr,'DISTORT') then begin
       if astr.distort.name EQ 'SIP' then begin 
; First remove any SIP parameters in the FITS header.
          nord = sxpar(hdr, 'A_Order',Count = N)
          if (N GT 0) && (nord GT 0) then begin 
             key = ''
             for i=0,nord do begin
                for j=0,nord-i do begin
                   if i+j NE 0 then $
                     key = [key, strtrim(i,2) + '_' + strtrim(j,2)]
                endfor
             endfor
             key = key[1:*]
             oldkey = ['A_' + key, 'B_' + key, 'AP_' + key,'BP_'+key]
             sxdelpar,oldkey, hdr
          endif
          add_distort, hdr, astr
       ENDIF ELSE IF astr.distort.name EQ 'TNX' then BEGIN

          ;; remove any existing WAT keywords
          w=WHERE(STREGEX(hdr,'^WAT2_',/BOOLEAN,/FOLD),count,COMPLEMENT=w1)
          IF(count GT 0)THEN hdr=hdr[w1]
          w=WHERE(STREGEX(hdr,'^WAT1_',/BOOLEAN,/FOLD),count,COMPLEMENT=w1)
          IF(count GT 0)THEN hdr=hdr[w1]
          w=WHERE(STREGEX(hdr,'^WAT0_',/BOOLEAN,/FOLD),count,COMPLEMENT=w1)
          IF(count GT 0)THEN hdr=hdr[w1]
          ;; and add in the new ones
          add_distort, hdr, astr
       ENDIF ELSE IF astr.distort.name EQ 'TPV' then BEGIN

          FOR i=0,N_ELEMENTS(astr.pv1)-1 DO BEGIN
             SXADDPAR, hdr, 'PV1_'+STRTRIM(i,2)+alt, astr.pv1[i]
          ENDFOR
          FOR i=0,N_ELEMENTS(astr.pv2)-1 DO BEGIN
             SXADDPAR, hdr, 'PV2_'+STRTRIM(i,2)+alt, astr.pv2[i]
          ENDFOR
          
       ENDIF
    endif
    
    sxaddhist,'PUTAST: ' + strmid(systime(),4,20) + hist,hdr
    
    return
 end

pro heuler,h_or_astr, Galactic = galactic, celestial = celestial, $
                      ecliptic = ecliptic, alt_in = alt_in, alt_out = alt_out
;+
; NAME:
;      HEULER 
;
; PURPOSE:
;       Change the coordinate system of a FITS header or astrometry structure
; EXPLANATION:
;       Converts a FITS header or a astrometry structure containing WCS (world 
;       coordinate system) information between celestial, ecliptic, and 
;       Galactic coordinates
;
; CALLING SEQUENCE:
;       HEULER, hdr, [/GALACTIC, /CELESTIAL, /ECLIPTIC, ALT_IN = , ALT_OUT=]  
;                      or
;       HEULER, astr, /GALACTIC, /CELESTIAL, /ECLIPTIC
;
; INPUT/OUTPUT PARAMETERS:
;       hdr - FITS header (string array) containing WCS information
;                            or
;       Astr - Astrometry structure as extracted from a FITS header
;             by extast.pro (See EXTAST for more info).
;
;       Header or astrometry structure will be modified by the program to 
;       contain astrometry in the new coordinates system.
; REQUIRED INPUT KEYWORDS:
;       One of the following exclusive keywords is *required*
;       /GALACTIC - Convert the header to Galactic coordinates
;       /CELESTIAL - Convert the header to celestial (RA & Dec) coordinates
;       /ECLIPTIC - Convert the header to ecliptic coordinates
;
; OPTIONAL INPUT KEYWORDS:
;      The following two keywords apply if the FITS header contains multiple
;      WCS keywords. See Section 3.3 of Greisen & Calabretta (2002, A&A, 395, 
;      1061) for information about alternate astrometry keywords.
;
;      ALT_IN -  single character 'A' through 'Z' or ' ' specifying an 
;          alternate astrometry system present in the input FITS header.  The 
;          default isto use the primary astrometry or ALT = ' '.   If /ALT_IN 
;          is set, then this is equivalent to ALT_IN = 'A'.
;      ALT_OUT - single character specifying the alternate WCS keywords 
;          to write the *output* astrometry.    If not specified, then ALT_OUT
;          is set equal to ALT_IN.
; RESTRICTIONS:
;       Currently assumes that celestial and ecliptic coordinates are in
;       J2000.   Use HPRECESS if this is not the case.
;
;       ST Guide Star (DSS) image headers are first converted to a standard
;       tangent projection, prior to the coordinate conversion
; METHOD:
;       The algorithm used is described in Section 2.7 of Calabretta & Greisen
;       (2002, A&A, 395, 1077).    The CRVAL coordinates are transformed
;       directly using EULER.    The new LONPOLE and LATPOLE values are then
;       determined by transforming the pole of the new system to the old, and
;       converted to native coordinates using WCS_ROTATE. 
; EXAMPLE:
;       A FITS header, hdr, has a standard tangent projection WCS information.
;       Add an alternate 'G' Galactic projection.    Note that the original
;       WCS information will be left unchanged 
;
;       IDL> heuler, hdr, /Galactic, alt='G'
; PROCEDURES USED:
;       EULER, EXTAST, GSSS_STDAST, PUTAST, SXADDHIST, WCS_ROTATE
; REVISION HISTORY:
;       Written    W. Landsman                  June 2003
;       Use PV2 tag in astrometry structure rather than PROJP1 W. L. May 2004
;       Use double precision to compute new North pole  W.L. Aug 2005
;       Check for non-standard CTYPE value W.L. Sep 2012
;-
compile_opt idl2
if N_params() LT 1 then begin
     print,'Syntax - HEULER, hdr, /GALACTIC, /CELESTIAL, /ECLIPTIC, ALT_IN=,'
     return
endif
sz = size(h_or_astr,/str)
if (sz.type_name EQ 'STRING') && (sz.N_dimensions EQ 1) then begin
    if N_elements(alt_out) EQ 0 then if N_elements(alt_in) NE 0 then $
       alt_out = alt_in
    EXTAST,h_or_astr,astr,status, alt = alt_in 
    if status LT 0 then message, $
       'ERROR - No astrometry present in supplied FITS header' else $
    if status EQ 4 then begin
         GSSS_STDAST, h_or_astr
         EXTAST, h_or_astr, astr, status, alt = alt_in
    endif
    
    ctype1 = sxpar(h_or_astr,'CTYPE1')         ;Check if non-standard CTYPE was used
   if strmid(astr.ctype[0],5,3) NE strmid(ctype1,5,3) then $
         putast,h_or_astr,astr
    
endif else if sz.type_name EQ 'STRUCT' then astr = h_or_astr else message, $
   'ERROR - First parameter must be a FITS header or astrometry structure'
 map_types=['DEF','AZP','SZP','TAN','STG','SIN','ARC','ZPN','ZEA','AIR','CYP',$
            'CEA','CAR','MER','SFL','PAR','MOL','AIT','COP','COE','COD','COO',$
            'BON','PCO','GLS','TSC','CSC','QSC']

ctype1 = astr.ctype[0] 
ctype2 = astr.ctype[1]
; Use Table 13 of Calbretta & Greisen to determine default values of theta0
coord = strmid(ctype1,0,4)
proj = strmid(ctype1,5,3)
imap = where(map_types EQ proj, N_imap)
if N_imap EQ 0 then message,'ERROR - Unrecognized map projection of ' + proj
imap = imap[0]
if imap LE 9 then theta0 = 90 else $
if (imap GE 18) && (imap LE 21) then theta0 = astr.pv2[0] else theta0 = 0
          
if keyword_set(GALACTIC) then begin
    case coord of
    'RA--': select= 1
    'ELON': select = 5
    'GLON': begin 
            message,/INF,'FITS header is already in Galactic: nothing changed'
            return
            end
    end
    strput,ctype1,'GLON'
    strput,ctype2,'GLAT'
    conv = 'Galactic'
endif else if keyword_set(CELESTIAL) then begin 
    case coord of
    'RA--': begin 
            message,/INF,'FITS header is already in Celestial: nothing changed'
            return
            end
    'ELON': select = 4
    'GLON': select = 2
     end
     strput,ctype1,'RA--'
     strput,ctype2,'DEC-'
     conv = 'Celestial'
endif else if keyword_set(ECLIPTIC) then begin
    case coord of
    'RA--': select =3  
    'ELON': begin 
            message,/INF,'FITS header is already in Celestial: nothing changed'
            return
            end
    'GLON': select = 6
     endcase
     strput,ctype1,'ELON'
     strput,ctype2,'ELAT'
     conv = 'Ecliptic'
endif else message, $
   'Either /CELESTIAL, /GALACTIC or /ECLIPTIC keyword must be specified'


 EULER,astr.crval[0],astr.crval[1],ncrval1,ncrval2,select

;Find new LONPOLE and LATPOLE values
 if select mod 2 eq 0 then iselect = select-1 else iselect = select+1
 EULER,0.0d,90.0d,lon1,lat1,iselect
 WCS_ROTATE,lon1,lat1,lonpole, latpole, astr.crval,LONGPOLE = astr.longpole, $
             LATPOLE = astr.latpole, THETA0 = theta0

;Update astrometry structure
 astr.ctype = [ctype1,ctype2]
 astr.longpole = lonpole
 astr.latpole = latpole
 astr.crval = [ncrval1, ncrval2]

 if sz.type_name EQ 'STRING' then begin        ;Update FITS header? 
          putast, h_or_astr, astr, alt = alt_out 
          sxaddhist, 'HEULER: ' + STRMID(systime(),4,20) +  $
                     ' Converted to ' + conv + ' coordinates', h_or_astr
 endif else h_or_astr = astr
 return
 end

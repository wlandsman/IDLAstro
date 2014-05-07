PRO HPRECESS, HDR, YEARF                                      
;+
; NAME:
;       HPRECESS
; PURPOSE:
;       Precess the astrometry in a FITS header to a new equinox
;
; CALLING SEQUENCE:
;       HPRECESS, HDR, [ yearf ]      
;
; INPUT-OUTPUT:
;       HDR - FITS Header, must contain the CRVAL astrometry keywords,
;               and either an EPOCH or EQUINOX keyword.
;               HDR will be modified to contain the precessed astrometry
;
; OPTIONAL INPUT:
;       YEARF - Scalar, giving the year of the new (Final) equinox.
;               If not supplied, user will be prompted for this value.
;
; METHOD:
;       The CRVAL and CD (or CROTA) keywords are extracted from the header 
;       and precessed to the new equinox.  The EPOCH or EQUINOX keyword in 
;       the header is  updated.  A HISTORY record is added
;
; RESTRICTIONS:
;       The FK5 reference frame is assumed for both equinoxes.
;
; PROCEDURES USED:
;       EXTAST, GET_EQUINOX(), SXADDPAR, SXADDHIST, PRECESS, PRECESS_CD
;       PUTAST, ZPARCHECK
; REVISION HISTORY:                                               
;       Written  W. Landsman        STX              July, 1988
;       CD matrix precessed -                        February, 1989
;       Update EQUINOX keyword when CROTA2 present   November, 1992
;       Recognize a GSSS header                      June, 1994
;       Additional Noparams value recognize for storing CDs.  RSH, 6 Apr 95
;       Understand reversed X,Y (X-Dec, Y-RA) axes,   W. Landsman  October 1998
;       Correct algorithm when CROTA2 is in header W. Landsman  April 2006
;       Correct sign error introduced April 2006, include CDELT values
;         when computing rotation of pole   W. Landsman July 2007
;       Call hprecess/jprecess for 1950<>2000   W. L. Aug 2009
;-     
 On_error, 2   
 compile_opt idl2
 
 if N_params() EQ 0 then begin       
        print,'Syntax - HPRECESS, hdr, [ yearf]'
        return   
 endif else zparcheck, 'HPRECESS', hdr, 1, 7, 1, 'FITS Header Array'

 yeari = GET_EQUINOX( hdr, code)    ;YEAR of Initial equinox
 if code EQ -1 then $     
       message,'Header does not contain EPOCH or EQUINOX keyword'

 if N_params() LT 2 then begin 
   print, 'HPRECESS: Astrometry in supplied header is in equinox ', $
   strtrim(yeari,2)      
   read, 'Enter year of new equinox: ',yearf 
 endif                                             

 if yeari EQ yearf then $                                           
    message,'Astrometry in header is already in Equinox ' + strtrim(YEARF,2)

 extast, hdr, astr, noparams        ;Extract astrometry from header

 if noparams EQ -1 THEN $
    message,'FITS Header does not contain CRVAL keywords'
        
 if strmid(astr.ctype[0],5,3) EQ 'GSS' then begin
        gsss_stdast, hdr
        extast, hdr, astr, noparams
 endif
        
 cd = astr.cd
 crval = astr.crval
 cdelt = astr.cdelt
 if N_elements(CDELT) GE 2 then if (cdelt[0] NE 1.0) then begin
        cd[0,0] = cd[0,0]*cdelt[0] & cd[0,1] =  cd[0,1]*cdelt[0]
        cd[1,1] = cd[1,1]*cdelt[1] & cd[1,0] =  cd[1,0]*cdelt[1]
 endif

 coord = strmid(astr.ctype,0,4)    ;Test if RA and Dec reversed in 'CTYPE*'
 reverse = ((coord[0] EQ 'DEC-') and (coord[1] EQ 'RA--'))
 if reverse then crval = rotate(crval,2)
 a = crval[0] & d = crval[1]
 if (yeari EQ 2000.) and (yearf EQ 1950.) then begin 
       bprecess,a,d,ai,di
       sxaddpar,hdr,'RADECSYS','FK4'
       a = ai & d = di
 endif else if (yeari EQ 1950) and (yearf EQ 2000) then begin 
       jprecess,a,d,ai,di
       sxaddpar,hdr,'RADECSYS','FK5'
       a = ai & d = di
       
 endif else precess, a, d, yeari, yearf                    ;Precess the CRVAL coordinates
 precess_cd, cd, yeari, yearf, crval,[ a, d]    ;Precess the CD matrix
 if N_elements(CDELT) GE 2 then if (cdelt[0] NE 1.0) then begin
        cd[0,0] = cd[0,0]/cdelt[0] & cd[0,1] =  cd[0,1]/cdelt[0]
        cd[1,1] = cd[1,1]/cdelt[1] & cd[1,0] =  cd[1,0]/cdelt[1]
 endif

 
 if reverse then begin                          ;Update CRVAL values
    sxaddpar, hdr, 'CRVAL1', double(d)             
    sxaddpar, hdr, 'CRVAL2', double(a)    
 endif else begin
    sxaddpar, hdr, 'CRVAL1', double(a)            
    sxaddpar, hdr, 'CRVAL2', double(d)    
 endelse

 if (noparams EQ 3) or (noparams EQ 2)  then begin
 
       putast, hdr, cd, EQUINOX = float(yearf)          ;Update CD values
 endif else begin
       astr.cd= cd
       getrot, astr, ROT                               ;or CROTA2 value
       sxaddpar,hdr, 'EQUINOX', yearf, ' Equinox of Ref. Coord.', 'HISTORY'
       sxaddpar, hdr, 'CROTA2', rot
 endelse        

 sxaddhist, 'HPRECESS: ' + STRMID(systime(),4,20) +  $ 
   ' Astrometry Precessed From Year' + string(form='(f7.1)',float(yeari)),hdr
 message, 'Header astrometry has been precessed to ' + strtrim(yearf,2),/INF

 return
 end                                                        

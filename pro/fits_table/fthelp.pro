pro fthelp,h,TEXTOUT=textout
;+
; NAME:
;       FTHELP
; PURPOSE:
;       Routine to print a description of a FITS ASCII table extension
;
; CALLING SEQUENCE:
;       FTHELP, H, [ TEXTOUT = ]
;
; INPUTS:
;       H - FITS header for ASCII table extension, string array
;
; OPTIONAL INPUT KEYWORD
;       TEXTOUT - scalar number (0-7) or string (file name) determining
;               output device (see TEXTOPEN).  Default is TEXTOUT=1, output 
;               to the user's terminal    
;
; NOTES:
;       FTHELP checks that the keyword XTENSION  equals 'TABLE' in the FITS
;               header.
;
; SYSTEM VARIABLES:
;       Uses the non-standard system variables !TEXTOUT and !TEXTUNIT
;       which must be defined (e.g. with ASTROLIB) prior to compilation.
; PROCEDURES USED:
;       REMCHAR, SXPAR(), TEXTOPEN, TEXTCLOSE, ZPARCHECK
;
; HISTORY:
;       version 1  W. Landsman  Jan. 1988
;       Add TEXTOUT option, cleaner format  W. Landsman   September 1991
;       TTYPE value can be longer than 8 chars,  W. Landsman  August 1995
;       Remove calls to !ERR, some vectorization  W. Landsman  February 2000 
;       Slightly more compact display  W. Landsman  August 2005
;-
 compile_opt idl2
 On_error,2                                  ;Return to caller

 if N_params() EQ 0 then begin
     print,'Syntax - FTHELP, hdr, [ TEXTOUT = ]'
     return
 endif

 zparcheck,'FTHELP',h,1,7,1,'Table Header'     ;Make sure a string array

 n = sxpar( h, 'TFIELDS' , Count = N_TFields) 
 if N_TFields EQ 0 then message, $
        'ERROR - FITS Header does not include required TFIELDS keyword'
 if strtrim(sxpar(h,'XTENSION'),2) ne 'TABLE' then $
        message,'WARNING - Header is not for a FITS Table',/INF

 if not keyword_set(TEXTOUT) then textout = 1
 textopen,'fthelp',TEXTOUT=textout

 naxis = sxpar( h, 'NAXIS*')
 printf,!TEXTUNIT,'FITS ASCII Table: ' +$
        'Size ',strtrim(naxis[0],2),' by ',strtrim(naxis[1],2)

 extname = sxpar(h,'EXTNAME', Count=N_ext)	
 if N_ext GT 0 then printf,!TEXTUNIT, 'Extension Name:   ',sxpar(h,'EXTNAME')
 extver = sxpar(h, 'EXTVER', Count = N_extver)
 if N_extver GT 0 then printf,!TEXTUNIT,'Version: ',extver
 printf,!TEXTUNIT,' '                         
 printf,!TEXTUNIT,  $
 'Field      Name               Unit           Format     Column'

 tbcol = intarr(n)
 tform = strarr(n) & tunit = tform & ttype =tform
 name = strmid(h,0,5)
 number = strtrim(strmid(h,5,3),2)
 value = strtrim(strmid(h,11,20),2)

 for i = 1, N_elements(h)-1 do begin
  case name[i] of
   'TTYPE':  ttype[fix(number[i]-1)] = value[i]
   'TFORM':  tform[fix(number[i]-1)] = value[i]
   'TUNIT':  tunit[fix(number[i]-1)] = value[i]
   'TBCOL':  tbcol[fix(number[i]-1)] = fix(value[i])
   'END  ':  goto, DONE 
    ELSE :
 end

 endfor

DONE:                            ;Done reading FITS header

 ttype = strtrim(ttype,2) & remchar,ttype,"'"
 remchar,tunit,"'"
 remchar,tform,"'"
 for i = 0,n-1 do  printf,!TEXTUNIT,i+1,ttype[i],tunit[i],tform[i],tbcol[i], $
              f='(I5,T9,A,T30,A,T47,A,T55,I8)'

 textclose,TEXTOUT=textout

 return
 end

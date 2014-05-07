pro ftab_help,file_or_fcb,EXTEN_NO = exten_no, TEXTOUT = textout
;+
; NAME:
;       FTAB_HELP
; PURPOSE:
;       Describe the columns of a FITS binary or ASCII table extension(s).
;
; CALLING SEQUENCE:
;       FTAB_HELP, filename, [ EXTEN_No = , TEXTOUT= ]
;               or
;       FTAB_HELP, fcb, [EXTEN_No=, TEXTOUT= ]
;
; INPUTS:
;       filename - scalar string giving name of the FITS file.  
;       fcb - FITS control block returned by a previous call to FITS_OPEN
;
; OPTIONAL KEYWORD INPUTS:
;       EXTEN_NO - integer scalar or vector specifying which FITS extensions 
;               to display.    Default is to display all FITS extension.
;       TEXTOUT - scalar number (0-7) or string (file name) determining
;               output device (see TEXTOPEN).  Default is TEXTOUT=1, output 
;               to the user's terminal    
;
; EXAMPLE:
;       Describe the columns in the second and fourth extensions of a FITS 
;       file spec.fits and write the results to a file 'spec24.lis'
;
;       IDL> ftab_help,'spec.fits',exten=[2,4],t='spec24.lis'
;
; SYSTEM VARIABLES:
;        Uses the non-standard system variables !TEXTOUT and !TEXTUNIT
;       which must be defined (e.g. with ASTROLIB) before compilation
; NOTES:
;       The behavior of FTAB_HELP was changed in August 2005 to display
;       all extensions by default, rather than just the first extension
; PROCEDURES USED:
;       FITS_READ, FITS_CLOSE, FITS_OPEN, FTHELP, TBHELP, TEXTOPEN, TEXTCLOSE
; HISTORY:
;       version 1  W. Landsman    August 1997
;       Corrected documentation W. Landsman   September 1997
;       Don't call fits_close if fcb supplied W. Landsman May 2001 
;       Default now is to display all extensions, EXTEN keyword can now
;        be a vector   W. Landsman Aug 2005
;-
;----------------------------------------------------------------------
 compile_opt idl2
 if N_params() LT 1 then begin
        print,'Syntax - FTAB_HELP, fcb_or_filename, [EXTEN_NO=, TEXTOUT= ]'
        return
 endif
 
 sz = size(file_or_fcb)                                                    
 if sz[sz[0]+1] NE 8 then fits_open,file_or_fcb,fcb else fcb=file_or_fcb
 if fcb.nextend EQ 0 then begin 
          message,'File contains no Table extensions',/INF
          if sz[sz[0]+1] NE 8 then fits_close,fcb else $
                      file_or_fcb.last_extension = exten_no
          return
  endif
 if N_elements(exten_no) EQ 0 then exten_no = indgen(fcb.nextend)+1

 nprint  = N_elements(exten_no)
 textopen,'ftab_help',textout=textout
 printf,!TEXTUNIT,' '
printf,!TEXTUNIT, 'FITS file: ' + fcb.filename 
 printf,!TEXTUNIT,' '

 for i=0, nprint-1 do begin

   fits_read,fcb, dummy, htab, /header_only,/no_pdu, exten_no=exten_no[i]
     ext_type = fcb.xtension[exten_no[i]]

 image = 0b
 case ext_type of
 'A3DTABLE': binary = 1b
 'BINTABLE': binary = 1b
 'TABLE': binary = 0b
 'IMAGE': image = 1b
 else: message,'ERROR - Extension type of ' + $
                ext_type + ' is not a recognized FITS extension'
 endcase

  enum = exten_no[i]
  printf,!TEXTUNIT, 'Extension No: ' + strtrim(enum,2)

 if image then begin
     dimen = sxpar(htab,'NAXIS*')
     printf, !TEXTUNIT,'FITS Image Extension: Size ' + $
              strjoin(strtrim(dimen,2),' by ')
 endif else begin   
      
      
 if binary then tbhelp, htab, TEXTOUT = 5 $
           else fthelp, htab, TEXTOUT = 5
 printf,!TEXTUNIT,' '
 endelse
 endfor
 if sz[sz[0]+1] NE 8 then fits_close,fcb else $
         file_or_fcb.last_extension = enum

  textclose, textout=textout
 return
 end

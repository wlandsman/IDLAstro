pro tbhelp,h, TEXTOUT = textout
;+
; NAME:
;       TBHELP
; PURPOSE:
;       Routine to print a description of a FITS binary table header
;
; CALLING SEQUENCE:
;       TBHELP, h, [TEXTOUT = ]
;
; INPUTS:
;       h - FITS header for a binary table, string array
;
; OPTIONAL INPUT KEYWORD:
;       TEXTOUT - scalar number (0-7) or string (file name) controling 
;               output device (see TEXTOPEN).  Default is TEXTOUT=1, output 
;               to the user's terminal    
;
; METHOD:
;       FITS Binary Table keywords NAXIS*,EXTNAME,TFIELDS,TTYPE*,TFORM*,TUNIT*,
;       are read from the header and displayed at the terminal
;
;       A FITS header is recognized as bein for a binary table if the keyword 
;       XTENSION has the value 'BINTABLE' or 'A3DTABLE'
;
; NOTES:
;       Certain fields may be truncated in the display
; SYSTEM VARIABLES:
;       Uses the non-standard system variables !TEXTOUT and !TEXTUNIT.   These
;       are automatically defined by TBHELP if they have not been defined
;       previously. 
; PROCEDURES USED:
;       REMCHAR, SXPAR(), TEXTCLOSE, TEXTOPEN, ZPARCHECK 
; HISTORY:
;       W. Landsman       February, 1991
;       Parsing of a FITS binary header made more robust    May, 1992
;       Added TEXTOUT keyword      August 1997
;       Define !TEXTOUT if not already present   W. Landsman  November 2002
;       Slightly more compact display   W. Landsman August 2005
;       Fix Aug 2005 error omitting TFORM display W. Landsman Sep 2005
;-
 compile_opt idl2
 On_error,2

 if N_params() LT 1 then begin
     print,'Syntax - tbhelp, hdr, [TEXTOUT= ]'     
     return
 endif


 zparcheck, 'TBHELP', h, 1, 7, 1, 'Table Header'

 naxis = sxpar( h, 'NAXIS*')
 if N_elements(naxis) LT 2 then $
         message,'ERROR - FITS Binary table must have NAXIS = 2'

 ext_type = strmid( strtrim( sxpar( h, 'XTENSION'), 2 ), 0, 8)
 if (ext_type NE 'A3DTABLE') && (ext_type NE 'BINTABLE') then message, $
 'WARNING - Header type of ' + ext_type + ' is not for a FITS Binary Table',/CON

 n = sxpar( h, 'TFIELDS', Count = N_tfields)  
 if N_tfields EQ 0 then message, $
        'ERROR - Required TFIELDS keyword is missing from binary table header'

 tform = sxpar(h,'TFORM*', Count = N_tform)      ;Get required TFORM* values
 n = n > N_tform
 
 textopen,'tbhelp',TEXTOUT=textout

 printf,!TEXTUNIT,'FITS Binary Table: ' + $
        'Size ',strtrim(naxis[0],2),' by ',strtrim(naxis[1],2)
 extname = sxpar(h,'EXTNAME', Count=N_ext)	
 if N_ext GT 0 then printf,!TEXTUNIT, 'Extension Name:   ',sxpar(h,'EXTNAME')

 tnull =  strarr(n)
 tunit = tnull & ttype =tnull & tcomm = tnull
 key = strmid( h, 0, 5)
 for i = 1, N_elements(h)-1 do begin

 case key[i] of
 'TTYPE':   begin
           j = fix(strtrim(strmid(h[i],5,3),2))
          apos = strpos( h[i], "'") 
          ttype[j-1] = strmid( h[i], apos+1, 20)
          slash = strpos(h[i],'/')
          if slash GT 0 then $
              tcomm[j-1] = strcompress( strmid(h[i], slash+1, 55))
          end

 'TUNIT':  begin 
          apos = strpos( h[i], "'") 
          tunit[fix(strtrim(strmid(h[i],5,3),2))-1] = strmid(h[i],apos+1,20)
          end
 'TNULL':  begin
          tnull[fix(strtrim(strmid(h[i],5,3),2))-1] = $
                                 strtrim( strmid( h[i], 10, 20 ),2)
          end
 'END  ':  goto, DONE 
 ELSE :
 endcase
 endfor

DONE:
 remchar,ttype,"'" & ttype = strtrim(ttype,2)
 remchar,tunit,"'" & tunit = strtrim(tunit,2)
 tform = strtrim(tform,2)
 remchar,tnull,"'" & tnull = strtrim(tnull,2)
 len_ttype = strtrim( max(strlen(ttype)) > 4,2)
 len_tunit = strtrim( max(strlen(tunit)) > 4,2)
 len_tform = strtrim( max(strlen(tform)) > 4,2)
 len_tnull = strtrim( max(strlen(tnull)) > 4,2)


 fmt = '(A5,1x,A' + len_ttype +',1x,A' + len_tunit + ',1x,A' + len_tform + $
        ',1x,A' + len_tnull +',1x,A)'

 printf,!TEXTUNIT,'Field','Name','Unit','Frmt','Null','Comment',f=fmt
 
 field = strtrim(sindgen(n)+1,2)
 for i=0,n-1 do begin 
        printf,!TEXTUNIT,field[i],ttype[i],tunit[i],tform[i],tnull[i],tcomm[i], $
                format=fmt
 endfor

 textclose, TEXTOUT = textout
 return
 end

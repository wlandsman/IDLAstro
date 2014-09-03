pro ftinfo, h, ft_str, Count = tfields
;+
; NAME:
;       FTINFO
; PURPOSE:
;       Return an informational structure from a FITS ASCII table header.
; CALLING SEQUENCE:
;       ftinfo,h,ft_str, [Count = ]
;
; INPUTS:
;       h - FITS ASCII table header, string array
;
; OUTPUTS:
;       ft_str - IDL structure with extracted info from the FITS ASCII table
;                header.   Tags include
;        .tbcol - starting column position in bytes
;        .width - width of the field in bytes
;        .idltype - idltype of field.
;                       7 - string, 4- real*4, 3-integer, 5-real*8
;        .tunit - string unit numbers
;        .tscal - scale factor
;        .tzero - zero point for field
;        .tnull - null value for the field
;        .tform - format for the field
;        .ttype - field name
;
; OPTIONAL OUTPUT KEYWORD:
;       Count - Integer scalar giving number of fields in the table
; PROCEDURES USED:
;       GETTOK(), SXPAR()
; NOTES:
;       This procedure underwent a major revision in May 2000, and **THE
;       NEW CALLING SEQUENCE IS INCOMPATIBLE WITH THE OLD ONE **
; HISTORY:
;       D. Lindler  July, 1987
;       Converted to IDL V5.0   W. Landsman   September 1997
;       Major rewrite, return structure   W. Landsman   April 2000
;-
;----------------------------------------------------------------------------
; On_error,2
;
  if N_params() LT 2 then begin
      print,'Syntax - FTINFO, header, ft_str'
      return
  endif

; get number of fields

 tfields = sxpar( h, 'TFIELDS' , Count = N_TFields)
 if N_TFields EQ 0 then $
        message,'Invalid FITS header. keyword TFIELDS is missing'

 if tfields EQ 0 then return
 tbcol = intarr(tfields)
 tform = replicate(' ',tfields)

; get info for specified field

 ttype = sxpar(h,'ttype*',Count=N_ttype)                    ;field name
 if N_ttype EQ 0 then ttype = strarr(tfields)

 tbcol[0] = sxpar(h,'tbcol*', Count = N_tbcol)       ;starting column position
 if N_tbcol NE tfields then message,/CON, $
 'Warning - Invalid FITS table header -- TBCOL not present for all fields'
;
 tform[0] = strtrim(sxpar(h,'tform*', Count = N_tform),2)   ; column format
 if N_tform NE tfields then message,/CON, $
   'Warning - Invalid FITS table header -- TFORM  not present for all fields'
 ;                                               ; physical units
 tunit = strarr(Tfields)
 temp = sxpar(h, 'TUNIT*', Count = N_tunit)
 if N_tunit GT 0 then tunit[0] = temp

 tscal = fltarr(Tfields)
 temp = sxpar(h, 'TSCAL*', Count = N_tscal)      ; data scale factor
 if N_tscal GT 0 then tscal[0] = temp

 tzero = fltarr(tfields)
 temp = sxpar(h,'TZERO*', Count = N_tzero)       ; zero point for field
 if N_tzero GT 0 then tzero[0] = temp

 tnull = strarr(Tfields)
 temp = sxpar(h,'TNULL*', Count = N_tnull)       ;null data value
 if N_tnull GT 0 then tnull[0] = temp
;
; determine idl data type from format
;
 type = strmid(tform,0,1)
 idltype = intarr(tfields)
 for i=0,tfields-1 do begin
 case strupcase(type[i]) of
        'A' : idltype[i] = 7
        'I' : idltype[i] = 3
        'E' : idltype[i] = 4
        'F' : idltype[i] = 4
        'D' : idltype[i] = 5
        else: message,'Invalid format specification for keyword ' + $
                        'TFORM' + strtrim(i+1,2)
 endcase
 endfor
;
; get field width in characters
;
 decpos = strpos(tform,'.')
 decimal = decpos GT 0
 len = strlen(tform)
 width = intarr(tfields)
 for i=0, tfields-1 do begin
     if decimal[i] then width[i] = fix(strmid(tform[i],1,decpos[i]-1)) else $
                     width[i] = fix(strmid(tform[i],1,len[i]-1))
 endfor
 ft_str = {TBCOL:tbcol,WIDTH:width,IDLTYPE:idltype,TUNIT:tunit, TSCAL:tscal, $
           TZERO:tzero, TNULL:tnull, TFORM:tform, TTYPE:ttype}

 return
 end

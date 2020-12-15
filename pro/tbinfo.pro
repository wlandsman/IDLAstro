pro tbinfo,h,tb_str, errmsg = errmsg, NOSCALE= noscale
;+
; NAME:
;       TBINFO
; PURPOSE:
;       Return an informational IDL structure from a FITS binary table header.
;
; CALLING SEQUENCE:
;       tbinfo, h, tb_str, [ERRMSG = ]
; INPUTS:
;       h - FITS binary table header, e.g. as returned by READFITS()
;
; OUTPUTS:
;       tb_str - IDL structure with extracted info from the FITS binary table
;               header.   Tags include
;       .tbcol - starting column position in bytes, integer vector
;       .width - width of the field in bytes, integer vector
;       .idltype - idltype of field, byte vector
;               7 - string, 4- real*4, 3-integer*4, 5-real*8
;       .numval - repeat count, 64 bit longword vector
;       .tunit - string unit numbers, string vector
;       .tnull - integer null value for the field, stored as a string vector
;                 so that an empty string indicates that TNULL is not present
;       .tform - format for the field, string vector
;       .ttype - field name, string vector
;       .maxval- maximum number of elements in a variable length array, long
;               vector
;       .tscal - pointer array giving the scale factor for converting to 
;                physical values, default 1.0
;       .tzero - pointer array giving the additive offset for converting to 
;                physical values, default 0.0
;       .tdisp - recommended output display format
;
;       All of the output vectors will have same number of elements, equal
;       to the number of columns in the binary table.
;
;       The .tscal and .tzero values are stored as pointers so as to preserve
;       the individual data types (e.g. float or double) which may differ 
;       in different columns.   For example, to obtain the value of TSCAL for
;       the third column use *tab_str.tscal[2]  
; OPTIONAL INPUT KEYWORD:
;       /NOSCALE - if set, then the TSCAL* and TZERO* keywords are not extracted
;            from the FITS header, and the .tscal and .tzero pointers do not
;            appear in the output structure.
; OPTIONAL OUTPUT KEYWORD:
;        ERRMSG = if present, then error messages are returned in this keyword
;            rather than displayed using the MESSAGE facility 
; PROCEDURES USED:
;       SXPAR()
; NOTES:
;       For variable length ('P' format) column, TBINFO returns values for
;       reading the 2 element longward array of pointers (numval=2, 
;       idltype = 3, width=4)
; HISTORY:
;       Major rewrite to return a structure      W. Landsman   August 1997
;       Added "unofficial" 64 bit integer "K" format W. Landsamn Feb. 2003
;       Store .tscal and .tzero tags as pointers, so as to preserve 
;       type information   W. Landsman          April 2003
;       Treat repeat count for string as specifying string length, not number
;          of elements, added ERRMSG    W. Landsman        July 2006
;       Treat logical as character string 'T' or 'F' W. Landsman  October 2006
;       Added NOSCALE keyword  W. Landsman   March 2007
;       Make .numval 64 bit for very large tables  W. Landsman   April 2014
;       Make sure XTENSION is for a FITS binary table W. Landsman  May 2017
;-
;----------------------------------------------------------------------------
 On_error,2
 compile_opt idl2
 if N_params() LT 2 then begin
        print,'Syntax - TBINFO, h, tb_str, [ERRMSG=, /NOSCALE]'
        return
 endif
 save_err = arg_present(errmsg)
 
;Make sure a FITS binary table 
  ext_type = strmid( strtrim( sxpar( h, 'XTENSION'), 2 ), 0, 8)
  if (ext_type NE 'A3DTABLE') && (ext_type NE 'BINTABLE') then begin 
  message,/INF, $
       'WARNING - XTENSION value of ' + ext_type + ' is not for a FITS Binary Table'
 endif	

; get number of fields

 tfields = sxpar( h, 'TFIELDS', COUNT = N_TFields)
 if N_TFields EQ 0 then begin    ;Legal Binary Table Header?
        errmsg = 'Invalid FITS binary table header. keyword TFIELDS is missing'
	if ~save_err then message,errmsg else return
   endif	    

 if tfields EQ 0 then begin     ;Any fields in table?
        errmsg = 'No Columns in FITS binary table, keyword TFIELDS = 0'
	if ~save_err then message,errmsg else return
  endif	    
 
; Create output arrays with default values

 idltype = intarr(tfields) & tnull = idltype
 numval = lon64arr(tfields) & tbcol = numval & width = numval & maxval = numval
 tunit = replicate('',tfields) & ttype = tunit & tdisp = tunit & tnull = tunit

 type = sxpar(h,'TTYPE*', COUNT = N_ttype)
 if N_ttype GT 0 then ttype[0] = strtrim(type,2) 

 tform = strtrim( sxpar(h,'tform*', COUNT = N_tform), 2)     ; column format
 if N_tform EQ 0 then $
        message,'Invalid FITS table header -- keyword TFORM not present
 tform =  strupcase(strtrim(tform,2))
                                                
 unit = strtrim(sxpar(h, 'TUNIT*', COUNT = N_tunit),2)     ;physical units
 if N_tunit GT 0 then tunit[0] = unit

 null = sxpar(h, 'TNULL*', COUNT = N_tnull)      ;null data value
 if N_tnull GT 0 then tnull[0] = null

 if ~keyword_set(noscale) then begin
  tscal = ptrarr(tfields,/all)
  tzero = ptrarr(tfields,/all)
  index = strtrim(indgen(tfields)+1,2)
  for i=0,tfields-1 do begin
    scale = sxpar(h,'TSCAL' + index[i], COUNT = N_tscal)     ;Scale factor
    if N_tscal GT 0 then *tscal[i] = scale else *tscal[i] = 1.0
    zero = sxpar(h,'TZERO' + index[i], Count = N_tzero)
    if N_tzero GT 0 then *tzero[i] = zero else *tzero[i] = 0
  endfor
 endif  

 disp = sxpar(h,'TDISP*', COUNT = N_tdisp)       ;Display format string
 if N_tdisp GT 0 then tdisp[0] = disp

; determine idl data type from format

 len = strlen(tform)

 for i = 0, N_elements(tform)-1 do begin

; Step through each character in the format, until a non-numerical character
; is encountered

        ichar = 0
NEXT_CHAR:
        if ichar GE len[i] then message, $
           'Invalid format specification for keyword TFORM ' + strtrim(i+1)
        char = strupcase( strmid(tform[i],ichar,1) )
        if ( (char GE '0') && ( char LE '9')) then begin
                ichar++
                goto, NEXT_CHAR
        endif

        if ichar EQ 0 then numval[i] = 1 else $
        numval[i] = strmid( tform[i], 0, ichar )

        if char EQ "P" then begin            ;Variable length array?
                char = strupcase( strmid(tform[i],ichar+1,1) )
                maxval[i] = long( strmid(tform[i],ichar+3, len[i]-ichar-4) )
                width[i] = 4  & numval[i] = 2  & idltype[i] = 3
        endif else begin

        tform[i] =  char

        case strupcase( tform[i] ) of

        'A' : begin 
	      idltype[i] = 7 &  width[i] = numval[i] & numval[i]=1 
	      end
        'I' : begin & idltype[i] = 2 &  width[i] = 2 &  end
        'J' : begin & idltype[i] = 3 &  width[i] = 4 &  end
        'E' : begin & idltype[i] = 4 &  width[i] = 4 &  end
        'D' : begin & idltype[i] = 5 &  width[i] = 8 &  end
        'L' : begin & idltype[i] = 7 &  width[i] = 1 &  end
        'B' : begin & idltype[i] = 1 &  width[i] = 1 &  end
        'C' : begin & idltype[i] = 6 &  width[i] = 8 &  end
        'M' : begin & idltype[i] = 9 &  width[i] =16 &  end
        'K' : begin & idltype[i] = 14 & width[i] = 8 &  end
;  Treat bit arrays as byte arrays with 1/8 the number of elements.

        'X' : begin
              idltype[i] = 1
              numval[i] = long((numval[i]+7)/8)
              width[i] = 1
              end

        else : message,'Invalid format specification for keyword ' + $
                        'TFORM'+ strtrim(i+1,2)
 endcase
 endelse

 if i ge 1 then tbcol[i] = tbcol[i-1] + width[i-1]*numval[i-1]

 endfor
 if keyword_set(noscale) then $ 

  tb_str = {TBCOL:tbcol,WIDTH:width,IDLTYPE:idltype,NUMVAL:numval,TUNIT:tunit,$
           TNULL:tnull,TFORM:tform,TTYPE:ttype,MAXVAL:maxval, TDISP:tdisp} $
 else $
 
 tb_str = {TBCOL:tbcol,WIDTH:width,IDLTYPE:idltype,NUMVAL:numval,TUNIT:tunit,$
           TNULL:tnull,TFORM:tform,TTYPE:ttype,MAXVAL:maxval, TSCAL:tscal, $
           TZERO:tzero, TDISP:tdisp}
 return
 end

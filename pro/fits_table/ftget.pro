function ftget,hdr_or_ftstr,tab,field,rows,nulls
;+
; NAME:
;      FTGET 
; PURPOSE:
;      Function to return value(s) from specified column in a FITS ASCII table
;
; CALLING SEQUENCE
;      values = FTGET( h, tab, field, [ rows, nulls ] )
;                    or
;      values = FTGET( ft_str, tab, field. [rows, nulls]
; INPUTS:
;      h - FITS ASCII extension header (e.g. as returned by FITS_READ)
;                            or
;      ft_str - FITS table structure extracted from FITS header by FTINFO
;                Use of the IDL structure will improve processing speed
;      tab - FITS ASCII table array (e.g. as returned by FITS_READ)
;      field - field name or number
;
; OPTIONAL INPUTS:
;      rows -  scalar or vector giving row number(s)
;               Row numbers start at 0.  If not supplied or set to
;               -1 then values for all rows are returned
;
; OUTPUTS:
;       the values for the row are returned as the function value.
;       Null values are set to 0 or blanks for strings.
;
; OPTIONAL OUTPUT:
;       nulls - null value flag of same length as the returned data.
;               It is set to 1 at null value positions and 0 elsewhere.
;               If supplied then the optional input, rows, must also 
;               be supplied.
;
; EXAMPLE:
;       Read the columns labeled 'WAVELENGTH' and 'FLUX' from the second
;       (ASCII table) extension of a FITS file 'spectra.fit'
;
;       IDL> fits_read,'spectra.fit',tab,htab,exten=2     ;Read 2nd extension
;       IDL> w = ftget( htab, tab,'wavelength')      ;Wavelength vector
;       IDL> f = ftget( htab, tab,'flux')            ;Flux vector
;
;       Slightly more efficient would be to first call FTINFO
;       IDL> ftinfo, htab, ft_str                     ;Extract structure
;       IDL> w = ftget(ft_str, tab,'wavelength')      ;Wavelength vector
;       IDL> f = ftget(ft_str, tab,'flux')            ;Flux vector
;
; NOTES:
;       (1) Use the higher-level procedure FTAB_EXT to extract vectors 
;               directly from the FITS file.
;       (2) Use FTAB_HELP or FTHELP to determine the columns in a particular
;               ASCII table.
; HISTORY:
;       coded by D. Lindler  July, 1987
;       Always check for null values    W. Landsman          August 1990
;       More informative error message  W. Landsman          Feb. 1996
;       Converted to IDL V5.0   W. Landsman   September 1997
;       Allow structure rather than FITS header  W. Landsman   May 2000
;       No case sensitivity in TTYPE name      W. Landsman   February 2002
;-
;------------------------------------------------------------------
; On_error,2

  sz = size(tab)
  nrows = sz(2)

; get characteristics of specified field

 size_hdr = size(hdr_or_ftstr)
 case size_hdr[size_hdr[0]+1] of 
      7: ftinfo,hdr_or_ftstr,ft_str
      8: ft_str = hdr_or_ftstr
      else: message,'ERROR - Invalid FITS header or structure supplied' 
 endcase
 
 sz = size(field)
 if ((sz[0] ne 0) or (sz[1] EQ 0)) then $
      message,'Invalid field specification, it must be a scalar'

 if sz[1] EQ 7 then begin
    field = strupcase(strtrim(field,2))
    ttype = strupcase( strtrim(ft_str.ttype,2) )
    ipos = where(ttype EQ field, Npos)
    if Npos EQ 0 then message, $ 
        'Specified field ' + strupcase(strtrim(field,2)) + ' not in table'
 endif else ipos = field -1
 ipos = ipos[0]

 tbcol = ft_str.tbcol[ipos]-1                   ;IDL starts at zero not one
 width = ft_str.width[ipos]
 tnull = ft_str.tnull[ipos]
 idltype = ft_str.idltype[ipos]

; if rows not supplied then return all rows

 if N_params() LT 4 then rows = -1

; determine if scalar supplied

 row = rows
 s = size(row) & ndim = s[0]
 if ndim EQ 0 then begin                ;scalar?
        if row LT 0 then begin  ; -1 get all rows
                ndim = 1
                row = lindgen(nrows)
           end else begin
                row = lonarr(1) + row
        end
 end

; check for valid row numbers

 if (min(row) lt 0) or (max(row) gt (nrows-1)) then $
        message,'ERROR - Row numbers must be between 0 and ' + $
                strtrim((nrows-1),2)

; get column

 if ndim EQ 0 then begin                                        ;scalar?
        dd = string(tab[tbcol:tbcol+width-1,row[0]])
        data = strarr(1)
        data[0] = dd
    end else begin                                      ;vector
        data = string(tab[tbcol:tbcol+width-1,*])
        data = data[row]
 end

; check for null values
   n = N_elements(data)
   d = make_array(size=[1,n,idltype,n])
  
 if strlen(tnull) GT 0 then begin  
    len = strlen(data[0])       ;field size
    while strlen(tnull) LT len do tnull = tnull + ' '   ;pad with blanks
    if strlen(tnull) GT len then tnull = strmid(tnull,0,len)
    nulls = data EQ tnull
    valid = where(nulls EQ 0b, nvalid)

; convert data to the correct type

    if nvalid GT 0 then d[valid] = data[valid]

 endif else d[0] = strtrim(data,2)

    return,d
 end

function tbget, hdr_or_tbstr, tab, field, rows, nulls, NOSCALE = noscale
;+
; NAME:
;       TBGET
; PURPOSE:
;       Return value(s) from specified column in a FITS binary table
;
; CALLING SEQUENCE
;       values = TBGET( h, tab, field, [ rows, nulls, /NOSCALE] )
;               or
;       values = TBGET( tb_str, tab, field, [ rows, nulls, /NOSCALE] )
;
; INPUTS:
;       h - FITS binary table header, e.g. as returned by FITS_READ
;                       or
;       tb_str - IDL structure extracted from FITS header by TBINFO.
;               Use of the IDL structure will improve processing speed
;       tab - FITS binary table array, e.g. as returned by FITS_READ
;       field - field name or number, scalar
;
; OPTIONAL INPUTS:
;       rows -  scalar or vector giving row number(s)
;               Row numbers start at 0.  If not supplied or set to
;               -1 then values for all rows are returned
;
; OPTIONAL KEYWORD INPUT:
;       /NOSCALE - If this keyword is set and nonzero, then the TSCALn and
;               TZEROn keywords will *not* be used to scale to physical values
;               Default is to perform scaling
; OUTPUTS:
;       the values for the row are returned as the function value.
;       Null values are set to 0 or blanks for strings.
;
; OPTIONAL OUTPUT:
;       nulls - null value flag of same length as the returned data.
;               Only used for integer data types, B, I, and J
;               It is set to 1 at null value positions and 0 elsewhere.
;               If supplied then the optional input, rows, must also
;               be supplied.
;
; EXAMPLE:
;       Read the columns labeled 'WAVELENGTH' and 'FLUX' from the second
;       extension of a FITS file 'spectra.fits' into IDL vectors w and f
;
;       IDL> fits_read,'spectra.fits',tab,htab,exten=2   ;Read 2nd extension
;       IDL> w = tbget(htab,tab,'wavelength')
;       IDL> f = tbget(htab,tab,'flux')
;
; NOTES:
;       (1) If the column is variable length ('P') format, then TBGET() will 
;       return the longword array of pointers into the heap area.   TBGET() 
;       currently lacks the ability to actually extract the data from the 
;       heap area.
;       (2) Use the higher-level procedure FTAB_EXT (which calls TBGET()) to
;       extract vectors directly from the FITS file.   
;       (3) Use the procedure FITS_HELP to determine which extensions are 
;       binary tables, and FTAB_HELP or TBHELP to determine the columns of the
;       table
; PROCEDURE CALLS:
;       TBINFO, TBSIZE 
; HISTORY:
;       Written  W. Landsman        February, 1991
;       Work for string and complex   W. Landsman         April, 1993
;       Default scaling by TSCALn, TZEROn, Added /NOSCALE keyword,
;       Fixed nulls output, return longword pointers for variable length
;               binary tables,     W. Landsman  December 1996
;       Added a check for zero width column  W. Landsman   April, 1997
;       Add TEMPORARY() and REFORM() for speed  W. Landsman  May, 1997
;       Use new structure returned by TBINFO    W. Landsman  August 1997
;       Add IS_IEEE_BIG(), No subscripting when all rows requested
;                               W. Landsman    March 2000
;       Use SIZE(/TNAME) instead of DATATYPE()  W. Landsman October 2001
;       Bypass IEEE_TO_HOST call for improved speed W. Landsman November 2002
;       Cosmetic changes to SIZE() calls W. Landsman December 2002
;       Added unofficial support for 64bit integers W. Landsman February 2003
;       Support unsigned integers, new pointer types of TSCAL and TZERO
;       returned by TBINFO   W. Landsman        April 2003
;       Add an i = i[0] for V6.0 compatibility  W. Landsman  August 2003
;       Use faster BYTEORDER byteswapping  W. Landsman April 2006
;       Free pointers if FITS header supplied W. Landsman March 2007
;       Use V6.0 notation W. Landsman  April 2014
;       Remove nonfunctional CONTINUE keyword W. Landsman  May 2017
;-
;------------------------------------------------------------------
 On_error,2
 compile_opt idl2
        
 if N_params() LT 3 then begin
    print, $
 'Syntax - values = TBGET(h, tab, field, [ rows, nulls, /NOSCALE ])'
    return, -1
 endif

; get size of table

 ndimen = size(tab,/n_dimen)
 if Ndimen EQ 1 then nrows =1 else $
 nrows = (size(tab,/dimen))[1]

; get characteristics of specified field

 case size(hdr_or_tbstr,/type) of 
 7: tbinfo,hdr_or_tbstr,tb_str,NOSCALE=noscale
 8: tb_str = hdr_or_tbstr
 else: message,'ERROR - Invalid FITS header or structure supplied' 
 endcase 

 tfields = N_elements(tb_str.ttype)

 case size(field,/TNAME) of

 'STRING': begin
      i = where( strupcase(tb_str.ttype) EQ strupcase(field), Nfound)
      if Nfound EQ 0 then $ 
         message,'Field ' + field + ' not found in header'
      i=i[0]
      end

 'UNDEFINED':message,'First parameter must be field name or number'
 
 ELSE: begin
      i = field[0]-1
      if (i LT 0 ) || (i GT tfields) then $
            message,'Field number must be between 1 and ' +strtrim(tfields,2)
      end

 endcase

; Now that the right column has been found, extract necessary info about this
; column 

 ttype = tb_str.ttype[i]
 numval = tb_str.numval[i]
 tform = tb_str.tform[i]
 tbcol = tb_str.tbcol[i]
 width = tb_str.width[i]
 idltype = tb_str.idltype[i]
 tnull = tb_str.tnull[i]

 if numval EQ 0 then begin 
        message,/INF, 'Column ' + ttype + ' has zero width'
        return, -1
 endif

 if tform EQ 'P' then message, /INF, $ 
           'Variable Length column - returning array of pointers'

; if rows not supplied then return all rows

 if N_params() LT 4 then rows = -1

; determine if scalar supplied

 row = rows
 ndim = size(row,/N_dimen)  
 if row[0] LT 0 then nrow = nrows else  begin
     nrow = N_elements(row)
                                              ; check for valid row numbers
     if (min(row) LT 0) || (max(row) GT (nrows-1)) then $
        message,'ERROR - Invalid row number: FITS table contains '+ $
        strtrim(nrows,2) + ' rows'
 endelse 
; get column

 if row[0] LT 0 then $                                 ;All rows?
        d = tab[tbcol:tbcol + numval*width-1,*]  $
  else if ndim EQ 0 then  $                              ;scalar?                                               
        d = tab[tbcol:tbcol + numval*width-1,row[0]] $
  else $                                        ;vector of rows
        d = tab[tbcol:tbcol + numval*width-1,row]
 Nnull = 0
; convert data to the correct type

 case idltype of

 1:  begin
     temp = byte( d, 0, numval, nrow)
     if tform EQ 'L' then begin
       d = strarr( numval, nrow )
       for j = 0, numval*nrow-1 do d[j] = string( temp[j] )
     endif else if tnull NE 0 then nullval = where(d EQ tnull, Nnull)
     end

 2:  begin
     byteorder,d,/NTOHS, /SWAP_IF_LITTLE
     d = fix(d,0, numval, nrow)
     if tnull NE 0 then nullval = where(d EQ tnull, Nnull)
     end
 
 3:  begin
     byteorder,d,/NTOHL, /SWAP_IF_LITTLE
     d = long( d, 0, numval, nrow)
     if tnull NE 0 then nullval = where(d EQ tnull, Nnull)
     end

 4:  begin
     d = float( d, 0, numval, nrow)
     byteorder,d,/LSWAP, /SWAP_IF_LITTLE
     end

 5:  begin
     d = double( d, 0, numval, nrow)
     byteorder,d,/L64SWAP, /SWAP_IF_LITTLE
      end

 6:  begin
     d = complex( d, 0, numval, nrow)
     byteorder,d,/LSWAP, /SWAP_IF_LITTLE
     end

 7:  d = string(d)


 14: begin
     d = long64(d, 0, numval, nrow)
     byteorder, d, /L64swap, /SWAP_IF_LITTLE
     end

 endcase


 if ~keyword_set(NOSCALE) then begin
    if tag_exist(tb_str,'TSCAL') then begin
        tscale = *tb_str.tscal[i]
        tzero = *tb_str.tzero[i]
        unsgn_int = (tzero EQ 32768) && (tscale EQ 1)
        unsgn_lng = (tzero EQ 2147483648) && (tscale EQ 1)
        if unsgn_int then d = uint(d) - uint(32768) $
        else if unsgn_lng then d = ulong(d) - ulong(2147483648) else $
        if ( (tscale NE 1.0) or (tzero NE 0.0) ) then $
                d = temporary(d)*tscale + tzero
	endif	
 endif

 if N_params() EQ 5 then begin
         nulls = bytarr(N_elements(d))
         if Nnull GT 0 then begin
                nulls[nullval] = 1b
                d[nullval] = 0
        endif
 endif  

; Extract correct rows if vector supplied

 if size(hdr_or_tbstr,/TYPE) NE 8 && (~keyword_set(NOSCALE)) then begin
       ptr_free, tb_str.tscal
       ptr_free, tb_str.tzero
 endif       

 if N_elements(d) EQ 1 then return, d[0] else return, reform(d,/overwrite)
 

 end

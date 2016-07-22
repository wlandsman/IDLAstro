;+
; NAME:
;       MWRFITS
; PURPOSE:
;       Write all standard FITS data types from input arrays or structures.
;
; EXPLANATION:
;       Must be used with a post-September 2009 version of FXADDPAR.
;
; CALLING SEQUENCE:
;       MWRFITS, Input, Filename, [Header],
;                       /LSCALE , /ISCALE, /BSCALE, 
;                       /USE_COLNUM, /Silent, /Create, /No_comment, /Version, $
;                       Alias=, /ASCII, Separator=, Terminator=, Null=,
;                       /Logical_cols, /Bit_cols, /Nbit_cols, 
;                       Group=, Pscale=, Pzero=, Status=
;
; INPUTS:
;       Input = Array or structure to be written to FITS file.
;
;               -When writing FITS primary data or image extensions
;                input should be an array.
;               --If data is to be grouped
;                 the Group keyword should be specified to point to
;                 a two dimensional array.  The first dimension of the
;                 Group array will be PCOUNT while the second dimension
;                 should be the same as the last dimension of Input.
;               --If Input is undefined, then a dummy primary dataset
;                 or Image extension is created [This might be done, e.g.,
;                 to put appropriate keywords in a dummy primary
;                 HDU].
;
;               -When writing an ASCII table extension, Input should
;                be a structure array where no element of the structure
;                is a structure or array (except see below).
;               --A byte array will be written as A field.  No checking
;                 is done to ensure that the values in the byte field
;                 are valid ASCII.
;               --Complex numbers are written to two columns with '_R' and
;                 '_I' appended to the TTYPE fields (if present).  The
;                 complex number is enclosed in square brackets in the output.
;               --Strings are written to fields with the length adjusted
;                 to accommodate the largest string.  Shorter strings are
;                 blank padded to the right.
;
;               -When writing a binary table extension, the input should
;                be a structure array with no element of the structure
;                being a substructure.
;
;               If a structure is specified on input and the output
;               file does not exist or the /CREATE keyword is specified
;               a dummy primary HDU is created.
;
;       Filename = String containing the name of the file to be written.
;                By default MWRFITS appends a new extension to existing
;                files which are assumed to be valid FITS.  The /CREATE
;                keyword can be used to ensure that a new FITS file
;                is created even if the file already exists.
;
; OUTPUTS:
;
; OPTIONAL INPUTS:
;       Header = Header should be a string array.  Each element of the
;                array is added as a row in the FITS  header.  No
;                parsing is done of this data.  MWRFITS will prepend
;                required structural (and, if specified, scaling)
;                keywords before the rows specified in Header.
;                Rows describing columns in the table will be appended
;                to the contents of Header.
;                Header lines will be extended or truncated to
;                80 characters as necessary.
;                If Header is specified then on return Header will have
;                the header generated for the specified extension.
;
; OPTIONAL INPUT KEYWORDS:
;       ALias=   Set up aliases to convert from the IDL structure
;                to the FITS column name.  The value should be
;                a STRARR(2,*) value where the first element of
;                each pair of values corresponds to a column
;                in the structure and the second is the name
;                to be used in the FITS file.
;                The order of the alias keyword is compatible with
;                use in MRDFITS.
;       ASCII  - Creates an ASCII table rather than a binary table.
;                This keyword may be specified as:
;                /ASCII - Use default formats for columns.
;                ASCII='format_string' allows the user to specify
;                  the format of various data types such using the following
;                  syntax 'column_type:format, column_type:format'.  E.g.,
;                ASCII='A:A1,I:I6,L:I10,B:I4,F:G15.9,D:G23.17,C:G15.9,M:G23.17'
;                gives the default formats used for each type.  The TFORM
;                fields for the real and complex types indicate will use corresponding
;                E and D formats when a G format is specified.
;                Note that the length of the field for ASCII strings and
;                byte arrays is automatically determined for each column.
;       BIT_COLS=   An array of indices of the bit columns.   The data should
;                comprise a byte array with the appropriate dimensions.
;                If the number of bits per row (see NBIT_COLS)
;                is greater than 8, then the first dimension of the array 
;                should match the number of input bytes per row.
;       BSCALE   Scale floats, longs, or shorts to unsigned bytes (see LSCALE)
;       /CREATE   If this keyword is non-zero, then a new FITS file will
;                be created regardless of whether the file currently
;                exists.  Otherwise when the file already exists,
;                a FITS extension will be appended to the existing file
;                which is assumed to be a valid FITS file.
;       GROUP=   This keyword indicates that GROUPed FITS data is to
;                be generated.
;                Group should be a 2-D array of the appropriate output type.
;                The first dimension will set the number of group parameters.
;                The second dimension must agree with the last dimension
;                of the Input array.
;       ISCALE   Scale floats or longs to short integer (see LSCALE)
;       LOGICAL_COLS=  An array of indices of the logical column numbers.
;                These should start with the first column having index *1*.
;                The structure element should either be an array of characters
;                with the values 'T' or 'F', or an array of bytes having the 
;                values byte('T')=84b, byte('F')=70b or 0b.     The use of bytes
;                allows the specification of undefined values (0b).
;       LSCALE   Scale floating point numbers to long integers.
;                This keyword may be specified in three ways.
;                /LSCALE (or LSCALE=1) asks for scaling to be automatically
;                determined. LSCALE=value divides the input by value.
;                I.e., BSCALE=value, BZERO=0.  Numbers out of range are 
;                given the value of NULL if specified, otherwise they are given
;                the appropriate extremum value.  LSCALE=(value,value)
;                uses the first value as BSCALE and the second as BZERO
;                (or TSCALE and TZERO for tables).
;       NBIT_COLS=  The number of bits actually used in the bit array.
;                This argument must point to an array of the same dimension
;                as BIT_COLS.
;       /NO_COPY = By default, MWRFITS makes a copy of the input variable
;                before any modifications necessary to write it to a FITS
;                file.    If you have a large array/structure, and don't 
;                require it for subsequent processing, then /NO_COPY will
;                save memory.
;       NO_TYPES  If the NO_TYPES keyword is specified, then no TTYPE
;                keywords will be created for ASCII and BINARY tables.
;       No_comment Do not write comment keywords in the header
;       NULL=    Value to be written for integers/strings which are
;                undefined or unwritable.
;       PSCALE=  An array giving scaling parameters for the group keywords.
;                It should have the same dimension as the first dimension
;                of Group.
;       PZERO=   An array giving offset parameters for the group keywords.
;                It should have the same dimension as the first dimension
;                of Group.
;       Separator= This keyword can be specified as a string which will
;                be used to separate fields in ASCII tables.  By default
;                fields are separated by a blank.
;       /SILENT   Suppress informative messages.  Errors will still
;                be reported.
;       Terminator= This keyword can be specified to provide a string which
;                will be placed at the end of each row of an ASCII table.
;                No terminator is used when not specified.
;                If a non-string terminator is specified (including
;                when the /terminator form is used), a new line terminator
;                is appended.
;       USE_COLNUM  When creating column names for binary and ASCII tables
;                MWRFITS attempts to use structure field name
;                values.  If USE_COLNUM is specified and non-zero then
;                column names will be generated as 'C1, C2, ... 'Cn'
;                for the number of columns in the table.
;       Version   Print the version number of MWRFITS.
;
; OPTIONAL OUTPUT KEYWORD:
;       Status - 0 if FITS file is successfully written, -1 if there is a
;                a problem (e.g. nonexistent directory, or no write permission)
; EXAMPLE:
;       Write a simple array:
;            a=fltarr(20,20)
;            mwrfits,a,'test.fits'
;
;       Append a 3 column, 2 row, binary table extension to file just created.
;            a={name:'M31', coords:(30., 20.), distance:2}
;            a=replicate(a, 2);
;            mwrfits,a,'test.fits'
;
;       Now append an image extension:
;            a=lonarr(10,10,10)
;            mkhdr,hdr,a,/image       ;Create minimal image extension FITS header
;            sxaddhist,["This is a comment line to put in the header", $
;                 "And another comment"],hdr,/comment
;            mwrfits,a,'test.fits',hdr
;
; RESTRICTIONS:
;       (1)     Variable length columns are not supported for anything
;               other than simple types (byte, int, long, float, double).
;       (2)     Empty strings are converted to 1 element blank strings (because
;               IDL refuses to write an empty string (0b) from a structure)
; NOTES:
;       This multiple format FITS writer is designed to provide a
;       single, simple interface to writing all common types of FITS data.
;       Given the number of options within the program and the
;       variety of IDL systems available it is likely that a number
;       of bugs are yet to be uncovered. 
;
; PROCEDURES USED:
;       FXPAR(), FXADDPAR
; MODIfICATION HISTORY:
;       Version 0.9: By T. McGlynn   1997-07-23
;              Initial beta release.
;       Dec 1, 1997, Lindler, Modified to work under VMS.
;       Version 0.91: T. McGlynn  1998-03-09
;               Fixed problem in handling null primary arrays.
;       Version 0.92: T. McGlynn 1998-09-09
;               Add no_comment flag and keep user comments on fields.
;               Fix handling of bit fields.
;       Version 0.93: T. McGlynn 1999-03-10
;               Fix table appends on VMS.
;       Version 0.93a  W. Landsman/D. Schlegel
;               Update keyword values in chk_and_upd if data type has changed 
;       Version 0.94: T. McGlynn 2000-02-02
;               Efficient processing of ASCII tables.
;               Use G rather than E formats as defaults for ASCII tables
;                and make the default precision long enough that transformations
;                binary to/from ASCII are invertible.
;               Some loop indices made long.
;               Fixed some ends to match block beginnings.
;       Version 0.95: T. McGlynn 2000-11-06
;               Several fixes to scaling.  Thanks to David Sahnow for
;               documenting the problems.
;               Added PCOUNT,GCOUNT keywords to Image extensions.
;               Version numbers shown in SIMPLE/XTENSION comments
;       Version 0.96: T. McGlynn 2001-04-06
;               Changed how files are opened to handle ~ consistently
;       Version 1.0: T. McGlynn 2001-12-04
;               Unsigned integers,
;               64 bit integers.
;               Aliases
;               Variable length arrays
;               Some code cleanup
;       Version 1.1: T. McGlynn 2002-2-18
;               Fixed major bug in processing of unsigned integers.
;               (Thanks to Stephane Beland)
;       Version 1.2: Stephane Beland 2003-03-17
;               Fixed problem in creating dummy dataset when passing undefined
;               data, caused by an update to FXADDPAR routine.
;       Version 1.2.1 Stephane Beland 2003-09-10
;               Exit gracefully if write privileges unavailable
;       Version 1.3 Wayne Landsman 2003-10-24
;               Don't use EXECUTE() statement if on a virtual machine
;       Version 1.3a Wayne Landsman 2004-5-21
;               Fix for variable type arrays
;       Version 1.4 Wayne Landsman 2004-07-16
;               Use STRUCT_ASSIGN when modifying structure with pointer tags
;       Version 1.4a Wayne Landsman 2005-01-03
;               Fix writing of empty strings in binary tables 
;       Version 1.4b Wayne Landsman 2006-02-23
;               Propagate /SILENT keyword to mwr_tablehdr
;       Version 1.5 Wayne Landsman  2006-05-24
;               Open file using /SWAP_IF_LITTLE_ENDIAN keyword 
;               Convert empty strings to 1 element blank strings before writing            
;       Version 1.5a Wayne Landsman 2006-06-29
;               Fix problem introduced 2006-05-24 with multidimensional strings
;       Version 1.5b K. Tolbert 2006-06-29
;               Make V1.5a fix work pre-V6.0
;       Version 1.5c I.Evans/W.Landsman 2006-08-08
;               Allow logical columns to be specified as bytes 
;       Version 1,5d K. Tolbert 2006-08-11 
;               Make V1.5a fix work for scalar empty string
;       Version 1.6  W. Landsman  2006-09-22
;               Assume since V5.5, remove VMS support
;       Version 1.6a  W. Landsman  2006-09-22
;               Don't right-justify strings 
;       Version 1.7  W. Landsman  2009-01-12
;               Added STATUS output keyword
;       Version 1.7a W. Landsman 2009-04-10
;               Since V6.4 strings are no longer limited to 1024
;               elements 
;       Version 1.8 Pierre Chanial 2009-06-23
;               trim alias, implement logical TFORM 'L', don't
;               add space after tform key.
;       Version 1.9 W. Landsman 2009-07-20
;               Suppress compilation messages of supporting routines
;       Version 1.10 W. Landsman 2009-09-30
;               Allow TTYPE values of 'T' and 'F', fix USE_COLNUM for bin tables
;       Version 1.11 W. Landsman 2010-11-18
;               Allow LONG64 number of bytes, use V6.0 notation 
;       Version 1.11a W. Landsman 2012-08-12
;               Better documentation, error checking for logical columns
;       Version 1.11b M. Haffner/W.L. 2012-10-12
;               Added /No_COPY keyword, fix problem with 32 bit overflow
;       Version 1.12 W. Landsman  2014-04-23
;       Version 1.12a W.Landsman/M. Fossati 2014-10-14
;               Fix LONG overflow for very large files
;       Version 1.12b I. Evans 2015-07-27
;               Fix value check for byte('T'), byte('F'), or 0b for logical
;               columns with null values
;       Version 1.13 W. Landsman 2016-02-24
;               Abort if a structure supplied with more than 999 tags 
;-

; What is the current version of this program?
function mwr_version
     compile_opt idl2,hidden
    return, '1.13'
end
    

; Find the appropriate offset for a given unsigned type
; or just return 0 if the type is not unsigned.

function mwr_unsigned_offset, type
     compile_opt idl2,hidden
     
    case type of            
    12: return, 32768US
    13: return, 2147483648UL
    15: return, 9223372036854775808ULL
    else: return,0
    endcase
end


; Add a keyword as non-destructively as possible to a FITS header
pro chk_and_upd, header, key, value, comment, nological=nological
     compile_opt idl2,hidden


    xcomm = ""
    if n_elements(comment) gt 0 then xcomm = comment
    if n_elements(header) eq 0 then begin
      
        fxaddpar, header, key, value, xcomm
       
    endif else begin
       
        oldvalue = fxpar(header, key, count=count, comment=oldcomment)
        if (count eq 1) then begin

           qchange = 0 ; Set to 1 if either the type of variable or its
                       ; value changes.
            size1 = size(oldvalue,/type) & size2 = size(value,/type)
            if size1 NE size2 then qchange = 1 $
            else if (oldvalue ne value) then qchange = 1

            if (qchange) then begin

               if n_elements(oldcomment) gt 0 then xcomm = oldcomment[0]
               fxaddpar, header, key, value, xcomm,nological=nological
              
           endif
           
       endif else begin
           
            fxaddpar, header, key, value, xcomm,nological=nological
        endelse
       
    endelse
end

; Get the column name appropriate for a given tag
function mwr_checktype, tag, alias=alias
     compile_opt idl2,hidden

    if ~keyword_set(alias) then return, tag

    sz = size(alias,/struc)
    ; 1 or 2 D string array with first dimension of 2
    if (sz.type_name EQ 'STRING') && (sz.dimensions[0] EQ 2) && $
       (sz.N_dimensions LE 2)  then begin 
       w = where(tag eq strtrim(alias[0,*],2),N_alias)
       if N_alias EQ 0 then return,tag else return,alias[1,w[0]]
    endif else begin
       print,'MWRFITS: Warning: Alias values not strarr(2) or strarr(2,*)'
    endelse
    return, tag
end

; Create an ASCII table
pro mwr_ascii, input, siz, lun, bof, header,     $
        ascii=ascii,                             $
       null=null,                               $
       use_colnum = use_colnum,                 $
       lscale=lscale, iscale=iscale,               $
       bscale=bscale,                           $
       no_types=no_types,                      $
       separator=separator,                     $
       terminator=terminator,                   $
        no_comment=no_comment,                   $
       silent=silent,                           $
       alias=alias
     compile_opt idl2,hidden
       
    ; Write the header and data for a FITS ASCII table extension.
  
    types=  ['A',   'I',   'L',   'B',   'F',    'D',      'C',     'M',     'K']
    formats=['A1',  'I6',  'I10', 'I4',  'G15.9','G23.17', 'G15.9', 'G23.17','I20']
    lengths=[1,     6,     10,     4,    15,     23,       15,      23,      20]

    ; Check if the user is overriding any default formats.
    sz = size(ascii)

    if sz[0] eq 0 and sz[1] eq 7 then begin
        ascii = strupcase(strcompress(ascii,/remo))
        for i=0, n_elements(types)-1  do begin
            p = strpos(ascii,types[i]+':')
            if p ge 0 then begin

               q = strpos(ascii, ',', p+1)
               if q lt p then q = strlen(ascii)+1
               formats[i] = strmid(ascii, p+2, (q-p)-2)
               len = 0
           
               reads, formats[i], len, format='(1X,I)'
               lengths[i] = len
            endif
        endfor
    endif

    i0      = input[0]
    ntag    = n_tags(i0)
    tags    = tag_names(i0)
    ctypes  = lonarr(ntag)
    strmaxs = lonarr(ntag)

    if ~keyword_set(separator) then separator=' '
    slen = strlen(separator)

    offsets = 0
    tforms = ''
    ttypes = ''
    offset = 0

    totalFormat = ""
    xsep = "";

    for i=0, ntag-1 do begin

        totalFormat = totalFormat + xsep;
    
        sz = size(i0.(i))
        if (sz[0] ne 0) && (sz[sz[0]+1] ne 1) then begin
            print, 'MWRFITS Error: ASCII table cannot contain arrays'
           return
        endif

        ctypes[i] = sz[1]

        xtype = mwr_checktype(tags[i], alias=alias)
    
        ttypes = [ttypes, xtype+' ']

        if sz[0] gt 0 then begin
            ; Byte array to be handled as a string.
           nelem = sz[sz[0]+2]
           ctypes[i] = sz[sz[0]+1]
            tf = 'A'+strcompress(string(nelem))
            tforms = [tforms, tf]
           offsets = [offsets, offset]
            totalFormat = totalFormat + tf
           offset = offset + nelem
       
        endif else if sz[1] eq 7 then begin
            ; Use longest string to get appropriate size.
           strmax = max(strlen(input.(i)))
           strmaxs[i] = strmax
           tf = 'A'+strcompress(string(strmax), /remo)
           tforms = [tforms, tf]
           offsets = [offsets, offset]
            totalFormat = totalFormat + tf
           ctypes[i] = 7
           offset = offset + strmax
       
        endif else if (sz[1] eq 6 ) || (sz[1] eq 9) then begin
            ; Complexes handled as two floats.
           offset++
       
           if sz[1] eq 6 then indx = where(types eq 'C')
           if sz[1] eq 9 then indx = where(types eq 'M')
           indx = indx[0]
           fx = formats[indx]
           if strcmp(fx,'g',1,/fold) then begin
               if (sz[1] eq 6) then begin
                   fx = "E"+strmid(fx,1 )
               endif else begin
                  fx = "D"+strmid(fx,1 )
               endelse
           endif
           tforms = [tforms, fx, fx]
            offsets = [offsets, offset, offset+lengths[indx]+1]
           nel = n_elements(ttypes)
           ttypes = [ttypes[0:nel-2], xtype+'_R', xtype+'_I']
           offset = offset + 2*lengths[indx] + 1

            totalFormat = totalFormat + '"[",'+formats[indx]+',1x,'+formats[indx]+',"]"'
            offset = offset+1
       
        endif else begin
         
            if sz[1] eq 1 then indx = where(types eq 'B')                      $
           else if (sz[1] eq 2) || (sz[1] eq 12) then indx = where(types eq 'I')  $
           else if (sz[1] eq 3) || (sz[1] eq 13) then indx = where(types eq 'L')  $
           else if sz[1] eq 4 then indx = where(types eq 'F')                 $
           else if sz[1] eq 5 then indx = where(types eq 'D')                 $
           else if (sz[1] eq 14) || (sz[1] eq 15) then indx = where(types eq 'K') $
           else begin
               print, 'MWRFITS Error: Invalid type in ASCII table'
               return
           endelse
       
           indx = indx[0]
           fx = formats[indx]
           if (strmid(fx, 0, 1) eq 'G' || strmid(fx, 0, 1) eq 'g') then begin
               if sz[1] eq 4 then begin
                   fx = 'E'+strmid(fx, 1, 99)
               endif else begin
                   fx = 'D'+strmid(fx, 1, 99)
               endelse
           endif
       
           tforms = [tforms, fx]
           offsets = [offsets, offset]
            totalFormat = totalFormat + formats[indx]
           offset = offset + lengths[indx]
        endelse
        if i ne ntag-1 then begin
            offset = offset + slen
        endif

        xsep = ", '"+separator+"', "
    
    endfor
    

    if  keyword_set(terminator) then begin
        sz = size(terminator);
        if sz[0] ne 0 || sz[1] ne 7 then begin
            terminator= string(10B)
        endif
    endif


    if keyword_set(terminator) then offset = offset+strlen(terminator)
    ; Write required FITS keywords.

    chk_and_upd, header, 'XTENSION', 'TABLE', 'ASCII table extension written by MWRFITS '+mwr_version()
    chk_and_upd, header, 'BITPIX', 8,'Required Value: ASCII characters'
    chk_and_upd, header, 'NAXIS', 2,'Required Value'
    chk_and_upd, header, 'NAXIS1', offset, 'Number of characters in a row'
    chk_and_upd, header, 'NAXIS2', n_elements(input), 'Number of rows'
    chk_and_upd, header, 'PCOUNT', 0, 'Required value'
    chk_and_upd, header, 'GCOUNT', 1, 'Required value'
    chk_and_upd, header, 'TFIELDS', n_elements(ttypes)-1, 'Number of fields'

    ; Recall that the TTYPES, TFORMS, and OFFSETS arrays have an
    ; initial dummy element.


    ; Write the TTYPE keywords.
    
    if ~keyword_set(no_types) then begin
        for i=1, n_elements(ttypes)-1 do begin
            key = 'TTYPE'+ strcompress(string(i),/remo)
            if keyword_set(use_colnum) then begin
               value = 'C'+strcompress(string(i),/remo)
           endif else begin
               value = ttypes[i]+' '
           endelse
           chk_and_upd, header, key, value
        endfor
        if (~keyword_set(no_comment)) then $
	    sxaddhist, [' ',' *** Column names ***',' '],header, $
	        /comment,location='TTYPE1'
     
    endif

    ; Write the TBCOL keywords.

    for i=1, n_elements(ttypes)-1 do begin
        key= 'TBCOL'+strcompress(string(i),/remo)
        chk_and_upd, header, key, offsets[i]+1
    endfor

    if ~keyword_set(no_comment) then $
        sxaddhist,[' ',' *** Column offsets ***',' '],header,/comm, $
	           location = 'TBCOL1'

    ; Write the TFORM keywords

    for i=1, n_elements(ttypes)-1 do begin
        key= 'TFORM'+strcompress(string(i),/remo)
        chk_and_upd, header, key, tforms[i]
    endfor

    if ~keyword_set(no_comment) then $
        sxaddhist,[' ',' *** Column formats ***',' '],header, $
	    /COMMENT, location = 'TFORM1'
 
    ; Write the header.

    mwr_header, lun, header

    ;  Write out the data applying the field formats

    totalFormat = "("+totalFormat+")";
    
     strings = string(input, format=totalFormat)
     if keyword_set(terminator) then strings = strings+terminator
     writeu, lun, strings
 
    ; Check to see if any padding is required.

    nbytes = long64(n_elements(input))*offset
    padding = 2880 - nbytes mod 2880
    if padding ne 0 then writeu, lun, replicate(32b, padding)
    
   return
end

; Write a dummy primary header-data unit.
pro mwr_dummy, lun
     compile_opt idl2,hidden

    fxaddpar, header, 'SIMPLE', 'T','Dummy Created by MWRFITS v'+mwr_version()
    fxaddpar, header, 'BITPIX', 8, 'Dummy primary header created by MWRFITS'
    fxaddpar, header, 'NAXIS', 0, 'No data is associated with this header'
    fxaddpar, header, 'EXTEND', 'T', 'Extensions may (will!) be present'

    mwr_header, lun, header
end

; Check if this is a valid pointer array for variable length data.
function mwr_validptr, vtypes, nfld, index, array
     compile_opt idl2,hidden
    
    type = -1
    offset = 0L
    for i=0, n_elements(array)-1 do begin
       if ptr_valid(array[i]) then begin
           
           sz = size(*array[i])
           if sz[0] gt 1 then begin
              print,'MWRFITS: Error: Multidimensional Pointer array'
              return, 0
           endif
           if type eq -1 then begin
              type = sz[sz[0] + 1]
           endif else begin
              if sz[sz[0] + 1] ne type then begin
                  print,'MWRFITS: Error: Inconsistent type in pointer array'
                  return, 0
              endif
           endelse
           xsz = sz[1]
           if sz[0] eq 0 then xsz = 1
           offset = offset + xsz
       endif
    endfor
    if type eq -1 then begin
        ; If there is no data assume an I*2 type
       type = 2
    endif

    if  (type lt 1 || type gt 5) &&(type lt 12 || type gt 15) then begin
       print,'MWRFITS: Error: Unsupported type for variable length array'
    endif

    types = 'BIJED      IJKK'
    sizes = [1,2,4,4,8,0,0,0,0,0,0,2,4,8,8]
    
    if n_elements(vtypes) eq 0 then begin
       
        vtype = {status:0, data:array,           $
            type: strmid(types, type-1, 1),            $
            itype: type, ilen: sizes[type-1],          $
            offset:offset }
    
       vtypes = replicate(vtype, nfld)

    endif else begin
       ; This ensures compatible structures without
       ; having to used named structures.
       
       vtype = vtypes[0]
       vtype.status = 0
       vtype.data   = array
       vtype.type   = strmid(types, type-1, 1)
       vtype.itype  = type
       vtype.ilen   = sizes[type-1]
       vtype.offset = offset
       vtypes[index] = vtype
       
       
    endelse
    vtypes[index].status = 1;

    return, 1
end
       
; Handle the header for a binary table.
pro mwr_tablehdr, lun, input, header, vtypes,     $
              no_types=no_types,                $
              logical_cols = logical_cols,         $
              bit_cols = bit_cols,                $
              nbit_cols= nbit_cols,             $
                no_comment=no_comment,            $
              alias=alias,                      $
              silent=silent,                     $
	      use_colnum = use_colnum
     compile_opt idl2,hidden

    if ~keyword_set(no_types) then no_types = 0

    nfld = n_tags(input[0])
    if nfld le 0 then begin
       print, 'MWRFITS Error: Input contains no structure fields.'
       return
    endif

    tags = tag_names(input)

    ; Get the number of rows in the table.

    nrow = n_elements(input)

    dims    = lonarr(nfld)
    tdims   = strarr(nfld)
    types   = strarr(nfld)
    pointers= lonarr(nfld)

    ; offsets = null...  Don't want to define this
    ; in advance since reference to ulon64 won't word with IDL < 5.2
    ;
    ; Get the type and length of each column.  We do this
    ; by examining the contents of the first row of the structure.
    ;

    nbyte = 0ULL

    islogical = bytarr(nfld)
    if keyword_set(logical_cols) then islogical[logical_cols-1] = 1b
   
    for i=0, nfld-1 do begin

       a = input[0].(i)

       sz = size(a)
       
       nelem    = ulong64(sz[sz[0]+2])
       type_ele = sz[sz[0]+1]
       if type_ele EQ 7 then maxstr = max(strlen(input.(i)) > 1)
       
       if islogical[i]  then begin        
          if (type_ele EQ 1) then begin
          gg = (input.(i) EQ 84b) or (input.(i) EQ 70b) or (input.(i) EQ 0b) 
	       if ~array_equal(gg,1b) then begin 
	       islogical[i] = 0b
	       message,/CON, 'Warning - ' + $ 
	  "Allowed Logical Column byte values are byte('T'), byte('F'), or 0b"
	   endif
	endif else if (type_ele EQ 7) then begin   	  	 
           gg =  (input.(i) eq 'T') or (input.(i) eq 'F')
	       if ~array_equal(gg,1b) then begin
	       islogical[i] = 0b
	       message,/CON, 'Warning - ' + $ 
	  'Allowed Logical column string values are "T" and "F"'
	   endif
	endif else begin 
	    message,/CON, $
	    'Warning - Logical Columns must be of type string or byte'
	    islogical[i] = 0b
	 endelse   	  
       endif               
       dims[i] = nelem
       
        if (sz[0] lt 1) || (sz[0] eq 1 && type_ele ne 7) then begin
           tdims[i] = ''
       endif else begin
           tdims[i] = '('
           
           if type_ele eq 7 then begin
               tdims[i] += strcompress(string(maxstr), /remo) + ','
           endif
           
           for j=1, sz[0] do begin
               tdims[i] += strcompress(sz[j])
               if j ne sz[0] then tdims[i] += ','
           endfor
           
           tdims[i] +=  ')'
       endelse
             
       case type_ele of
          1:        begin
                     types[i] = 'B'
                     nbyte += nelem
              end
          2:       begin
                         types[i] = 'I'
                     nbyte += 2*nelem
              end
          3:       begin
                     types[i] = 'J'
                     nbyte += 4*nelem
              end
          4:       begin
                        types[i] = 'E'
                     nbyte += 4*nelem
               end
          5:       begin
                     types[i] = 'D'
                     nbyte += 8*nelem
              end
          6:       begin
                        types[i] = 'C'
                     nbyte += 8*nelem
              end
          7:       begin
                     maxstr = max(strlen(input.(i)) > 1 )
                     types[i] = 'A'
                     nbyte += maxstr*nelem
                     dims[i] = maxstr*nelem		    
              end
          9:   begin
                       types[i] = 'M'
                     nbyte += 16*nelem
              end

         10:   begin
                       if ~mwr_validptr(vtypes, nfld, i, input.(i)) then begin
                         return
                     endif
                     
                       types[i] = 'P'+vtypes[i].type
                     nbyte += 8
                     dims[i] = 1

                     test = mwr_unsigned_offset(vtypes[i].itype)
                     if test gt 0 then begin
                         if (n_elements(offsets) lt 1) then begin
                             offsets = ulon64arr(nfld)
                         endif
                         offsets[i] = test
                     endif
                     
               end

         12:   begin
                      types[i] = 'I'
                     if (n_elements(offsets) lt 1) then begin
                         offsets = ulon64arr(nfld)
                     endif
                     offsets[i] = mwr_unsigned_offset(12);
                     nbyte += 2*nelem
              end

         13:   begin
                      types[i] = 'J'
                     if (n_elements(offsets) lt 1) then begin
                         offsets = ulon64arr(nfld)
                     endif
                     offsets[i] = mwr_unsigned_offset(13);
                     nbyte += 4*nelem
              end
              
                ; 8 byte integers became standard FITS in December 2005
         14:   begin
                     types[i] = 'K'
                     nbyte += 8*nelem
               end

         15:   begin
                      types[i] = 'K'
                     nbyte += 8*nelem
                     if (n_elements(offsets) lt 1) then begin
                         offsets = ulon64arr(nfld)
                     endif
                     offsets[i] = mwr_unsigned_offset(15)
               end
                  
          0:   begin
                        print,'MWRFITS Error: Undefined structure element??'
                     return
              end
              
          8:   begin
                        print, 'MWRFITS Error: Nested structures'
                     return
              end
              
          else:begin
                        print, 'MWRFITS Error: Cannot parse structure'
                     return
              end
       endcase
    endfor

    ; Put in the required FITS keywords.
    chk_and_upd, header, 'XTENSION', 'BINTABLE', 'Binary table written by MWRFITS v'+mwr_version()
    chk_and_upd, header, 'BITPIX', 8, 'Required value'
    chk_and_upd, header, 'NAXIS', 2, 'Required value'
    chk_and_upd, header, 'NAXIS1', nbyte, 'Number of bytes per row'
    chk_and_upd, header, 'NAXIS2', n_elements(input), 'Number of rows'
    chk_and_upd, header, 'PCOUNT', 0, 'Normally 0 (no varying arrays)'
    chk_and_upd, header, 'GCOUNT', 1, 'Required value'
    chk_and_upd, header, 'TFIELDS', nfld, 'Number of columns in table'

    ;
    ; Handle the special cases.
    ;
    g = where(islogical,Nlogic)
    if Nlogic GT 0 then types[g] = 'L'
       
    if keyword_set(bit_cols) then begin
       nb = n_elements(bit_cols)
       if nb ne n_elements(nbit_cols) then begin
           print,'WARNING: Bit_cols and Nbit_cols not same size'
           print,'         No bit columns generated.'
          goto, after_bits
       endif
       for i = 0, nb-1 do begin
           nbyte = (nbit_cols[i]+7)/8
           icol = bit_cols[i]
           if types[icol-1] ne 'B'  || (dims[icol-1] ne nbyte) then begin
              print,'WARNING: Invalid attempt to create bit column:',icol
                    goto, next_bit
           endif
           types[icol-1] = 'X'
           tdims[icol-1] = ''
           dims[icol-1] = nbit_cols[i]
  next_bit:
       endfor
  after_bits:
    endif



    ; Write scaling info as needed.
    if n_elements(offsets) gt 0 then begin
        w = where(offsets gt 0)

        for i=0, n_elements(w) - 1 do begin
            key = 'TSCAL'+strcompress(string(w[i])+1,/remo)
           chk_and_upd, header, key, 1
        endfor
    
        for i=0, n_elements(w) - 1 do begin
           key = 'TZERO'+strcompress(string(w[i]+1),/remo)
           chk_and_upd, header, key, offsets[w[i]]
        endfor
    
        if ~keyword_set(no_comment) then begin
            key = 'TSCAL'+strcompress(string(w[0])+1,/remo)
	   sxaddhist,[' ',' *** Unsigned integer column scalings *',' '], $
	        header,/COMMENT,location = key
    endif
    endif
  
    ; Now add in the TFORM keywords
    for i=0, nfld-1 do begin
       if dims[i] eq 1 then begin
           form = types[i]
       endif else begin
           form=strcompress(string(dims[i]),/remove) + types[i]
        endelse
       
       tfld = 'TFORM'+strcompress(string(i+1),/remove)
       
       ; Check to see if there is an existing value for this keyword.
       ; If it has the proper value we will not modify it.
       ; This can matter if there is optional information coded
       ; beyond required TFORM information.
              
       oval = fxpar(header, tfld)
       oval = strcompress(string(oval),/remove_all)
       if (oval eq '0')  ||  (strmid(oval, 0, strlen(form)) ne form) then begin
           chk_and_upd, header, tfld, form
       endif
    endfor

    if ~keyword_set(no_comment) then $
        sxaddhist,[' ',' *** Column formats ***',' '],header, $
	    /COMMENT, location='TFORM1'
 
    ; Now write TDIM info as needed.
    for i=nfld-1, 0,-1 do begin
        if tdims[i] ne '' then begin
            fxaddpar, header, 'TDIM'+strcompress(string(i+1),/remo), tdims[i],after=tfld
        endif
    endfor

    w=where(tdims ne '',N_tdims)
    if (N_tdims GT 0) && ~keyword_set(no_comment) then begin
        fxaddpar, header, 'COMMENT', ' ', after=tfld
        fxaddpar, header, 'COMMENT', ' *** Column dimensions (2 D or greater) ***', after=tfld
        fxaddpar, header, 'COMMENT', ' ', after=tfld
    endif

    for i=0, nfld-1 do begin
        if tdims[i] ne '' then begin
            chk_and_upd, header, 'TDIM'+strcompress(string(i+1),/remo), tdims[i]
        endif
    endfor

    if n_elements(vtypes) gt 0 then begin
        fxaddpar, header, 'THEAP', nbyte*n_elements(input), 'Offset of start of heap'
        offset = 0L
        for i=0,n_elements(vtypes)-1 do begin
           if vtypes[i].status then offset = offset + vtypes[i].offset*vtypes[i].ilen
        endfor
        fxaddpar, header, 'PCOUNT', offset, 'Size of heap'
    endif

    ;
    ; Last add in the TTYPE keywords if desired.
    ;
    if ~no_types then begin
       for i=0, nfld - 1 do begin
           key = 'TTYPE'+strcompress(string(i+1),/remove)
           if ~keyword_set(use_colnum) then begin
               value= mwr_checktype(tags[i],alias=alias)
           endif else begin
               value = 'C'+strmid(key,5,2) + ' '
           endelse
          chk_and_upd, header, key, value, /nological
       endfor
       
        if ~keyword_set(no_comment) then $
	   sxaddhist,[' ',' *** Column names *** ',' '],header,/comment, $
	        location = 'TTYPE1'
     endif

    if ~keyword_set(no_comment) then begin
        fxaddpar, header, 'COMMENT', ' ', after='TFIELDS'
        fxaddpar, header, 'COMMENT', ' *** End of mandatory fields ***', after='TFIELDS'
        fxaddpar, header, 'COMMENT', ' ', after='TFIELDS'
    endif

    ; Write to the output device.
    mwr_header, lun, header

end

; Modify the structure to put the pointer column in.
function mwr_retable, input, vtypes

     compile_opt idl2,hidden

    offset = 0L
    tags = tag_names(input);
;Create an output structure identical to the input structure but with pointers replaced
; by a 2 word lonarr to point to the heap area

      if vtypes[0].status then begin
        output = CREATE_STRUCT(tags[0],lonarr(2))
      endif else begin
         output = CREATE_STRUCT(tags[0],input[0].(0))
      endelse
      for i=1, n_elements(tags) -1 do begin
         if vtypes[i].status then begin
           output = CREATE_STRUCT(temporary(output), tags[i], lonarr(2))
         endif else begin
           output = CREATE_STRUCT(temporary(output), tags[i], input[0].(i))
         endelse
      endfor
      output = replicate(temporary(output), N_elements(input) )
      struct_assign, input, output      ;Available since V5.1

    for i=0, n_elements(tags)-1 do begin
       if vtypes[i].status then begin
           for j=0, n_elements(input)-1 do begin
              ptr = input[j].(i)
              if ptr_valid(ptr) then begin
                  sz = size(*ptr)
                  if sz[0] eq 0 then xsz = 1 else xsz= sz[1]

                  output[j].(i)[0] = xsz
                  output[j].(i)[1] = offset
                  
                  offset = offset + vtypes[i].ilen*xsz
              endif
           endfor
       endif
    endfor
    return,output
end

; Write the heap data.
function mwr_writeheap, lun, vtypes

    offset = 0L
    
    for i=0, n_elements(vtypes)-1 do begin
       if vtypes[i].status then begin
           
           itype = vtypes[i].itype
           unsigned = mwr_unsigned_offset(itype)
           
           ptrs = vtypes[i].data
           
           for j=0,n_elements(ptrs)-1 do begin
              if ptr_valid(ptrs[j]) then begin
                  if (unsigned gt 0) then begin
                     *ptrs[j] = *ptrs[j] + unsigned
                  endif

                      writeu, lun, *ptrs[j]
                  
                  sz = size(*ptrs[j])
                  xsz = 1 > sz[1]
                  offset = offset + xsz * vtypes[i].ilen
              endif
           endfor
       endif
    endfor

    return, offset
    
end

; Write the binary table.
pro mwr_tabledat, lun, input, header, vtypes
     compile_opt idl2,hidden
    ;
    ; file              -- unit to which data is to be written.
    ; Input              -- IDL structure
    ; Header       -- Filled header

    nfld = n_tags(input)

    ; Any special processing?

    typ = intarr(nfld)
    for i=0, nfld-1 do begin
        
        typ[i] = size(input.(i),/type)
	    if (typ[i] eq 7) then begin

             dim = size(input.(i),/dimen) >1
             siz = max(strlen(input.(i))) > 1
	     input.(i) = $
	        strmid( input.(i) + string(replicate(32b, siz)), 0, siz)

       endif
 
       unsigned = mwr_unsigned_offset(typ[i])
       if (unsigned gt 0) then begin
           input.(i) = input.(i) + unsigned
       endif
       
    endfor

    if n_elements(vtypes) gt 0 then begin
          
      
        input = mwr_retable(input, vtypes)
    endif

    ; Write the data segment.
    ;
    writeu, lun, input

    nbyte = long64(fxpar(header, 'NAXIS1'))
    nrow  = n_elements(input)

    heap = 0
    if n_elements(vtypes) gt 0 then $
        heap = mwr_writeheap(lun, vtypes)

    siz   = nbyte*nrow + heap
    padding = 2880 - (siz mod 2880)
    if padding eq 2880 then padding = 0

    ;
    ; If necessary write the padding.
    ;
    if padding gt 0 then begin
        pad = bytarr(padding)  ; Should be null-filled by default.
        writeu, lun, pad
    endif

end


; Scale parameters for GROUPed data.
pro mwr_pscale, grp, header, pscale=pscale, pzero=pzero
     compile_opt idl2,hidden
    

; This function assumes group is a 2-d array.

    if ~keyword_set(pscale) && ~keyword_set(pzero) then return

    if ~keyword_set(pscale) then begin
        pscale = dblarr(sizg[1])
        pscale[*] = 1.
    endif

    w = where(pzero eq 0.d0)

    if w[0] ne 0 then begin
        print, 'MWRFITS  Warning: PSCALE value of 0 found, set to 1.'
        pscale[w] = 1.d0
    endif

    if keyword_set(pscale) then begin
        for i=0L, sizg[1]-1 do begin
            key= 'PSCAL' + strcompress(string(i+1),/remo)
            chk_and_upd, header, key, pscale[i]
        endfor
    endif

    if ~keyword_set(pzero) then begin
        pzero = dblarr(sizg[1])
        pzero[*] = 0.
    endif else begin
        for i=0L, sizg[1]-1 do begin
            key= 'PZERO' + strcompress(string(i+1),/remo)
            chk_and_upd, header, key, pscale[i]
        endfor
    endelse

    for i=0L, sizg[1]-1 do begin
        grp[i,*] = grp[i,*]/pscale[i] - pzero[i]
    endfor

end


; Find the appropriate scaling parameters.
pro mwr_findscale, flag, array, nbits, scale, offset, error

     compile_opt idl2,hidden

    error = 0
    if n_elements(flag) eq 2 then begin
         scale  = double(flag[0])
        offset = double(flag[1])
    endif else if n_elements(flag) eq 1 and flag[0] ne 1 then begin
         minmum = min(array, max=maxmum)
        offset = 0.d0
        scale  = double(flag[0])
    endif else if n_elements(flag) ne 1 then begin
         print, 'MWRFITS Error: Invalid scaling parameters.'
        error  = 1
        return
    endif else begin
        
         minmum = min(array, max=maxmum)
        scale  = (maxmum-minmum)/(2.d0^nbits)
        amin   = -(2.d0^(nbits-1))
        if (amin gt -130) then amin = 0  ; looking for -128
        offset = minmum - scale*amin
        
    endelse
    return
end

; Scale and possibly convert array according to information
; in flags.
pro mwr_scale, array, scale, offset, lscale=lscale, iscale=iscale,  $
   bscale=bscale, null=null

     compile_opt idl2,hidden

    ; First deallocate scale and offset
    if n_elements(scale)  gt 0 then xx = temporary(scale)
    if n_elements(offset) gt 0 then xx = temporary(offset)

    if ~keyword_set(lscale) && ~keyword_set(iscale) &&  $
       ~keyword_set(bscale) then return

    siz = size(array)
    if keyword_set(lscale) then begin

        ; Doesn't make sense to scale data that can be stored exactly.
        if siz[siz[0]+1] lt 4 then return
        amin = -2.d0^31
        amax = -(amin + 1)
    
        mwr_findscale, lscale, array, 32, scale, offset, error

    endif else if keyword_set(iscale) then begin
        if siz[siz[0]+1] lt 3 then return
        amin = -2.d0^15
        amax = -(amin + 1)
    
        mwr_findscale, iscale, array, 16, scale, offset, error

    endif else begin
        if siz[siz[0]+1] lt 2 then return
    
        amin = 0
        amax = 255
    
        mwr_findscale, bscale, array, 8, scale, offset, error
    endelse

    ; Check that there was no error in mwr_findscale
    if error gt 0 then return

    if scale le 0.d0 then begin
        print, 'MWRFITS Error: BSCALE/TSCAL=0'
        return
    endif

    array = round((array-offset)/scale)

    w = where(array gt amax)
    if w[0] ne -1 then $
        array[w] = keyword_set(null) ? null : amax
 
    w = where(array lt amin)
    if w[0] ne -1 then $
        array[w] = keyword_set(null) ? null : amin
 
    if keyword_set(lscale) then      array = long(array) $
    else if keyword_set(iscale) then array = fix(array)  $
    else                             array = byte(array)
    
end

; Write a header
pro mwr_header, lun, header

     compile_opt idl2,hidden
    ; Fill strings to at least 80 characters and then truncate.

    space = string(replicate(32b, 80))
    header = strmid(header+space, 0, 80)

    w = where(strcmp(header,"END     ",8), Nw)

    if Nw eq 0 then begin

       header = [header, strmid("END"+space,0,80)]
       
    endif else begin
        if (Nw gt 1) then begin 
           ; Get rid of extra end keywords;
           print,"MWRFITS Warning: multiple END keywords found."
           for irec=0L, n_elements(w)-2 do begin
              header[w[irec]] = strmid('COMMENT INVALID END REPLACED'+  $
                space, 0, 80)
           endfor
       endif

       ; Truncate header array at END keyword.
       header = header[0:w[n_elements(w)-1]]
    endelse

    nrec = n_elements(header)
    if nrec mod 36 ne 0 then header = [header, replicate(space,36 - nrec mod 36)]

    writeu, lun, byte(header)
end


; Move the group information within the data.
pro mwr_groupinfix, data, group, hdr
     compile_opt idl2,hidden

    siz = size(data)
    sizg = size(group)

    ; Check if group info is same type as data 

    if siz[siz[0]+1] ne sizg[3] then begin
        case siz[siz[0]+1] of
         1: begin
               mwr_groupscale, 127.d0, group, hdr
               group = byte(group)
           end
         2: begin
               mwr_groupscale, 32767.d0, group, hdr
               group = fix(group)
           end
         3: begin
               mwr_groupscale, 2147483647.d0, group, hdr
               group = long(group)
           end
         4: group = float(group)
         5: group = double(group)
      else: begin
                print,'MWRFITS Internal error: Conversion of group data'
               return
            end
        endcase
    endif

    nrow = 1
    for i=1, siz[0]-1 do begin
        nrow = nrow*siz[i]
    endfor

    data = reform(data, siz[siz[0]+2])
    for i=0L, siz[siz[0]] - 1 do begin
        if i eq 0 then begin
            gdata = group[*,0]
           gdata = reform(gdata)
            tdata = [ gdata , data[0:nrow-1]]
        endif else begin
            start = nrow*i
           fin = start+nrow-1
           gdata = group[*,i]
            tdata = [tdata, gdata ,data[start:fin]]
       endelse
    endfor

    data = temporary(tdata)
end

; If an array is being scaled to integer type, then
; check to see if the group parameters will exceed the maximum
; values allowed.  If so scale them and update the header.
pro mwr_groupscale, maxval, group, hdr
     compile_opt idl2,hidden

    sz = size(group)
    for i=0L, sz[1]-1 do begin
         pmax = max(abs(group[i,*]))
         if (pmax gt maxval) then begin
             ratio = pmax/maxval
            psc = 'PSCAL'+strcompress(string(i+1),/remo)
            currat = fxpar(hdr, psc)
            if (currat ne 0) then begin
                fxaddpar, hdr, psc, currat*ratio, 'Scaling overriden by MWRFITS'
            endif else begin
                fxaddpar, hdr, psc, ratio, ' Scaling added by MWRFITS'
            endelse
             group[i,*] = group[i,*]/ratio
         endif
    endfor
end
        
        
; Write out header and image for IMAGE extensions and primary arrays.
pro mwr_image, input, siz, lun, bof, hdr,       $
       null=null,                              $
       group=group,                            $
       pscale=pscale, pzero=pzero,             $
       lscale=lscale, iscale=iscale,              $
       bscale=bscale,                          $
        no_comment=no_comment,                  $
       silent=silent


    compile_opt idl2,hidden
    type = siz[siz[0] + 1]

    bitpixes=[8,8,16,32,-32,-64,-32,0,0,-64,0,0,16,32,64,64]

    ; Convert complexes to two element real array.

    if type eq 6 || type eq 9 then begin
 
        if ~keyword_set(silent) then begin
            print, "MWRFITS Note: Complex numbers treated as arrays"
        endif
    
        array_dimen=(2)
        if siz[0] gt 0 then array_dimen=[array_dimen, siz[1:siz[0]]] 
        if siz[siz[0]+1] eq 6 then data = float(input,0,array_dimen)  $
        else data = double(input,0,array_dimen)

    ; Convert strings to bytes.
    endif else if type eq 7 then begin
        data = input
        len = max(strlen(input))
        if len eq 0 then begin
            print, 'MWRFITS Error: strings all have zero length'
           return
        endif

        for i=0L, n_elements(input)-1 do begin
            t = len - strlen(input[i])
           if t gt 0 then input[i] = input[i] + string(replicate(32B, len))
        endfor
    
        ; Note that byte operation works on strings in a special way
        ; so we don't go through the subterfuge we tried above.
    
        data = byte(data)
    
    endif else if n_elements(input) gt 0 then data = input


    ; Do any scaling of the data.
    mwr_scale, data, scalval, offsetval, lscale=lscale, $
      iscale=iscale, bscale=bscale, null=null

    ; This may have changed the type.
    siz  = size(data)
    type = siz[siz[0]+1]


    ; If grouped data scale the group parameters.
    if keyword_set(group) then mwr_pscale, group, hdr, pscale=pscale, pzero=pzero

    if bof then begin
        chk_and_upd, hdr, 'SIMPLE', 'T','Primary Header created by MWRFITS v'+mwr_version()
        chk_and_upd, hdr, 'BITPIX', bitpixes[type]
        chk_and_upd, hdr, 'NAXIS', siz[0]
        chk_and_upd, hdr, 'EXTEND', 'T', 'Extensions may be present'
    endif else begin
        chk_and_upd, hdr, 'XTENSION', 'IMAGE','Image Extension created by MWRFITS v'+mwr_version()
        chk_and_upd, hdr, 'BITPIX', bitpixes[type]
        chk_and_upd, hdr, 'NAXIS', siz[0]
        chk_and_upd, hdr, 'PCOUNT', 0
        chk_and_upd, hdr, 'GCOUNT', 1
    endelse


    if keyword_set(group) then begin
        group_offset = 1
    endif else group_offset = 0

    if keyword_set(group) then begin
       chk_and_upd, hdr, 'NAXIS1', 0
    endif

    for i=1L, siz[0]-group_offset do begin
        chk_and_upd, hdr, 'NAXIS'+strcompress(string(i+group_offset),/remo), siz[i]
    endfor


    if keyword_set(group) then begin
        chk_and_upd, hdr, 'GROUPS', 'T'
        sizg = size(group)
        if sizg[0] ne 2 then begin
            print,'MWRFITS Error: Group data is not 2-d array'
           return
        endif
        if sizg[2] ne siz[siz[0]] then begin
            print,'MWRFITS Error: Group data has wrong number of rows'
           return
        endif
        chk_and_upd,hdr,  'PCOUNT', sizg[1]
        chk_and_upd, hdr, 'GCOUNT', siz[siz[0]]
    endif
    
    if n_elements(scalval) gt 0 then begin
    
        chk_and_upd, hdr, 'BSCALE', scalval
        chk_and_upd, hdr, 'BZERO', offsetval
    
    endif else begin
       
       ; Handle unsigned offsets
       bzero = mwr_unsigned_offset(type)
       if bzero gt 0 then begin
           chk_and_upd,hdr,'BSCALE', 1
           chk_and_upd, hdr, 'BZERO', bzero
           data += bzero
        endif
       
    endelse

    if keyword_set(group) then begin
        if keyword_set(pscale) then begin
            if n_elements(pscale) ne sizg[1] then begin
               print, 'MWRFITS Warning: wrong number of PSCALE values'
           endif else begin
                for i=1L, sizg[1] do begin
                    chk_and_upd, hdr, 'PSCALE'+strcompress(string(i),/remo)
               endfor
           endelse
        endif
        if keyword_set(pzero) then begin
            if n_elements(pscale) ne sizg[1] then begin
               print, 'MWRFITS Warning: Wrong number of PSCALE values'
           endif else begin
                for i=1L, sizg[1] do begin
                    chk_and_upd, hdr, 'PZERO'+strcompress(string(i),/remo)
               endfor
           endelse
        endif
    endif

    bytpix=abs(bitpixes[siz[siz[0]+1]])/8             ; Number of bytes per pixel.
    npixel = n_elements(data) + n_elements(group)     ; Number of pixels.

    if keyword_set(group) then mwr_groupinfix, data, group, hdr

    ; Write the FITS header
    mwr_header, lun, hdr

    ; This is all we need to do if input is undefined.
    if (n_elements(input) eq 0) || (siz[0] eq 0) then return

    ; Write the data.
    writeu, lun, data

    nbytes = long64(bytpix)*npixel
    filler = 2880 - nbytes mod 2880
    if filler eq 2880 then filler = 0
  
    ; Write any needed filler.
    if filler gt 0 then writeu, lun, replicate(0B,filler)
end


; Main routine -- see documentation at start
pro mwrfits, xinput, file, header,              $
        ascii=ascii,                            $
       separator=separator,                    $
       terminator=terminator,                  $
       create=create,                          $
       null=null,                              $
       group=group,                            $
       pscale=pscale, pzero=pzero,             $
       alias=alias,                            $
       use_colnum = use_colnum,                $
       lscale=lscale, iscale=iscale,              $
       no_copy = no_copy,                      $
       bscale=bscale,                          $
       no_types=no_types,                      $
       silent=silent,                          $
       no_comment=no_comment,                  $
       logical_cols=logical_cols,              $
       bit_cols=bit_cols,                      $
       nbit_cols=nbit_cols,                    $
       status = status,                        $
       version=version


    ; Check required keywords.
    compile_opt idl2
    status = -1                     ;Status changes to 0 upon completion
    if keyword_set(Version) then begin
        print, "MWRFITS V"+mwr_version()+":  February 24, 2016"
    endif

    if n_elements(file) eq 0 then begin
        if ~keyword_set(Version) then begin
            print, 'MWRFITS: Usage:'
            print, '    MWRFITS, struct_name, file, [header,] '
            print, '             /CREATE, /SILENT, /NO_TYPES, /NO_COMMENT, '
            print, '             GROUP=, PSCALE=, PZERO=,'
            print, '             LSCALE=, ISCALE=, BSCALE=,'
            print, '             LOGICAL_COLS=, BIT_COLS=, NBIT_COLS=,'
            print, '             ASCII=, SEPARATOR=, TERMINATOR=, NULL='
           print, '             /USE_COLNUM, ALIAS=, STATUS='
        endif
        return
    endif

    if size(xinput,/TNAME) EQ 'STRUCT' then $
        if N_tags(xinput) GT 999 then begin
        message,'ERROR - Input structure contains ' + strtrim(N_tags(xinput),2) + ' tags',/CON
        message,'ERROR - FITS files are limited to 999 columns',/CON
        return
    endif     

    ; Save the data into an array/structure that we can modify.
 
    if n_elements(xinput) gt 0 then $
        if keyword_set(no_copy) then input = temporary(xinput) $
                                else input = xinput

    on_ioerror, open_error

    ; Open the input file.    If it exists, and the /CREATE keyword is not 
    ; specified, then we append to to the existing file.
     ;

    if  ~keyword_set(create) && file_test(file) then begin
        openu, lun, file, /get_lun, /append,/swap_if_little
        if ~keyword_set(silent) then $
	    message,/inf,'Appending FITS extension to file ' + file
        bof = 0
    endif else begin 
        openw, lun, file, /get_lun, /swap_if_little
         bof = 1
    endelse 	 
    on_ioerror, null


    siz = size(input) 
     if siz[siz[0]+1] ne 8 then begin

        ; If input is not a structure then call image writing utilities.
        mwr_image, input, siz, lun, bof, header,    $
         null=null,                              $
         group=group,                            $
         pscale=pscale, pzero=pzero,             $
         lscale=lscale, iscale=iscale,              $
         bscale=bscale,                          $
         no_comment=no_comment,                  $
         silent=silent

    endif else if keyword_set(ascii) then begin

        if bof then mwr_dummy, lun
        ; Create an ASCII table.
	mwr_ascii, input, siz, lun, bof, header,     $
         ascii=ascii,                             $
         null=null,                               $
         use_colnum = use_colnum,                 $
         lscale=lscale, iscale=iscale,               $
         bscale=bscale,                           $
         no_types=no_types,                      $
         separator=separator,                     $
         terminator=terminator,                   $
         no_comment=no_comment,                   $
         alias=alias,                             $
         silent=silent

    endif else begin

        if bof then mwr_dummy, lun

        ; Create a binary table.
        mwr_tablehdr, lun, input, header, vtypes,    $
          no_types=no_types,                        $
          logical_cols = logical_cols,                    $
          bit_cols = bit_cols,                           $
          nbit_cols= nbit_cols,                     $
          alias=alias,                              $
          no_comment=no_comment,                    $
	  silent=silent,                             $
	  use_colnum = use_colnum
       
        mwr_tabledat, lun, input, header, vtypes

    endelse

    free_lun, lun
    status=0
    return
    
    ; Handle error in opening file.
  open_error:
    on_ioerror, null
    print, 'MWRFITS Error: Cannot open output: ', file
	 print,!ERROR_STATE.SYS_MSG
    if n_elements(lun) gt 0 then free_lun, lun
    
    return
end

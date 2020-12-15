pro writefits, filename, data, header, heap, Append = Append, Silent = Silent, $
       compress = compress, CheckSum = checksum, NaNValue = NaNvalue
       
;+
; NAME:
;       WRITEFITS
; PURPOSE:
;       Write IDL array and header variables to a disk FITS file.    
;
; EXPLANATION:
;       A minimal FITS header is created if not supplied.
;       WRITEFITS works for all types of FITS files except random groups
;
; CALLING SEQUENCE:
;       WRITEFITS, filename, data [, header, /APPEND, /COMPRESS, /CHECKSUM] 
;
; INPUTS:
;       FILENAME = String containing the name of the file to be written.
;
;       DATA = Image array to be written to FITS file.    If DATA is 
;              undefined or a scalar, then only the FITS header (which
;              must have NAXIS = 0) will be written to disk
;
; OPTIONAL INPUT:
;       HEADER = String array containing the header for the FITS file.
;                If variable HEADER is not given, the program will generate
;                a minimal FITS header.
;       HEAP -   A byte array giving the heap area following, e.g. a variable
;                length binary table
;
; OPTIONAL INPUT KEYWORD:
;       /APPEND - If this keyword is set then the supplied header and data
;                array are assumed to be an extension and are appended onto
;                the end of an existing FITS file.    If the file does not 
;                exist, then WRITEFITS will create one with a minimal primary
;                header (and /EXTEND keyword) and then append the supplied
;                extension header and array.     Note that the primary
;                header in an existing file must already have an EXTEND
;                keyword to indicate the presence of an FITS extension.
;       /COMPRESS - If this keyword is set, then the FITS file is written as
;                a gzip compressed file.   An extension '.gz' is appended to
;                to the file name if it does not already exist.   The /COMPRESS
;                option is incompatible with the /APPEND option.
;      /Checksum - If set, then the CHECKSUM keywords to monitor data integrity
;                 will be included in the FITS header.    For more info, see
;                 http://fits.gsfc.nasa.gov/registry/checksum.html
;                 By default, checksum keywords will updated if they are already
;                 in the FITS header.
;       NaNvalue - Value in the data array which represents missing pixels.
;		 This keyword should only used when missing pixels are not
;		 represented by NaN values in the input array.
; OUTPUTS:
;       None
;
; RESTRICTIONS:
;       (1) It recommended that BSCALE and BZERO not be used (or set equal
;           to 1. and 0) except with integer data
;       (2) WRITEFITS will remove any group parameters from the FITS header
;       (3) As of Feb 2008, WRITEFITS no longer requires the primary header of a
;           FITS file with extensions to contain the EXTEND keyword, consistent 
;           with Section 4.4.2.1 of the FITS 3.0 standard.    A warning is still 
;           given.  See http://fits.gsfc.nasa.gov/fits_standard.html
;
; EXAMPLE:
;       Write a randomn 50 x 50 array as a FITS file creating a minimal header.
;
;       IDL> im = randomn(seed, 50, 50)        ;Create array
;       IDL> writefits, 'test', im             ;Write to a FITS file "test"
;
; PROCEDURES USED:
;       CHECK_FITS, FITS_ADD_CHECKSUM, MKHDR, MRD_HREAD, SXDELPAR, SXADDPAR, 
;       SXPAR()
;
; MODIFICATION HISTORY:
;       WRITTEN, Jim Wofford, January, 29 1989
;       Added call to IS_IEEE_BIG()  W. Landsman  Apr 96
;       Make sure SIMPLE is written in first line of header  W. Landsman Jun 97
;       Use SYSTIME() instead of !STIME    W. Landsman  July 97
;       Create a default image extension header if needed W. Landsman June 98
;       Write unsigned data types W. Landsman       December 1999
;       Update for IDL V5.3, add /COMPRESS keyword W. Landsman  February 2000
;       Correct BZERO value for unsigned data  W. Landsman   July 2000
;       Eliminate duplication of input array if possible W. Landsman April 2001
;       Use FILE_SEARCH for V5.5 or later     W. Landsman    April 2002
;       Create the file if not already present and /APPEND is set
;                                             W. Landsman    September 2002
;       Proper call to MRD_HREAD if /APPEND is set  W. Landsman December 2002 
;       Added /CHECKSUM keyword              W. Landsman     December 2002
;	    Restored NANvalue keyword, William Thompson,	     October 2003
;       Write BZERO in beginning of header for unsigned integers WL April 2004
;       Added ability to write heap array       WL             October 2004
;       Correct checksum if writing heap array   WL           November 2004
;       Assume since V5.5, no VMS support, use file_search() WL   September 2006
;       Set nbytes variable to LONG64 for very large files WL  May 2007
;       Update CHECKSUM keywords if already present  WL   Oct 2007
;       EXTEND keyword no longer required in FITS files with extensions WL Feb 2008
;       Bug fix when filename ends with '.gz' and COMPRESS is used,
;            the output file must be compressed          S. Koposov June 2008
;       Introduce V6.0 notation                W.L. Nov. 2010 
;       Set /APPEND if XTENSION specifies a table   W.L.  July 2012
;       Bug fix when /CHECKSUM used with unsigned data  W.L. June 2013
;       June 2013 bug fix introduced problem when NAXIS=0  W.L. July 2013
;       Added /Silent keyword W.L. April 2016
;		Support unsigned 64 bit data type  W.L.  January 2018
;       Fix case of header with no data and checksum  W.L.   August 2018
;-

  compile_opt idl2  

  if N_params() LT 2 then begin 
       print,'Syntax - WRITEFITS, filename, data,[ header, /APPEND, /CHECKSUM]'
       return
  endif
  
  Catch, theError
  IF theError NE 0 then begin
	Catch,/Cancel
	void = cgErrorMsg(/quiet)
	RETURN
  ENDIF

; Get information about data

  siz = size( data )      
  naxis = siz[0]                    ;Number of dimensions
  if naxis GT 0 then nax = siz[ 1:naxis ]              ;Vector of dimensions
  lim = siz[ naxis+2 ]              ;Total number of data points
  type = siz[naxis + 1]             ;Data type

;Create a primary or image extension header if not supplied by the user

        if N_elements(header) LT 2 then begin 
                if keyword_set(append) then mkhdr, header, data, /IMAGE  $
                                       else mkhdr, header, data, /EXTEND
        endif else if naxis GT 0 then $         
              check_FITS, data, header, /UPDATE, Silent= silent

  hdr = header     ;Don't modify supplied header
  
;If header indicates a table extension then set the append keyword  
  if ~keyword_set( APPEND) && ( strmid(hdr[0],0,8) EQ 'XTENSION' ) then begin
	 xten = strtrim(sxpar(hdr,'XTENSION'),2)
	 if (xten EQ 'TABLE') || (xten Eq 'BINTABLE') || (xten Eq 'A3DTABLE') $
	     then begin 
	     append = 1
	     message,'Writing FITS table extension',/INF,NoPrint = silent
	 endif    
   endif	     

  if ~keyword_set( APPEND) then begin 
         simple = 'SIMPLE  =                    T / Written by IDL:  ' $
                        + systime()  
         hdr[0] =  simple + string( replicate(32b,80-strlen(simple) ) )
         sxdelpar, hdr, [ 'GCOUNT', 'GROUPS', 'PCOUNT', 'PSIZE' ]     ;Remove random groups keywords
  endif
  
; If necessary,convert unsigned to signed.    Do not destroy the original data

  unsigned = 0
  if naxis NE 0 then begin
              
        unsigned = (type EQ 12) || (type EQ 13) || (type EQ 15)
        if  unsigned then begin
             if type EQ 12 then begin
                     sxaddpar,hdr,'BZERO',32768,' Data is Unsigned Integer', $
                              before = 'DATE'
                     newdata = fix(data - 32768)
             endif else if type EQ 13 then begin 
                    sxaddpar,hdr,'BZERO',2147483648,' Data is Unsigned Long', $
                              before = 'DATE'
                    newdata = long(data - 2147483648)
             endif else if type EQ 15 then begin
                offset = ulong64(2)^63
             	sxaddpar,hdr,'BZERO',offset,' Data is 64 bit Unsigned Long', $
             			before = 'DATE'
             	newdata = long64(data - offset )
             endif	
        endif


; For floating or double precision test for NaN values to write

  NaNtest = keyword_set(NaNvalue) && ( (type EQ 4) || (type EQ 5) )
  if NaNtest then begin
     NaNpts = where( data EQ NaNvalue, N_NaN)
     if (N_NaN GT 0) then begin
         if type EQ 4 then data[NaNpts]  = !Values.F_NaN	$
     else if type EQ 8 then data[NaNpts] = !Values.D_NaN
     endif
  endif 
  endif

; Open file and write header information

        if keyword_set( APPEND) then begin
            if (strmid( hdr[0],0,8 ) NE 'XTENSION') then begin
                   message, $
            'ERROR - "XTENSION" must be first keyword in header extension',/CON
                  return
            endif
            if ~file_test(filename)  then  begin       ;Create default primary header
                 mkhdr,h0,0b,/exten
                 writefits,filename,0b,h0, checksum = checksum
                 openu, unit, filename, /GET_LUN, /swap_if_little_endian
             endif else begin
            openu, unit, filename, /GET_LUN, /swap_if_little_endian
            mrd_hread, unit, hprimary
            extend = where( strcmp(hprimary,'EXTEND  ',8), Nextend)
            if Nextend EQ 0 then $
               message,'WARNING - EXTEND keyword not found in primary FITS header',/CON,NoPrint=silent
            endelse
                   
            file = fstat(unit)
            nbytes  = file.size
            point_lun, unit, nbytes
            npad = nbytes mod 2880
            if npad NE 0 then writeu, unit, replicate(32b, 2880 - npad)

    endif else begin

        ext = ''
        if keyword_set(COMPRESS) then begin 
            if strlowcase(strmid(filename,2,3,/reverse)) NE '.gz' $
               then ext = '.gz' 
        endif else compress = 0


       openw, unit, filename + ext, /GET_LUN, /swap_if_little_endian, $
                             compress = compress

    endelse

; Determine if an END line occurs, and add one if necessary

       endline = where( strcmp(hdr, 'END     ', 8), Nend)
     if Nend EQ 0 then begin

     message,'WARNING - An END statement has been appended to the FITS header',/INF,NoPrint=silent
     hdr = [ hdr, 'END' + string( replicate(32b,77) ) ]
     endline = N_elements(hdr) - 1 

   endif

; Add any CHECKSUM keywords if desired or already present
   
    do_Checksum = keyword_set(checksum)
    if ~do_checksum then test = sxpar(hdr,'CHECKSUM',count=do_checksum)
     if do_checksum then begin 
               if unsigned then begin 
	       if N_elements(heap) GT 0 then $
	         FITS_ADD_CheckSum, hdr, [newdata,heap] else $
		 FITS_Add_CheckSum, hdr, newdata
	       endif else begin 	 
               if N_elements(heap) GT 0 then $
	         FITS_ADD_CHECKSUM, hdr, [data,heap] else $
                 FITS_ADD_CHECKSUM, hdr, data
	       endelse	 
               endline = where( strcmp(hdr,'END     ',8), Nend)
       endif
       nmax = endline[0] + 1

; Convert to byte and force into 80 character lines

       bhdr = replicate(32b, 80l*nmax)
       for n = 0l, endline[0] do bhdr[80*n] = byte( hdr[n] )
       npad = 80l*nmax mod 2880
       writeu, unit, bhdr
       if npad GT 0 then writeu, unit,  replicate(32b, 2880 - npad)

; Write data
       if naxis EQ 0 then goto, DONE
        bitpix = sxpar( hdr, 'BITPIX' )
        nbytes = long64( N_elements( data)) * (abs(bitpix) / 8 )
        npad = nbytes mod 2880

        if unsigned then writeu, unit, newdata $
                    else writeu, unit, data 

; Write optional heap area
        if N_elements(heap) GT 0 then begin
              theap = sxpar(hdr,'THEAP', Count=N_Theap)
              if N_Theap GT 0 then begin
                  offset = theap - nbytes
                  if offset GT 0 then begin
                      writeu, unit, bytarr(offset)
                      npad = (npad + offset) mod 2880
                  endif
                  writeu, unit, heap
                  npad = (npad + N_elements(heap)) mod 2880
              endif
         endif

; ASCII Tables padded with blanks (32b) otherwise pad with zeros
        if keyword_set( APPEND) then begin
             exten = sxpar( header, 'XTENSION')
             padnum =  exten EQ 'TABLE   ' ? 32b : 0b
        endif else padnum = 0b
         
        if npad GT 0 then writeu, unit, replicate( padnum, 2880 - npad)
DONE:
        free_lun, unit  

  return
  end

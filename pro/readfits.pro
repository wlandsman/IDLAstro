;+
; NAME:
;       READFITS
; PURPOSE:
;       Read a FITS file into IDL data and header variables. 
; EXPLANATION:
;       READFITS() can read FITS files compressed with gzip (.gz), Unix (.Z) 
;       or BZip (.bz2) compression.  FPACK ( http://heasarc.gsfc.nasa.gov/fitsio/fpack/ )
;       compressed FITS files can also be read provided that the FPACK software
;       is installed.
;
;       See http://idlastro.gsfc.nasa.gov/fitsio.html for other ways of
;       reading FITS files with IDL.   
;
; CALLING SEQUENCE:
;       Result = READFITS( Filename/Fileunit,[ Header, heap, /NOSCALE, EXTEN_NO=,
;                     NSLICE=, /SILENT , STARTROW =, NUMROW = , HBUFFER=,
;                     /CHECKSUM, /COMPRESS, /FPACK, /No_Unsigned ]
;
; INPUTS:
;       Filename = Scalar string containing the name of the FITS file  
;                 (including extension) to be read.   If the filename has
;                  a *.gz extension, it will be treated as a gzip compressed
;                  file.   If it has a .Z extension, it will be treated as a
;                  Unix compressed file.     If Filename is an empty string then
;                  the user will be queried for the file name.
;                                   OR
;       Fileunit - A scalar integer specifying the unit of an already opened
;                  FITS file.  The unit will remain open after exiting 
;                  READFITS().  There are two possible reasons for choosing 
;                  to specify a unit number rather than a file name:
;          (1) For a FITS file with many extensions, one can move to the 
;              desired extensions with FXPOSIT() and then use READFITS().  This
;              is more efficient than repeatedly starting at the beginning of 
;              the file.
;          (2) For reading a FITS file across a Web http: address after opening
;              the unit with the SOCKET procedure 
;
; OUTPUTS:
;       Result = FITS data array constructed from designated record.
;                If the specified file was not found, then Result = -1
;
; OPTIONAL OUTPUT:
;       Header = String array containing the header from the FITS file.
;
;       heap = For extensions, the optional heap area following the main
;              data array (e.g. for variable length binary extensions).
;
; OPTIONAL INPUT KEYWORDS:
;       /CHECKSUM - If set, then READFITS() will call FITS_TEST_CHECKSUM to 
;                verify the data integrity if CHECKSUM keywords are present
;                in the FITS header.   Cannot be used with the NSLICE, NUMROW
;                or STARTROW keywords, since verifying the checksum requires 
;               that all the data be read.  See FITS_TEST_CHECKSUM() for more
;               information.
;
;       /COMPRESS - Signal that the file is gzip compressed.  By default, 
;               READFITS will assume that if the file name extension ends in 
;               '.gz' then the file is gzip compressed.   The /COMPRESS keyword
;               is required only if the the gzip compressed file name does not 
;               end in '.gz' or .ftz.   BZip compressed files must have a .bz2
;               extension.
;              
;       EXTEN_NO - non-negative scalar integer specifying the FITS extension to
;               read.  For example, specify EXTEN = 1 or /EXTEN to read the 
;               first FITS extension.   
;
;       /FPACK - Signal that the file is compressed with the FPACK software. 
;               http://heasarc.gsfc.nasa.gov/fitsio/fpack/ ) By default, 
;               (READFITS will assume that if the file name extension ends in 
;               .fz that it is fpack compressed.     The FPACK software must
;               be installed on the system 
;   
;        HBUFFER - Number of lines in the header, set this to slightly larger
;                than the expected number of lines in the FITS header, to 
;               improve performance when reading very large FITS headers. 
;               Should be a multiple of 36 -- otherwise it will be modified
;               to the next higher multiple of 36.   Default is 180
;
;       /NOSCALE - If present and non-zero, then the ouput data will not be
;                scaled using the optional BSCALE and BZERO keywords in the 
;                FITS header.   Default is to scale.
;
;       /NO_UNSIGNED - By default, if the header indicates an unsigned integer 
;               (BITPIX = 16, BZERO=2^15, BSCALE=1) then READFITS() will output 
;               an IDL unsigned integer data type (UINT).   But if /NO_UNSIGNED
;               is set, then the data is converted to type LONG.  
;
;       NSLICE - An integer scalar specifying which N-1 dimensional slice of a 
;                N-dimensional array to read.   For example, if the primary 
;                image of a file 'wfpc.fits' contains a 800 x 800 x 4 array, 
;                then 
;
;                 IDL> im = readfits('wfpc.fits',h, nslice=2)
;                           is equivalent to 
;                 IDL> im = readfits('wfpc.fits',h)
;                 IDL> im = im[*,*,2]
;                 but the use of the NSLICE keyword is much more efficient.
;                 Note that any degenerate dimensions are ignored, so that the
;                 above code would also work with a 800 x 800 x 4 x 1 array.
;
;       NUMROW -  Scalar non-negative integer specifying the number of rows 
;                 of the image or table extension to read.   Useful when one 
;                 does not want to read the entire image or table.  
;
;       POINT_LUN  -  Position (in bytes) in the FITS file at which to start
;                 reading.   Useful if READFITS is called by another procedure
;                 which needs to directly read a FITS extension.    Should 
;                 always be a multiple of 2880, and not be used with EXTEN_NO
;                 keyword.
;
;       /SILENT - Normally, READFITS will display the size the array at the
;                 terminal.  The SILENT keyword will suppress this
;
;        STARTROW - Non-negative integer scalar specifying the row
;               of the image or extension table at which to begin reading. 
;               Useful when one does not want to read the entire table.  
;
;       NaNVALUE - This keyword is included only for backwards compatibility
;                  with routines that require IEEE "not a number" values to be
;                  converted to a regular value.
;
;       /UNIXPIPE - When a FileUnit is supplied to READFITS(), then /UNIXPIPE
;                 indicates that the unit is to a Unix pipe, and that 
;                 no automatic byte swapping is performed.
;
; EXAMPLE:
;       Read a FITS file test.fits into an IDL image array, IM and FITS 
;       header array, H.   Do not scale the data with BSCALE and BZERO.
;
;              IDL> im = READFITS( 'test.fits', h, /NOSCALE)
;
;       If the file contains a FITS extension, it could be read with
;
;              IDL> tab = READFITS( 'test.fits', htab, /EXTEN )
;
;       The function TBGET() can be used for further processing of a binary 
;       table, and FTGET() for an ASCII table.
;       To read only rows 100-149 of the FITS extension,
;
;              IDL> tab = READFITS( 'test.fits', htab, /EXTEN, 
;                                   STARTR=100, NUMR = 50 )
;
;       To read in a file that has been compressed:
;
;              IDL> tab = READFITS('test.fits.gz',h)
;
; ERROR HANDLING:
;       If an error is encountered reading the FITS file, then 
;               (1) the system variable !ERROR_STATE.CODE is set negative 
;                   (via the MESSAGE facility)
;               (2) the error message is displayed (unless /SILENT is set),
;                   and the message is also stored in !ERROR_STATE.MSG
;               (3) READFITS returns with a value of -1
; RESTRICTIONS:
;       (1) Cannot handle random group FITS
;
; NOTES:
;       (1) If data is stored as integer (BITPIX = 16 or 32), and BSCALE
;       and/or BZERO keywords are present, then the output array is scaled to 
;       floating point (unless /NOSCALE is present) using the values of BSCALE
;       and BZERO.   In the header, the values of BSCALE and BZERO are then 
;       reset to 1. and 0., while the original values are written into the 
;       new keywords O_BSCALE and O_BZERO.     If the BLANK keyword was
;       present (giving the value of undefined elements *prior* to the 
;       application of BZERO and BSCALE) then the *keyword* value will be
;       updated with the values of BZERO and BSCALE.
;       
;       (2) The use of the NSLICE keyword is incompatible with the NUMROW
;       or STARTROW keywords.
;
;       (3) On some Unix shells, one may get a "Broken pipe" message if reading
;        a Unix compressed (.Z) file, and not reading to the end of the file 
;       (i.e. the decompression has not gone to completion).     This is an 
;        informative message only, and should not affect the output of READFITS.   
; PROCEDURES USED:
;       Functions:   SXPAR()
;       Procedures:  MRD_SKIP, SXADDPAR, SXDELPAR
;
; MODIFICATION HISTORY:
;       Original Version written in 1988, W.B. Landsman   Raytheon STX
;       Revision History prior to October 1998 removed          
;       Major rewrite to eliminate recursive calls when reading extensions
;                  W.B. Landsman  Raytheon STX                    October 1998
;       Add /binary modifier needed for Windows  W. Landsman    April 1999
;       Read unsigned datatypes, added /no_unsigned   W. Landsman December 1999
;       Output BZERO = 0 for unsigned data types   W. Landsman   January 2000
;       Update to V5.3 (see notes)  W. Landsman                  February 2000
;       Fixed logic error in use of NSLICE keyword  W. Landsman  March 2000
;       Fixed byte swapping for Unix compress files on little endian machines
;                                    W. Landsman    April 2000
;       Added COMPRESS keyword, catch IO errors W. Landsman September 2000
;       Option to read a unit number rather than file name W.L    October 2001
;       Fix undefined variable problem if unit number supplied W.L. August 2002
;       Don't read entire header unless needed   W. Landsman  Jan. 2003
;       Added HBUFFER keyword    W. Landsman   Feb. 2003
;       Added CHECKSUM keyword   W. Landsman   May 2003
;       Restored NaNVALUE keyword for backwards compatibility,
;               William Thompson, 16-Aug-2004, GSFC
;       Recognize .ftz extension as compressed  W. Landsman   September 2004
;       Fix unsigned integer problem introduced Sep 2004 W. Landsman Feb 2005
;       Don't modify header for unsigned integers, preserve double precision
;           BSCALE value  W. Landsman March 2006
;       Use gzip instead of compress for Unix compress files W.Landsman Sep 2006
;       Call MRD_SKIP to skip bytes on different file types W. Landsman Oct 2006
;       Make ndata 64bit for very large files E. Hivon/W. Landsman May 2007
;       Fixed bug introduced March 2006 in applying Bzero C. Magri/W.L. Aug 2007
;       Check possible 32bit overflow when using NSKIP W. Landsman Mar 2008
;       Always reset BSCALE, BZERO even for unsigned integers W. Landsman May 2008
;       Make ndata 64bit for very large extensions J. Schou/W. Landsman Jan 2009
;       Use PRODUCT() to compute # of data points  W. Landsman  May 2009
;       Read FPACK compressed file via UNIX pipe. W. Landsman May 2009
;       Fix error using NUMROW,STARTROW with non-byte data, allow these 
;           keywords to be used with primary array  W. Landsman July 2009
;       Ignore degenerate trailing dimensions with NSLICE keyword W.L. Oct 2009
;       Add DIALOG_PICKFILE() if filename is an empty string  W.L. Apr 2010
;       Set BLANK values *before* applying BSCALE,BZERO, use short-circuit
;           operators  W.L. May 2010
;      Skip extra SPAWN with FPACK decompress J. Eastman, W.L. July 2010
;      Fix possible problem when startrow=0 supplied J. Eastman/W.L. Aug 2010
;      First header is not necessarily primary if unit supplied WL Jan 2011
;      Fix test for 'SIMPLE' at beginning of header WL November 2012
;      Fix problem passing extensions with > 2GB WL, M. Carlson August 2013
;      Always read entire header, even if header variable not supplied W. Landsman May 2017
;      Support BZip compression   W. Landsman Aug 2017
;	Support unsigned long64 data W. Landsman Jan 2018 
;-
function READFITS, filename, header, heap, CHECKSUM=checksum, $ 
                   COMPRESS = compress, HBUFFER=hbuf, EXTEN_NO = exten_no, $
                   NOSCALE = noscale, NSLICE = nslice, $
                   NO_UNSIGNED = no_unsigned,  NUMROW = numrow, $
                   POINTLUN = pointlun, SILENT = silent, STARTROW = startrow, $
                   NaNvalue = NaNvalue, FPACK = fpack, UNIXpipe=unixpipe

  compile_opt idl2
  On_IOerror, BAD

; Check for filename input

   if N_params() LT 1 then begin                
      print,'Syntax - im = READFITS( filename, [ h, heap, /NOSCALE, /SILENT,'
      print,'                 EXTEN_NO =, STARTROW = , NUMROW=, NSLICE = ,'
      print,'                 HBUFFER = ,/NO_UNSIGNED, /CHECKSUM, /COMPRESS]'
      return, -1
   endif
   
Catch, theError
if theError NE 0 then begin
	Catch,/Cancel
	void = cgErrorMsg(/quiet)
	return, -1
endif

unitsupplied = size(filename,/TNAME) NE 'STRING'

; Set default keyword values

   silent = keyword_set( SILENT )
   do_checksum = keyword_set( CHECKSUM )
   if N_elements(exten_no) EQ 0 then exten_no = 0

;  Check if this is a Unix compressed file.   (gzip files are handled 
;  separately using the /compress keyword to OPENR).

    if N_elements(unixpipe) EQ 0 then unixpipe = 0                  
    if unitsupplied then unit = filename else begin
    len = strlen(filename)
    if len EQ 0 then begin
        filename =dialog_pickfile(filter=['*.fit*;*.fts*;*.img*'], $
	title='Please select a FITS file',/must_exist)
        len = strlen(filename)
    endif 
    ext = strlowcase(strmid(filename,len-3,3))
    gzip = (ext EQ '.gz') || (ext EQ 'ftz')
    compress = keyword_set(compress) || gzip[0]
    unixZ =  (strmid(filename, len-2, 2) EQ '.Z') 
    bzip = ext EQ 'bz2'
    fcompress = keyword_set(fpack) || ( ext EQ '.fz') 
    unixpipe = unixZ || fcompress || bzip      

 
;  Go to the start of the file.

   openr, unit, filename, ERROR=error,/get_lun, $
                COMPRESS = compress, /swap_if_little_endian
   if error NE 0 then begin
        message,/con,' ERROR - Unable to locate file ' + filename
        return, -1
   endif

;  Handle Unix or Fpack compressed files which will be opened via a pipe using
;  the SPAWN command.     

        if unixZ || bzip then begin
                free_lun, unit
                if bzip then cmd = 'bunzip2' else cmd = 'gzip'
                spawn, cmd + ' -cd '+filename, unit=unit                 

        endif else if fcompress then begin 
	        free_lun, unit
		spawn,'funpack -S ' + filename, unit=unit,/sh
                if eof(unit) then begin 
		    message,'Error spawning FPACK decompression',/CON
		    free_lun,unit
		    return,-1
		endif    
	endif	
  endelse
  if N_elements(POINTLUN) GT 0 then mrd_skip, unit, pointlun


  if N_elements(hbuf) EQ 0 then hbuf = 180 else begin
                  remain = hbuf mod 36
                  if remain GT 0 then hbuf = hbuf + 36-remain
  endelse

  for ext = 0L, exten_no do begin
               
;  Read the next header, and get the number of bytes taken up by the data.

       block = string(replicate(32b,80,36))
       w = [-1]
       if (ext EQ exten_no)  then header = strarr(hbuf) $
                             else header = strarr(36)
       headerblock = 0L
       i = 0L      

       while w[0] EQ -1 do begin
          
       if EOF(unit) then begin 
            message,/ CON, $
               'EOF encountered attempting to read extension ' + strtrim(ext,2)
            if ~unitsupplied then free_lun,unit
            return,-1
       endif

      readu, unit, block
      headerblock++
      w = where(strlen(block) NE 80, Nbad)
      if (Nbad GT 0) then begin
           message,'Warning-Invalid characters in header',/INF,NoPrint=Silent
           block[w] = string(replicate(32b, 80))
      endif

      w = where(strcmp(block,'END     ',8), Nend)
      if (headerblock EQ 1) || (ext EQ exten_no) then begin
              if Nend GT 0 then  begin
             if headerblock EQ 1 then header = block[0:w[0]]   $
                                 else header = [header[0:i-1],block[0:w[0]]]
             endif else begin
                header[i] = block
                i += 36
                if i mod hbuf EQ 0 then $
                              header = [header,strarr(hbuf)]
           endelse
          endif

      if (ext EQ 0 ) && ~((N_elements(pointlun) GT 0) || unitsupplied ) then $
             if strmid( header[0], 0, 8)  NE 'SIMPLE  ' then begin
              message,/CON, $
                 'ERROR - Header does not contain required SIMPLE keyword'
                if ~unitsupplied then free_lun, unit
                return, -1
      endif

      endwhile          
; Get parameters that determine size of data region.
                
       bitpix =  sxpar(header,'BITPIX')
       byte_elem = abs(bitpix)/8               ;Bytes per element
       naxis  = sxpar(header,'NAXIS')
       gcount = sxpar(header,'GCOUNT') > 1
       pcount = sxpar(header,'PCOUNT')
                
       if naxis GT 0 then begin 
            dims = sxpar( header,'NAXIS*')           ;Read dimensions
	    ndata = product(dims[0:naxis-1],/integer)    ;Update 7-31-2017
       endif else ndata = 0
                
       nbytes = byte_elem * gcount * (pcount + ndata)

;  Move to the next extension header in the file.   Use MRD_SKIP to skip with
;  fastest available method (POINT_LUN or readu) for different file
;  types (regular, compressed, Unix pipe, socket) 

      if ext LT exten_no then begin
                nrec = long64((nbytes + 2879) / 2880)
                if nrec GT 0 then mrd_skip, unit, nrec*2880L    
       endif
       endfor

 case BITPIX of 
           8:   IDL_type = 1          ; Byte
          16:   IDL_type = 2          ; 16 bit integer
          32:   IDL_type = 3          ; 32 bit integer
          64:   IDL_type = 14         ; 64 bit integer
         -32:   IDL_type = 4          ; Float
         -64:   IDL_type = 5          ; Double
        else:   begin
                message,/CON, 'ERROR - Illegal value of BITPIX (= ' +  $
                strtrim(bitpix,2) + ') in FITS header'
                if ~unitsupplied then free_lun,unit
                return, -1
                end
  endcase     
 
  if nbytes EQ 0 then begin
        if ~SILENT then message, $
                "FITS header has NAXIS or NAXISi = 0,  no data array read",/CON
        if do_checksum then begin
             result = FITS_TEST_CHECKSUM(header, data, ERRMSG = errmsg)
             if ~SILENT then begin
               case result of 
                1: message,/INF,'CHECKSUM keyword in header is verified'
               -1: message,/CON, errmsg
                else: 
                endcase
              endif
        endif
        if ~unitsupplied then free_lun, unit
        return,-1
 endif

; Check for FITS extensions, GROUPS

 groups = sxpar( header, 'GROUPS' ) 
 if groups then message,NoPrint=Silent, $
           'WARNING - FITS file contains random GROUPS', /INF

; If an extension, did user specify row to start reading, or number of rows
; to read?

   if N_elements(STARTROW) EQ 0 then startrow = 0       ;updated Aug 2010
   if naxis GE 2 then nrow = dims[1] else nrow = ndata
   if N_elements(NUMROW) EQ 0 then numrow = nrow
   if do_checksum then if ((startrow GT 0) || $
      (numrow LT nrow) || (N_elements(nslice) GT 0)) then begin 
      message,/CON, $
      'Warning - CHECKSUM not applied when STARTROW, NUMROW or NSLICE is set'
      do_checksum = 0
   endif 

   if exten_no GT 0 then begin
        xtension = strtrim( sxpar( header, 'XTENSION' , Count = N_ext),2)
        if N_ext EQ 0 then message, /INF, NoPRINT = Silent, $
                'WARNING - Header missing XTENSION keyword'
   endif 

   if ((startrow NE 0) || (numrow NE nrow)) then begin
        if startrow GE dims[1] then begin
           message,'ERROR - Specified starting row ' + strtrim(startrow,2) + $
          ' but only ' + strtrim(dims[1],2) + ' rows in extension',/CON
           if ~unitsupplied then free_lun,unit
           return,-1
        endif 
        dims[1] = dims[1] - startrow    
        dims[1] = dims[1] < numrow
        sxaddpar, header, 'NAXIS2', dims[1]
	if startrow GT 0 then mrd_skip, unit, byte_elem*startrow*dims[0]

    endif else if (N_elements(NSLICE) EQ 1) then begin
 
        ldim = naxis-1
        lastdim = dims[ldim]
	while lastdim EQ 1 do begin
	      ldim = ldim-1
	      lastdim = dims[ldim]
	endwhile
	       if nslice GE lastdim then begin 
	      message,/CON, $
        'ERROR - Value of NSLICE must be less than ' + strtrim(lastdim,2)
               if ~unitsupplied then free_lun, unit
 	      return, -1
	endif      
        dims = dims[0:ldim-1]
        for i = ldim,naxis-1 do sxdelpar,header,'NAXIS' + strtrim(i+1,2)
        naxis = ldim
        sxaddpar,header,'NAXIS' + strtrim(ldim,2),1
        ndata = ndata/lastdim
        nskip = long64(nslice)*ndata*byte_elem
	if Ndata GT 0 then mrd_skip, unit, nskip  
  endif


  if ~SILENT then begin   ;Print size of array being read

         if exten_no GT 0 then message, $
                     'Reading FITS extension of type ' + xtension, /INF  
	 if N_elements(dims) EQ 1 then $
	 st = 'Now reading ' + strtrim(dims,2) + ' element vector' else $	          
	 st = 'Now reading ' + strjoin(strtrim(dims,2),' by ') + ' array'
         if (exten_no GT 0) && (pcount GT 0) then st = st + ' + heap area'
         message,/INF,st   
   endif

; Read Data in a single I/O call.   Only need byteswapping for data read with
; bidirectional pipe.

    data = make_array( DIM = dims, TYPE = IDL_type, /NOZERO)
    readu, unit, data
    if unixpipe  then swap_endian_inplace,data,/swap_if_little
    if (exten_no GT 0) && (pcount GT 0) then begin
        theap = sxpar(header,'THEAP')
        skip = theap - N_elements(data)
        if skip GT 0 then begin 
                temp = bytarr(skip,/nozero)
                readu, unit, skip
        endif
        heap = bytarr(pcount*gcount*byte_elem)
        readu, unit, heap
        if do_checksum then $
        result = fits_test_checksum(header,[data,heap],ERRMSG=errmsg)
    endif else if do_checksum then $
        result = fits_test_checksum(header, data, ERRMSG = errmsg)
    if ~unitsupplied then free_lun, unit
    if do_checksum then if ~SILENT then begin
        case result of 
        1: message,/INF,'CHECKSUM keyword in header is verified'
       -1: message,/CON, 'CHECKSUM ERROR! ' + errmsg
        else: 
        endcase
    endif

; Scale data unless it is an extension, or /NOSCALE is set
; Use "TEMPORARY" function to speed processing.  

   do_scale = ~keyword_set( NOSCALE )
   if (do_scale && (exten_no GT 0)) then do_scale = xtension EQ 'IMAGE' 
   if do_scale then begin

          if bitpix GT 0 then $
                blank = sxpar( header, 'BLANK', Count = N_blank) $
		else N_blank = 0
 
          Bscale = sxpar( header, 'BSCALE' , Count = N_bscale)
          Bzero = sxpar(header, 'BZERO', Count = N_Bzero )
         if (N_blank GT 0) && ((N_bscale GT 0) || (N_Bzero GT 0)) then $
                 sxaddpar,header,'O_BLANK',blank,' Original BLANK value'
       
 
 
; Check for unsigned integer (BZERO = 2^15) or unsigned long (BZERO = 2^31) or
; unsigned long 64 bit (BZERO = 2^63)

          if ~keyword_set(No_Unsigned) then begin

            no_bscale = (Bscale EQ 1) || (N_bscale EQ 0)
            unsgn_int = (bitpix EQ 16) && (Bzero EQ 32768) && no_bscale
            unsgn_lng = (bitpix EQ 32) && (Bzero EQ 2147483648) && no_bscale
            unsgn_lng64 = (bitpix EQ 64) && (Bzero EQ 9223372036854775808) && no_bscale
            
            unsgn = unsgn_int || unsgn_lng || unsgn_lng64
           endif else unsgn = 0

          if unsgn then begin
                if unsgn_int then begin  
                        data =  uint(data) - 32768US
			            if N_blank then blank = uint(blank) - 32768US 
		   		endif else if unsgn_lng then begin 
                         data = ulong(data) - 2147483648UL
			             if N_blank then blank = ulong(blank) - 2147483648UL
		   		endif else begin
		                offset = ulong64(9223372036854775808)
		   				data = ulong64(data) - offset
		   				if N_blank then blank = ulong64(blank) - offset
		   endelse 
		   if N_blank then sxaddpar,header,'BLANK',blank
                   sxaddpar, header, 'BZERO', 0
                   sxaddpar, header, 'O_BZERO', Bzero,' Original BZERO Value'
               
          endif else begin
 
          if N_Bscale GT 0  then $ 
               if ( Bscale NE 1. ) then begin
	           if size(Bscale,/TNAME) NE 'DOUBLE' then $
                      data *= float(Bscale) else $ 
		      data *= Bscale 
		  if N_blank then blank *= bscale    
                  sxaddpar, header, 'BSCALE', 1.
                   sxaddpar, header, 'O_BSCALE', Bscale,' Original BSCALE Value'
		   
               endif

         if N_Bzero GT 0  then $
               if (Bzero NE 0) then begin
	             if size(Bzero,/TNAME) NE 'DOUBLE' then $
                      data += float(Bzero) else $    ;Fixed Aug 07
                      data +=  Bzero
		      if N_blank then blank += bzero
                     sxaddpar, header, 'BZERO', 0.
                     sxaddpar, header, 'O_BZERO', Bzero,' Original BZERO Value'
               endif
        
        endelse
	if  N_blank then sxaddpar,header,'BLANK',blank
        endif


; Return array.  If necessary, first convert NaN values.

        if n_elements(nanvalue) eq 1 then begin
            w = where(finite(data,/nan),count)
            if count gt 0 then data[w] = nanvalue
        endif
        return, data    

; Come here if there was an IO_ERROR
    
 BAD:   print,!ERROR_STATE.MSG
        if (~unitsupplied) && (N_elements(unit) GT 0) then free_lun, unit
        if N_elements(data) GT 0 then return,data else return, -1

 end 

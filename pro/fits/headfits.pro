function HEADFITS, filename, EXTEN = exten, Compress = compress, $ 
                   ERRMSG = errmsg, SILENT = silent
;+
; NAME:
;       HEADFITS
; PURPOSE:
;       Read a FITS (primary or extension) header into a string array.
; EXPLANATION:
;       HEADFITS() supports several types of compressed files including 
;       gzip (.gz), Unix compressed (.Z), Bzip2 (.bz2) or FPACK (.fz 
;       http://heasarc.gsfc.nasa.gov/fitsio/fpack/ )
;
; CALLING SEQUENCE:
;       Result = HEADFITS(Filename/Fileunit ,[ ERRMSG =, EXTEN= , COMPRESS=, 
;                                            /SILENT ])
;
; INPUTS:
;       Filename = String containing the name of the FITS file to be read.
;                If set to an empty string, then user will be prompted for name.
;                File names ending in '.gz' are assumed to be gzip'ed compressed
;                and under Unix file names ending in '.Z' are assumed to be
;                Unix compressed, and file names ending in .bz2 are assumed to
;                be bzip2 compressed.    If this default behaviour is not 
;                sufficient then use the COMPRESS keyword.
;                            or
;       Fileunit - A scalar integer specifying the unit of an already opened
;                  FITS file.  The unit will remain open after exiting 
;                  HEADFITS().  There are two possible reasons for choosing 
;                  to specify a unit number rather than a file name:
;          (1) For a FITS file with many extensions, one can move to the 
;              desired extensions with FXPOSIT() and then use HEADFITS().  This
;              is more efficient that repeatedly starting at the beginning of 
;              the file.
;          (2) For reading a FITS file across a Web http: address after opening
;              the unit with the SOCKET procedure.
; OPTIONAL INPUT KEYWORDS:
;      EXTEN  = Either an integer scalar, specifying which FITS extension to 
;               read, or a scalar string specifying the extension name (stored
;               in the EXTNAME keyword).   For example, to read the header of 
;               the first extension set EXTEN = 1.   Default is to read the 
;               primary FITS header  (EXTEN = 0).    The EXTEN keyword cannot 
;               be used when a unit number is supplied instead of a file name.
;     COMPRESS - If this keyword is set and non-zero, then treat the file
;              as compressed.  If 1 assume a gzipped file.   Use IDL's
;              internal decompression facilities for gzip files, while for 
;              Unix or bzip2 compression spawn off a process to decompress and 
;              use its output as the FITS stream.  If the keyword is not 1, 
;              then use its value as a string giving the command needed for 
;              decompression.   See FXPOSIT for more info.
;     /SILENT - If set, then suppress any warning messages about invalid 
;              characters in the FITS file.
; OPTIONAL KEYWORD OUTPUT:
;       ERRMSG	= If this keyword is present, then any error messages will be
;                 returned to the user in this parameter rather than
;                 depending on the MESSAGE routine in IDL.  If no errors are
;                 encountered, then a null string is returned.  
;
; OUTPUTS:
;       Result of function = FITS header, string array
;
; EXAMPLE:
;       Print the main FITS header of a file 'test.fits' into a string 
;       variable, h
;
;       IDL>  print, headfits( 'test.fits')
;
;       Print the second extension header of a gzip compressed FITS file
;       'test.fits.gz'.  Use HPRINT for pretty format
;
;       IDL> hprint, headfits( 'test.fits.gz', ext=2)
;
;       Read the extension named CALSPEC 
;
;       IDL> hprint,headfits('test.fits.gz',ext='CALSPEC')
;
; PROCEDURES CALLED
;       FXPOSIT(), MRD_HREAD
;       The version of fxposit.pro must be post- May 2009.
; MODIFICATION HISTORY:
;       adapted by Frank Varosi from READFITS by Jim Wofford, January, 24 1989
;       Keyword EXTEN added, K.Venkatakrishna, May 1992
;       Make sure first 8 characters are 'SIMPLE'  W. Landsman October 1993
;       Check PCOUNT and GCOUNT   W. Landsman    December 1994
;       Major rewrite, work for Unix gzip files,   W. Landsman  April 1996
;       Added COMPRESS keyword  W. Landsman   April 2000
;       Added ERRMSG keyword    W. Landsman   July 2000
;       Added /SILENT keyword   W. Landsman    December 2000
;       Option to read a unit number rather than file name W.L    October 2001
;       Test output status of MRD_HREAD call October 2003 W. Landsman
;       Allow extension to be specified by name Dec 2006 W. Landsman
;       No need to uncompress FPACK compressed files  May 2009 W. Landsman
;       Use V6.0 notation   W.L.   Feb. 2011  
;-
 On_error,2
 compile_opt idl2

 if N_params() LT 1 then begin
     print,'Syntax - header = headfits( filename,[ EXTEN=, ERRMSG=, ' + $
                   '/SILENT, COMPRESS= ])'
     return, -1
 endif

  printerr = ~arg_present(errmsg) 
  errmsg = ''
  if ~keyword_set(exten) then exten = 0

  unitsupplied = size(filename,/TNAME) NE 'STRING'
  if unitsupplied then unit = filename else begin 
     unit = FXPOSIT( filename, exten, errmsg = errmsg, $
                   /READONLY,compress = compress, SILENT=silent,/headeronly)
     if unit EQ -1 then begin 
          if printerr then  $
	         message,'ERROR - ' + errmsg,/CON 
       return,-1
     endif
     if eof(unit) then begin
        free_lun,unit
        message = 'Extension past EOF'
        if ~printerr then errmsg = message else $
               message,'ERROR - ' + message,/CON 
        return,-1
     endif
  endelse
  
  MRD_HREAD, unit, header, status, SILENT = silent
  if ~unitsupplied then free_lun, unit
  if status LT 0 then begin
         if N_elements(errmsg) GT 0 then errmsg = !ERROR_STATE.MSG else $
          message,'ERROR - ' + !ERROR_STATE.MSG,/CON 
          return, -1
  endif else return, header	  
  end

        FUNCTION FXPOSIT, XFILE, EXT_NO, readonly=readonly, COMPRESS=COMPRESS, $
                 SILENT = Silent, EXTNUM = extnum, ERRMSG= ERRMSG, $
		 LUNIT = lunit, UNIXPIPE= unixpipe, FPACK= fpack, $
		 NO_FPACK = no_fpack,HEADERONLY=headeronly
;+
; NAME:
;     FXPOSIT
; PURPOSE:
;     Return the unit number of a FITS file positioned at specified extension
; EXPLANATION:
;     The FITS file will be ready to be read at the beginning of the 
;     specified extension.    Either an extension number or extension name
;     can be specified.   Called by headfits.pro, mrdfits.pro
;
;     Modified in March 2009 to set the /SWAP_IF_LITTLE_ENDIAN keyword
;     when opening a file, and **may not be compatible with earlier versions**
; CALLING SEQUENCE:
;     unit=FXPOSIT(FILE, EXT_NO_OR_NAME, /READONLY, COMPRESS=program, 
;                       UNIXPIPE=, ERRMSG= , EXTNUM= , UNIT=, /SILENT
;                        /FPACK, /NO_FPACK
;
; INPUT PARAMETERS:
;     FILE    = FITS file name, scalar string.    If an empty string is supplied
;              then the user will be prompted for the file name.   The user
;              will also be prompted if a wild card is supplied, and more than 
;              one file matches the wildcard.
;     EXT_NO_OR_NAME  = Either the extension to be moved to (scalar 
;               nonnegative integer) or the name of the extension to read 
;               (scalar string)
;
; RETURNS:
;     Unit number of file or -1 if an error is detected.
;
; OPTIONAL INPUT KEYWORD PARAMETER:
;     COMPRESS - If this keyword is set and non-zero, then then treat
;                the file as compressed.  If 1 assume a gzipped file.
;                and use IDLs internal decompression facility.    For Unix 
;                compressed or bzip2 compressed files spawn off a process to 
;                decompress and use its output as the FITS stream.  If the 
;                keyword is not 1, then use its value as a string giving the 
;                command needed for decompression.
;     /FPACK - Signal that the file is compressed with the FPACK software. 
;               http://heasarc.gsfc.nasa.gov/fitsio/fpack/ ) By default, 
;               (FXPOSIT will assume that if the file name extension ends in 
;              .fz that it is fpack compressed.)     The FPACK software must
;               be installed on the system 
;     /NO_FPACK - The unit will only be used to read the FITS header.  In
;                 that case FPACK compressed files need not be uncompressed.
;      LUNIT -    Integer giving the file unit number.    Use this keyword if
;                you want to override the default use of GET_LUN to obtain
;                a unit number.
;     /READONLY - If this keyword is set and non-zero, then OPENR rather 
;                than OPENU will be used to open the FITS file.    Note that
;                 compressed files are always set to /READONLY
;     /SILENT    If set, then suppress any messages about invalid characters
;                in the FITS file.
;
; OPTIONAL OUTPUT KEYWORDS:
;       EXTNUM - Nonnegative integer give the extension number actually read
;               Useful only if the extension was specified by name.
;       ERRMSG  = If this keyword is present, then any error messages will be
;                 returned to the user in this parameter rather than
;                 depending on the MESSAGE routine in IDL.  If no errors are
;                 encountered, then a null string is returned.
;       UNIXPIPE - If set to 1, then the FITS file was opened with a UNIX pipe
;                rather than with the OPENR command.    This is only required 
;                 when reading a FPACK, bzip or Unix compressed file.   Note 
;                 that automatic byteswapping cannnot be set for a Unix pipe, 
;                 since the SWAP_IF_LITTLE_ENDIAN keyword is only available for the
;                 OPEN command, and it is the responsibility of the calling 
;                 routine to perform the byteswapping.
; SIDE EFFECTS:
;      Opens and returns a file unit.
; PROCEDURE:
;      Open the appropriate file, or spawn a command and intercept
;      the output.
;      Call FXMOVE to get to the appropriate extension.
; PROCEDURE CALLS:
;      FXMOVE()
; MODIFICATION HISTORY:
;      Derived from William Thompson's FXFINDEND routine.
;      Modified by T.McGlynn, 5-October-1994.
;       Modified by T.McGlynn, 25-Feb-1995 to handle compressed
;          files.  Pipes cannot be accessed using FXHREAD so
;          MRD_HREAD was written.
;       W. Landsman 23-Apr-1997    Force the /bin/sh shell when uncompressing 
;       T. McGlynn  03-June-1999   Use /noshell option to get rid of processes left by spawn.
;                                  Use findfile to retain ability to use wildcards
;       W. Landsman 03-Aug-1999    Use EXPAND_TILDE under Unix to find file
;       T. McGlynn  04-Apr-2000    Put reading code into FXMOVE,
;                                  additional support for compression from D.Palmer.
;       W. Landsman/D.Zarro 04-Jul-2000    Added test for !VERSION.OS EQ 'Win32' (WinNT)
;       W. Landsman    12-Dec-2000 Added /SILENT keyword
;       W. Landsman April 2002     Use FILE_SEARCH for V5.5 or later
;       W. Landsman Feb 2004       Assume since V5.3 (OPENR,/COMPRESS available)
;       W. Landsman,W. Thompson, 2-Mar-2004, Add support for BZIP2 
;       W. Landsman                Don't leave open file if an error occurs
;       W. Landsman  Sep 2004      Treat FTZ extension as gzip compressed
;       W. Landsman  Feb 2006      Removed leading spaces (prior to V5.5)
;       W. Landsman  Nov 2006      Allow specification of extension name
;                                  Added EXTNUM, ERRMSG keywords
;       W. Landsman/N.Piskunov Dec 2007  Added LUNIT keyword
;       W. Landsman     Mar 2009   OPEN with /SWAP_IF_LITTLE_ENDIAN
;                                  Added UNIXPIPE output keyword
;       N. Rich        May 2009    Check if filename is an empty string
;       W. Landsman   May 2009     Support FPACK compressed files
;                                  Added /FPACK, /HEADERONLY keywords
;       W.Landsman    July 2009    Deprecated /HEADERONLY add /NO_FPACK
;       W.Landsman    July 2011    Check for SIMPLE in first 8 chars 
;               Use gunzip to decompress Unix. Z file since compress utility 
;               often not installed anymore)
;       W. Landsman   October 2012 Add .fz extension if /FPACK set
;       W. Landsman   July 2013    More diagnostics if file not found
;-
;
        On_Error,2
        compile_opt idl2  
;
;  Check the number of parameters.
;
        IF N_Params() LT 2 THEN BEGIN 
            PRINT,'SYNTAX:  UNIT = FXPOSIT(FILE, EXT_NO, /Readonly,' + $
	                   'ERRMSG= , /SILENT, compress=prog, LUNIT = lunit)'
            RETURN,-1
        ENDIF
        PRINTERR = ~ARG_PRESENT(ERRMSG)
	ERRMSG = ''
	UNIXPIPE=0
; The /headeronly keyword has been replaced with /no_fpack	
        if ~keyword_set(no_fpack) then no_fpack = keyword_set(headeronly)
	exten = ext_no

   	COUNT=0
	IF XFILE[0] NE '' THEN BEGIN 
             FILE = FILE_SEARCH(XFILE, COUNT=COUNT)  
	     IF COUNT GT 1 THEN $
	          FILE = DIALOG_PICKFILE(FILTER=XFILE, /MUST_EXIST, $
		         TITLE = 'Please select a FITS file') $	 		 
	    ELSE IF COUNT EQ 0 THEN BEGIN 
	        ERRMSG = 'Specified FITS file not found: ' + XFILE[0]
	        IF PRINTERR THEN MESSAGE,ERRMSG,/CON 
                RETURN, -1   ; Don't print anything out, just report an error
	     ENDIF 
	ENDIF ELSE $
             FILE =DIALOG_PICKFILE(FILTER=['*.fit*;*.fts*;*.img*;*.FIT*'], $
	           TITLE='Please select a FITS file',/MUST_EXIST)

            IF FILE[0] EQ '' THEN BEGIN
	      ERRMSG = 'No FITS file specified '   
	    IF PRINTERR THEN MESSAGE,ERRMSG,/CON 
            RETURN, -1   ; Don't print anything out, just report an error
	ENDIF    
                   
        FILE = FILE[0]
	IF KEYWORD_SET(FPACK) then $
	    if strlowcase(strmid(FILE,2,3,/reverse)) NE '.fz' then $
	    FILE += '.fz'
	    
;
;  Check if logical unit number is specified explicitly.
;
        IF KEYWORD_SET(LUNIT) THEN BEGIN 
	   UNIT=LUNIT 
	   GLUN = 0
	ENDIF ELSE BEGIN 
	    UNIT = -1
            GLUN = 1
       ENDELSE
; 
;  Check if this is a compressed file.
;
        UCMPRS = ' '
	IF KEYWORD_SET(compress) THEN BEGIN
	    IF strcompress(string(compress),/remo) eq '1' THEN BEGIN
	        compress = 'gunzip'
	    ENDIF
	    UCMPRS = compress;
	ENDIF ELSE IF KEYWORD_SET(FPACK) THEN $
	    UCMPRS = 'funpack'      $
	ELSE BEGIN
        
            LEN = STRLEN(FILE)
            IF LEN GT 3 THEN $
	        tail = STRLOWCASE(STRMID(file, len-3, 3))  $
	    ELSE tail = ' '
	    
            IF STRMID(tail,1,2) EQ '.z'  THEN $
                UCMPRS = 'gunzip'   $
	    ELSE IF (tail EQ '.gz') || (tail EQ 'ftz') THEN $
	        UCMPRS = 'gzip'       $
	    ELSE IF tail EQ 'bz2' THEN $
	        UCMPRS = 'bunzip2'     $
	    ELSE IF ~KEYWORD_SET(NO_FPACK) THEN $
	          IF tail EQ '.fz' THEN UCMPRS = 'funpack'	
	    
	ENDELSE

;  Handle compressed files which are always opened for Read only.

	IF UCMPRS EQ 'gzip' THEN BEGIN
	        
                OPENR, UNIT, FILE, /COMPRESS, GET_LUN=glun, ERROR = ERROR, $
		           /SWAP_IF_LITTLE       
                IF ERROR NE 0 THEN BEGIN
                        IF PRINTERR THEN PRINT,!ERROR_STATE.MSG ELSE $
			    ERRMSG = !ERROR_STATE.MSG 
                        RETURN,-1
                ENDIF
 
	ENDIF ELSE IF UCMPRS NE ' ' THEN BEGIN
; Handle FPACK compressed file.        If an extension name is supplied then
; first recursively call FXPOSIT to get the extension number.    Then open 
; the bidirectional pipe. 	
		        if UCMPRS EQ 'funpack' then begin
			if size(exten,/TNAME) EQ 'STRING' THEN BEGIN
			unit = fxposit( file, ext_no, /no_fpack,extnum=extnum)
			free_lun,unit
			exten = extnum
			endif 
			SPAWN, [UCMPRS,'-S',FILE], UNIT=UNIT, /NOSHELL 
			ENDIF else $
                        SPAWN, [UCMPRS,'-c',FILE], UNIT=UNIT, /NOSHELL
			UNIXPIPE = 1
                  
        ENDIF ELSE BEGIN
;
;  Go to the start of the file.
;
                IF KEYWORD_SET(READONLY) THEN $
                    OPENR, UNIT, FILE, GET_LUN=glun, ERROR = ERROR, $
                           /SWAP_IF_LITTLE ELSE                     $
                    OPENU, UNIT, FILE, GET_LUN=glun, ERROR = ERROR, $
                           /SWAP_IF_LITTLE 

                IF ERROR NE 0 THEN BEGIN
                        IF PRINTERR THEN PRINT,!ERROR_STATE.MSG ELSE $
			    ERRMSG = !ERROR_STATE.MSG 
                        RETURN,-1
                ENDIF
        ENDELSE
	
        IF SIZE(EXT_NO,/TNAME) NE 'STRING' THEN $
	      IF EXT_NO LE 0 THEN RETURN, UNIT

;For Uncompresed files test that the first 8 characters are 'SIMPLE'

        IF ucmprs EQ ' ' THEN BEGIN
          simple = BytArr(6)
	  READU,unit,simple
          if string(simple) NE 'SIMPLE' then begin 
                IF ~KEYWORD_SET(LUNIT) THEN Free_Lun, unit
	        ERRMSG = "ERROR - FITS File must begin with 'SIMPLE'" 
		if printerr THEN MESSAGE,errmsg,/CON
		return,-1
           endif 	
	point_lun,unit,0    	
	endif
	
	stat = FXMOVE(unit, exten, SILENT = Silent, EXT_NO = extnum, $
	ERRMSG=errmsg)

	IF stat LT 0 THEN BEGIN
            IF ~KEYWORD_SET(LUNIT) THEN Free_Lun, unit
	    IF PrintErr THEN MESSAGE,ErrMsg
	    RETURN, stat
	ENDIF ELSE RETURN, unit
END

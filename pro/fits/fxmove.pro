FUNCTION FXMOVE, UNIT, EXTEN, SILENT = Silent, EXT_NO = ext_no, ERRMSG=errmsg

;+
; NAME:
;     FXMOVE
; PURPOSE:
;     Skip to a specified extension number or name in a FITS file
;
; CALLING SEQUENCE:
;     STATUS=FXMOVE(UNIT, EXT, /Silent)
;     STATUS=FXMOVE(UNIT, EXTNAME, /Silent, EXT_NO=, ERRMSG= )
;
; INPUT PARAMETERS:
;     UNIT     = An open unit descriptor for a FITS data stream.
;     EXTEN   = Number of extensions to skip.
;                              or
;             Scalar string giving extension name (in the EXTNAME keyword)           
; OPTIONAL INPUT PARAMETER:
;     /SILENT - If set, then any messages about invalid characters in the 
;               FITS file are suppressed.
; OPTIONAL OUTPUT PARAMETER:
;       ERRMSG  = If this keyword is present, then any error messages will be
;                 returned to the user in this parameter rather than
;                 depending on the MESSAGE routine in IDL.  If no errors are
;                 encountered, then a null string is returned.
;       EXT_NO - Extension number, scalar integer, useful if the user supplied 
;                an extension name in the EXTEN parameter
; RETURNS:
;     0 if successful.
;    -1 if an error is encountered.
;
; COMMON BLOCKS:
;      None.
; SIDE EFFECTS:
;      Repositions the file pointer.
; PROCEDURE:
;      Each FITS header is read in and parsed, and the file pointer is moved
;      to where the next FITS extension header until the desired
;      extension is reached.
; PROCEDURE CALLS:
;      FXPAR(), MRD_HREAD, MRD_SKIP
; MODIFICATION HISTORY:
;      Extracted from FXPOSIT 8-March-2000 by T. McGlynn
;      Added /SILENT keyword  14-Dec-2000 by W. Landsman
;      Save time by not reading the full header  W. Landsman   Feb. 2003
;      Allow extension name to be specified, added EXT_NO, ERRMSG keywords
;         W. Landsman  December 2006
;      Make search for EXTNAME case-independent  W.Landsman March 2007 
;      Avoid round-off error for very large extensions N. Piskunov Dec 2007
;      Assume since V6.1 (/INTEGER keyword available to PRODUCT() ) Dec 2007
;      Capture error message from MRD_HREAD (must be used with post-June 2009
;        version of MRD-HREAD)   W. Landsman  July 2009
;-
     On_error, 2
     compile_opt idl2

         DO_NAME = SIZE( EXTEN,/TNAME) EQ 'STRING'
	 PRINT_ERROR = ~ARG_PRESENT(ERRMSG)
         ERRMSG = ''
         IF DO_NAME THEN BEGIN 
	              FIRSTBLOCK = 0
		      EXT_NO = 9999
		      ENAME = STRTRIM( STRUPCASE(EXTEN), 2 )
		      ON_IOERROR, ALLOW_PLUN
		      POINT_LUN, -UNIT, DUM
		      ON_IOERROR, NULL
         ENDIF ELSE BEGIN 
	              FIRSTBLOCK = 1
		      EXT_NO = EXTEN
	ENDELSE 	            
		
        FOR I = 1, EXT_NO DO BEGIN
               
;
;  Read the next header, and get the number of bytes taken up by the data.
; 

                IF EOF(UNIT) THEN BEGIN 
		    IF DO_NAME THEN ERRMSG = $
	'Extension name ' + ename + ' not found in FITS file' ELSE ERRMSG = $	    
	'EOF encountered while moving to specified extension'
	        if PRINT_ERROR then message,errmsg	
		RETURN, -1
		ENDIF
           
                ; Can't use FXHREAD to read from pipe, since it uses
                ; POINT_LUN.  So we read this in ourselves using mrd_hread

                MRD_HREAD, UNIT, HEADER, STATUS, SILENT = Silent, $
		    FIRSTBLOCK=FIRSTBLOCK, ERRMSG = ERRMSG
		   
                IF STATUS LT 0 THEN BEGIN 
		    IF PRINT_ERROR THEN MESSAGE,ERRMSG   ;Typo fix 04/10
		    RETURN, -1
		ENDIF    
                        
                ; Get parameters that determine size of data
                ; region.
                IF DO_NAME THEN IF I GT 1 THEN BEGIN
		       EXTNAME = STRTRIM(SXPAR(HEADER,'EXTNAME',COUNT=N_name),2)
			 if N_NAME GT 0 THEN $
			  IF ENAME EQ STRUPCASE(EXTNAME) THEN BEGIN
			        EXT_NO= I-1
				BLOCK = 1 + ((N_ELEMENTS(HEADER)-1)/36)
				POINT_LUN, -UNIT, CURR_POSS
				POINT_LUN, UNIT, CURR_POSS - BLOCK*2880 
			        BREAK
			ENDIF	
		ENDIF	         
                BITPIX = FXPAR(HEADER,'BITPIX')
                NAXIS  = FXPAR(HEADER,'NAXIS')
                GCOUNT = FXPAR(HEADER,'GCOUNT') 
                IF GCOUNT EQ 0 THEN GCOUNT = 1
                PCOUNT = FXPAR(HEADER,'PCOUNT')
                
                IF NAXIS GT 0 THEN BEGIN 
                        DIMS = FXPAR(HEADER,'NAXIS*')           ;Read dimensions
			NDATA = PRODUCT(DIMS,/INTEGER) 
                ENDIF ELSE NDATA = 0
                
                NBYTES = LONG64(ABS(BITPIX) / 8) * GCOUNT * (PCOUNT + NDATA)
;
;  Move to the next extension header in the file.
;
                NREC = (NBYTES + 2879) / 2880
                MRD_SKIP, UNIT, NREC*2880L 

        ENDFOR
	        
        RETURN, 0
ALLOW_PLUN:
        
        ERRMSG =  $
	'Extension name cannot be specified unless POINT_LUN access is available'
	if PRINT_ERROR then message,errmsg	
	RETURN, -1        
END

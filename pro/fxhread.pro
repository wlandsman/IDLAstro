	PRO FXHREAD,UNIT,HEADER,STATUS
;+
; NAME: 
;	FXHREAD
; Purpose     : 
;       Reads a FITS header from an opened disk file.
; Explanation : 
;       Reads a FITS header from an opened disk file.
; Use         : 
;	FXHREAD, UNIT, HEADER  [, STATUS ]
; Inputs      : 
;	UNIT	= Logical unit number.
; Opt. Inputs : 
;
; Outputs     : 
;	HEADER	= String array containing the FITS header.
; Opt. Outputs: 
;	STATUS	= Condition code giving the status of the read.  Normally, this
;		  is zero, but is set to !ERR if an error occurs, or if the
;		  first byte of the header is zero (ASCII null).
; Keywords    : 
;	None.
; Calls       : 
;	None.
; Common      : 
;	None.
; Restrictions: 
;	The file must already be positioned at the start of the header.  It
;	must be a proper FITS file.
; Side effects: 
;	The file ends by being positioned at the end of the FITS header, unless
;	an error occurs.
; Category    : 
;	Data Handling, I/O, FITS, Generic.
; Prev. Hist. : 
;	W. Thompson, Feb 1992, from READFITS by J. Woffard and W. Landsman.
;	W. Thompson, Aug 1992, added test for SIMPLE keyword.
; Written     : 
;	William Thompson, GSFC, February 1992.
; Modified    : 
;	Version 1, William Thompson, GSFC, 12 April 1993.
;		Incorporated into CDS library.
; Version     : 
;	Version 1, 12 April 1993.
;	Converted to IDL V5.0   W. Landsman   September 1997
;-
;
;
	ON_ERROR,2			;Return to caller
	STATUS = 0
;
;  Check the number of parameters.
;
	IF N_PARAMS() LT 2 THEN MESSAGE,	$
		'Calling sequence:  FXHREAD, UNIT, HEADER  [, STATUS ]'
;
;  Find out whether one is at the beginning of the file (POSITION=0) or not.
;
	POINT_LUN,-UNIT,POSITION
;
;  Read in the first 2880 byte FITS logical block as a series of 36 card images
;  of 80 bytes each.
;
	HDR = BYTARR( 80, 36, /NOZERO )
	ON_IOERROR, RETURN_STATUS
	READU, UNIT, HDR
;
;  If not the primary header, then the first eight bytes should decode to
;  XTENSION.  If not, then set status to -1, and return.
;
	IF POSITION NE 0 THEN BEGIN
		FIRST = STRING(HDR[0:7])
		IF FIRST NE 'XTENSION' THEN BEGIN
			MESSAGE,'XTENSION keyword not found',/CONTINUE
			STATUS = -1
			GOTO, DONE
		ENDIF
	ENDIF
;
;  Interpret the header as a string, and check to see if the END line has been
;  reached.
;
	HEADER = STRING( HDR > 32B )
	ENDLINE = WHERE( STRMID(HEADER,0,8) EQ 'END     ', NEND)
	IF NEND GT 0 THEN HEADER = HEADER[ 0:ENDLINE[0] ] 
;
;  If the primary header (POSITION=0) and the SIMPLE keyword can't be found in
;  the first record, then this can't be a FITS file.
;
	IF POSITION EQ 0 THEN BEGIN
		SIMPLE_LINE = WHERE(STRMID(HEADER,0,8) EQ 'SIMPLE  ',N_SIMPLE)
		IF N_SIMPLE EQ 0 THEN BEGIN
			MESSAGE,'SIMPLE keyword not found',/CONTINUE
			STATUS = -1
			GOTO, DONE
		ENDIF
	ENDIF
;
;  Keep reading until the END line is reached.
;
	WHILE NEND EQ 0 DO BEGIN
		READU, UNIT, HDR
		HDR1 = STRING( HDR > 32B )
		ENDLINE = WHERE( STRMID(HDR1,0,8) EQ 'END     ', NEND)
		IF NEND GT 0 THEN HDR1 = HDR1[ 0:ENDLINE[0] ] 
		HEADER = [HEADER, HDR1 ]
	ENDWHILE
	GOTO, DONE	
;
;  Error encounter.  Store the error code in status.
;
RETURN_STATUS:
	STATUS = !ERR
;
;  Reset the ON_IOERROR condition.
;
DONE:
	ON_IOERROR,NULL
	END

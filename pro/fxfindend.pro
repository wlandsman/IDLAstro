	PRO FXFINDEND,UNIT, EXTENSION
;+
; NAME: 
;	FXFINDEND
; Purpose     : 
;	Find the end of a FITS file.
; Explanation : 
;	This routine finds the end of the last logical record in a FITS file,
;	which may be different from that of the physical end of the file.  Each
;	FITS header is read in and parsed, and the file pointer is moved to
;	where the next FITS extension header would be if there is one, or to
;	the end of the file if not.
; Use         : 
;	FXFINDEND, UNIT [, EXTENSION]
; Inputs      : 
;	UNIT	= Logical unit number for the opened file.
; Opt. Inputs : 
;	None.
; Outputs     : 
;	None.
; Opt. Outputs: 
;       EXTENSION = The extension number that a new extension would
;                   have if placed at the end of the file.
; Keywords    : 
;	None.
; Calls       : 
;	FXHREAD, FXPAR
; Common      : 
;	None.
; Restrictions: 
;	The file must have been opened for block I/O.  There must not be any
;	FITS "special records" at the end of the file.
; Side effects: 
;	None.
; Category    : 
;	Data Handling, I/O, FITS, Generic.
; Prev. Hist. : 
;	William Thompson, Feb. 1992.
; Written     : 
;	William Thompson, GSFC, February 1992.
; Modified    : 
;	Version 1, William Thompson, GSFC, 12 April 1993.
;		Incorporated into CDS library.
; Version     : 
;	Version 1, 12 April 1993.
;	Converted to IDL V5.0   W. Landsman   September 1997
;       Added EXTENSION parameter, CM 1999 Nov 18
;       Allow for possible 64bit integer number of bytes W. Landsman Nov 2007
;       make Ndata a long64 to deal with large files. E. Hivon Mar 2008
;-
;
	ON_ERROR,2
;
;  Check the number of parameters.
;
	IF N_PARAMS() EQ 0 THEN MESSAGE,'Syntax:  FXFINDEND, UNIT [,EXTENSION]'
;
;  Go to the start of the file.
;
	POINT_LUN,UNIT,0
        EXTENSION = 0L
;
;  Read the next header, and get the number of bytes taken up by the data.
;
NEXT_EXT:
	FXHREAD,UNIT,HEADER,STATUS
	IF STATUS NE 0 THEN GOTO, DONE
	BITPIX = FXPAR(HEADER,'BITPIX')
	NAXIS  = FXPAR(HEADER,'NAXIS')
	GCOUNT = FXPAR(HEADER,'GCOUNT')  &  IF GCOUNT EQ 0 THEN GCOUNT = 1
	PCOUNT = FXPAR(HEADER,'PCOUNT')
	IF NAXIS GT 0 THEN BEGIN 
		DIMS = FXPAR(HEADER,'NAXIS*')		;Read dimensions
		NDATA = long64(DIMS[0])
		IF NAXIS GT 1 THEN FOR I=2,NAXIS DO NDATA = NDATA*DIMS[I-1]
	ENDIF ELSE NDATA = 0
	NBYTES = LONG64(ABS(BITPIX) / 8) * GCOUNT * (PCOUNT + NDATA)
;
;  Move to the next extension header in the file.
;
	NREC = (NBYTES + 2879) / 2880
	POINT_LUN, -UNIT, POINTLUN			;Current position
	POINT_LUN, UNIT, POINTLUN + NREC*2880L		;Next FITS extension
        EXTENSION = EXTENSION + 1L
	IF NOT EOF(UNIT) THEN GOTO, NEXT_EXT
;
;  When done, make sure that the pointer is positioned at the first byte after
;  the last data set.
;
DONE:
	POINT_LUN, UNIT, POINTLUN + NREC*2880L
	RETURN
	END

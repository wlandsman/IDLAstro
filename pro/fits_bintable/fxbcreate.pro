	PRO FXBCREATE, UNIT, FILENAME, HEADER, EXTENSION, ERRMSG=ERRMSG
;+
; NAME: 
;	FXBCREATE
; Purpose     : 
;	Open a new binary table at the end of a FITS file.
; Explanation : 
;	Write a binary table extension header to the end of a disk FITS file,
;	and leave it open to receive the data.
;
;	The FITS file is opened, and the pointer is positioned just after the
;	last 2880 byte record.  Then the binary header is appended.  Calls to
;	FXBWRITE will append the binary data to this file, and then FXBFINISH
;	will close the file.
;
; Use         : 
;	FXBCREATE, UNIT, FILENAME, HEADER
; Inputs      : 
;	FILENAME = Name of FITS file to be opened.
;	HEADER	 = String array containing the FITS binary table extension
;		   header.
; Opt. Inputs : 
;	None.
; Outputs     : 
;	UNIT	 = Logical unit number of the opened file.
;       EXTENSION= Extension number of newly created extension.
; Opt. Outputs: 
;	None.
; Keywords    : 
;	ERRMSG	  = If defined and passed, then any error messages will be
;		    returned to the user in this parameter rather than
;		    depending on the MESSAGE routine in IDL.  If no errors are
;		    encountered, then a null string is returned.  In order to
;		    use this feature, ERRMSG must be defined first, e.g.
;
;			ERRMSG = ''
;			FXBCREATE, ERRMSG=ERRMSG, ...
;			IF ERRMSG NE '' THEN ...
;
; Calls       : 
;	FXADDPAR, FXBFINDLUN, FXBPARSE, FXFINDEND
; Common      : 
;	Uses common block FXBINTABLE--see "fxbintable.pro" for more
;	information.
; Restrictions: 
;	The primary FITS data unit must already be written to a file.  The
;	binary table extension header must already be defined (FXBHMAKE), and
;	must match the data that will be written to the file.
; Side effects: 
;	None.
; Category    : 
;	Data Handling, I/O, FITS, Generic.
; Prev. Hist. : 
;	W. Thompson, Jan 1992, based on WRITEFITS by J. Woffard and W. Landsman.
;	W. Thompson, Feb 1992, changed from function to procedure.
;	W. Thompson, Feb 1992, removed all references to temporary files.
; Written     : 
;	William Thompson, GSFC, January 1992.
; Modified    : 
;	Version 1, William Thompson, GSFC, 12 April 1993.
;		Incorporated into CDS library.
;	Version 2, William Thompson, GSFC, 21 July 1993.
;		Fixed bug with variable length arrays.
;	Version 3, William Thompson, GSFC, 21 June 1994
;		Added ERRMSG keyword.
;	Version 4, William Thompson, GSFC, 23 June 1994
;               Modified so that ERRMSG is not touched if not defined.
;	Version 5, Antony Bird, Southampton, 25 June 1997
;		Modified to allow very long tables 
; Version     :
;	Version 5, 25 June 1997
;	Converted to IDL V5.0   W. Landsman   September 1997
;       Added EXTENSION parameter, C. Markwardt 1999 Jul 15
;       More efficient zeroing of file, C. Markwardt, 26 Feb 2001
;       Recompute header size if updating THEAP keyword B. Roukema April 2010
;-
;
@fxbintable
	ON_ERROR, 2
;
;  Check the number of parameters.
;
	IF N_PARAMS() LT 3 THEN BEGIN
		MESSAGE = 'Syntax:  FXBCREATE, UNIT, FILENAME, HEADER'
		IF N_ELEMENTS(ERRMSG) NE 0 THEN BEGIN
			ERRMSG = MESSAGE
			RETURN
		END ELSE MESSAGE, MESSAGE
	ENDIF
;
;  Get a logical unit number, open the file, and find the end.
;
	GET_LUN,UNIT
       	OPENU, UNIT, FILENAME, /BLOCK
	FXFINDEND, UNIT, EXTENSION
;
;  Store the UNIT number in the common block, and leave space for the other
;  parameters.  Initialize the common block if need be.  ILUN is an index into
;  the arrays.
;
	ILUN = FXBFINDLUN(UNIT)
;
;  Store the current position as the start of the header.  Mark the file as
;  open for write.
;
	POINT_LUN,-UNIT,POINTER
	MHEADER[ILUN] = POINTER
	STATE[ILUN] = 2
;
;  Determine if an END line occurs, and add one if necessary
;
CHECK_END:
	ENDLINE = WHERE(STRMID(HEADER,0,8) EQ 'END     ', NEND)
	ENDLINE = ENDLINE[0]
	IF NEND EQ 0 THEN BEGIN
		MESSAGE,/INF,'WARNING - An END statement has been appended ' +$
			'to the FITS header'
		HEADER = [HEADER, 'END' + STRING(REPLICATE(32B,77))]
		ENDLINE = N_ELEMENTS(HEADER) - 1 
	ENDIF
	NMAX = ENDLINE + 1		;Number of 80 byte records
	NHEAD = FIX((NMAX+35)/36)	;Number of 2880 byte records
;
;  Convert the header to byte and force into 80 character lines.
;
WRITE_HEADER:
	BHDR = REPLICATE(32B, 80, 36*NHEAD)
	FOR N = 0,ENDLINE DO BHDR[0,N] = BYTE( STRMID(HEADER[N],0,80) )
	WRITEU, UNIT, BHDR
;
;  Get the rest of the information, and store it in the common block.
;
	IF N_ELEMENTS(ERRMSG) NE 0 THEN BEGIN
		FXBPARSE,ILUN,HEADER,ERRMSG=ERRMSG
		IF ERRMSG NE '' THEN RETURN
	END ELSE FXBPARSE,ILUN,HEADER
;
;  Check the size of the heap offset.  If the heap offset is smaller than the
;  table, then reset it to the size of the table.
;
	DDHEAP = HEAP[ILUN] - NAXIS1[ILUN]*NAXIS2[ILUN]
	IF DDHEAP LT 0 THEN BEGIN
		MESSAGE,'Heap offset smaller than table size--resetting', $
			/CONTINUE
		HEAP[ILUN] = NAXIS1[ILUN]*NAXIS2[ILUN]
		FXADDPAR,HEADER,'THEAP',HEAP[ILUN]
		POINT_LUN, UNIT, MHEADER[ILUN]
	
; Have we changed position of the END keyword?
		GOTO, CHECK_END
	ENDIF
;
;  Fill out the file to size it properly.
;
        ;; This segment is now optimized to write out more than one
        ;; row at a time, which is crucial for tables with many small
        ;; rows.  The code heuristically chooses a buffer size which
        ;; is 1% of the file, but no bigger than 512k, and always a
        ;; multiple of the row size.


        BUFSIZE = LONG(NAXIS1[ILUN]*NAXIS2[ILUN]/100) > NAXIS1[ILUN] < 524288L
        BUFSIZE = (FLOOR(BUFSIZE/NAXIS1[ILUN])>1) * NAXIS1[ILUN]
        BUFFER = BYTARR(BUFSIZE)
        TOTBYTES = NAXIS1[ILUN]*NAXIS2[ILUN]

        ;; TOTBYTES keeps count of bytes left to write
        WHILE TOTBYTES GT 0 DO BEGIN
            ;; Case of final rows which might not be EQ BUFSIZE
            IF TOTBYTES LT BUFSIZE THEN BUFFER = BYTARR(TOTBYTES)
            WRITEU,UNIT,BUFFER
            TOTBYTES = TOTBYTES - BUFSIZE
        ENDWHILE
;
;  If there's any extra space before the start of the heap, then write that out
;  as well.
;
	IF DDHEAP GT 0 THEN BEGIN
		BUFFER = BYTARR(DDHEAP)
		WRITEU,UNIT,BUFFER
	ENDIF
;
;  Initialize DHEAP, and return.
;
	DHEAP[ILUN] = 0
;
	IF N_ELEMENTS(ERRMSG) NE 0 THEN ERRMSG = ''
	RETURN
	END


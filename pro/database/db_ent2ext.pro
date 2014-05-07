	PRO DB_ENT2EXT, ENTRY
;+
; NAME:
;	DB_ENT2EXT
; PURPOSE:
;	Convert a database entry to external (IEEE) data format
; EXPLANATION: 
;	Converts a database entry to external (IEEE) data format prior to
;	writing it.  Called from DBWRT.
;
; CALLING SEQUENCE:
;	DB_ENT2EXT, ENTRY
;
; INPUTS:
;	ENTRY	= Byte array containing a single record to be written to the
;		  database file.
;
; OUTPUTS:
;	ENTRY	= The converted array is returned in place of the input array.
;
; COMMON BLOCKS:
;	DB_COM
;
; HISTORY:
;	Version 1, William Thompson, GSFC/CDS (ARC), 1 June 1994
;	Version 2, William Thompson, GSFC/CDS (ARC), 15 September 1995
;			Fixed bug where only the first element in a
;			multidimensional array was converted.
;       Version 2.1 W. Landsman August 2010 Fix for multidimensional strings
;       Version 2.2 W. Landsman Sep 2011 Work with new DB format
;-
;
	ON_ERROR,2
        COMPILE_OPT IDL2
;
;
; QDB[*,i] contains the following for each data base opened
;
;	bytes
;	  0-18   data base name character*19
;	  19-79  data base title character*61
;	  80-81  number of items (integer*2)
;	  82-83  record length of DBF file (integer*2)
;	  84-87  number of entries in file (integer*4)
;	  88-89  position of first item for this file in QITEMS (I*2)
;	  90-91  position of last item for this file (I*2)
;	  92-95  Last Sequence number used (item=SEQNUM) (I*4)
;	  96	 Unit number of .DBF file
;	  97	 Unit number of .dbx file (0 if none exists)
;	  98-99  Index number of item pointing to this file (0 for first db)
;	  100-103 Number of entries with space allocated
;	  104	 Update flag (0 open for read only, 1 open for update)
;	  119	 True if database is in external (IEEE) data format
;
;  QITEMS[*,i] contains description of item number i with following
;  byte assignments:
;
;	0-19	item name (character*20)
;	20-21   IDL data type (integet*2)
;	22-23 	Number of values for item (1 for scalar) (integer*2)
;	24-25	Starting byte position in original DBF record (integer*2)
;	26-27	Number of bytes per data value (integer*2)
;	28	Index type
;	29-97	Item description
;	98-99	Print field length
;	100	Flag set to one if pointer item
;	101-119 Data base this item points to
;	120-125 Print format
;	126-170 Print headers
;	171-172 Starting byte in record returned by DBRD
;	173-174 Data base number in QDB
;	175-176 Data base number this item points to
;
;
; QLINK[i] contains the entry number in the second data base
;	corresponding to entry i in the first data base.
;
	COMMON DB_COM,QDB,QITEMS,QLINK
;
;  Check the number of parameters.
;
	IF N_PARAMS() NE 1 THEN MESSAGE, 'Syntax:  DB_ENT2EXT, ENTRY'
;
;  Get some information on the data base.
;
	LEN = DB_INFO( 'LENGTH', 0 )		;Record length
	N_ITEMS = DB_INFO( 'ITEMS', 0 )		;Number of items
;
;  Determine if ENTRY is correct.
;
	S = SIZE(ENTRY)
	IF S[0] NE 1 THEN MESSAGE, 'ENTRY must be a 1-dimensional array'
	IF S[1] NE LEN THEN MESSAGE,	$
		'ENTRY not the proper length of ' + STRTRIM(LEN,2) + ' bytes'
	IF S[2] NE 1 THEN MESSAGE, 'ENTRY must be a byte array'
;
;  Extract information about the individual items.
;
         newdb = qdb[118, 0]
        
	IDLTYPE = FIX(QITEMS[20:21,*],0,N_ITEMS)
	NVALUES = NEWDB ? LONG(QITEMS[179:182,*],0,N_ITEMS) : $
	                  FIX(QITEMS[22:23,*],0,N_ITEMS)
	SBYTE	= NEWDB ? LONG(QITEMS[183:186,*],0,N_ITEMS) : $
	                  FIX(QITEMS[24:25,*],0,N_ITEMS)
	NBYTES	= FIX(QITEMS[26:27,*],0,N_ITEMS)*NVALUES
        BSWAP =  (IDLTYPE NE 7) AND (IDLTYPE NE 1)
;
;  For each entry, convert the data into external format.
;
	FOR I = 0, N_ITEMS-1 DO BEGIN	      
	    IF BSWAP[I] THEN BEGIN
	    
		ITEM = DBXVAL(ENTRY,IDLTYPE[I],NVALUES[I],SBYTE[I],NBYTES[I])
		SWAP_ENDIAN_INPLACE, ITEM, /SWAP_IF_LITTLE
		DBXPUT, ITEM, ENTRY, IDLTYPE[I], SBYTE[I], NBYTES[I]
	    ENDIF	
	ENDFOR
;
	RETURN
	END

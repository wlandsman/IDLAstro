;+
; NAME: 
;	FXBINTABLE
; Purpose     : 
;	Common block FXBINTABLE used by "FXB" routines.
; Explanation : 
;	This is not an IDL routine as such, but contains the definition of the
;	common block FXBINTABLE for inclusion into other routines.  By defining
;	the common block in one place, the problem of conflicting definitions
;	is avoided.
;
;	This file is included into routines that need this common block with
;	the single line (left justified)
;
;				  @fxbintable
;
;	FXBINTABLE contains the following arrays:
;
;		LUN	= An array of logical unit numbers of currently (or
;			  previously) opened binary table files.
;		STATE	= Array containing the state of the FITS files
;			  associated with the logical unit numbers, where
;			  0=closed, 1=open for read, and 2=open for write.
;		HEAD	= FITS binary table headers.
;		MHEADER	= Array containing the positions of the first data byte
;			  of the header for each file referenced by array LUN.
;		NHEADER	= Array containing the positions of the first data byte
;			  after the header for each file referenced by array
;			  LUN.
;		NAXIS1	= Values of NAXIS1 from the binary table headers.
;		NAXIS2	= Values of NAXIS2 from the binary table headers.
;		TFIELDS	= Values of TFIELDS from the binary table headers.
;		HEAP	= The start of the first byte of the heap area
;			  for variable length arrays.
;		DHEAP	= The start of the first byte of the next variable
;			  length array, if writing.
;		BYTOFF	= Byte offset from the beginning of the row for each
;			  column in the binary table headers.
;		TTYPE	= Values of TTYPE for each column in the binary table
;			  headers.
;		FORMAT	= Character code formats of the various columns.
;		IDLTYPE	= IDL type code for each column in the binary table
;			  headers.
;		N_ELEM	= Number of elements for each column in the binary
;			  table headers.
;		TSCAL	= Scale factors for the individual columns.
;		TZERO	= Zero offsets for the individual columns.
;		MAXVAL	= For variable length arrays, contains the maximum
;			  number of elements for each column in the binary
;			  table headers.
;		N_DIMS	= Number of dimensions, and array of dimensions for
;			  each column of type string in the binary table
;			  headers.
; Category    : 
;	Data Handling, I/O, FITS, Generic.
; Prev. Hist. : 
;	William Thompson, Feb 1992.
; Written     : 
;	William Thompson, GSFC, February 1992.
; Modified    : 
;	Version 1, William Thompson, GSFC, 12 April 1993.
;		Incorporated into CDS library.
;	Version 2, William Thompson, GSFC, 21 July 1993.
;		Added DHEAP variable to fix bug with variable length arrays.
; Version     : 
;	Version 2, 21 July 1993.
;-
;
	COMMON FXBINTABLE,LUN,STATE,HEAD,MHEADER,NHEADER,NAXIS1,NAXIS2,	$
		TFIELDS,HEAP,DHEAP,BYTOFF,TTYPE,FORMAT,IDLTYPE,N_ELEM,TSCAL, $
		TZERO,MAXVAL,N_DIMS

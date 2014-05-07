			      FITS Binary Tables

			  Last updated:  13 Dec 2007


The routines in this directory are for reading and writing (disk) FITS binary
table extensions.  .  Some routines for reading
and writing primary FITS files are also included.  Additional information is 
available in the LateX file fits_bintable.tex in the /text directory. 


The various routines are:


			 Primary FITS files -- writing

These routines are for writing primary FITS headers.  Primary data arrays can
also be written, but are not required.  The sequence to be used is to first
create the header (FXHMAKE), add any additional desired keywords (FXADDPAR) and
write the primary header and optional data array (FXWRITE).

	FXHMAKE		Create FITS header
	FXADDPAR	Add or modify header keywords
	FXWRITE		Write primary FITS file
	FXHMODIFY	Modify FITS header on disk


			 Primary FITS files -- reading

These routines are for reading primary FITS header and data units, and for
extracting information from the FITS header.  Other routines from the Astronomy
User's Library (e.g. READFITS) could also be used to read the primary header
and data arrays from FITS files.

	FXREAD		Read primary FITS file
	FXPAR		Extract values from a FITS header


			   Binary tables -- writing

These routines are used write or modify FITS binary table extensions.  The
sequence to be used is to create the header (FXBHMAKE), define the table
columns (FXBADDCOL), open the file and write the header (FXBCREATE), write the
binary table data (FXBWRITE), then close the table (FXBFINISH).  See the LaTeX
file "fits_bintable.tex" for an example of writing and reading a binary table.

	FXBHMAKE	Create FITS extension header.
	FXBADDCOL	Create columns in the binary table extension header.
	FXBCREATE	Open binary table file, and write header.
        FXBGROW         Increase the number of rows in a binary table
	FXBWRITE	Write data into binary table.
        FXBWRITM        Write multiple columns/rows to a FITS binary table file
	FXBFINISH	Close binary table file.

One thing that sometimes confuses first time users is that the logical unit
variable UNIT is an output parameter of FXBCREATE, not an input parameter.  One
does not specify the logical unit number for the file; instead FXBCREATE
assigns one via a call to GET_LUN.  The returned value is then used by the
other routines.  A similar situation exists in regard to the column number
argument to FXBADDCOL.  Column numbers are assigned in the order in which
FXBADDCOL is called.  See the LaTeX file "fits_bintable.tex" for more
information.


			   Binary tables -- reading

These routines are used for reading FITS binary table extensions.  The sequence
to be used is to open the table (FXBOPEN), read data from the table (FXBREAD),
and close the table (FXBCLOSE).  Other routines allow the user to parse and
examine information from the binary table header.

	FXBOPEN		Open FITS binary table extension for reading.
	FXBREAD		Read data from FITS binary table.
        FXBREADM        Read multiple columns/rows from a FITS binary table file
	FXBCLOSE	Close FITS binary table.
	FXBTDIM		Parse keywords from binary tables with a TDIM-like
			format.
	FXBHELP		Show information about the binary table columns.
	FXBFIND		Find column keywords in header.
	FXBCOLNUM	Returns the column number of a specified column.
	FXBHEADER	Returns the header of an opened FITS binary table.
	FXBISOPEN	Returns whether or not a FITS binary table is open.
	FXBSTATE	Returns the state of a FITS binary table.
	FXBDIMEN	Returns the dimensions of a binary table column.

The same comment above about FXBCREATE and UNIT also applies to FXBOPEN.



			      Associated routines

The remaining routines are mainly internal routines used by the other routines
mentioned above.

	FXHCLEAN	Remove obsolete keywords--called by FXHMAKE, FXHBMAKE.
	FXPARPOS	Find position in FITS header--called by FXADDPAR.
	FXBFINDLUN	Find LUN in FXBINTABLE--called by FXBCREATE, FXBOPEN.
	FXBPARSE	Parse binary table header--called by FXBCREATE,FXBOPEN.
	FXBTFORM	Parse TFORM column descriptor--called by FXBPARSE.
	FXHREAD		Read FITS header--called by FXBOPEN.
	FXFINDEND	Find the last FITS record--called by FXBCREATE.
	WHERENAN	Find points equal to big-endian IEEE NaN.
        BLKSHIFT        Shift a block of data to a new position in a file 
	BOOST_ARRAY	Resize array, and append another array.
	STORE_ARRAY	Resize array, and insert another array.
	DETABIFY	Removes tabs from strings.

			Multidimensional Array Facility

These routines support the Multidimensional Array Facility described in the
binary tables extension proposal.  This convention uses keywords TDIMn of the
format "TDIMn = '(dim1,dim2,...)'" to define the dimensions associated with
column "n".  Support for this convention is automatic--FXBADDCOL inserts TDIMn
keywords into the header, and FXBOPEN interprets any found in the header--
unless the /NO_TDIM keyword is used.  Values of TDIMn can also be overridden
with the DIMENSIONS keyword in the FXBREAD routine.

In addition to the keywords described in the binary tables extension proposal,
several additional keywords are supported by FXBADDCOL.  These keywords have a
one-to-one correspondence with standard keywords used in primary FITS headers,
i.e.

		  Additional Keyword	  Standard Equivalent

			TDMINn			DATAMIN	
			TDMAXn			DATAMAX

			TDESCn			CTYPEm
			TCUNIn			CUNITm
			TROTAn			CROTAm
			TRPIXn			CRPIXm
			TRVALn			CRVALm
			TDELTn			CDELTm

The anticipated use of these keywords is such that TDMIN and TDMAX would have a
standard format, and that the rest would have a format similar to TDIMn.


			Variable-Length Array Facility

These routines also support the Variable-Length Array Facility described in the
binary tables extension proposal.  Variable-length array columns are defined by
using FXBADDCOL with the /VARIABLE keyword.  Other than that, support for
variable-length arrays is automatic.  Some operations, such as reading entire
columns, and the multidimensional array facility described above, are not
allowed with variable-length arrays.

Ordinarily, the default THEAP value (NAXIS1*NAXIS2) is used to write the
variable-length arrays.  However, a different THEAP value can be used by using
FXADDPAR to insert the desired value into the binary table header before
calling FXBCREATE.


		    IEEE Not-a-Number (NaN) Special Values

Data dropout in FITS binary table arrays are signalled in one of two ways.
Dropouts in integer arrays are signalled with values specified by TNULLn
keywords.  However, dropouts in floating point arrays (including single or
double precision, and real or complex) are signalled with standard IEEE NaN
(not-a-number) special values.  The routine FXBREAD will optionally translate
these NaN numbers into a user-specified value, given by the NANVALUE keyword.
Conversely, the same keyword, when used with the FXWRITE or FXBWRITE routines,
will write out NaN for any points in the array with that value.

At present, there is no support for IEEE +/- Infinity and -0 special values.
However, it would be a simple matter to add support similar to IEEE NaN.


	       Bit and Logical Arrays


Bit arrays (type "X" in FITS binary tables) are treated in IDL as byte arrays
with approximately 1/8 the number of elements.  Support for this is automatic
when reading binary tables.  Columns can be defined as type "X" when writing
binary tables if the BIT keyword is passed to FXBADDCOL giving the number of
bits, and the data array is of type byte.  Dimension information is ignored for
bit arrays, since the dimensions apply to the bits, and not to the bytes that
IDL processes.

Logical arrays (type "L" in FITS binary tables) are treated in IDL as byte
arrays.  Support for this is automatic when reading binary tables.  Columns can
be defined as type "L" when writing binary tables if the /LOGICAL keyword is
passed to FXBADDCOL, and the data array is of type byte.


				Virtual Columns

It is possible to treat keywords in binary table headers as if they were
columns in the table, with the same value replicated for every row.  This
virtual column convention allows the user to have a unified view in a table
regardless of whether the information is stored in a table column and thus
capable of varying from row to row, or stored in the header and thus the same
for every row.

To use the virtual column convention, the user must call FXBREAD with the
/VIRTUAL keyword, and must also reference the desired information by name
rather than by column number.  FXBREAD will then look first for a column with
that name.  If it doesn't find one, it then looks for a keyword with that name
in the header.


			     Implementation notes

The routines in this directory also make use internally of other routines from
the /sdas, /fits, and /misc directories from the Astronomy User's Library.

The file "fxbintable.pro" is an include file containing the definition of the
IDL common block FXBINTABLE.  This file must be in one of the directories
pointed to by the IDL search path parameter !PATH.  Normally, this is ensured
by keeping this file in the same directory with the IDL procedures found here.

Questions should be directed to:

	William.T.Thompson.1@gsfc.nasa.gov

----------------------------------------------------------------------------
 
 
FXADDPAR          - Add or modify a parameter in a FITS header array.
FXBADDCOL         - Adds a column to a binary table extension.
FXBCLOSE          - Close a FITS binary table extension opened for read.
FXBCOLNUM()       - Returns a binary table column number.
FXBCREATE         - Open a new binary table at the end of a FITS file.
FXBDIMEN()	  - Returns the dimensions for a column in a FITS binary table.
FXBFIND           - Find column keywords in a FITS binary table header.
FXBFINDLUN()      - Find logical unit number UNIT in FXBINTABLE common block.
FXBFINISH         - Close a FITS binary table extension file opened for write.
FXBHEADER()       - Returns the header of an open FITS binary table.
FXBHELP           - Prints short description of columns in a FITS binary table.
FXBHMAKE          - Create basic FITS binary table extension (BINTABLE) header.
FXBINTABLE        - Common block FXBINTABLE used by "FXB" routines.
FXBISOPEN()       - Returns true if UNIT points to an open FITS binary table.
FXBOPEN           - Open binary table extension in a disk FITS file for reading.
FXBPARSE          - Parse the binary table extension header.
FXBREAD           - Read a data array from a disk FITS binary table file.
FXBREADM          - Read mulitple columns/rows from a FITS binary table file.
FXBSTATE()        - Returns the state of a FITS binary table.
FXBTDIM()         - Parse TDIM-like keywords.
FXBTFORM          - Returns information about FITS binary table columns.
FXBWRITE          - Write a binary data array to a disk FITS binary table file.
FXBWRITEM          -Write multiple columns/rows to a FITS binary table file.
FXFINDEND         - Find the end of a FITS file.
FXHCLEAN          - Removes required keywords from FITS header.
FXHMAKE           - Create a basic FITS header array.
FXHMODIFY         - Modify a FITS header in a file on disk.
FXHREAD           - Reads a FITS header from an opened disk file.
FXPAR()           - Obtain the value of a parameter in a FITS header.
FXPARPOS()        - Finds position to insert record into FITS header.
FXREAD            - Read basic FITS files.
FXWRITE           - Write a disk FITS file.

The procedures for supporting tape I/O below are no longer available in the
main library, but are still available in
http://idlastro.gsfc.nasa.gov/ftp/obsolete/

FITSTAPE()        - Function to perform FITS tape I/O.
FXTAPEREAD        - Interactive procedure copies FITS files from tape to disk.
FXTAPEWRITE       - Interactive procedure to copy disk FITS files to tape.
FXTPIO_READ       - Copies FITS files from tape to disk -- internal routine.
FXTPIO_WRITE      - Copy FITS files from disk to tape -- internal routine.


Some procedures from other directories are also useful

CHECK_FITS        - Checks validity of FITS header.
GET_DATE          - Gets date in format used by FITS headers.
HPRINT            - Prints FITS headers on terminal screen.
STRNUMBER()       - Function to determine if a string is a valid numeric value.
SXDELPAR()        - Procedure to delete keyword parameter(s) from FITS header.
SXPAR()           - Obtain the value of a parameter in a FITS header


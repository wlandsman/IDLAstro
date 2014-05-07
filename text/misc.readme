Miscellaneous Procedures                      pro/misc         July 2009

The procedures in this directory are general utility procedures that do not
involve astronomy.   They are included here either because they are required
by an astronomy-related procedure, or because they are thought to be of 
sufficient general interest.

The procedure ASTROLIB defines the non-standard system variables !TEXTOUT,
!TEXTUNIT and !PRIV which some Astronomy Library procedures require to be 
defined before they can compile.


The procedures BREAK_PATH, FIND_ALL_DIR, FIND_WITH_DEF and CONCAT_DIR were 
originally from the Solar Library 
( http://sohowww.nascom.nasa.gov/solarsoft/gen/idl/system/ ) but do not
contain the VMS capabilities of the Solar Library versions.    

ASTROLIB - Add the non-standard system variables in use in the IDL
           Astronomy User's Library
BLKSHIFT - Shift a block of data to a new (possibly overlapping) position in a 
          file
BOOST_ARRAY - Append one array onto another, adjusting dimensions if necessary
BREAK_PATH() - break up a !PATH-like string into individual directories
BSORT() - Like the IDL SORT function but subscript order is maintained when
        value are equal -- like a bubble sort.
CHECKSUM32 - Compute the 32bit checksum of an array (ones-complement arithmetic)
CIRRANGE - Force an angle to be in the range 0 to 2*!PI (or 0 to 360).
CONCAT_DIR - concatenate directory and file name for current OS
DELVARX - Delete an IDL variable; like DELVAR but works at any calling level
DETABIFY() - Replace tabs in a character string by equivalent number of spaces
EXPAND_TILDE() - Expand the tilde in Unix directory names
F_FORMAT() - Find the "best" F format to display an array of REAL*4 numbers.
FDECOMP - Decompose a file name (Disk + Directory + Name + Extension + Version)
FIND_ALL_DIR() - Finds all directories under a specified directory.
FIND_WITH_DEF - Search for files with default path and extension
FINDPRO - Find all occurrences of a procedure in one's !PATH
FORPRINT -Print a set of vectors by looping over each index value
GETOPT() -  Parse a user supplied string into numeric value(s).
GETPRO -  Search !PATH directory for a procedure and copy into user's directory
GETTOK() -  Extract a string up to a specified character.
GETWRD() -  Get specified item (word) from a string (in /jhuapl)
HGREP -  Find a substring in a FITS header (or any other string array)
HOST_TO_IEEE - Convert IDL variable from host machine bit order to IEEE
HPRINT -  Pretty terminal display of a FITS header (or other string array)
IEEE_TO_HOST - Convert IDL variable from IEEE bit order to host machine
ISARRAY() - Determine if an IDL variable is an array (in /jhuapl)
IS_IEEE_BIG() - Determine if the host machine is IEEE big endian 
LIST_WITH_PATH() - Search for files within specified directory path
MAKE_2D - Change from 1-D indexing to 2-D indexing
MATCH -   Find the subscripts where the values of two vectors match.
MATCH2 -   Find the matches for each element of two arrays.
MINMAX() -  Return the min and max of an array in an 2 element vector
MRD_SKIP - Skip a number of bytes from current location in a file or Unix pipe
N_BYTES() - Return the number of bytes in an IDL variable
NINT() - Like intrinsic ROUND() function but returns integer instead of long
NULLTRIM() -Delete all characters after, and including, the the first null
          byte(0).  Called by TAB_PUT.
ONE_ARROW - Draw an arrow labeled with a single character
ONE_RAY - Draw a ray by specifying starting point, angle, and length
ORDINAL() - Return the English equivalent of ordinal numbers, i.e. '1st','2nd'
POLREC - Convert from polar to rectangular coordinates (in /jhuapl)
QGET_STRING() - Read a string (eg. password) from the keyboard without echoing it
RDFLOAT - Quickly read an ASCII file with columns of data into IDL vectors
READ_KEY() - Like GET_KBRD but returns a code for escape sequences.
READCOL - Read a file of free-format  ASCII data into IDL vectors
READFMT - Quickly read a file of fixed-format ASCII data into IDL vectors
RECPOL - Convert from rectangular to polar coordinates (in /jhuapl)
REMCHAR - Remove all appearances of a character from a string.
REM_DUP() - Remove duplicate values from a vector.
REMOVE -  Contract a vector or up to 8 vectors by removing specified
          elements.
REPCHR() -  Replace all occurrences of one character by another (in /jhuapl)
REPSTR() -  Replace all occurrences of one substring in a string by another.
SELECT_W - Allow user to interactively select from a list of strings
SPEC_DIR() - Complete specification of a file name using default disk & directory
STORE_ARRAY - Insert one array into another, adjusting dimensions if necessary
STR_INDEX - Get all indicies of a substring within a string
STRCOMPRESS2() - Remove blanks around specified spaces in a string
STRN() - Convert a number to a string and remove padded blanks.
STRNUMBER()- Determine whether a string is a valid numeric value.
TEXTOPEN - Open a file for text output as specified by !TEXTOUT
           Controls the print output device for many procedures.
TEXTCLOSE - Close a file that had been opened by TEXTOPEN.
TO_HEX()    - Translate a decimal integer to a hex string.  Called by AIPSNAME.

VALID_NUM() - Determine if a string is a valid number (cf. STRNUMBER)
VECT() - Display a set of numbers to a string with delimiters
WHERENAN() - Find points equal to big-endian IEEE NaN (not a number) values
XDISPSTR - Display a string array in a widget a with simple search capability
ZPARCHECK - Check the type and size of a parameter

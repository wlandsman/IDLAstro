/FITS_TABLE                                                August 2000

This directory contains IDL procedures for reading and writing FITS ASCII 
tables, and for reading FITS binary tables.   The procedures each have one of
three different prefixes; either FTAB*, TB*, or FT*.    The TB* and FT*
procedures, which are somewhat lower level, manipulate binary or ASCII table 
arrays, respectively, *after* these array have already been read with a program 
such as READFITS().    The FTAB* procedures operate directly on a FITS file
and can be used with either binary or ASCII tables.   

The procedures in this directory cannot be used to write any type of binary 
table, or to read a variable length binary table.    Users who need to perform 
these tasks with IDL should either use Bill Thompson's binary table software in
the pro/fits_bintable directory, or Tom McGlynn's MRDFITS()/MWRFITS software to
convert between FITS tables and IDL structures, in the pro/fits directory.

Below is an example session, showing the use of the FTAB_* procedures with a 
FITS file 'rosat.fits' containing two table extensions.

;Describe FITS binary table, first (default) extension
IDL> ftab_help,'rosat.fits'        
                                   
;FITS Binary Table Header
;Size of Table Array: 20 by 89
;Extension Name:   AVGBKS  
; 
;Field      Name       Unit      Frmt   Null    Comment
; 
;  1 SECT_START_TIME     SECS      1D            
;  2 SECT_STOP_TIME     SECS       1D            
;  3 AVGBK_IN_SEC    CNTS/PIX2/SE  1E            

;Extract columns 1 and 3 
IDL> ftab_ext,'rosat.fits',[1,3],start,avg    

;Extract second column, first 50 rows only
IDL> ftab_ext,'rosat.fits','sect_stop_time',stop,rows =indgen(50)

;Print second extension w/default formats
IDL> ftab_print,'rosat.fits',ext=2    

;Copy the FITS file, but delete first 10 rows from the first extension
IDL> ftab_delrow,'rosat.fits',new='new.fits',indgen(10)

To use the lower level procedures, that table data and header must first be
read into IDL variables. For example, to read the first table extension of  the
FITS file test.fits into a table array, tab, and header, htab:

IDL> tab = readfits( 'test.fits', h, /EXTEN)
IDL> vector = tbget(h, tab, 'VECTOR')      ;Extract column named "VECTOR"

The LaTeX file ft.tex gives some documentation on the use of these lower-level
procedures.

The procedures in this directory make use of FITS header procedures 
in the /FITS subdirectory and some additional procedures in the /MISC
directory.  

The procedures FTAB_HELP, FTAB_PRINT, FTHELP, FTPRINT, TBPRINT, TBHELP use 
the non-standard system variables !TEXTOUT and !TEXTUNIT.  These can be 
added to one's session using the procedure ASTROLIB.

High-level FITS table procedures:

FTAB_DELROW - Delete specified rows in a FITS table extension
FTAB_EXT - Extract specified columns of a FITS table extension into IDL vectors
FTAB_HELP - Print info about the fields in a FITS table extension
FTAB_PRINT - Print specified columns and rows of a FITS table extension

Lower level FITS ASCII table procedures:

FTADDCOL - Add a new column to a FITS ASCII table
FTCREATE - Create an empty FITS ASCII table (H and TAB)
FTDELCOL - Delete specified column from a FITS ASCII table
FTDELROW - Delete specified row(s) from a FITS ASCII table 
FTGET() - Extract a specified field from a column in a FITS ASCII table
FTHELP - Display the fields in a FITS ASCII table header
FTHMOD - Modify the attributes of a field in a FITS ASCII table
FTINFO - Return an informational structure from a FITS ASCII table header
FTKEEPROW - Subscript (and reorder) a FITS ASCII table array
FTPRINT - Print specified columns and rows in a FITS ASCII table
FTPUT - Update or add data to a field in a FITS ASCII table
FTSIZE - Return the size and number of fields in a FITS ASCII table
FTSORT - Sort a FITS ASCII table according to the values in a specified field.

Lower level FITS binary table procedures:

TBDELCOL - Delete specified column from a FITS Binary table
TBDELROW - Delete specified row(s) from a FITS Binary table
TBGET() - Extract a specified field from a column in a FITS Binary table
TBHELP - Display the fields in a FITS Binary table header
TBINFO - Return an informational structure from a FITS Binary table header
TBPRINT - Print specified columns and rows in a FITS Binary table
TBSIZE - Return the size and number of fields in a FITS Binary table

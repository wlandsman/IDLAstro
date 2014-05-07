/FITS directory                                                  Jan-2012

FITS (Flexible Image Transport Sytem) is a standardized data format which is
widely used in astronomy.   Information on the FITS format can be obtained 
from the FITS supportoffice at http://fits.gsfc.nasa.gov/.  

The IDL Astronomy Library contains a variety of procedures for reading, 
writing, and modifying FITS files.   Information on how to use these procedures
is given in http://idlastro.gsfc.nasa.gov/fitsio.html and is also briefly 
described here.    There are four main types of FITS I/O 
procedures available in the Library:

   (1) MRDFITS() will read all standard FITS data types into IDL arrays (for
primary images and image extensions) or structures (for binary or ASCII tables).
The procedure MWRFITS() will write an IDL structure to any type of FITS file,
with many  options available (binary or ASCII table, default generation of
column names). These procedures were originally written by Tom  McGlynn
(USRA/Goddard) but are now maintained by Wayne Landsman.   Further  information
on MRDFITS is available in the mrdfits.txt file in this directory or at 
http://idlastro.gsfc.nasa.gov/mrdfits.html.

To compile MRDFITS and MWRFITS the following procedures must be included in
the !PATH
	FXADDPAR, FXPAR(), FXPARPOS()             (in pro/fits_bintable)
        GETTOK(), VALID_NUM()                     (in pro/misc)
	FXMOVE, FXPOSIT, MRD_HREAD, MRD_SKIP      (in pro/fits)
	MRD_STRUCT()                              (in pro/structure)


   (2) The function READFITS can be used to read FITS headers and arrays into
IDL variables.   Additional procedures in the pro/fits_table directory
are then required to interpret binary and ASCII tables.   The FT* procedures in 
the /fits_table directory are used to interpret a FITS ASCII table, and the
TB* procedures are used interpret FITS Binary table.    The procedure WRITEFITS
can be used to write a primary image, ASCII extension, or image extension
(see Appendix 2).      The READFITS/WRITEFITS software currently cannot read 
variable length binary tables, or write any type of binary table.    The
READFITS/WRITEFITS software requires the following procedures from the /misc
directory: GETTOK(), STRN(), VALID_NUM().

   (3) The directory pro/fits_bintable contains IDL procedures for 
FITS I/O written by Bill Thompson (Goddard/ARC).    This software can be used 
to read or write all types of FITS images and binary 
tables.    The one limitation of these procedures is that they do not handle 
ASCII tables.    Further information on these procedures is available in the 
LaTeX file fits_bintable.tex in the /text directory.

  (4) The five procedures FITS_CLOSE, FITS_HELP, FITS_OPEN, FITS_READ, and
FITS_WRITE were originally written by D. Lindler (NASA/Goddard) for handling
data from the Hubble Space Telescope (HST).     These
procedures are especially efficient for FITS files with many extensions, and
they also recognize conventions used in HST FITS files that are not part of the
standard FITS definition.   Like READFITS/WRITEFITS, these procedures require
further processing of binary and ASCII tables, by using procedures in the
/fits_table directory.   In particular, the procedure FTAB_EXT uses  FITS_READ
to provide a quick and easy extraction of FITS ASCII or Binary  tables into IDL
vectors.


A FITS header is a string array with 80 characters per line.    Using the IDL
PRINT command to display the header will result in a space between every
line.   The HPRINT procedure in the /misc directory will display a FITS
header without this extra space.

The procedures FITS_INFO and FITSDIR use the non-standard system variables 
!TEXTUNIT and !TEXTOUT.   These can be added to one's session using the 
procedure ASTROLIB in the /misc directory.

The available FITS I/O routines can be summarized as follows:          


      The procedure MRDFITS is a very generalized FITS reader that can read
      almost any type of FITS file, including random groups, and variable
      length binary tables.   ASCII and binary columns are directly mapped
      into the tags of an IDL structure.   MWRFITS() will write an IDL 
      structure to a FITS file.   

      READFITS() will read a disk FITS file into IDL data and header arrays. It
      handles all legal FITS formats except random groups.
      READFITS() also has STARTROW and NUMROW keywords to read
      selected rows from a primary image or extension.  
      READFITS() can  directly read a gzip or Unix compress file.   ASCII and 
      binary tables require further processing with the FT* and TB* procedures 
      in the /fits_bintable directory.     In particular, FTAB_EXT can be used 
      to extract IDL vectors from a FITS binary or ASCII table.  

      WRITEFITS will write an IDL data and header array to a disk FITS file.   
      It can handle all valid FITS formats except random groups.     Use the 
      /APPEND keyword to WRITEFITS to add a FITS extension to an existing FITS
       file (see Appendix 2).

      The procedure HEADFITS() can be used to read just the FITS header.

      The procedure FXREAD in /fits_bintable can be used as an alternative
      to READFITS for reading a primary array.    It is a procedure rather
      than a function and has the ability to read a subarray or every nth 
      pixel from the primary FITS array.    The procedure FXWRITE in 
      /fits_bintable  can be used as an alternative to WRITEFITS.

      The procedure FITS_INFO displays info about FITS file(s) in a 
      directory.  The info include the number of extensions, and the size
      and type of each header or array.    The procedure FITS_HELP displays
      more information about a FITS file and in nicer format, but only works 
      with one file at a time. The procedure FITSDIR displays
      selected keywords from the primary (or extension) headers in a set of 
      FITS files.

      The procedure RDFITS_STRUCT will read an entire FITS file into an IDL
      structure.   Each header, image or spectra, or table is placed into a 
      separate structure tag.

     Prior to November 2001, the IDL Astronomy Library contained a set of
     Unix-only procedures  for reading, listing and writing FITS files on tape
     devices.    These procedures are still available in
     ftp://idlastro.gsfc.nasa.gov/pub/obsolete and in the SolarSoft directory
     http://sohowww.nascom.nasa.gov/solarsoft/gen/idl/
*************************************************************************
APPENDIX 1 - Summary of FITS I/O procedures

Only procedures likely to be used at the IDL prompt level are listed

in pro/fits

CHECK_FITS - Check that the NAXISi and BITPIX keywords of a FITS header
             agree with a supplied array
FITS_CLOSE - Close a FITS file defined by a FITS Control Block (see FITS_OPEN)
FITS_HELP - Summarize the primary data units and extensions in a FITS file
FITS_OPEN - Open a FITS file and define a FITS Control Block (FCB)
FITS_READ - Read a FITS file specified by name or FITS Control Block (FCB)
FITS_WRITE - Write a FITS primary data unit or extension
FITS_INFO - Display info about disk FITS file(s) at a terminal or in Common 
FITSRGB_to_TIFF - Combine separate red, green, blue FITS files into TIFF format
FXPOSIT() - Open a FITS file positioned to beginning of a specified extension
HEADFITS() - Read a FITS header from a disk FITS file.
MKHDR - Make a minimal FITS header for an image array.
MODFITS - Modify the header or data in a FITS file (without changing the size)
MRDFITS() - Read all standard FITS data types into IDL arrays or structures
MRD_HREAD -  Like FXHREAD but can also read a FITS header from a Unix pipe
MWRFITS - Write a FITS file from an IDL array or structure
RDFITS_STRUCT - Read an entire disk FITS file into an IDL structure
READFITS() - Read a disk FITS file into an IDL data and header array.
SXADDHIST - Add history records to a FITS header
SXADDPAR - Add a keyword and value to a FITS  header
SXDELPAR -  Delete a keyword from a FITS header
SXPAR()  -   Obtain the value of a keyword in a FITS header
WRITEFITS - Write IDL data and header array to a disk FITS file.

in pro/fits_bintable

FXADDPAR - Add a keyword and value to a FITS header
FXPAR()  -   Obtain the value of a keyword in a FITS header
FXREAD -  Read a primary array or subarray from a FITS file
FXWRITE - Write primary FITS header and array to disk.

This directory contains numerous other procedures for reading and writing
FITS binary tables.    Note that FXADDPAR is essentially identical to SXADDPAR,
and that FXPAR() is essentially identical with SXPAR().

in pro/fits_table

FTAB_DELROW - Delete specified rows in a FITS table extension
FTAB_EXT - Extract specified columns of a FITS table extension into IDL vectors
FTAB_HELP - Print info about the fields in a FITS table extension
FTAB_PRINT - Print specified columns and rows of a FITS table extension

************************************************************
Appendix 2:  Writing an IMAGE Extensions with  WRITEFITS

The format of a FITS IMAGE extension duplicates that of a primary FITS array.  
Thus, unlike binary or ASCII tables extensions, a FITS IMAGE extension does 
not require any further interpretation after it has been read by READFITS with
the EXTEN keyword.    A FITS extension can be added to an existing FITS file 
by using WRITEFITS with the /APPEND keyword.     Users need to make sure that 
(1) the primary FITS header includes an EXTEND keyword, and (2) the required 
keywords of the IMAGE extension header match those described in the defining 
document.   One way to make sure that these criteria are met is to use the 
/EXTEND and /IMAGE keywords of the MKDHR procedure.

	For example,  to create a FITS file with a dummy primary header (i.e.
no primary array) and two IMAGE extensions containing the IDL arrays 
"im1" and "im2"  to a file 'test.fits'

        IDL> mkhdr,h,'',/EXTEN        ;Create a dummy header (NAXIS=0) and
                                      ;include an EXTEND keyword

	IDL> writefits,'test.fits','',h     ;Write the header to disk
                                            ;Since there is no primary image 
                                            ;it is set to ''

	IDL> mkhdr,h1,im1,/IMAGE      ;Make a minimal IMAGE extension header h1 
                                      ;appropriate to the array im1 using 
                                      ;Additional header keywords can be added
                                      ;using SXADDPAR

                                     
	IDL> writefits,'test.fits',im1,h1,/app  ;Append to existing FITS file

        IDL> mkhdr,h2,im2,/IMAGE       ;Create IMAGE header for im2

 	IDL> writefits,'test.fits',im2,h2,/APPEND    ;Append second extension

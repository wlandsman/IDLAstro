/SDAS                                                   Dec-2006

The procedures in this directory are useful for accessing image files used by
STSDAS (Space Telescope Science Data Analysis System), developed for the
analysis of images from HST (Hubble Space Telescope).     The appendix below
gives more information about this data format.   The use of the STSDAS format 
is being phased out; the second and third generation instruments on HST (STIS,
NICMOS, ACS) now use the FITS data format. 

The LaTex file idl_stsdas.tex in the /text directory gives  a detailed 
description of the IDL interface with STSDAS images and tables.   

The following procedures from the /FITS directory can also be used to
manipulate STSDAS headers

SXADDHIST - Add history records to a FITS or STSDAS header
SXADDPAR - Add a keyword and value to a FITS or STSDAS header
SXDELPAR -  Delete a keyword from a FITS or STSDAS header
SXPAR  -   Obtain the value of a keyword in a FITS or STSDAS header

. For reading WFPC2 images (in either FITS or STSDAS format) one
should use the procedure WFPC2_READ located in the /disk_io directory.

Procedures in /sdas
ST_DISKREAD - Read an HST FITS file and reconstruct a GEIS (STSDAS) file
SXGINFO -  Return info on all group parameters in a FITS header
SXGPAR() - Obtain group parameter values from a FITS header and parameter block
SXGREAD() -  Read group parameters from an STSDAS file
SXHCOPY -  Copy a selected portion of one header into another
SXHEDIT -  Interactively edit a STSDAS header using EDT or the default UNIX 
           editor           
SXHREAD -  Read a STSDAS header from disk
SXHWRITE - Write a STSDAS header to disk
SXMAKE -   Make a basic STSDAS header from an IDL array
SXOPEN -   Open an STSDAS (modified FITS) disk header for subsequent I/O
SXREAD() -   Read an STSDAS (modified FITS) image from disk
SXWRITE -  Write an image to STSDAS (modified FITS) format


**************************************************

APPENDIX  - STSDAS Format  

Closely related to FITS format is the STSDAS ( GEIS ) format.     This is data
format used by the Space Telescope Science Data  Analysis System, and it differs
from standard FITS in several ways (also see
:http://www.stsci.edu/instruments/wfpc2/Wfpc2_dhb/intro_ch24.html#1905747)

      (1)  The FITS header is kept as a separate ASCII '.hhh' file.   This 
           allows the header to be read outside of program control. 

      (2)  The data is stored in 512 byte records in an '.hhd' file, rather 
           than the FITS standard of 2880 bytes.   
  
      (3)  The binary data is stored in a format appropriate to the host
           machine, i.e. unlike a disk FITS file, an STSDAS file does not
           require byte swapping.

      (4)  STSDAS data files is in a "group" format which differs somewhat 
           from the standard FITS "group" format.  In particular, the STSDAS
           group parameters do not have to be the same data type as the data,
           and the group parameter block follows rather than precedes the data.
           If the data is not actually going to be used with STSDAS, the we
           suggest dropping the requirement that the data be "group" format.

 A bit of ancient history here.    Especially because of (2) and (3), the
 STSDAS format became popular with users of IDL Version 1 in the mid 1980s.   
 The Version 1 Users library (part of the standard IDL
 package) included many procedures, all beginning with 'ST' for accessing
 STSDAS files.  The procedure STREAD was even an intrinsic IDL function.   
 Unfortunately, in 1986 Space Telescope changed the position of their 
 group parameters, and a new set of procedures (now beginning with 'SX') was
 needed, and is available in this directory.   (Because STREAD was an
 intrinsic IDL function, a different prefix had to be used.)    Some of the SX
 procedures are generally useful for FITS I/O, in particular,  SXPAR to read 
 the value of a FITS keyword, and SXADDPAR to add a FITS keyword to a header. 


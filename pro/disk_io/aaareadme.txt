/DISK_IO                                                   Feb 2010

This subdirectory contains IDL procedures to read popular disk formats in
astronomy.  Currently available are procedures to access

     (1) IRAF image (.imh) files  (read and write)
     (2) Journal (ApJ, AJ) machine readable tables
     (3) WFPC2 images (FITS or STSDAS)
     

The procedures to access disk FITS files are kept in a separate subdirectory
/FITS, and those to access STSDAS image files are in the subdirectory /SDAS.

The procedure IRAFDIR uses the non-standard system variables !TEXTOUT and
!TEXTUNIT.   These can be added to one's session using the procedure
ASTROLIB.


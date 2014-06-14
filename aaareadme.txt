THE IDL ASTRONOMY USER'S LIBRARY (April 2014)

The IDL Astronomy Users Library is a central repository for general purpose 
astronomy procedures written in IDL.    The library currently contains
500+ procedures including general FITS I/O, interfaces with STSDAS and IRAF,
astronomical utilities, and photometry and spectral analysis procedures.  
The library is not meant to be an integrated package, but rather is a
collection of procedures  from which users can pick and choose (and possibly
modify) for their own use. Submitted procedures are given a cursory testing,
but are basically stored in the library as submitted.     The IDL
Astronomy User's Library was funded through November 2000 under the NASA 
Astrophysics Data program.

The homepage for the IDL Astronomy Library is  http://idlastro.gsfc.nasa.gov.
There is no longer any FTP access and downloads must be performed from the Web
or using WGET.     The default version of the Library requires IDL V6.4,
although earlier frozen versions are available in the old/ directory.

The success of the IDL Astronomy User's Library depends upon the
willingness of users to give as well as take.   Submission of relevant
procedures is strongly encouraged.  Equally important is the notification 
(or correction) of programming bugs or documentation errors.

I will post news about major updates to the comp.lang.idl-pvwave 
Usenet newsgroup.

Questions about the IDL Astronomy Library can be addressed to 
Wayne Landsman     Wayne.Landsman@nasa.gov
                   (301)-286-3625


The directory contains the following files

  aaareadme.txt - this file
  astron.tar.gz - a gzip'ed Unix tar file containing all Library procedures 
        text files, and data files.    
  astron.dir.tar.gz - This file is an alternative to astron.tar.gz.   It 
        contains the same files as astron.tar.gz but maintains the library
        procedures in their respective sub-directories.
  astron.zip - A .zip version of the Astronomy Library, but with X-windows-only
        procedures (e.g. curs.pro) removed
  contents.txt - an ASCII file giving one-line descriptions of all
        500+ procedures currently in the Library.   This listing is also 
        available at http://idlastro.gsfc.nasa.gov/contents.html
  coyote_astron.tar.gz - A gzip'ed tar file of the Coyote library procedures 
        used by the Astronomy library.    This file is an alternative to 
	downloading the entire Coyote library at 
	http://www.idlcoyote.com/documents/programs.php		
  guidelines.txt - Suggested programming guidelines for Astronomy library
        procedures.
  news.txt - an ASCII file listing all additions or changes to the Library 
        in the past 6 months in reverse chronological order.  This file 
        should be checked periodically as new and modified procedures are added
        to the Library.   Also availabile as an HTML file at 
        http://idlastro.gsfc.nasa.gov/news.html

The following subdirectories are available

  text - contains a collection of ASCII and LaTex files concerning various 
            categories of IDL procedures.
  coyote - Contains Coyote Library routines which are needed by at least one 
         Astronomy Library routine.     These are *not* included in the Astron
	 Library zip or tar files.    
  data - contains data files used by a couple of Library procedures.   *Due to
         their size, the files testpo.405 and JPLEPH.405 are not included in
         the .tar and.zip files.    This environment variable ASTRO_DATA should
         point to this directory.
  markwardt - Contains the procedures from the Markwardt library which are used
         by SOLVE_ASTRO.     These are *not* included in the Astron library zip
         zip or tar files.
  obsolete - repository for procedures removed from the Library because their 
         use has diminished or their functionality has been replaced by other
         procedures.
  pro  - Contains all the Library procedures as individual ASCII files.
            These procedures are placed in subdirectories according to their
            category, e.g. pro/fits, pro/sdas, pro/idlphot
  old - Contains tar files of frozen versions of the IDL Astronomy Library 
        compatible with earlier IDL versions
  v8 - A beta test directory of procedures using new features in IDL V8
  zdbase - Contains compressed binary tar files of popular astronomical
           catalogs formatted as IDL databases.   See the file
           zdbase/aaareadme.txt for more info. 
  contrib - contains self-contained IDL astronomy-related packages that
           are *not* part of the standard astronomy library distribution.
           See contrib/aaareadme.txt for more info.




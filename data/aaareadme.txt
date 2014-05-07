                                                            Dec 2006

This directory contains any  data files which are used by procedures in the IDL
astronomy library.   These procedures look for these data files in a directory
with the  environment variable name ASTRO_DATA.

As of May 2002, this directory contains two files 


1.   JPLEPH.405 - FITS file contain the JPL DE405 ephemeris (chebychev
	   polynomials based upon the International Celestial  Reference Frame
	   (ICRF).    Because of the size of this file it is not distributed
           along with the standard IDL Astronomy Library tar or zip files.

2.   testpo.405 -- an ASCII file containing sample ephemeris test data for use
           the IDL jpl* ephemeris procedure 


Because of the large size of JPLEPH.405 and testpo.405 they are not included
with the IDL Astronomy Library .tar or .zip files.   Users who need these files
should instead fetch them from http://idlastro.gsfc.nasa.gov/ftp/data/ .

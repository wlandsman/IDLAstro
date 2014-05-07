DAOPHOT-type Photometry                                  March 2008

  These are a set of IDL procedures adapted from an early FORTRAN
  version of DAOPHOT aperture photometry.   The creators of DAOPHOT
  have no responsibility whatsoever for the IDL code.  The IDL code
  will give similar, but not identical, results as the original FORTRAN.
  A LaTex file daophot.tex  in /text supplies further documentation for 
  the IDL-DAOPHOT procedures for CCD images.    The PSF fitting portion of the 
  code (e.g. nstar.pro) is now fairly obsolete, but the routines for source
  detection, aperture photometry and sky level determination have been kept
  up to date.

Changes: 

 March 2008: GCNTRD - Modified to match IRAF/DAOFIND and use a more accurate
    (though possibly less robust) 
             FIND - Now uses the Gaussian fits to the marginal X & Y
	     distributions (as in GCNTRD) rather than finding where the
	     derivative goes to zero.

 June 2004: SKY,MMM updated to better match more recent versions of DAOPHOT

 June 2004: the procedure GCNTRD was added to determine centroids using
 Gaussian fits to the marginal X and Y distributions.    This is similar to the
 method used in current DAOPHOT versions, and allows the user to ignore possible
 bad pixels.     (Very early -- pre-1987 -- versions of DAOPHOT used the 
 centroid algorithm in CNTRD.PRO where the centroid is located where the X
 and Y derivatives go to zero.)

 June 2000: the procedure aper.pro was modified to allow it to compute the
 exact area of the intersection of a circle with square pixels. 

 July 1997: the procedures were modified so that the PSF residuals are
 written to a FITS file, rather than a STSDAS file.    To convert a PSF 
 file 'psfname' created earlier in STSDAS format, use the following commands:
	IDL> sxopen,1,'psfname',h
	IDL> psf = sxread(1)
	IDL> writefits,'psfname.fits',psf,h 


 May 1996:  the following updates were made to the code
       (1) Non-standard system variables are no longer used.   The PRINT 
           keyword is used instead of !TEXTOUT, and the DEBUG keyword is used
           instead of !DEBUG.
       (2) The T_* procedures now request the *name* of a disk FITS ASCII table
           for storing the input and output results.  
       (3) NSTAR now has a /VARSKY keyword to allow the skylevel to vary.

   

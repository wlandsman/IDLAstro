/astrom                                                   November 2017

This directory contain IDL procedures for manipulating FITS images which include
header keywords specifying the coordinate system.   The FITS header must follow
the world coordinate (WCS) specification in the paper "Representations of
Celestial Coordinates in FITS"  by Eric Greisen and Mark Calabretta (2002, A&A,
395, 1061).   This paper and other WCS documentation may be obtained from    
http://fits.gsfc.nasa.gov/fits_wcs.html.  The IDL code has been  validated using
the test images on
http://www.atnf.csiro.au/people/mcalabre/WCS/sample_data/index.html 

Bill Thompson has written a different IDL implementation of the WCS
specification which also includes support for solar and spectroscopic
coordinates.    His routines are available at the SolarSoftWare (SSW) library
at     http://sohowww.nascom.nasa.gov/solarsoft/gen/idl/wcs/

The FITS convention defines the center of the first pixel in the
array to have coordinates [1,1].    This differs from the IDL convention where
the first pixel has coordinates ]0,0].    Whenever an [X,Y] position is
extracted or inserted into a FITS header then this difference must be accounted
for.

Several procedures use an IDL structure that contains astrometry information in
the following tags:

  .NAXIS - 2 element long vector giving dimensions of the images
  .CD	-  2 x 2 array containing the astrometry parameters CD1_1 CD1_2
	   in DEGREES/PIXEL				      CD2_1 CD2_2
  .CDELT - 2 element vector giving physical increment at the reference pixel
  .CRPIX - 2 element vector giving X and Y coordinates of reference pixel
	   (def = NAXIS/2) in FITS convention (first pixel is 1,1)
  .CRVAL - 2 element double precision vector giving R.A. and DEC of 
	 reference pixel in DEGREES
  .CTYPE - 2 element string vector giving projection types, default
	 ['RA---TAN','DEC--TAN']
  .LONGPOLE - scalar longitude of north pole (default = 180)
  .LATPOLE - scalar giving native latitude of the celestial pole default=0)
  .PV2 - Vector of parameters (PV2_1, PV2_2...) needed in some projections 
  .DISTORT - Optional substructure giving distortion parameters.   Currently
         only implemented for the Spitzer simple imaging polynomial (SIP) see
         http://fits.gsfc.nasa.gov/registry/sip.html

In July 2013, Paddy Leahy (Jodrell Bank) updated the astrometry structure to
include additional information, and allow for a more complete
implementation of the WCS standard.    The relevant IDL procedures 
were updated to work with the new astrometry structure, but to also be
completely backwards compatible.    The new astrometry tags include the
following:
	 
      .PV1 - Vector of projection parameters associated with longitude axis
      .AXES  - 2 element integer vector giving the FITS-convention axis 
               numbers associated with astrometry, in ascending order. 
               Default [1,2].
      .REVERSE - byte, true if first astrometry axis is Dec/latitude
      .COORD_SYS - 1 or 2 character code giving coordinate system, including
                 'C' = RA/Dec, 'G' = Galactic, 'E' = Ecliptic, 'X' = unknown.
      .PROJECTION  - 3-letter WCS projection code
      .KNOWN    - true if IDL WCS routines recognise this projection
      .RADECSYS - String giving RA/Dec system e.g. 'FK4', 'ICRS' etc.
      .EQUINOX  - Double giving the epoch of the mean equator and equinox
      .DATEOBS  - Text string giving (start) date/time of observations
      .MJDOBS   - Modified julian date of start of observations.
      .X0Y0     - Implied offset in intermediate world coordinates (x,y)
                  if a non-standard fiducial point is set via PV1 and also
                  PV1_0a =/ 0, indicating that an offset should be
                  applied to place CRVAL at the (x,y) origin.
                  Should be *added* to the IWC derived from application of
                  CRPIX, CDELT, CD to the pixel coordinates.
	 
The procedures EXTAST and PUTAST can be used, respectively, to extract
astrometry information from a FITS header and place it into an astrometry
structure, and to put the information from an astrometry structure into a FITS
header.  

      IDL> extast, hdr, astr     ;Extract astrometry from a FITS header
      IDL> putast, hdr, astr     ;Put astrometry info into a FITS header 

(1) Following the discussion in Section 5 of the Greisen & Calabretta paper,
this software supports three WCS formats: (1) the PC matrix with the separate
CDELT parameters (preferred), (2) the CD matrix without CDELT parameters (IRAF
standard),  and (3) the old-style AIPS (CDELT + CROTA) notation. The CDELT tag
is not used in format (2), and is normally set to [1.0, 1.0].    However, for
an AIPS-style header, the CDELT values are stored in the CDELT tag, and the
CROTA value is stored in the CD matrix     

cd = [ [ cos(crota), -sin(crota) ] , [ sin(crota), cos(crota)] ] 

If either element of the CDELT tag differs from 1.0 then this signals that the
astrometry came from an AIPS type header.    Programs using the astrometry
structure should always use the following code 

 if N_elements(CDELT) GE 2 then if (cdelt[0] NE 1.0) then begin
        cd[0,0] = cd[0,0]*cdelt[0] & cd[0,1] =  cd[0,1]*cdelt[0]
        cd[1,1] = cd[1,1]*cdelt[1] & cd[1,0] =  cd[1,0]*cdelt[1]
 endif

(2) The _flt files for the ACS and WFC3 instruments on the Hubble Space Telescope (HST)
include distortion lookup tables for high precision coordinates.    These lookup tables
are located in separate FITS extensions.    Thus it is not sufficient to supply only a FITS
header, but the entire (mult-extension) FITS file is required for astrometry.   This is 
done with the procedures FITS_XYAD and FITS_ADXY 

(3) The CRPIX value is stored in the astrometry structure the same way as it is
in the FITS header, i.e. in FORTRAN (first pixel is 1) convention.    

(4) The procedures HREBIN and HCONGRID will update the astrometry, including the
SIP distortion coefficients,  when expanding or contracting an image.   However,
note that HROT or HROTATE currently will not update any SIP coefficient.

(5) Very old draft versions of the World Coordinate System paper represented the
CD matrix in a FITS header with names such as 'CD001001' rather than 'CD1_1', and
'PC001001' instead of 'PC1_1'.   This notation is no longer recognized, but the
procedure FITS_CD_FIX in the /obsolete directory can be used to convert the FITS header 
to modern notation.

Special astrometry procedures are required for the Digitized Sky Survey Images (
http://archive.stsci.edu/dss/ ).   The Guidestar images can also be obtained
from the SKYVIEW facility at http://skyview.gsfc.nasa.gov/skyview.html.  The
Schmidt plates used in this survey have a highly nonlinear plate solution that
is not covered by the Greisen & Calabretta conventions.     The procedures
GSSSEXTAST,  GSSSXYAD and GSSSADXY are the Guidestar survey analogues of the
procedures EXTAST, XY2AD, and ADXY for standard astrometry.     All the
astrometry procedures in the library will test for a Guidestar image header (by
looking for the 'PPO1' keyword) and call the appropriate procedures.    The
procedure GSSS_STDAST will convert the astrometry in a guidestar image header
to  an standard tangent projection with very slightly degraded accuracy.       

A couple of procedures in other directories also use the FITS world coordinate
system including
  IMCONTOUR - (in /astro) Contour plots with astronomical labeling (either 
               RA,Dec or arc distance from the image center
  IMDBASE - (in /database) Find all catalog sources within the field of an 
            image (given a FITS header with astrometry)

ADD_DISTORT - Add a distortion structure into a FITS header
AD2XY - Use astrometry structure to convert celestial to pixel coordinates 
ADXY  - Use FITS header to convert celestial (RA,Dec) to pixel coordinates
CONS_DEC() - Obtain the X and Y coordinates of a line of constant 
        declination
CONS_RA() - Obtain the X and Y coordinates of a line of constant right
        ascension
EXTAST- EXTract ASTrometry parameters from a FITS header into an IDL structure
FITS_ADXY - Use a FITS file with astrometry to convert world coordinates to pixel (X,Y)
FITS_XYAD - Use a FITS file with astrometry to convert pixel (X,Y) to world coordinates
GET_EQUINOX() - Return a numeric equinox value from a FITS header
GETROT - GET ROTation and plate scale from a FITS header
GSSS_STDAST - Insert the closest tangent projection astrometry into an STScI
         Guidestar Survey Image
GSSSADXY - Convert RA, Dec to pixel coordinates for an STScI survey image
GSSSEXTAST - Extract astrometry parameters from an STScI Survey Image
GSSSXYAD - Convert pixel coordinates to RA, Dec for an STScI survey image
HASTROM - Rotate, Congrid, and/or shift an image until astrometry matches
        that in a reference FITS header.  Used to align images.
HBOXAVE - Boxaverage an image and update astrometry in a FITS header
HCONGRID - CONGRID an image and update astrometry in a FITS header
HEULER - Convert between Galactic, celestial and ecliptic coordinates in FITS a
header
HEXTRACT - Extract a subimage and update astrometry in a FITS header
HPRECESS - Precess the astrometry in a FITS header to a new equinox.
HREBIN - REBIN an image and update the astrometry in a FITS header
HREVERSE - Reverse an image about either dimension and update astrometry
        in a FITS header                                    
HROT  - Rotate an image and update astrometry in a FITS header.
HROTATE - Apply IDL ROTATE function and update astrometry in a FITS header
MAKE_ASTR - Build an astrometry structure from input parameter values
PRECESS_CD - Precess coordinate description (CD) matrix in a FITS header 
          to a new equinox.   Called by HPRECESS
PUTAST - Put astrometry parameters (e.g. rotation, plate scale) into a FITS header. 
SIP_EVAL() - Compute distorted coordinates given SIP (simple imaging polynomial)
coefficients
SOLVE_ASTRO - Solve for an TANgent-plane astrometric plate solution with optional
 distortion terms
STARAST - Obtain an exact astrometry solution given the coordinates and
        plate position of 2 or 3 stars.
TNX_EVAL() - Compute distorted coordinates given IRAF TNX projection
coefficients 	
TPV_EVAL() - Compute distorted coordinates given TPV (Tangent + PV_ polynomial)
coefficients	
UPDATE_DISTORT - Update SIP distortion coefficients for a linear transformation	
WCS_CHECK_CTYPE - Checks that a pair of CTYPE parameters conform to WCS format
and return the projection type
WCS_GETPOLE -  Compute the coordinates of the native pole for non-polar projection
WCSSPH2XY - Convert between longitude,latitude to X,Y angular coordinates for
        25 different map projection types
WCSXY2SPH - Inverse of WCSSPH2XY
WCS_DEMO - Demo program for WCSSPH2XY and WCSXY2SPH
WCS_ROTATE - Rotate between standard (e.g. celestial) and native coordinates
WFPC2_METRIC -  Compute the distortion in a WFPC2 image and return coordinates
XYAD  - Use FITS header to convert pixel (X,Y) to celestial(RA, Dec) coordinates
XY2AD - Use astrometry structure to convert pixel to celestial coordinates
XYXY -  Convert X,Y values on one image to X,Y values in another image
         using astrometry in the FITS headers

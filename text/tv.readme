TV Display Procedures                       pro/tv             August 2000

The procedures in this directory involve the use of direct graphics or the
image display.

BLINK - Blink two or more windows in an image display
CURS - Change the shape of the (X windows only) cursor
CURVAL - Interactive display of image intensities and astronomical coordinates
PIXCOLOR - Set specified pixel values to a specified color
SIGRANGE() - Find range of pixel values which contain 99% of the image values
TVBOX - Draw a box of specified size on the image display
TVCIRCLE - Draw a circle of specified radius on the current device
TVELLIPSE - Draw an ellipse of specified axes on the current device
TVLASER - Write an image to postscript file with annotation from a FITS header
TVLIST - Display intensity values surrounding the cursor position
UNZOOM_XY - Convert from window coordinates to image coordinates
ZOOM_XY - Convert from image coordinates to window coordinates

Notes:

TVLIST requires the use of the non-standard system variable !TEXTOUT.
This can be added to one's session using the astronomy library procedure
ASTROLIB.

To display astronomical coordinates with CURVAL, one must have a FITS header
with astrometry information.  Several procedures from the ASTROM directory
are used.

Users probably want to tailor TVLASER to choose their preferred FITS
keywords for supplying annotation

Bill Thompson has written an additional set of image display routines that are
not part of the standard IDL astronomy library but which are available in the
solar IDL Library at 
http://sohowww.nascom.nasa.gov/solarsoft/gen/idl/image/.    See the
aaareadme.txt file in that directory for more info.

The version of SIGRANGE in this directory differs slightly from that found in
the solar Library, in that it does not use the non-standard !MISSING system
variable.

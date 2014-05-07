 PRO FITSRGB_to_TIFF, path, rgb_files, tiff_name, BY_PIXEL=by_pixel, $
                      PREVIEW=preview, RED=r_mix, GREEN=g_mix, BLUE=b_mix
;+
; NAME:
;       FITSRGB_to_TIFF
; PURPOSE:
;       Combine separate red, green, and blue FITS images into TIFF format
; EXPLANATION:
;       The output TIFF (class R) file can have colors interleaved either
;       by pixel or image.  The colour mix is also adjustable.
;
; CALLING SEQUENCE:
;       FITSRGB_to_TIFF, path, rgb_files, tiff_name [,/BY_PIXEL, /PREVIEW,
;                         RED= , GREEN =, BLUE =]
;
; INPUTS:
;       path = file system directory path to the RGB files required.
;       rgb_files = string array with three components - the red FITS file
;                   filename, the blue FITS file filename and the green FITS
;                   file filename
;
; OUTPUTS:
;       tiff_name = string containing name of tiff file to be produced
;
; OPTIONAL OUTPUT:
;       Header = String array containing the header from the FITS file.
;
; OPTIONAL INPUT KEYWORDS:
;       BY_PIXEL = This causes TIFF file RGB to be interleaved by pixel
;                  rather than the default of by image.
;       PREVIEW  = Allows a 24 bit image to be displayed on the screen
;                  to check the colour mix.
;       RED = Real number containing the fractional mix of red
;       GREEN = Real number containing the fractional mix of green
;       BLUE = Real number containing the fractional mix of blue
;
; EXAMPLE:
;       Read three FITS files, 'red.fits', 'blue.fits' and 'green.fits' from
;       the directory '/data/images/space' and output a TIFF file named
;       'colour.tiff'
;
;               IDL> FITSRGB_to_TIFF, '/data/images/space', ['red.fits', $
;                    'blue.fits', 'green.fits'], 'colour.tiff'
;
;       Read three FITS files, 'red.fits', 'blue.fits' and 'green.fits' from
;       the current directory and output a TIFF file named '/images/out.tiff'
;       In this case, the red image is twice as strong as the green and the
;       blue is a third more intense.  A preview on screen is also wanted.
;
;               IDL> FITSRGB_to_TIFF, '.', ['red.fits', $
;                    'blue.fits', 'green.fits'], '/images/out.tiff', $
;                    /PREVIEW, RED=0.5, GREEN=1.0, BLUE=0.666
;
;
; RESTRICTIONS:
;       (1) Limited to the ability of the routine READFITS
;
; NOTES:
;       None
;
; PROCEDURES USED:
;     Functions:   READFITS, CONCAT_DIR
;     Procedures:  WRITE_TIFF
;
; MODIFICATION HISTORY:
;     16th January 1995 - Written by Carl Shaw, Queen's University Belfast
;	27 Jan 1995 - W. Landsman, Add CONCAT_DIR for VMS, Windows compatibility
;	Converted to IDL V5.0   W. Landsman   September 1997
;    Use WRITE_TIFF instead of obsolete TIFF_WRITE  W. Landsman  December 1998
;    Cosmetic changes  W. Landsman    February 2000
;-
;
;  Make sure user has supplied valid parameters
;
 IF N_PARAMS() LT 3 THEN BEGIN
      print, 'Syntax -  FITSRGB_to_TIFF, path, rgb_files, tiff_name'
      print,'                     [/BY_PIXEL,/PREVIEW, RED=, GREEN=, BLUE= ]'
      return
 ENDIF
;
 IF N_ELEMENTS(rgb_files) LT 3 THEN $
  MESSAGE, 'Three filenames for the colour components have not been supplied'
;
  by_pixel =  KEYWORD_SET(BY_PIXEL)
;
 IF ~KEYWORD_SET(r_mix) THEN r_mix = 1.0
 IF ~KEYWORD_SET(g_mix) THEN g_mix = 1.0
 IF ~KEYWORD_SET(b_mix) THEN b_mix = 1.0
;
;  Now load the colour components
;
 fname = CONCAT_DIR( path, rgb_files )
 red = READFITS( fname[0], /SILENT)
 green = READFITS( fname[1], /SILENT)
 blue = READFITS( fname[2], /SILENT)
;
;  Data now needs to be scaled to the same byte range (0-255) and also
;  scaled according to the RGB mix values supplied by the user
;
 red = red[*,*] * r_mix
 green = green[*,*] * g_mix
 blue = blue[*,*] * b_mix          ;scale intensity by supplied mix
;
 maxlim = MAX(red) > MAX(green) > MAX(blue)   ;max intensity
 minlim = MIN(red) < MIN(green) < MIN(blue)   ;min intensity
 red = BYTSCL(red, MIN=minlim, MAX=maxlim)
 green = BYTSCL(green, MIN=minlim, MAX=maxlim)
 blue = BYTSCL(blue, MIN=minlim, MAX=maxlim)  ;scale colours to same byte range
;
;  Preview image on window system if required
;
 IF keyword_set(PREVIEW) THEN BEGIN
  window, 0, colors=256
  wset, 0
  tv, color_quan(red, green, blue, r, g, b, colors=255)
  tvlct, r, g, b
 ENDIF
;
; Now write out result as a tiff file
;
 IF by_pixel THEN BEGIN
  ;
  ;  Interleave by pixel
  ;
  extent = SIZE(red)
  xsize = extent[1]
  ysize = extent[2]                       ;get image size
  interarr = FLTARR(3, xsize, ysize, /NOZERO)      ;make interleaved array
  interarr[0, *, *] = red
  interarr[1, *, *] = green
  interarr[2, *, *] = blue
  ;
  WRITE_TIFF, tiff_name, interarr
  ;
 ENDIF ELSE BEGIN
  ;
  ;  Interleave by image
  ;
  WRITE_TIFF, tiff_name, RED=red, BLUE=blue, GREEN=green, PLANARCONFIG=2
  ;
 ENDELSE
;
 END

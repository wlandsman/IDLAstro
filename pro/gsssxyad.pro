pro GSSSxyad, gsa, xin, yin, ra, dec, PRINT = print
;+ 
; NAME:
;       GSSSXYAD
; PURPOSE:
;       Convert (X,Y) coordinates in a STScI Guide Star image to RA and Dec
; EXPLANATION:
;       The sky coordinates may be printed and/or returned in variables.
;
; CALLING SEQUENCE:
;       GSSSxyad, gsa, x, y, ra, dec, [ /PRINT ]
; INPUT:
;       GSA  - The GSSS Astrometry structure extracted from a FITS header 
;              by GSSSEXTAST
;       X - The X pixel coordinate(s) of the image, scalar or vector
;       Y - The Y pixel coordinate(s) of the image, scalar or vector
;
; OUTPUT:
;       RA - The RA coordinate of the given pixel(s) in *degrees*
;       DEC - The DEC coordinate of the given pixel(s) in *degrees*
;
;       Both RA and Dec will be returned as double precision
;
; OPTIONAL KEYWORD INPUT:
;       /PRINT - If this keyword is set and non-zero, then coordinates will be
;               displayed at the terminal
; EXAMPLE:
;       Given a FITS header,hdr, from a GSSS image, print the astronomical
;       coordinates of (X,Y) = (200.23, 100.16) at the terminal
;
;       IDL> GSSSExtast, hdr, gsa        ;Extract astrometry structure
;       IDL> GSSSxyad, gsa, 200.23, 100.16, /print
;
; NOTES:
;       For most purpose users can simply use XYAD, which will call GSSSXYAD
;       if it is passed a GSSS header.
;
; PROCEDURES CALLED:
;       ASTDISP - print RA, Dec in a standard format
; HISTORY:
;       01-JUL-90 Version 1 written by Eric W. Deutsch
;       Vectorized Code   W. Landsman        March, 1991
;       14-AUG-91 Fixed error which caused returned RA and DEC to be off by
;       -.5 pixels in both X,Y.  Now X,Y follows same protocol as ADXY.
;       20-AUG-91 Modified to use AstDisp procedure.
;       June 94 Added /PRINT keyword instead of PRFLAG W. Landsman June 94
;       Converted to IDL V5.0   W. Landsman   September 1997
;       29-JUN-99 Added support for AMD[X,Y]1[2-3] for DSS images by E. Deutsch
;-

  arg = N_params()
  if (arg lt 3) then begin
    print,'Syntax - GSSSXYAD, GSSS_Astrom_struct, x, y, ra, dec, [/PRINT ]'
    return
    endif

  x = xin + 0.5 & y = yin + 0.5
  obx = ( gsa.ppo3-(gsa.xll+X)*gsa.xsz )/1000.0d0
  oby = ( (gsa.yll+Y)*gsa.ysz-gsa.ppo6 )/1000.0d0

  xi=gsa.amdx[0]*obx+               $
     gsa.amdx[1]*oby+               $
     gsa.amdx[2]+                   $
     gsa.amdx[3]*obx^2+             $
     gsa.amdx[4]*obx*oby+           $
     gsa.amdx[5]*oby^2+             $
     gsa.amdx[6]*(obx^2+oby^2)+     $
     gsa.amdx[7]*obx^3+             $
     gsa.amdx[8]*obx^2*oby+         $
     gsa.amdx[9]*obx*oby^2+         $
     gsa.amdx[10]*oby^3+               $
     gsa.amdx[11]*obx*(obx^2+oby^2)+   $
     gsa.amdx[12]*obx*(obx^2+oby^2)^2

  eta=gsa.amdy[0]*oby+              $
      gsa.amdy[1]*obx+              $
      gsa.amdy[2]+                  $
      gsa.amdy[3]*oby^2+            $
      gsa.amdy[4]*oby*obx+          $
      gsa.amdy[5]*obx^2+            $
      gsa.amdy[6]*(obx^2+oby^2)+    $
      gsa.amdy[7]*oby^3+            $
      gsa.amdy[8]*oby^2*obx+        $
      gsa.amdy[9]*oby*obx^2+        $
      gsa.amdy[10]*obx^3+               $
      gsa.amdy[11]*oby*(obx^2+oby^2)+   $
      gsa.amdy[12]*oby*(obx^2+oby^2)^2

  twopi = 2.0d*!DPI
  radeg = 180.0d/!DPI
  arcsec_per_radian = 360.*60.*60./twopi
  pltra = gsa.crval[0]/radeg
  pltdec = gsa.crval[1]/radeg

  xi = xi/arcsec_per_radian
  eta = eta/arcsec_per_radian

  numerator = xi/cos(pltdec)
  denominator = 1.0-eta*tan(pltdec)
  ra = atan(numerator,denominator)+pltra

  bad = where(ra LT 0,nbad)
  if (nbad GT 0) then ra[bad] = ra[bad]+twopi
  bad = where(ra GT twopi,nbad)
  if (nbad GT 0) then ra[bad] = ra[bad]-twopi

  numerator = cos(ra-pltra)
  denominator = (1.0-eta*tan(pltdec))/(eta+tan(pltdec))
  dec = atan(float(numerator/denominator))

  ra = ra*radeg
  dec = dec*radeg
  if keyword_set(PRINT) then AstDisp, xin, yin, ra, dec

  return
  end

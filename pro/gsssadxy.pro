pro GSSSadxy,gsa,ra,dec,x,y, PRINT = print
;+
; NAME:
;      GSSSADXY
; PURPOSE:
;       Converts RA and DEC (J2000) to (X,Y) for an STScI GuideStar image.   
; EXPLANATION:
;       The sky coordinates may be printed and/or returned in variables.
;
; CALLING SEQUENCE:
;       GSSSADXY, GSA, Ra,Dec, [ X, Y, /Print ] 

; INPUT:
;       GSA - the GSSS Astrometry structure created by GSSSEXTAST
;       RA  - the RA coordinate(s) in *degrees*, scalar or vector
;       DEC - the DEC coordinate(s) in *degrees*, scalar or vector
;
; OPTIONAL KEYWORD INPUT:
;       /PRINT - If this keyword is set and non-zero, then coordinates will be
;               displayed at the terminal
; OUTPUT:
;       X - the corresponding X pixel coordinate(s), double precision
;       Y - the corresponding Y pixel coordinate(s), double precision
;
;       X and Y will be in IDL convention (first pixel 0,0)
; EXAMPLE:
;       Given a FITS header, hdr, from the STScI Guidestar Survey, determine
;       the X,Y coordinates of 3C 273 (RA = 12 29 6.7  +02 03 08)
;
;       IDL> GSSSEXTAST, hdr, gsa          ;Extract astrometry structure
;       IDL> GSSSADXY, gsa, ten(12,29,6.7)*15,ten(2,3,8),/print
;
; NOTES:
;       For most purpose users can simply use ADXY, which will call GSSSADXY
;       if it is passed a GSSS header.
;
; PROCEDURES CALLED:
;       ASTDISP - Print RA, Dec in standard format
; HISTORY:
;       10-JUL-90 Version 1 written by Eric W. Deutsch
;               Derived from procedures written by Brian McLean
;       Vectorized code   W. Landsman        March, 1991
;       14-AUG-91 Fixed error which caused returned X and Y to be .5 pixels too
;               large.  Now X,Y follows same protocol as ADXY.
;       June 1994 - Dropped PRFLAG parameter, added /PRINT  W. Landsman (HSTX)
;       Converted to IDL V5.0   W. Landsman   September 1997
;       29-JUN-99 Added support for AMD[X,Y]1[2-3] for DSS images by E. Deutsch
;       Reduce memory requirements for large arrays D. Finkbeiner April 2004
;       Remove 
;-
  On_error,2
  arg = N_params()
  if (arg lt 5) then begin
    print,'Syntax - GSSSADXY, GSSS_Astrom_struct, ra, dec, x, y, print_flag'
    print,'e.g.: IDL> GSSSADXY, gsa, ra, dec, x, y, 1'
    return
    endif

; Set Constants
  iters = 0 & maxiters=50 & tolerance=0.0000005
  radeg = 180.0d/!DPI  & arcsec_per_radian= 3600.0d*radeg

  pltdec = gsa.crval[1]/radeg

  dec_rad = dec/radeg 
  cosd = cos(dec_rad)
  sind = sin(temporary(dec_rad))
  ra_dif = ra/radeg - gsa.crval[0]/radeg

  div = ( sind*sin(pltdec) + cosd*cos(pltdec)*cos(ra_dif))
  xi = cosd*sin(ra_dif)*arcsec_per_radian/div
  eta = ( sind*cos(pltdec)-cosd*sin(pltdec)*cos(ra_dif))* $
    (arcsec_per_radian/temporary(div))
  ra_dif = 0
  cosd = 0 & sind = 0
  
  obx = xi/gsa.pltscl
  oby = eta/gsa.pltscl

  repeat begin
    iters++

    f= gsa.amdx[0]*obx+                 $
       gsa.amdx[1]*oby+                 $
       gsa.amdx[2]+                     $
       gsa.amdx[3]*obx*obx+             $
       gsa.amdx[4]*obx*oby+             $
       gsa.amdx[5]*oby*oby+             $
       gsa.amdx[6]*(obx*obx+oby*oby)+   $
       gsa.amdx[7]*obx*obx*obx+         $
       gsa.amdx[8]*obx*obx*oby+         $
       gsa.amdx[9]*obx*oby*oby+         $
       gsa.amdx[10]*oby*oby*oby+             $
       gsa.amdx[11]*obx*(obx*obx+oby*oby)+   $
       gsa.amdx[12]*obx*(obx*obx+oby*oby)^2

    fx=gsa.amdx[0]+                     $
       gsa.amdx[3]*2.0*obx+             $
       gsa.amdx[4]*oby+                 $
       gsa.amdx[6]*2.0*obx+             $
       gsa.amdx[7]*3.0*obx*obx+         $
       gsa.amdx[8]*2.0*obx*oby+         $
       gsa.amdx[9]*oby*oby+             $
       gsa.amdx[11]*(3.0*obx*obx+oby*oby)+   $
       gsa.amdx[12]*(5.0*obx^4 + 6.0*obx^2*oby^2 + oby^4)

    fy=gsa.amdx[1]+                     $
       gsa.amdx[4]*obx+                 $
       gsa.amdx[5]*2.0*oby+             $
       gsa.amdx[6]*2.0*oby+             $
       gsa.amdx[8]*obx*obx+             $
       gsa.amdx[9]*obx*2.0*oby+         $
       gsa.amdx[10]*3.0*oby*oby+        $
       gsa.amdx[11]*2.0*obx*oby+        $
       gsa.amdx[12]*(4.0*obx^3*oby + 4.0*obx*oby^3)


    g= gsa.amdy[0]*oby+                 $
       gsa.amdy[1]*obx+                 $
       gsa.amdy[2]+                     $
       gsa.amdy[3]*oby*oby+             $
       gsa.amdy[4]*oby*obx+             $
       gsa.amdy[5]*obx*obx+             $
       gsa.amdy[6]*(obx*obx+oby*oby)+   $
       gsa.amdy[7]*oby*oby*oby+         $
       gsa.amdy[8]*oby*oby*obx+         $
       gsa.amdy[9]*oby*obx*obx+         $
       gsa.amdy[10]*obx*obx*obx+             $
       gsa.amdy[11]*oby*(obx*obx+oby*oby)+   $
       gsa.amdy[12]*oby*(obx*obx+oby*oby)^2

    gx=gsa.amdy[1]+                     $
       gsa.amdy[4]*oby+                 $
       gsa.amdy[5]*2.0*obx+             $
       gsa.amdy[6]*2.0*obx+             $
       gsa.amdy[8]*oby*oby+             $
       gsa.amdy[9]*oby*2.0*obx+         $
       gsa.amdy[10]*3.0*obx*obx+        $
       gsa.amdy[11]*2.0*obx*oby+        $
       gsa.amdy[12]*(4.0*obx^3*oby + 4.0*obx*oby^3)



    gy=gsa.amdy[0]+                     $
       gsa.amdy[3]*2.0*oby+             $
       gsa.amdy[4]*obx+                 $
       gsa.amdy[6]*2.0*oby+             $
       gsa.amdy[7]*3.0*oby*oby+         $
       gsa.amdy[8]*2.0*oby*obx+         $
       gsa.amdy[9]*obx*obx+             $
       gsa.amdy[11]*(3.0*oby*oby+obx*obx)+   $
       gsa.amdy[12]*(5.0*oby^4 + 6.0*obx^2*oby^2 + obx^4)



    f -= xi
    g -= eta
    deltx = (-f*gy+g*fy) / (fx*gy-fy*gx)
    delty = (-g*fx+f*gx) / (fx*gy-fy*gx)
    obx += deltx
    oby += delty

    ;print,deltx,delty,tolerance,iters,maxiters

    endrep until (min(abs([deltx,delty])) lt tolerance) || (iters gt maxiters)

    eta = 0 &  xi = 0 &  deltx = 0  & delty = 0
    x = (gsa.ppo3-obx*1000.0)/gsa.xsz-gsa.xll - 0.5
    y = (gsa.ppo6+oby*1000.0)/gsa.ysz-gsa.yll - 0.5
    
  if keyword_set(PRINT) then AstDisp, x, y, ra, dec

  return
  end

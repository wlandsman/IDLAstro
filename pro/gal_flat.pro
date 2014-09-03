FUNCTION GAL_FLAT,IMAGE,ANG,INC,CEN,INTERP = interp
;+
; NAME:
;	GAL_FLAT
;
; PURPOSE:
;	Transforms the image of a galaxy so that the galaxy appears face-on
; EXPLANATION:
;	Either a nearest-neighbor approximations or a bilinear interpolation 
;	may  be used.
;
; CALLING SEQUENCE:
;	RESULT = GAL_FLAT( image, ang, inc, [, cen, /INTERP ] )  
;
; INPUTS:   
;	IMAGE  - Image to be transformed
;	ANG  - Angle of major axis, counterclockwise from Y-axis, degrees
;		For an image in standard orientation (North up, East left)
;		this is the Position Angle
;	INC - Angle of inclination of galaxy, degrees
;
; OPTIONAL INPUTS:
;	CEN - Two element vector giving the X and Y position of galaxy center
;		If not supplied, then the galaxy center is assumed to coincide
;		 with the image center
;
; INPUT KEYWORDS:
;	INTERP - If present, and non-zero, then bilinear interpolation will be
;		performed.  Otherwise a nearest neighbor approximation  is used.
;
; OUTPUTS:
;	RESULT - the transformed image, same dimensions and type as IMAGE
;
; METHOD:
;	A set of 4 equal spaced control points are corrected for inclination
;	using the procedure POLYWARP.   These control points are used by 
;	POLY_2D to correct the whole image.
;
; REVISION HISTORY:
;	Written by R. S. Hill, SASC Technologies Inc., 4 December 1985
;	Code cleaned up a bit    W. Landsman      December 1992
;	Converted to IDL V5.0   W. Landsman   September 1997
;-

  if ( N_params() lt 3 ) then begin  
      print,'Syntax - result = gal_flat( image, ang, inc, [ cen, /INTERP ])'
      print,'ANG - Position Angle of major axis (degrees)'                
      print,'INC - Inclination of galaxy (degrees)'
      return, -1
  endif 

  if not keyword_set( INTERP ) then interp = 0

  angr = (ang+90)/!RADEG
  tanang = tan(angr)
  cosang = cos(angr)
  cosinc = cos(inc/!RADEG)
;                                    Parameters of image
  dims = SIZE(image)

  if N_elements(cen) NE 2 then begin 

      xcen = dims[1]/2.0                  ;Center
      ycen = dims[2]/2.0
      if not !QUIET then message,'Galaxy nucleus assumed in image center',/CONT

  endif else begin

      xcen = cen[0]
      ycen = cen[1]

  endelse
;                                    Equation of rotation axis
  b = ycen - xcen*tanang
;                                    Fiducial grid (as in ROT_INT)   
  gridx = xcen + [ [-1,1], [-1,1] ] * dims[1]/6.0
  gridy = ycen + [ [-1,-1], [1,1] ] * dims[2]/6.0      
;                                    Distorted version of grid
  yprime = gridx*tanang + b            ;Equation of major axis
  r0 = (gridy-yprime)*cos(angr)        ;Dist of control pts to major axis
  delr = r0*(1.0-cosinc)               ;Correct distance for inclination
  dely = -delr*cos(angr)               
  delx =  delr*sin(angr)
  distx = gridx + delx
  disty = gridy + dely
;                                    Parameters of undistorted grid
  x0 = dims[1]/3.0
  y0 = dims[2]/3.0
  dx = x0                              ;In this case only
  dy = y0
;                                    Do it
  polywarp, distx, disty, gridx, gridy, 1, kx, ky
  RETURN,poly_2d( image, kx, ky, interp, MISSING = 0)
  end  

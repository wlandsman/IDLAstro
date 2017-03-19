pro tic_one, min, pixx, incr, min2, tic1, RA=ra
;+
; NAME:
;	TIC_ONE
; PURPOSE:
;	Determine the position of the first tic mark for astronomical images.
; EXPLANATION:
;	For use in labelling images with right ascension
;	and declination axes. This routine determines the 
;	position in pixels of the first tic.
;
; CALLING SEQUENCE:
;	tic_one, zmin, pixx, incr, min2, tic1, [RA = ]
;
; INPUTS:
;	zmin  - astronomical coordinate value at axis zero point (degrees 
;		or hours)
;	pixx - distance in pixels between tic marks (usually obtained from TICS)
;	incr - increment in minutes for labels (usually an even number obtained 
;		from the procedure TICS)
;
; OUTPUTS:
;	min2 - astronomical coordinate value at first tic mark 
;	tic1 - position in pixels of first tic mark
;
; EXAMPLE:
;	Suppose a declination axis has a value of 30.2345 degrees at its
;	zero point.  A tic mark is desired every 10 arc minutes, which 
;	corresponds to 12.74 pixels.  Then
;
;	IDL> TIC_ONE, 30.2345, 12.74, 10, min2, tic1
;
;	yields values of min2 = 30.333 and tic1 = 7.55, i.e. the first tic
;	mark should be labeled 30 deg 20 minutes and be placed at pixel value
;	7.55
;
; REVISION HISTORY:
;	by B. Pfarr, 4/15/87
;	Corrected documentation example  W. Landsman   Mar 2017
;-
  On_error,2
;                             convert min to minutes
  if keyword_set(RA) then mul = 4.0000 else mul = 60.00000
  min1 = min*mul		;Convert from degrees to minutes
;
  incra =  abs(incr)
  rem = min1 mod incra                     ;get remainder
  sign = min1*incr

  if ( sign GT 0 ) then begin 

	tic1 = pixx - abs(rem)*(pixx/incra)  
	min2 = (min1+incr-rem)/mul 

  endif else begin 

	tic1 = abs(rem)*(pixx/incra)       
	min2 = (min1 - rem)/mul      

  endelse

  return
  end

function correl_images, image_A, image_B, XSHIFT = x_shift,	$
					  YSHIFT = y_shift, 	$
					  XOFFSET_B = x_offset, $
					  YOFFSET_B = y_offset, $
					  REDUCTION = reducf,	$
					  MAGNIFICATION = Magf, $
					  NUMPIX=numpix, MONITOR=monitor
;+
; NAME:
;	CORREL_IMAGES
; PURPOSE:
;       Compute the 2-D cross-correlation function of two images
; EXPLANATION:
;       Computes the 2-D cross-correlation function of two images for
;       a range of (x,y) shifting by pixels of one image relative to the other.
;
; CALLING SEQUENCE:
;       Result = CORREL_IMAGES( image_A, image_B, 
;                        [XSHIFT=, YSHIFT=, XOFFSET_B=, YOFFSET_B=, REDUCTION=, 
;                        MAGNIFICATION=, /NUMPIX, /MONITOR  )
;
; INPUTS:
;       image_A, image_B = the two images of interest.
;
; OPTIONAL INPUT KEYWORDS:
;       XSHIFT = the + & - shift to be applied in X direction, default=7.
;       YSHIFT = the Y direction + & - shifting, default=7.
;
;       XOFFSET_B = initial X pixel offset of image_B relative to image_A.
;       YOFFSET_B = Y pixel offset, defaults are (0,0).
;
;       REDUCTION = optional reduction factor causes computation of
;                       Low resolution correlation of bin averaged images,
;                       thus faster. Can be used to get approximate optimal
;                       (x,y) offset of images, and then called for successive
;                       lower reductions in conjunction with CorrMat_Analyze
;                       until REDUCTION=1, getting offset up to single pixel.
;
;       MAGNIFICATION = option causes computation of high resolution correlation
;                       of magnified images, thus much slower.
;                       Shifting distance is automatically = 2 + Magnification,
;                       and optimal pixel offset should be known and specified.
;                       Optimal offset can then be found to fractional pixels
;                       using CorrMat_Analyze( correl_images( ) ).
;
;       /NUMPIX - if set, causes the number of pixels for each correlation
;                       to be saved in a second image, concatenated to the
;                       correlation image, so Result is fltarr( Nx, Ny, 2 ).
;       /MONITOR causes the progress of computation to be briefly printed.
;
; OUTPUTS:
;       Result is the cross-correlation function, given as a matrix.
;
; PROCEDURE:
;       Loop over all possible (x,y) shifts, compute overlap and correlation
;       for each shift. Correlation set to zero when there is no overlap.
;
; MODIFICATION HISTORY:
;       Written, July,1991, Frank Varosi, STX @ NASA/GSFC
;       Use ROUND instead of NINT, June 1995, Wayne Landsman HSTX
;       Avoid divide by zero errors, W. Landsman HSTX April 1996
;	Remove use of !DEBUG    W. Landsman   June 1997
;       Subtract mean of entire image before computing correlation, not just 
;          mean of overlap region   H. Ebeling/W. Landsman   June 1998
;       Always REBIN() using floating pt arithmetic W. Landsman  Nov 2007
;       
;-
 compile_opt idl2
 if N_params() LT 2 then begin 
        print,'Syntax  -  Result = CORREL_IMAGES( image_A, image_B,'
	print,'[         XSHIFT=, YSHIFT=, XOFFSET_B=, YOFFSET_B=, REDUCTION=, '
	print,'          MAGNIFICATION=, /NUMPIX, /MONITOR  )'
	return,-1
 endif
 	
	simA = size( image_A )
	simB = size( image_B )
	do_int = (simA[3] LE 3) or (simA[3] GE 12) or $ 
                 (simB[3] LE 3) or (simB[3] GE 12)
		 
	if (simA[0] LT 2) OR (simB[0] LT 2) then begin
		message,"first two arguments must be images",/INFO,/CONTIN
		return,[-1]
	   endif

	if N_elements( x_offset ) NE 1 then x_offset=0
	if N_elements( y_offset ) NE 1 then y_offset=0

	if N_elements( x_shift ) NE 1 then x_shift = 7
	if N_elements( y_shift ) NE 1 then y_shift = 7
	x_shift = abs( x_shift )
	y_shift = abs( y_shift )

	if keyword_set( reducf ) then begin

		reducf = fix( reducf ) > 1
		if keyword_set( monitor ) then $
				print,"Reduction = ",strtrim( reducf, 2 )
		simA = simA/reducf
		LA = simA * reducf -1	;may have to drop edges of images.
		simB = simB/reducf
		LB = simB * reducf -1

                if do_int then begin 
		
		imtmp_A = Rebin( float( image_A[ 0:LA[1], 0:LA[2] ]),  $
		                       simA[1], simA[2] )
		imtmp_B = Rebin( float( image_B[ 0:LB[1], 0:LB[2] ]),  $ 
		                        simB[1], simB[2] )
		endif else begin 
		imtmp_A =Rebin( image_A[ 0:LA[1], 0:LA[2] ], simA[1], simA[2] )
		imtmp_B =Rebin( image_B[ 0:LB[1], 0:LB[2] ], simB[1], simB[2] )
                 endelse 

		xoff = round ( x_offset/reducf )
		yoff = round ( y_offset/reducf )
		xs = x_shift/reducf
		ys = y_shift/reducf

		return, correl_images( imtmp_A, imtmp_B, XS=xs,YS=ys,$
							XOFF=xoff, YOFF=yoff, $
						MONITOR=monitor, NUMPIX=numpix )

	  endif else if keyword_set( Magf ) then begin

		Magf = fix( Magf ) > 1
		if keyword_set( monitor ) then $
				print,"Magnification = ",strtrim( Magf, 2 )
		simA = simA*Magf
		simB = simB*Magf

		imtmp_A = rebin( image_A, simA[1], simA[2], /SAMPLE )
		imtmp_B = rebin( image_B, simB[1], simB[2], /SAMPLE )

		xoff = round( x_offset*Magf )
		yoff = round( y_offset*Magf )

		return, correl_images( imtmp_A, imtmp_B, XS=Magf+2, YS=Magf+2,$
							XOFF=xoff, YOFF=yoff, $
						MONITOR=monitor, NUMPIX=numpix )
	   endif

	Nx = 2 * x_shift + 1
	Ny = 2 * y_shift + 1
	if keyword_set( numpix ) then Nim=2 else Nim=1

	correl_mat = fltarr( Nx, Ny, Nim )

	xs = round( x_offset ) - x_shift
	ys = round( y_offset ) - y_shift

	sAx = simA[1]-1
	sAy = simA[2]-1
	sBx = simB[1]-1
	sBy = simB[2]-1
	meanA = total( image_A )/(simA[1]*simA[2])
	meanB = total( image_B )/(simB[1]*simB[2])

	for y = 0, Ny-1 do begin	;compute correlation for each y,x shift.

	    yoff = ys + y
	    yAmin = yoff > 0
	    yAmax = sAy < (sBy + yoff)
	    yBmin = (-yoff) > 0
	    yBmax = sBy < (sAy - yoff)		;Y overlap

	    if (yAmax GT yAmin) then begin

	       for x = 0, Nx-1 do begin

		   xoff = xs + x
		   xAmin = xoff > 0
		   xAmax = sAx < (sBx + xoff)
		   xBmin = (-xoff) > 0
		   xBmax = sBx < (sAx - xoff)		;X overlap

		   if (xAmax GT xAmin) then begin

			im_ov_A = image_A[ xAmin:xAmax, yAmin:yAmax ]
			im_ov_B = image_B[ xBmin:xBmax, yBmin:yBmax ]
			Npix = N_elements( im_ov_A )

			if N_elements( im_ov_B ) NE Npix then begin
				message,"overlap error: # pixels NE",/INFO,/CONT
				print, Npix, N_elements( im_ov_B )
			   endif

			im_ov_A = im_ov_A - meanA
			im_ov_B = im_ov_B - meanB			
			totAA = total( im_ov_A * im_ov_A )
			totBB = total( im_ov_B * im_ov_B )

                        if (totAA EQ 0) or (totBB EQ 0) then $
                        correl_mat[x,y] = 0.0 else $
			correl_mat[x,y] = total( im_ov_A * im_ov_B ) / $
							sqrt( totAA * totBB )

			if keyword_set( numpix ) then correl_mat[x,y,1] = Npix
		     endif

	          endfor
		endif

		if keyword_set( monitor ) then print, Ny-y, FORM="($,i3)"
	  endfor

	if keyword_set( monitor ) then print," "

return, correl_mat
end

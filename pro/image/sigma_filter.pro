function sigma_filter, image, box_width, N_SIGMA=Nsigma, ALL_PIXELS=all,   $
					ITERATE=iterate, MONITOR=monitor,  $
					KEEP_OUTLIERS=keep, RADIUS=radius, $
		N_CHANGE=nchange, VARIANCE_IMAGE=imvar, DEVIATION_IMAGE=imdev
;+
; NAME:
;	SIGMA_FILTER
; PURPOSE:
;	Replace pixels more than a specified pixels deviant from its neighbors
; EXPLANATION:
;	Computes the mean and standard deviation of pixels in a box centered at 
;	each pixel of the image, but excluding the center pixel. If the center 
;	pixel value exceeds some # of standard deviations from the mean, it is 
;	replaced by the mean in box. Note option to process pixels on the edges.
; CALLING SEQUENCE:
;	Result = sigma_filter( image, box_width, N_sigma=(#), /ALL,/MON )
; INPUTS:
;	image = 2-D image (matrix)
;	box_width = width of square filter box, in # pixels (default = 3)
; KEYWORDS:
;	N_sigma = # standard deviations to define outliers, floating point,
;			recommend > 2, default = 3. For gaussian statistics:
;			N_sigma = 1 smooths 35% of pixels, 2 = 5%, 3 = 1%.
;	RADIUS = alternative to specify box radius, so box_width = 2*radius+1.
;      /ALL_PIXELS causes computation to include edges of image,
;      /KEEP causes opposite effect: pixels with values outside of specified
;		deviation are not changed, pixels within deviation are smoothed.
;      /ITERATE causes sigma_filter to be applied recursively (max = 20 times)
;		until no more pixels change (only allowed when N_sigma >= 2).
;      /MONITOR prints information about % pixels replaced.
; Optional Outputs:
;	N_CHANGE = # of pixels changed (replaced with neighborhood mean).
;	VARIANCE = image of pixel neighborhood variances * (N_sigma)^2,
;	DEVIATION = image of pixel deviations from neighborhood means, squared.
; CALLS:
;	function filter_image( )
; PROCEDURE:
;	Compute mean over moving box-cars using smooth, subtract center values,
;	compute variance using smooth on deviations from mean,
;	check where pixel deviation from mean is within variance of box,
;	replace those pixels in smoothed image (mean) with orignal values,
;	return the resulting partial mean image.
; MODIFICATION HISTORY:
;	Written, 1991, Frank Varosi and Dan Gezari NASA/GSFC
;	F.V.1992, added optional keywords /ITER,/MON,VAR=,DEV=,N_CHANGE=.
;	Converted to IDL V5.0   W. Landsman   September 1997
;-
	if N_elements( radius ) EQ 1 then  box_width = 2*radius+1  else begin
		if N_elements( box_width ) NE 1 then box_width=3
		box_width = 2*(fix( box_width )/2) + 1	;make sure width is odd.
	   endelse

	if (box_width LT 3) then return,image
	bw2 = box_width^2

	mean=( filter_image( image,SMO=box_width,ALL=all )*bw2 - image )/(bw2-1)

	if N_elements( Nsigma ) NE 1 then Nsigma=3
	if (Nsigma LE 0) then return, mean

	imdev = (image - mean)^2
	fact = float( Nsigma^2 )/(bw2-2)
	imvar = fact*( filter_image( imdev,SMO=box_width,ALL=all )*bw2 - imdev )

	if keyword_set( keep )  then  wok = where( imdev GE imvar, nok ) $
				else  wok = where( imdev LT imvar, nok )

	npix = N_elements( image )
	nchange = npix - nok
	if keyword_set( monitor ) then $
		print, nchange*100./npix, Nsigma, $
			FORM="(F6.2,' % of pixels replaced, N_sigma=',F3.1)"

	if (nok EQ npix) then return,image
	if (nok GT 0) then mean[wok] = image[wok]

	if keyword_set( iterate ) AND (Nsigma GE 2) then begin
		iterate = iterate+1
		if (iterate GT 20) then begin
			iterate = 1
			return,mean
		   endif
	    return, sigma_filter( mean, box_width, N_SIGMA=Nsigma, ALL=all,$
					KEEP=keep, ITER=iterate, MONIT=monitor )
	   endif

return, mean
end

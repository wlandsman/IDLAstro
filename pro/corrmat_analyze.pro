pro corrmat_analyze, correl_mat, xoffset_optimum, yoffset_optimum, $
				max_corr, edge, plateau,           $
				XOFF_INIT = xoff_init,             $
				YOFF_INIT = yoff_init,             $
				REDUCTION = reducf, MAGNIFICATION = Magf,  $
				PRINT = print, PLATEAU_THRESH = plateau_thresh
;+
; NAME:
;	CORRMAT_ANALYZE 
; PURPOSE:
;	Find the optimal (x,y) offset to maximize correlation of 2 images
; EXPLANATION:
;	Analyzes the 2-D cross-correlation function of two images
;	and finds the optimal(x,y) pixel offsets.
;	Intended for use with function CORREL_IMAGES.
;
; CALLING SEQUENCE:
;	corrmat_analyze, correl_mat, xoffset_optimum, yoffset_optimum, 
;		max_corr, edge, plateau, [XOFF_INIT=, YOFF_INIT=, REDUCTION=, 
;		MAGNIFICATION=, PLATEAU_THRESH=, /PRINT]
;
; INPUTS:
;	correl_mat = the cross-correlation matrix of 2 images.
;			(as computed by function CORREL_IMAGES( imA, imB ) ).
;
; NOTE:
;	If correl_mat(*,*,1) is the number of pixels for each correlation,
;	(the case when /NUMPIX was specified in call to CORREL_IMAGES)
;	then sqrt( sqrt( # pixels )) is used as correlation weighting factor.
;
; OPTIONAL INPUT KEYWORDS:
;	XOFF_INIT = initial X pixel offset of image_B relative to image_A.
;	YOFF_INIT = Y pixel offset, (both as specified to correl_images).
;	REDUCTION = reduction factor used in call to CORREL_IMAGES.
;	MAGNIFICATION = magnification factor used in call to CORREL_IMAGES,
;		this allows determination of offsets up to fractions of a pixel.
;	PLATEAU_THRESH = threshold used for detecting plateaus in 
;		the cross-correlation matrix near maximum, (default=0.01),
;		used only if MAGNIFICATION > 1
;	/PRINT causes the result of analysis to be printed.
;
; OUTPUTS:
;	xoffset_optimum = optimal X pixel offset of image_B relative to image_A.
;	yoffset_optimum = optimal Y pixel offset.
;	max_corr = the maximal correlation corresponding to optimal offset.
;	edge = 1 if maximum is at edge of correlation domain, otherwise=0.
;	plateau = 1 if maximum is in a plateau of correlation function, else=0.
;
; PROCEDURE:
;	Find point of maximum cross-correlation and calc. corresponding offsets.
;	If MAGNIFICATION > 1:
;	the  correl_mat is checked for plateau near maximum, and if found,
;	the center of plateau is taken as point of maximum cross-correlation.
;
; MODIFICATION HISTORY:
;	Written, July-1991, Frank Varosi, STX @ NASA/GSFC
;	Use ROUND instead of NINT, June 1995 Wayne Landsman HSTX
;	Remove use of non-standard !DEBUG system variable   W.L. HSTX 
;	Converted to IDL V5.0   W. Landsman   September 1997
;-
	scm = size( correl_mat )

	if (scm[0] LT 2) then begin
		message,"first argument must be at least 2-D matrix",/INFO,/CON
		return
	   endif

	Nx = scm[1]
	Ny = scm[2]
	x_shift = Nx/2
	y_shift = Ny/2
	if N_elements( xoff_init ) NE 1 then xoff_init=0
	if N_elements( yoff_init ) NE 1 then yoff_init=0

	if (scm[0] GE 3) then begin		;weight by # of overlap pixels:

		Npix_mat = correl_mat[*,*,1]
		Maxpix = max( Npix_mat )
		corr_mat = correl_mat[*,*,0] * sqrt( sqrt( Npix_mat/Maxpix ) )

	  endif else  corr_mat = correl_mat

	max_corr = max( corr_mat, maxLoc )
	xi = (maxLoc MOD Nx)
	yi = (maxLoc/Nx)

	if N_elements( Magf ) NE 1 then Magf=1
	if N_elements( reducf ) NE 1 then reducf=1
	if N_elements( plateau_thresh ) NE 1 then plateau_thresh=0.01
	plateau=0
	edge=0

	if ( reducf GT 1 ) then begin

		xoffset_optimum = ( xi - x_shift + xoff_init/reducf ) * reducf
		yoffset_optimum = ( yi - y_shift + yoff_init/reducf ) * reducf
		xoffset_optimum = round( xoffset_optimum )
		yoffset_optimum = round( yoffset_optimum )
		format = "(2i5)"

	 endif else if ( Magf GT 1 ) then begin

		w = where( (max_corr - corr_mat) LE plateau_thresh, Npl )

		if (Npl GT 1) then begin

			wx = [ w MOD Nx ]
			wy = [ w/Nx ]
			wxmin = min( wx )
			wymin = min( wy )
			wxmax = max( wx )
			wymax = max( wy )
			npix = (wxmax - wxmin)+(wymax - wymin)

			if (Npl GE npix)  AND $
			   (xi GE wxmin) AND (xi LE wxmax) AND $
			   (yi GE wymin) AND (yi LE wymax) then begin
				plateau=1
				xi = wxmin + (wxmax - wxmin)/2.
				yi = wymin + (wymax - wymin)/2.
				max_corr = corr_mat[xi,yi]
			   endif
		   endif

		xoffset_optimum = xoff_init + float( xi - x_shift )/Magf
		yoffset_optimum = yoff_init + float( yi - y_shift )/Magf
		format = "(2f9.3)"

	  endif else begin
		xoffset_optimum = xi - x_shift + round( xoff_init )
		yoffset_optimum = yi - y_shift + round( yoff_init )
		format = "(2i5)"
	   endelse

	if (xi EQ 0) OR (xi EQ Nx-1) OR $
	   (yi EQ 0) OR (yi EQ Ny-1) then edge=1

	if keyword_set( print ) then begin

		mincm = min( corr_mat, minLoc )

		if (scm[0] GE 3) then begin
			xm = (minLoc MOD Nx)
			ym = (minLoc/Nx)
			Npixmin = Long( Npix_mat[xm,ym] ) * reducf * reducf
			Npixmax = Long( Npix_mat[xi,yi] ) * reducf * reducf
			info_min = "  ( " + strtrim( Npixmin, 2 ) + " pixels )"
			info_max = "  ( " + strtrim( Npixmax, 2 ) + " pixels )"
		  endif else begin
			info_min = ""
			info_max = ""
		   endelse

		print," min Correlation = ", strtrim( mincm, 2 ), info_min
		print," MAX Correlation = ", strtrim( max_corr, 2 ), info_max,$
			"  at (x,y) offset:", $
		    string( [ xoffset_optimum, yoffset_optimum ], FORM=format )

		if (plateau) then begin
			print," plateau of MAX Correlation:"
			print," (Correl - MAX + " + $
			     string( plateau_thresh, FORM="(F5.3)" ) + ") > 0"
			print,(corr_mat - max(corr_mat) + plateau_thresh) > 0
		   endif

		if (edge) then begin
			print," Maximum is at EDGE of shift range, " + $
				"result is inconclusive"
			print," try larger shift or new starting offset"
		   endif
	   endif

return
end

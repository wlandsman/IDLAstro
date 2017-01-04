pro correl_optimize, image_A, image_B, xoffset_optimum, yoffset_optimum, $
					XOFF_INIT = xoff_init,   $
					YOFF_INIT = yoff_init,   $
					PRINT=print, MONITOR=monitor, $
					NUMPIX=numpix, MAGNIFICATION=Magf, $
					PLATEAU_TRESH = plateau, Init_factor  = Init_factor
;+
; NAME:
;	CORREL_OPTIMIZE
;
; PURPOSE:
;	Find the optimal (x,y) pixel offset of image_B relative to image_A
; EXPLANATION"
;	Optimal offset is computed by means of maximizing the correlation 
;	function of the two images.
;
; CALLING SEQUENCE:
;	CORREL_OPTIMIZE, image_A, image_B, xoffset_optimum, yoffset_optimum 
;		[ XOFF_INIT=, YOFF_INIT=, MAGNIFICATION=, /PRINT, /NUMPIX, 
;		  /MONITOR, PLATEAU_THRESH=  ]
;
; INPUTS:
;	image_A, image_B = the two images of interest.
;
; OPTIONAL INPUT KEYWORDS:
;	XOFF_INIT = initial X pixel offset of image_B relative to image_A,
;	YOFF_INIT = Y pixel offset, (default offsets are 0 and 0).
;   INIT_FACTOR  = Initial factor to reduce image (default = 8)
;	MAGNIFICATION = option to determine offsets up to fractional pixels,
;			(example: MAG=2 means 1/2 pixel accuracy, default=1).
;	/NUMPIX: sqrt( sqrt( # pixels )) used as correlation weighting factor.
;	/MONITOR causes the progress of computation to be briefly printed.
;	/PRINT causes the results of analysis to be printed.
;	PLATEAU_THRESH = threshold used for detecting plateaus in 
;		the cross-correlation matrix near maximum, (default=0.01),
;		used only if MAGNIFICATION > 1.    Decrease this value for
;		high signal-to-noise data
;
; OUTPUTS:
;	xoffset_optimum = optimal X pixel offset of image_B relative to image_A.
;	yoffset_optimum = optimal Y pixel offset.
;
; CALLS:
;	function  correl_images( image_A, image_B )
;	pro  corrmat_analyze
;
; PROCEDURE:
;	The combination of function correl_images( image_A, image_B ) and
;	corrmat_analyze of the result is used to obtain the (x,y) offset
;	yielding maximal correlation. The combination is first executed at
;	large REDUCTION factors to speed up computation, then zooming in 
;	recursively on the optimal (x,y) offset by factors of 2.
;	Finally, the MAGNIFICATION option (if specified)
;	is executed to determine the (x,y) offset up to fractional pixels.
;	
; MODIFICATION HISTORY:
;	Written, July,1991, Frank Varosi, STX @ NASA/GSFC
;	Added PLATEAU_THRESH keyword  June 1997,  Wayne Landsman  STX   
;	Added INIT_FACTOR keyword, default is still 8,  Dec. 2016 W. Landsman 
;-
        if N_params() LT 2 then begin
		print,'Syntax - CORREL_OPTIMIZE, imA, imB, Xoffset, Yoffset'
		print,'Keywords - /Monitor, /Print, XoffInit =, YoffInit =' + $
		      ', Magnification =, /Numpix'
		return
        endif

	simA = size( image_A )
	simB = size( image_B )

	if (simA[0] LT 2) OR (simB[0] LT 2) then begin
		message,"first two arguments must be images",/INFO,/CONTIN
		return
	   endif

	if N_elements( xoff_init ) NE 1 then xoff_init=0
	if N_elements( yoff_init ) NE 1 then yoff_init=0
	if N_elements( plateau ) NE 1 then plateau = 0.01
	xoff = xoff_init
	yoff = yoff_init

    mindim = min( [simA[1:2],simB[1:2]] ) 
    if N_elements(Init_factor) EQ 0 then Init_factor = 8 
	reducf = (mindim / Init_factor)	> 1 ;Bin average to about
							; 8 by 8 pixel image.
	if N_elements( Magf ) NE 1 then Magf=1

	xsiz = max( [simA[1],simB[1]] )
	ysiz = max( [simA[2],simB[2]] )
	xshift = xsiz
	yshift = ysiz		;shift over the whole images first correlation.

	while (reducf GT 1) do begin

		corrmat = correl_images( image_A, image_B, XOFF=xoff,YOFF=yoff,$
					       NUM=numpix, XS=xshift,YS=yshift,$
					       REDUCTION=reducf, MONIT=monitor )
		corrmat_analyze, corrmat, xoff, yoff, XOFF=xoff, YOFF=yoff, $
						PRINT=print, REDUCTION=reducf
		xshift = 2*reducf
		yshift = 2*reducf	;shift over coarse pixels to refine
		reducf = reducf/2	; in further correlations.
	  endwhile

	xshift = xshift/2	;now refine offsets to actual pixels.
	yshift = yshift/2
	corrmat = correl_images( image_A, image_B, XOFF=xoff, YOFF=yoff,$
				 MON=monitor, NUM=numpix, XS=xshift, YS=yshift )

	corrmat_analyze, corrmat, xoffset_optimum, yoffset_optimum, $
					XOFF=xoff, YOFF=yoff, PRINT=print

	if (Magf GE 2) then begin

		xoff = xoffset_optimum		;refine offsets to
		yoff = yoffset_optimum		; fractional pixels.

		corrmat = correl_images( image_A, image_B, XOFF=xoff,YOFF=yoff,$
						MAGNIFIC=Magf, MONITOR=monitor )

		corrmat_analyze, corrmat, xoffset_optimum, yoffset_optimum, $
							XOFF=xoff,YOFF=yoff,$
							PRINT=print, MAG=Magf, $
							PLATEAU_THRESH = plateau
	   endif
return
end

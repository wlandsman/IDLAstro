;+
; NAME:
;	MAX_ENTROPY
;
; PURPOSE:
;	Deconvolution of data by Maximum Entropy analysis, given the PSF
; EXPLANATION:
;	Deconvolution of data by Maximum Entropy analysis, given the 
;	instrument point spread response function (spatially invariant psf).
;	Data can be an observed image or spectrum, result is always positive.
;	Default is convolutions using FFT (faster when image size = power of 2).
;
; CALLING SEQUENCE:
;	for i=1,Niter do begin
;	Max_Entropy, image_data, psf, image_deconv, multipliers, FT_PSF=psf_ft
;
; INPUTS:
;	data = observed image or spectrum, should be mostly positive,
;					with mean sky (background) near zero.
;	psf = Point Spread Function of instrument (response to point source,
;							must sum to unity).
;	deconv = result of previous call to Max_Entropy,
;	multipliers = the Lagrange multipliers of max.entropy theory
;		(on first call, set = 0, giving flat first result).
;
; OUTPUTS:
;	deconv = deconvolution result of one more iteration by Max_Entropy.
;	multipliers = the Lagrange multipliers saved for next iteration.
;
; OPTIONAL INPUT KEYWORDS:
;	FT_PSF = passes (out/in) the Fourier transform of the PSF,
;		so that it can be reused for the next time procedure is called,
;      /NO_FT overrides the use of FFT, using the IDL function convol() instead.
;      /LINEAR switches to Linear convergence mode, much slower than the
;		default Logarithmic convergence mode.
;	LOGMIN = minimum value constraint for taking Logarithms (default=1.e-9).
; EXTERNAL CALLS:
;	function convolve( image, psf ) for convolutions using FFT or otherwise.
; METHOD:
;	Iteration with PSF to maximize entropy of solution image with
;	constraint that the solution convolved with PSF fits data image.
;	Based on paper by Hollis, Dorband, Yusef-Zadeh, Ap.J. Feb.1992,
;	which refers to Agmon, Alhassid, Levine, J.Comp.Phys. 1979.
;
;       A more elaborate image deconvolution program using maximum entropy is 
;       available at 
;       http://sohowww.nascom.nasa.gov/solarsoft/gen/idl/image/image_deconvolve.pro
; HISTORY:  
;	written by Frank Varosi at NASA/GSFC, 1992.
;	Converted to IDL V5.0   W. Landsman   September 1997
;-

pro max_entropy, data, psf, deconv, multipliers, FT_PSF=psf_ft, NO_FT=noft, $
			LINEAR=Linear, LOGMIN=Logmin, RE_CONVOL_IMAGE=Re_conv

	if N_elements( multipliers ) LE 1 then begin
		multipliers = data
		multipliers[*] = 0
	   endif

	deconv = exp( convolve( multipliers, psf, FT_PSF=psf_ft, $
						 /CORREL, NO_FT=noft ) )
	totd = total( data )
	deconv = deconv * ( totd/total( deconv ) )

	Re_conv = convolve( deconv, psf, FT_PSF=psf_ft, NO_FT=noft )
	scale = total( Re_conv )/totd

	if keyword_set( Linear ) then begin

		multipliers = multipliers + (data * scale - Re_conv)

	  endif else begin

		if N_elements( Logmin ) NE 1 then Logmin=1.e-9
		multipliers = multipliers + $
			aLog( ( ( data * scale )>Logmin ) / (Re_conv>Logmin) )
	   endelse
end

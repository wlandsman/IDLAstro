;+
; NAME:
;	MAX_LIKELIHOOD
;
; PURPOSE:
;	Maximum likelihood deconvolution of an image or a spectrum.
; EXPLANATION:
; 	Deconvolution of an observed image (or spectrum) given the 
;	instrument point spread response function (spatially invariant psf).
;	Performs iteration based on the Maximum Likelihood solution for
;	the restoration of a blurred image (or spectrum) with additive noise.
;	Maximum Likelihood formulation can assume Poisson noise statistics
;	or Gaussian additive noise, yielding two types of iteration.
;
; CALLING SEQUENCE:
;	for i=1,Niter do Max_Likelihood, data, psf, deconv, FT_PSF=psf_ft
;
; INPUTS PARAMETERS:
;	data = observed image or spectrum, should be mostly positive,
;				with mean sky (background) near zero.
;	psf = Point Spread Function of the observing instrument,
;				(response to a point source, must sum to unity).
; INPUT/OUTPUT PARAMETERS:
;	deconv = as input: the result of previous call to Max_Likelihood,
;		(initial guess on first call, default = average of data),
;		as output: result of one more iteration by Max_Likelihood.
;	Re_conv = (optional) the current deconv image reconvolved with PSF
;		for use in next iteration and to check convergence.
;
; OPTIONAL INPUT KEYWORDS:
;      /GAUSSIAN causes max-likelihood iteration for Gaussian additive noise
;		to be used,  otherwise the default is Poisson statistics.
;	FT_PSF = passes (out/in) the Fourier transform of the PSF,
;		so that it can be reused for the next time procedure is called,
;      /NO_FT overrides the use of FFT, using the IDL function convol() instead.
;	POSITIVITY_EPS = value of epsilon passed to function positivity,
;			default = -1 which means no action (identity).
;	UNDERFLOW_ZERO = cutoff to consider as zero, if numbers less than this.
;
; EXTERNAL CALLS:
;	function convolve( image, psf ) for convolutions using FFT or otherwise.
;	function positivity( image, EPS= ) to make image positive.
;
; METHOD:
;	Maximum Likelihood solution is a fixed point of an iterative eq.
;	(derived by setting partial derivatives of Log(Likelihood) to zero).
;	Poisson noise case was derived by Richardson(1972) & Lucy(1974).
;	Gaussian noise case is similar with subtraction instead of division.
; NOTES:
;       WARNING: The Poisson case may not conserve flux for an odd image size.  
;       This behavior is being investigated.
; HISTORY:
;	written: Frank Varosi at NASA/GSFC, 1992.
;	F.V. 1993, added optional arg. Re_conv (to avoid doing it twice).
;	Converted to IDL V5.0   W. Landsman   September 1997
;       Use COMPLEMENT keyword to WHERE()   W. Landsman  Jan 2008
;-

pro Max_Likelihood, data, psf, deconv, Re_conv, FT_PSF=psf_ft, NO_FT=noft, $
						GAUSSIAN=gaussian, $
						POSITIVITY_EPS=epsilon, $
						UNDERFLOW_ZERO=under
        compile_opt idl2
	if N_elements( deconv ) NE N_elements( data ) then begin
		deconv = data
		deconv[*] = total( data )/N_elements( data )
		Re_conv = 0
	   endif

	if N_elements( under ) NE 1 then under = 1.e-22
	if N_elements( epsilon ) NE 1 then epsilon = -1
	if N_elements( Re_conv ) NE N_elements( deconv ) then $
		Re_conv = convolve( positivity( deconv, EPS=epsilon ), psf, $
						  FT_PSF=psf_ft, NO_FT=noft )
	if keyword_set( gaussian ) then begin

		deconv = deconv + convolve( data - Re_conv, psf, /CORREL, $
						   FT_PSF=psf_ft, NO_FT=noft )
	  endif else begin
		wp = where( Re_conv GT under, npos, $
		     ncomplement=nneg,complement=wz)
              
		if (npos GT 0) then Re_conv[wp] = ( data[wp]/Re_conv[wp] ) > 0
		if (nneg GT 0) then Re_conv[wz] = 1.
		deconv = deconv * convolve( Re_conv, psf, FT_PSF=psf_ft, $
							/CORREL, NO_FT=noft )
	   endelse
	   
	if N_params() GE 4 then $
		Re_conv = convolve( positivity( deconv, EPS=epsilon ), psf, $
						FT_PSF = psf_ft, NO_FT = noft )
						
 end

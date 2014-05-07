pro rdpsf,psf,hpsf,psfname
;+
; NAME:
;       RDPSF
; PURPOSE:
;       Read the FITS file created by GETPSF in the DAOPHOT sequence 
; EXPLANATION:
;       Combines the Gaussian with the residuals to create an output PSF array.
;
; CALLING SEQUENCE:
;       RDPSF, PSF, HPSF, [ PSFname]
;
; OPTIONAL INPUTS
;       PSFname - string giving the name of the FITS file containing the PSF
;               residuals
;
; OUTPUTS
;       psf - array containing the actual PSF
;       hpsf - header associated with psf
;
; PROCEDURES CALLED:
;       DAO_VALUE(), MAKE_2D, SXADDPAR, READFITS(), SXPAR()
; REVISION HISTORY:
;       Written W. Landsman              December, 1988
;       Checked for IDL Version 2, J. Isensee & J. Hill, December, 1990
;       Converted to IDL V5.0   W. Landsman   September 1997
;-
 On_error,2

 if N_params() LT 2 then begin
    print,'Syntax - RDPSF, psf, Hpsf, [ PSFname ]'
    print,'    PSF,HPSF - are the output PSF array and header'
    print,'    PSFNAME - the name of the file containing the PSF, input'
   return
 endif

 if N_params() EQ 2 then begin
    psfname = ''
    read,'Enter name of the FITS file containing the PSF residuals: ',psfname
 endif

 resid = readfits(psfname, hpsf)
 gauss = sxpar(hpsf,'GAUSS*')  ;Get Gaussian parameters (5)
 psfrad = sxpar(hpsf,'PSFRAD') ;Get PSF radius
 npsf = 2*psfrad+1             ;Width of output array containing PSF
 psf = fltarr(npsf,npsf)       ;Create output array
 dx = indgen(npsf) - psfrad    ;Vector gives X distance from center of array
 dy = dx                       ;Ditto for dy
 make_2d,dx,dy                 ;Now have X and Y values for each pixel in
;                              the output array   

 psf = psf + dao_value(dx,dy,gauss,resid) ;Compute DAOPHOT value at each point

 sxaddpar,hpsf,'NAXIS1',npsf   ;Update header to contain PSF size
 sxaddpar,hpsf,'NAXIS2',npsf   ;rather than residual array size

 return
 end

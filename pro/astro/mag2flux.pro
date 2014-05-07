function mag2flux, mag, zero_pt, ABwave = ABwave
;+
; NAME:
;	MAG2FLUX
; PURPOSE:
;	Convert from magnitudes to flux (ergs/s/cm^2/A). 
; EXPLANATION:
;	Use FLUX2MAG() for the opposite direction.
;
; CALLING SEQUENCE:
;	flux = mag2flux( mag, [ zero_pt, ABwave = ] )
;
; INPUTS:
;	mag - scalar or vector of magnitudes
;
; OPTIONAL INPUT:
;	zero_pt - scalar giving the zero point level of the magnitude.
;		If not supplied then zero_pt = 21.1 (Code et al. 1976)
;               Ignored if the ABwave keyword is set.
;
; OPTIONAL KEYWORD INPUT:
;     ABwave - wavelength scalar or vector in Angstroms.   If supplied, then 
;              the input vector, mag, is assumed to contain Oke AB magnitudes
;              (Oke & Gunn 1983, ApJ, 266, 713)
;
; OUTPUT:
;	flux - scalar or vector flux vector, in erg cm-2 s-1 A-1
;              If the ABwave keyword is set, then the flux is given by
;
;              f = 10^(-0.4*(mag +2.406 + 4*alog10(ABwave)))     
;
;              Otherwise the flux is given by
;              f =  10^(-0.4*(mag + zero_pt))
;
; EXAMPLE:
;       Suppose one is given vectors of wavelengths and AB magnitudes, w (in
;       Angstroms) and mag.   Plot the spectrum in erg cm-2 s-1 A-1
;
;       IDL> plot, w, mag2flux(mag,ABwave = w)
; REVISION HISTORY:
;	Written    J. Hill        STX Co.       1988
;	Converted to IDL V5.0   W. Landsman   September 1997
;       Added ABwave keyword,   W. Landsman   September 1998
;-   
 if ( N_params() lt 2 ) then zero_pt = 21.10

 if keyword_set(ABwave) then $
           return, 10^(-0.4*(mag + 2.406 + 5*alog10(ABwave))) else $
           return, 10^(-0.4*( mag + zero_pt))

 end

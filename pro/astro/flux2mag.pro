function flux2mag, flux, zero_pt, ABwave = abwave
;+
; NAME:
;     FLUX2MAG
; PURPOSE:
;     Convert from flux (ergs/s/cm^2/A) to magnitudes.
; EXPLANATION:
;     Use MAG2FLUX() for the opposite direction.
;
; CALLING SEQUENCE:
;     mag = flux2mag( flux, [ zero_pt, ABwave=  ] )
;
; INPUTS:
;     flux - scalar or vector flux vector, in erg cm-2 s-1 A-1
;
; OPTIONAL INPUT:
;     zero_pt - scalar giving the zero point level of the magnitude.
;               If not supplied then zero_pt = 21.1 (Code et al 1976)
;               Ignored if the ABwave keyword is supplied
;
; OPTIONAL KEYWORD INPUT:
;     ABwave - wavelength scalar or vector in Angstroms.   If supplied, then 
;           FLUX2MAG() returns Oke AB magnitudes (Oke & Gunn 1983, ApJ, 266,
;           713).
;
; OUTPUT:
;     mag - magnitude vector.   If the ABwave keyword is set then mag
;           is given by the expression 
;           ABMAG = -2.5*alog10(f) - 5*alog10(ABwave) - 2.406 
;             
;           Otherwise, mag is given by the expression  
;           mag = -2.5*alog10(flux) - zero_pt
; EXAMPLE:
;       Suppose one is given wavelength and flux vectors, w (in Angstroms) and 
;       f (in erg cm-2 s-1 A-1).   Plot the spectrum in AB magnitudes
;
;       IDL> plot, w, flux2mag(f,ABwave = w), /nozero
;
; REVISION HISTORY:
;       Written    J. Hill        STX Co.       1988
;       Converted to IDL V5.0   W. Landsman   September 1997
;       Added ABwave keyword    W. Landsman   September 1998
;-   

 if ( N_params() LT 2 ) then zero_pt = 21.10        ;Default zero pt

 if keyword_set(ABwave) then $
     return, -2.5*alog10(flux) - 5*alog10(ABwave) - 2.406 else $
     return, -2.5*alog10(flux) - zero_pt

 end

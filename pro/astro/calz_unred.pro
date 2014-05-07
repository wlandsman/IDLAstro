pro calz_unred, wave, flux, ebv, funred, R_V = R_V
;+
; NAME:
;     CALZ_UNRED
; PURPOSE:
;     Deredden a galaxy spectrum using the Calzetti et al. (2000) recipe
; EXPLANATION:
;     Calzetti et al.  (2000, ApJ 533, 682) developed a recipe for dereddening 
;     the spectra of galaxies where massive stars dominate the radiation output,
;     valid between 0.12 to 2.2 microns.     (CALZ_UNRED extrapolates between
;     0.12 and 0.0912 microns.)   
;
; CALLING SEQUENCE:
;     CALZ_UNRED, wave, flux, ebv, [ funred, R_V = ]
; INPUT:
;      WAVE - wavelength vector (Angstroms)
;      FLUX - calibrated flux vector, same number of elements as WAVE
;               If only 3 parameters are supplied, then this vector will
;               updated on output to contain the dereddened flux.
;      EBV  - color excess E(B-V), scalar.  If a negative EBV is supplied,
;               then fluxes will be reddened rather than deredenned.
;               Note that the supplied color excess should be that derived for 
;               the stellar  continuum, EBV(stars), which is related to the 
;               reddening derived from the gas, EBV(gas), via the Balmer 
;               decrement by EBV(stars) = 0.44*EBV(gas)
;
; OUTPUT:
;      FUNRED - unreddened flux vector, same units and number of elements
;               as FLUX.   FUNRED values will be zeroed outside valid domain
;               Calz_unred (0.0912 - 2.2 microns).
;           
; OPTIONAL INPUT KEYWORD:
;       R_V - Ratio of total to selective extinction, default = 4.05.  
;             Calzetti et al. (2000) estimate R_V = 4.05 +/- 0.80 from optical
;             -IR observations of 4 starbursts.
; EXAMPLE:
;       Estimate how a flat galaxy spectrum (in wavelength) between 1200 A 
;       and 3200 A is altered by a reddening of E(B-V) = 0.1.   
;
;       IDL> w = 1200 + findgen(40)*50      ;Create a wavelength vector
;       IDL> f = w*0 + 1                    ;Create a "flat" flux vector
;       IDL> calz_unred, w, f, -0.1, fnew  ;Redden (negative E(B-V)) flux vector
;       IDL> plot,w,fnew                   
;
; NOTES:
;       Use the 4 parameter calling sequence if you wish to save the 
;               original flux vector.
; PROCEDURE CALLS:
;      POLY()
; REVISION HISTORY:
;       Written   W. Landsman        Raytheon  ITSS   December, 2000
;-
 On_error, 2

 if N_params() LT 3 then begin
     print,'Syntax: CALZ_UNRED, wave, flux, ebv, [ funred, R_V=]'
    return
 endif

 if N_elements(R_V) EQ 0 then R_V = 4.05
 w1 = where((wave GE 6300) AND (wave LE 22000), c1)
 w2 = where((wave GE  912) AND (wave LT  6300), c2)
 x  = 10000.0/wave                      ;Wavelength in inverse microns

 IF (c1 + c2) NE N_elements(wave) THEN message,/INF, $
       'Warning - some elements of wavelength vector outside valid domain'

 klam = 0.0*flux

 IF c1 GT 0 THEN $
    klam[w1] = 2.659*(-1.857 + 1.040*x[w1]) + R_V
   
 IF c2 GT 0 THEN $
    klam[w2] = 2.659*(poly(x[w2], [-2.156, 1.509d0, -0.198d0, 0.011d0])) + R_V
 
 funred = flux*10.0^(0.4*klam*ebv)
 if N_params() EQ 3 then flux = funred

 end

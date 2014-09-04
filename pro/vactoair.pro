pro vactoair,wave_vac, wave_air
;+
; NAME:
;	VACTOAIR
; PURPOSE:
;	Convert vacuum wavelengths to air wavelengths
; EXPLANATION:
;	Corrects for the index of refraction of air under standard conditions.  
;	Wavelength values below 2000 A will not be altered.  Accurate to 
;	about 10 m/s.
;
; CALLING SEQUENCE:
;	VACTOAIR, WAVE_VAC, [WAVE_AIR]
;
; INPUT/OUTPUT:
;	WAVE_VAC - Vacuum Wavelength in Angstroms, scalar or vector
;		If the second parameter is not supplied, then this will be
;               updated on output to contain double precision air wavelengths.
;
; OPTIONAL OUTPUT:
;        WAVE_AIR - Air wavelength in Angstroms, same number of elements as
;                 WAVE_VAC, double precision
;
; EXAMPLE:
;	If the vacuum wavelength is  W = 2000, then 
;
;	IDL> VACTOAIR, W 
;
;	yields an air wavelength of W = 1999.353 Angstroms
;
; METHOD:
;	Formula from Ciddor 1996  Applied Optics , 35, 1566
;
; REVISION HISTORY
;	Written, D. Lindler 1982 
;	Documentation W. Landsman  Feb. 1989
;       Use Ciddor (1996) formula for better accuracy in the infrared 
;           Added optional output vector, W Landsman Mar 2011
;-
  On_error,2
  compile_opt idl2
  
  if N_params() EQ 0 then begin
     print,'Syntax - VACTOAIR, Wave_Vac, [Wave_Air]'
     return
  endif
  
    wave_air = double(wave_vac)
    g = where(wave_vac GE 2000, Ng)     ;Only modify above 2000 A
    
    if Ng GT 0 then begin 
 
    sigma2 = (1d4/double(wave_vac[g]) )^2.   ;Convert to wavenumber squared

; Compute conversion factor

  fact = 1.D +  5.792105D-2/(238.0185D0 - sigma2) + $
                            1.67917D-3/( 57.362D0 - sigma2)
    

; Convert wavelengths

  wave_air[g] = wave_vac[g]/fact
  if N_Params() eq 1 then wave_vac = wave_air
  endif 

  return
  end                        

pro airtovac,wave_air, wave_vac                  
;+
; NAME:
;       AIRTOVAC
; PURPOSE:
;       Convert air wavelengths to vacuum wavelengths 
; EXPLANATION:
;       Wavelengths are corrected for the index of refraction of air under 
;       standard conditions.  Wavelength values below 2000 A will not be 
;       altered.  Uses relation of Ciddor (1996).
;
; CALLING SEQUENCE:
;       AIRTOVAC, WAVE_AIR, [ WAVE_VAC]
;
; INPUT/OUTPUT:
;       WAVE_AIR - Wavelength in Angstroms, scalar or vector
;               If this is the only parameter supplied, it will be updated on
;               output to contain double precision vacuum wavelength(s). 
; OPTIONAL OUTPUT:
;        WAVE_VAC - Vacuum wavelength in Angstroms, same number of elements as
;                 WAVE_AIR, double precision
;
; EXAMPLE:
;       If the air wavelength is  W = 6056.125 (a Krypton line), then 
;       AIRTOVAC, W yields an vacuum wavelength of W = 6057.8019
;
; METHOD:
;	Formula from Ciddor 1996, Applied Optics 62, 958
;
; NOTES: 
;       Take care within 1 A of 2000 A.   Wavelengths below 2000 A *in air* are
;       not altered.       
; REVISION HISTORY
;       Written W. Landsman                November 1991
;       Use Ciddor (1996) formula for better accuracy in the infrared 
;           Added optional output vector, W Landsman Mar 2011
;       Iterate for better precision W.L./D. Schlegel  Mar 2011
;-
   On_error,2
   compile_opt idl2

  if N_params() EQ 0 then begin
      print,'Syntax - AIRTOVAC, WAVE_AIR, [WAVE_VAC]'
      print,'WAVE_AIR (Input) is the air wavelength in Angstroms'
       return
  endif

    wave_vac = double(wave_air)
    g = where(wave_vac GE 2000, Ng)     ;Only modify above 2000 A
    
    if Ng GT 0 then begin 
 
  for iter=0, 1 do begin
  sigma2 = (1d4/double(wave_vac[g]) )^2.     ;Convert to wavenumber squared

; Compute conversion factor
  fact = 1.D +  5.792105D-2/(238.0185D0 - sigma2) + $
                            1.67917D-3/( 57.362D0 - sigma2)
    

  wave_vac[g] = wave_air[g]*fact              ;Convert Wavelength
  endfor
  if N_params() EQ 1 then wave_air = wave_vac
  endif
  
  return            
  end

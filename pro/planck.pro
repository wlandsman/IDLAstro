function planck,wave,temp
;+
; NAME:
;       PLANCK()   
; PURPOSE: 
;       To calculate the Planck function in units of ergs/cm2/s/A  
;
; CALLING SEQUENCE: 
;       bbflux = PLANCK( wave, temp) 
;
; INPUT PARAMETERS: 
;       WAVE   Scalar or vector giving the wavelength(s) in **Angstroms**
;               at which the Planck function is to be evaluated.
;       TEMP   Scalar giving the temperature of the planck function in degree K
;
; OUTPUT PARAMETERS:
;       BBFLUX - Scalar or vector giving the blackbody flux (i.e. !pi*Intensity)
;               in erg/cm^2/s/A in at the specified wavelength points.
;
; EXAMPLES:
;       To calculate the blackbody flux at 30,000 K every 100 Angstroms between
;       2000A and 2900 A
;   
;       IDL> wave = 2000 + findgen(10)*100
;       IDL> bbflux = planck(wave,30000)
;
;       If a star with a blackbody spectrum has a radius R, and distance,d, then
;       the flux at Earth in erg/cm^2/s/A will be bbflux*R^2/d^2
; PROCEDURE:
;       The wavelength data are converted to cm, and the Planck function
;       is calculated for each wavelength point. See Allen (1973), Astrophysical
;       Quantities, section 44 for more information.
;
; NOTES:
;       See the procedure planck_radiance.pro in 
;       ftp://origin.ssec.wisc.edu/pub/paulv/idl/Radiance/planck_radiance.pro
;       for computation of Planck radiance given wavenumber in cm-1 or  
;       wavelength in microns 
; MODIFICATION HISTORY:
;       Adapted from the IUE RDAF               August, 1989
;       Converted to IDL V5.0   W. Landsman   September 1997
;       Improve precision of constants    W. Landsman  January 2002
;-
 On_error,2

 if ( N_elements(wave) LT 1 ) then begin
     print,'Syntax - bbflux = planck( wave, temp)'
     return,0
  endif    

  if ( N_elements( temp ) NE 1 ) then $
      read,'Enter a blackbody temperature', temp

  bbflux = wave*0.

; Gives the blackbody flux (i.e. PI*Intensity) ergs/cm2/s/a

  w = wave / 1.E8                              ; Angstroms to cm    
;constants appropriate to cgs units.
  c1 =  3.7417749d-5                ; =2*!DPI*h*c*c       
  C2 =  1.4387687d                  ; =h*c/k
  val =  c2/w/temp  
  mstr = machar(double = (size(val,/type) EQ 5) )  ;Get machine precision      
  good = where( val LT alog(mstr.xmax), Ngood )    ;Avoid floating underflow

  if ( Ngood GT 0 ) then  $
      bbflux[ good ] =  C1 / ( w[good]^5 * ( exp( val[good])-1. ) )

  return, bbflux*1.E-8              ; Convert to ergs/cm2/s/A

  end 

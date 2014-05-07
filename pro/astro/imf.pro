function imf, mass, expon, mass_range
;+
; NAME:
;       IMF
; PURPOSE:
;       Compute an N-component power-law logarithmic initial mass function 
; EXPLANTION:
;       The function is normalized so that the total mass distribution 
;       equals one solar mass.
;
; CALLING SEQUENCE:
;       psi = IMF( mass, expon,  mass_range )
;
; INPUTS:
;       mass - mass in units of solar masses (scalar or vector)
;               Converted to floating point if necessary
;       expon - power law exponent, usually negative, scalar or vector
;               The number of values in expon equals the number of different
;               power-law components in the IMF
;               A Saltpeter IMF has a scalar value of expon = -1.35
;       mass_range - vector containing the mass upper and lower limits of the 
;               IMF and masses where the IMF exponent changes.   The number 
;               of values in mass_range should be one more than in expon.   
;               The values in mass_range should be monotonically increasing.
;
; OUTPUTS
;       psi - mass function, number of stars per unit logarithmic mass interval
;               evaluated for supplied masses
;
; NOTES:
;       The mass spectrum f(m) giving the number of stars per unit mass 
;       interval is related to psi(m) by  m*f(m) = psi(m).    The normalization
;       condition is that the integral of psi(m) between the upper and lower
;       mass limit is unity.
;
; EXAMPLE:
;       (1) Print the number of stars per unit mass interval at 3 Msun 
;               for a Salpeter (expon = -1.35) IMF, with a mass range from 
;               0.1 MSun to 110 Msun.
;
;               IDL> print, imf(3, -1.35, [0.1, 110] ) / 3
;
;       (2) Lequex et al. (1981, A & A 103, 305) describes an IMF with an
;               exponent of -0.6 between 0.007 Msun and 1.8 Msun, and an
;               exponent of -1.7 between 1.8 Msun and 110 Msun.    Plot
;               the mass spectrum f(m)
;
;               IDL> m = [0.01,0.1,indgen(110) + 1 ]  ;Make a mass vector
;               IDL> expon = [-0.6, -1.7]       ;Exponent Vector
;               IDL> mass_range = [ 0.007, 1.8, 110]    ;Mass range
;               IDL> plot,/xlog,/ylog, m, imf(m, expon, mass_range ) / m
;
; METHOD
;       IMF first calculates the constants to multiply the power-law 
;       components such that the IMF is continuous at the intermediate masses, 
;       and that the total mass integral is one solar mass.  The IMF is then 
;       calculated for the supplied masses.   Also see Scalo (1986, Fund. of
;       Cosmic Physics, 11, 1)
;
; PROCEDURES CALLED:
;       None
; REVISION HISTORY:
;       Written    W. Landsman              August, 1989  
;       Set masses LE mass_u rather than LT mass_u  August, 1992
;       Major rewrite to accept arbitrary power-law components   April 1993
;       Convert EXPON to float if necessary  W. Landsman     March 1996
;       Remove call to DATATYPE, V5.3 version  W. Landsman  August 2000
;-
  On_error,2

 if N_params() LT 3 then begin
        print,'Syntax - psi = IMF( mass, expon, mass_range)'
        return,-1
 endif

  Ncomp = N_elements(expon)
  if N_elements( mass_range) NE Ncomp + 1 then message, $
    'ERROR - Mass Range Vector must have ' + strtrim(Ncomp+1,2) + ' components' 

 if ( min(mass_range) LE 0 ) then message, $
        'ERROR - Mass range Vector must be positive definite'

 npts = N_elements(mass)
 if ( npts LT 1 ) then begin
     message, 'Mass vector (first parameter) has not been defined',/CON
     return,0
 endif

 if size(mass,/TNAME) NE 'DOUBLE' then mass = float(mass)     ;Make sure not integer
 if size(expon,/TNAME) NE 'DOUBLE' then expon = float(expon)  

; Get normalization constants for supplied power-law exponents

 integ = fltarr(ncomp)

;Compute the unnormalized integral over each power law section

 for i = 0, Ncomp-1 do begin

 if ( expon[i] NE -1 ) then integ[i] =   $
    (mass_range[i+1]^(1+expon[i]) - mass_range[i]^(1+expon[i]))/(1+expon[i]) $

 else integ[i] = alog(mass_range[i+1]/mass_range[i])

 endfor 

; Insure continuity where the power law functions meet

 joint = fltarr(ncomp)
 joint[0] = 1
 if ncomp GT 1 then for i = 1,ncomp-1 do begin
           joint[i] = joint[i-1]*mass_range[i]^( expon[i-1] - expon[i] )
 endfor

 norm = fltarr(ncomp)
 norm[0] = 1./ total(integ*joint)
 if ncomp GT 1 then for i = 1,ncomp-1 do norm[i] = norm[0]*joint[i]

 f = mass*0.

 for i = 0, Ncomp-1 do begin

 test = where( (mass GT mass_range[i]) and (mass LE mass_range[i+1]), Ntest ) 
 if ( Ntest GT 0 ) then f[test] = norm[i]*mass[test]^(expon[i])

 endfor

 return,f
 end

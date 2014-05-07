function zang,dl,z, h0=h0, k = k, Lambda0 = lambda0, Omega_m = Omega_m, $
                    q0 = q0, SILENT = silent
;+
; NAME:
;       ZANG
; PURPOSE:
;       Determine the angular size of an object as a function of redshift
; EXPLANATION:
;       Requires an input size in kpc and returns an angular size in arc seconds
;       Default cosmology has a Hubble constant of 70 km/s/Mpc, Omega (matter)
;       =0.3 and a normalized cosmological constant Lambda = 0.7; however these
;       values can be changed with appropriate keywords.
;
; CALLING SEQUENCE:
;       angsiz = zang( dl, [ z, H0 =, Omega_m =, Lambda0 = , q0 = , k =, 
;                               /SILENT] )
;
; INPUTS:
;       dl - linear size of the object *in kpc*, non-negative scalar or vector
;       z - redshift of object, postive  scalar or vector
;           Either dl and z must have the same number of elements, or at least
;           one of them must be a vector.
; OPTIONAL INPUT KEYWORDS
;    H0 -  Hubble constant in km/s/Mpc, default is 70
;
;        No more than two of the following four parameters should be
;        specified.    None of them need be specified, default values are given
;    k - curvature constant, normalized to the closure density.   Default is
;        0, indicating a flat universe
;    Omega_m -  Matter density, normalized to the closure density, default
;        is 0.3.   Must be non-negative
;    Lambda0 - Cosmological constant, normalized to the closure density,
;        default is 0.7
;    q0 - Deceleration parameter, numeric scalar = -R*(R'')/(R')^2, default
;        is -0.55
;
;    Note that Omega_m + lambda0 + k = 1 and q0 = 0.5*omega_m - lambda0
; OUTPUT:
;       angsiz - Angular size of the object at the given redshift in 
;               arc seconds 
; EXAMPLE:
;  (1) What would be the angular size of galaxy of diameter 50 kpc at a redshift
;      of 1.5 in an open universe with Lambda = 0 and Omega (matter) = 0.3.
;      Assume the default Hubble constant value of 70 km/s/Mpc.
;      
;      IDL> print,zang(50,1.5, Lambda = 0,omega_m = 0.3)
;             ====> 6.58 arc seconds
;
;  (2) Now plot the angular size of a 50 kpc diameter galaxy as a function of 
;      redshift for the default cosmology (Lambda = 0.7, Omega_m=0.3) up to 
;      z = 0.5
;      IDL> z = findgen(50)/10. + 0.1    ;Angular size undefined at z = 0
;      IDL> plot,z,zang(50,z),xtit='z',ytit='Angular Size (")'
; NOTES:
;      This procedure underwent a major revision in April 2000 to allow for a 
;      cosmological constant, ***including a change of the calling sequence***
;
;      Be sure to supply the input linear size dl in units of kpc.
; PROCEDURES CALLED:
;      LUMDIST() -- Calculates the luminosity distance
; REVISION HISTORY:
;      Written    J. Hill   STX           July, 1988
;      Converted to IDL V5.0   W. Landsman   September 1997
;      Major rewrite to call LUMDIST function  W. Landsman   April 2000     
;-

 if N_params() LT 2 then begin 
      print,'Sytnax - ' + $
 'angsiz = zang( dl, z, [H0 =, Omega_m =, Lambda0 = , q0 = , k =, /SILENT])'
      return,-1
 endif

 d = lumdist(z,H0 = h0,k = k, Lambda0 = lambda0, Omega_m = Omega_m,  q0 = q0, $
               SILENT = silent)

; Angular distance is equal to the luminosity distance times (1+z)^2
 return,!RADEG*3600.*dl*(1.+z)^2/(1000.*d)
 end

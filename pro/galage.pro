;+
; NAME: 
;   GALAGE
;
; PURPOSE:
;   Determine the age of a galaxy given its redshift and a formation redshift.
;
; CALLING SEQUENCE:
;   age = galage(z, [zform,  H0 =, k=, lambda0 =, Omega_m= , q0 =, /SILENT])' 
;
; INPUTS:
;    z - positive numeric vector or scalar of measured redshifts 
;    zform - redshift of galaxy formation (> z), numeric positive scalar 
;        To determine the age of the universe at a given redshift, set zform
;        to a large number (e.g. ~1000).
;
; OPTIONAL KEYWORD INPUTS: 
;    H0 - Hubble constant in km/s/Mpc, positive scalar, default is 70
;    /SILENT - If set, then the adopted cosmological parameters are not 
;         displayed at the terminal.
;
;        No more than two of the following four parameters should be
;        specified.   None of them need be specified -- the adopted defaults
;        are given. 
;    k - curvature constant, normalized to the closure density.   Default is
;        0, (indicating a flat universe)
;    Omega_m -  Matter density, normalized to the closure density, default
;        is 0.3.   Must be non-negative
;    Lambda0 - Cosmological constant, normalized to the closure density,
;        default is 0.7
;    q0 - Deceleration parameter, numeric scalar = -R*(R'')/(R')^2, default
;        is -0.55
;       
; OUTPUTS:
;    age -  age of galaxy in years, will have the same number of elements
;           as the input Z vector
;
; EXAMPLE:
;    (1) Determine the age of a galaxy observed at z = 1.5 in a cosmology with
;    Omega_matter = 0.3 and Lambda = 0.0.    Assume the formation redshift was
;    at z = 25, and use the default Hubble constant (=70 km/s/Mpc)
;
;    IDL> print,galage(1.5,25,Omega_m=0.3, Lambda = 0)
;             ===> 3.35 Gyr
;     
;    (2) Plot the age of a galaxy in Gyr out to a redshift of z = 5, assuming 
;        the default cosmology (omega_m=0.3, lambda=0.7), and zform = 100
;
;    IDL> z = findgen(50)/10.
;    IDL> plot,z,galage(z,100)/1e9,xtit='z',ytit = 'Age (Gyr)'
;
; PROCEDURE:
;    For a given formation time zform and a measured z, integrate dt/dz from 
;    zform to z. Analytic formula of dt/dz in Gardner, PASP 110:291-305, 1998 
;    March  (eq. 7)
; 
; COMMENTS:
;    (1) Integrates using the IDL Astronomy Library procedure QSIMP.    (The 
;    intrinsic IDL QSIMP() function is not called because of its ridiculous
;    restriction that only scalar arguments can be passed to the integrating
;    function.)    The function 'dtdz' is defined at the beginning of the 
;    routine (so it can compile first).
;    
;    (2) Should probably be fixed to use a different integrator from QSIMP when
;    computing age from an "infinite" redshift of formation.    But using a 
;    large value of zform seems to work adequately.
;
;     (3) An alternative set of IDL procedures for computing cosmological
;    parameters is available at 
;            https://github.com/jlfischer/red-idl-cosmology
; PROCEDURES CALLED:
;    COSMO_PARAM, QSIMP
; HISTORY: 
;     STIS version by P. Plait (ACC)                  June 1999
;     IDL Astro Version   W. Landsman (Raytheon ITSS)      April 2000
;     Avoid integer overflow for more than 32767 redshifts  July 2001
;-
;
; define function dtdz
;

function dtdz, z, lambda0 = lambda0, q0 = q0
   term1 = (1.0d + z)
   term2 = 2.0d * (q0 + lambda0) * z + 1.0d - lambda0
   term3 = (1.0d + z) * (1.0d +z)
   return, 1.0 / (term1 * sqrt(term2 * term3 + lambda0))
   end

;;;;;;;;;;;;;;;;;;;;;;;;;

function galage, z, zform, h0 = h0, Omega_m=omega_m, lambda0 = lambda0, k = k, $
                  q0 = q0, SILENT = silent

 if N_params() LE 1 then begin
    print,$
  'Syntax: age = GALAGE(z, zform, [H0= , Omega_M = ,lambda0 =, k= , q0=, /SIL]'
   return, 0
 endif

;
; initialize numbers
;

  if N_elements(h0) EQ 0 then h0 = 70.0
  COSMO_PARAM, Omega_m, lambda0, k, q0
  if not keyword_set(silent) then $
     print,'GALAGE: H0:', h0, ' Omega_m:', omega_m, ' Lambda0',lambda0, $
        ' q0: ',q0, ' k: ', k, f='(A,I3,A,f5.2,A,f5.2,A,f5.2,A,F5.2)' 

   nz = N_elements(z)
   age = z*0.            ;Return same dimensions and data type as Z

; 
; use qsimp to integrate dt/dz to get age for each z
;   watch out for null case of z >= zform 
;
 
    for i= 0L, nz-1 do begin
      if (z[i] ge zform) then age_z = 0 else $
          qsimp,'dtdz', z[i], zform, age_z, q0 = q0, lambda0 = lambda0
      age[i] = age_z
   endfor

; convert units of age: km/s/Mpc to years, divide by H0
;    3.085678e19 km --> 1 Mpc
;    3.15567e+07 sec --> 1 year 

   return, age * 3.085678e+19 / 3.15567e+7/ H0
   end


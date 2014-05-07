pro cosmo_param,Omega_m, Omega_Lambda, Omega_k, q0
;+
; NAME:
;     COSMO_PARAM
; PURPOSE:
;     Derive full set of cosmological density parameters from a partial set
; EXPLANATION:
;     This procedure is called by LUMDIST and GALAGE to allow the user a choice
;     in defining any two of four cosmological density parameters.
;
;     Given any two of the four input parameters -- (1) the normalized matter 
;     density Omega_m (2) the normalized cosmological constant, Omega_lambda 
;     (3) the normalized curvature term, Omega_k and (4) the deceleration 
;     parameter q0 --  this  program will derive the remaining two.     Here 
;     "normalized" means divided by the closure density so that 
;     Omega_m + Omega_lambda + Omega_k = 1.    For a more
;     precise definition see Carroll, Press, & Turner (1992, ArAA, 30, 499).     
;
;     If less than two parameters are defined, this procedure sets default 
;     values of Omega_k=0 (flat space), Omega_lambda = 0.7, Omega_m = 0.3
;     and q0 = -0.55
; CALLING SEQUENCE:
;       COSMO_PARAM, Omega_m, Omega_lambda, Omega_k, q0
;
; INPUT-OUTPUTS:
;     Omega_M - normalized matter energy density, non-negative numeric scalar
;     Omega_Lambda - Normalized cosmological constant, numeric scalar
;     Omega_k - normalized curvature parameter, numeric scalar.   This is zero
;               for a flat universe
;     q0 - Deceleration parameter, numeric scalar = -R*(R'')/(R')^2
;          = 0.5*Omega_m - Omega_lambda
; NOTES:
;     If more than two parameters are defined upon input (overspecification), 
;     then the first two defined parameters in the ordered list Omega_m, 
;     Omega_lambda, Omega_k, q0 are used to define the cosmology.
; EXAMPLE:
;     Suppose one has Omega_m = 0.3, and Omega_k = 0.5 then to determine
;     Omega_lambda and q0
;    
;       IDL> cosmo_param, 0.3, omega_lambda, 0.5, q0
;   
;       which will return omega_lambda = 0.2 and q0 = -2.45
; REVISION HISTORY:
;       W. Landsman         Raytheon ITSS         April 2000
;       Better Error checking  W. Landsman/D. Syphers   October 2010
;-

 On_error,2
 compile_opt idl2
 
 if N_params() LT 3 then begin
      print,'Syntax - COSMO_PARAM, Omega_m, Omega_lambda, Omega_k, q0'
      return
 endif
 
 Nk = n_elements(Omega_k) < 1
 NLambda = N_elements(Omega_lambda) < 1
 Nomega = N_elements(Omega_m) < 1
 Nq0 = N_elements(q0) < 1

; Use must specify 0 or 2 parameters

 if total(Nk + Nlambda + Nomega + Nq0,/int) EQ 1 then $
     message,'ERROR - At least 2 cosmological parameters must be specified'
     
; Check which two parameters are defined, and then determine the other two

 if (Nomega and Nlambda) then begin 
       if Nk EQ 0 then Omega_k = 1 - omega_m - Omega_lambda 
       if Nq0 EQ 0 then q0 = omega_m/2. - Omega_lambda
 endif else $

 if (Nomega and Nk) then begin 
        if Nlambda EQ 0 then Omega_lambda = 1. -omega_m - Omega_k
        if Nq0 EQ 0 then q0 = -1 + Omega_k + 3*Omega_m/2
 endif else $

 if (Nlambda and Nk) then begin 
         if Nomega EQ 0 then omega_m = 1.-Omega_lambda - Omega_k
         if Nq0 EQ 0 then q0 = (1 - Omega_k - 3.*Omega_lambda)/2.
 endif else $

 if (Nomega and Nq0) then begin
         if Nk EQ 0 then Omega_k = 1 + q0 - 3*omega_m/2. 
         if Nlambda EQ 0 then Omega_lambda  = 1. - omega_m - Omega_k
 endif else $

 if (Nlambda and Nq0) then begin
         if Nk EQ 0 then Omega_k = 1 - 2*q0 - 3*Omega_lambda
         if Nomega EQ 0 then omega_m = 1.-Omega_lambda - Omega_k
 endif else $

 if (Nk and Nq0) then begin
         if Nomega EQ 0 then omega_m = (1 + q0 - Omega_k)*2/3.
         if Nlambda EQ 0 then Omega_lambda = 1. - omega_m - Omega_k
 endif

;Set default values

 if N_elements(Omega_k) EQ 0 then Omega_k = 0       ;Default is flat space
 if N_elements(Omega_lambda) EQ 0 then Omega_lambda = 0.7
 if N_elements(omega_m) EQ 0 then omega_m = 1 - Omega_lambda
 if N_elements(q0) EQ 0 then q0 = (1 - Omega_k - 3*Omega_lambda)/2.

 return
 end

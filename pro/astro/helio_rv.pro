function helio_rv,HJD,T,P,V0,K,e,omega
;+ 
; NAME:
;    HELIO_RV
;
; PURPOSE:
;     Return the heliocentric radial velocity of a spectroscopic binary
;
; EXPLANATION:
;    This function will return the heliocentric radial velocity of a 
;    spectroscopic binary star at a given heliocentric date 
;    given its orbit.
;
; CALLING SEQUENCE:
;
;  Result = HELIO_RV ( JD ,T ,Period ,Gamma , K, [,e ,Omega ] )
;
; INPUT:
;
; JD            - Time of observation
; T             - Time of periastron passage (max. +ve velocity
;                 for circular orbits), same time system as JD
; Period        - the period in same units as JD
; Gamma         - systemic velocity
; K             - velocity semi-amplitude in the same units as Gamma.
; e             - eccentricity of the orbit, default is 0.
; Omega         - longitude of periastron in degrees. Must be specified for
;                 eccentric orbits.
;
; OUTPUT:
;
;  The predicted heliocentric radial velocity in the same units as Gamma
;  for the date(s) specified by Reduced_HJD.
;
; RESTRICTIONS:
;
;  The user should ensure consistency with all time systems being
;  used (i.e. JD and T should be in the same units and time system).
;  Generally, users should reduce large time values by subtracting 
;  a large constant offset, which may improve numerical accuracy.
;  
;  If using the the routines JULDATE and HELIO_JD, the reduced HJD
;  time system must be used throughtout.
;
; EXAMPLES:
;
; Example 1
;
;  What was the heliocentric radial velocity of the primary component of HU Tau
; at 1730 UT 25 Oct 1994?
; 
; IDL> juldate ,[94,10,25,17,30],JD                 ;Get Geocentric julian date
; IDL> hjd = helio_jd(jd,ten(04,38,16)*15.,ten(20,41,05)) ; Convert to HJD
; IDL> print, helio_rv(hjd,46487.5303D,2.0563056D,-6.0,59.3)
;      -62.965569
;
; NB. 1. The routines JULDATE and HELIO_JD return a reduced HJD (HJD - 2400000)
;        and so T and P must be specified in the same fashion.
;     2. The user should be careful to use double precision format to specify
;        T and P to sufficient precision where necessary. 
; 
; Example 2
;
;  Plot two cycles of an eccentric orbit, e=0.6, omega=45 for both
;  components of a binary star
;
; IDL> phi=findgen(100)/50.0             ; Generates 100 phase points
; IDL> plot, phi,helio_rv(phi,0,1,0,100,0.6,45),yrange=[-100,150]
; IDL> oplot, phi,helio_rv(phi,0,1,0,50,0.6,45+180)
; 
; This illustrates both the use of arrays to perform multiple calculations
; and generating radial velocities for a given phase by setting T=0 and P=1.
; Note also that omega has been changed by 180 degrees for the orbit of the
; second component  (the same 'trick' can be used for circular orbits).
;
; 
; MODIFICATION HISTORY:
;
;  Written by:  Pierre Maxted CUOBS, October, 1994
;
;  Circular orbits handled by setting e=0 and omega=0 to allow
;  binary orbits to be handled using omega and omega+180.
;                                                      Pierre Maxted,Feb 95
;  BUG - omega was altered by the routine - corrected Feb 95,Pierre Maxted
;  Iteration for E changed to that  given by Reidel , Feb 95,Pierre Maxted
;  /SINGLE keyword removed.                           May 96,Pierre Maxted
;;       
;  Removed limitation of time system on HJD, C. Markwardt, 2011-04-15
;
;  Change convergence test from relative to absolute precision on E
;                                                     Pierre Maxted, Apr 12
;-
;
; 
  ON_ERROR, 2   ; Return to caller
  compile_opt idl2
;
; Check suitable no. of parameters have been entered.
;
  if N_params() ne 5 and N_params() ne 7 then begin
   print,'Syntax - Result = HELIO_RV (JD ,T ,Period ,Gamma, K)'
   print,'         OR'
   print,'         Result = HELIO_RV (JD ,T ,Period ,Gamma, K ,e ,Omega)'
   print,'Further help - type doc_library,"HELIO_RV".'
 endif else begin
;
; Circular orbits
;
  if ~keyword_set(omega) and ~keyword_set(e) then begin
   e = 0.0
   omega = 0.0
  endif 
;
;
; Calculate the approximate eccentric anomaly, E1, via the mean 
; anomaly, M.
; (from Heintz DW, "Double stars", Reidel, 1978)
;
 M=2.D*!dpi*( (HJD-T)/P MOD 1.)
 E1=M + e*sin(M)  + ((e^2)*sin(2.0D*M)/2.0D)
;
; Now refine this estimate using  formulae given by Reidel.
;
 repeat begin
  E0=E1 
  M0 = E0 - e*sin(E0)
  E1 = E0 + (M-M0)/(1.0 - e*cos(E0))
 endrep until max(abs(E1-E0)) lt 1D-8
;
; Now calculate nu
;
 nu=2.0D*atan(sqrt((1.D0 + e)/(1.D - e))*tan(E1/2.0D))
; nu=nu+((nu<0D)*(2D*!dpi))
;
; Can now calculate radial velocities
;
 rv = (K*(cos(nu+!dtor*omega) + (e*cos(!dtor*omega))))+V0
 return ,rv
;
;
 endelse
;
;
 end


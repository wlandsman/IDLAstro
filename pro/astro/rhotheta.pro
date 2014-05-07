FUNCTION RHOTHETA,P,T,e,a,i,Omega,omega2,t2

;+ 
; NAME:
;    RHOTHETA
;
; PURPOSE:
;     Calculate the separation and position angle of a binary star
;
; EXPLANATION:
;    This function will return the separation rho and position angle
;    theta of a visual binary star derived from its orbital elements.
;    The algorithms described in the following book will be used:
;    Meeus J., 1992, Astronomische Algorithmen, Barth.
;    Compared to the examples given at p. 400 and no discrepancy found.
;    Input parameters will never be changed.
;
; CALLING SEQUENCE:
;
;  Result = RHOTHETA ( P, T, e, a, i, Omega, omega2, t2)
;
; INPUT:
;
; P             - Period [year]
; T             - Time of periastron passage [year]
; e             - eccentricity of the orbit
; a             - semi-major axis [arc second]
; i             - inclination [degree]
; Omega         - node [degree]
; omega2        - longitude of periastron [degree] 
; t2            - epoch of observation [year]
;
; OUTPUT:
;
;  structure containing
;  rho          - separation [arc second]
;  theta        - position angle [degree]
;  In case of errors rho and theta are -1.
;
; RESTRICTIONS:
;
;  All input parameters have to be scalars and floating point numbers.
;  
; EXAMPLE:
;       Find the position of Eta Coronae Borealis at the epoch 1980.0
;
;       IDL> test=rhotheta(41.623,1934.008,0.2763,0.907,59.025,23.717,219.907,1980.0)
;       rho=     0.411014    theta=       318.42307
; 
; PROCEDURES CALLED:
;       CIRRANGE  - from IDL Astronomy Library
;       
; MODIFICATION HISTORY:
;
;  Written by:  Sebastian Kohl Hamburg Observatory, November, 2012
;-
;
result={rho:DOUBLE(-1),theta:DOUBLE(-1)}

IF (N_PARAMS() EQ 8) THEN BEGIN 
; see chapter 55
n=360.0/P
M=n*(t2-T)
M=M/360.0*2.0*!PI; convert M in radians

; solution of Kepler equation, see chapter 29, 3rd method
F= M GT 0 ? 1 : -1
M=ABS(M)/2.0/!PI
M=(M-FLOOR(M))*2.0*!PI*F
IF (M LT 0.0) THEN M=M+2.0*!PI
F=1.0
IF (M GT !PI) THEN F=-1.0
IF (M GT !PI) THEN M=2.0*!PI-M
E0=!PI/2.0
D=!PI/4.0
FOR j=1,33 DO BEGIN
M1=E0-e*sin(E0)
SGN_M = (M-M1) GT 0 ? 1 : -1
E0=E0+D*SGN_M
D=D/2.0
ENDFOR
E0=E0*F

; return to chapter 55
r=a*(1.0-e*cos(E0))
nu=2.0*ATAN(SQRT((1.0+e)/(1.0-e))*TAN(E0/2.0))
my_omega2=omega2/180.0*!PI; convert variables in radians and copy them to a new variable to prevent changes to the input parameter
my_i=i/180.0*!PI
my_Omega=Omega/180.0*!PI
theta=my_Omega+ATAN(SIN(nu+my_omega2)*COS(my_i),COS(nu+my_omega2))
rho=r*COS(nu+my_omega2)/COS(theta-my_Omega)
theta=theta*180.0/!PI; convert theta in degree

CIRRANGE,theta; force theta to be in 0..360 range
print,'rho=   ',rho,'    theta= ',theta
result.rho=rho
result.theta=theta

ENDIF ELSE print,'Syntax - RHOTHETA, P, T, e, a, i, Omega, omega2, t2'

RETURN,result

 end

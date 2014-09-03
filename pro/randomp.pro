pro randomp,x,pow,n,range_x=range_x,seed=s
;+
; NAME:
;       RANDOMP
; PURPOSE:
;       Generates an array of random numbers distributed as a power law.
; CALLING SEQUENCE:
;       RANDOMP, X, Pow, N, [ RANGE_X = [low,high], SEED= ]'
; INPUTS:
;       Pow:  Exponent of power law.
;               The pdf of X is f_X(x) = A*x^pow, low <= x <= high
;               ASTRONOMERS PLEASE NOTE:  
;               pow is little gamma  = big gamma - 1 for stellar IMFs.
;       N:    Number of elements in generated vector.
;
; OPTIONAL INPUT KEYWORD PARAMETER:
;       RANGE_X:  2-element vector [low,high] specifying the range of 
;               output X values; the default is [5, 100].
;
; OPTIONAL INPUT-OUTPUT KEYWORD PARAMETER:
;       SEED:    Seed value for RANDOMU function.    As described in the 
;               documentation for RANDOMU, the value of SEED is updated on 
;               each call to RANDOMP, and taken from the system clock if not
;               supplied.   This keyword can be used to have RANDOMP give 
;               identical results on different runs.
; OUTPUTS:
;       X:    Vector of random numbers, distributed as a power law between
;               specified range
; PROCEDURE:  
;       "Transformation Method" for random variables is described in Bevington 
;       & Robinson, "Data Reduction & Error Analysis for Physical Sciences", 2nd
;       Edition (McGraw-Hill, 1992). p. 83.
;       Output of RANDOMU function is transformed to power-law
;       random variable.
;
; EXAMPLE:
;       Create a stellar initial mass function (IMF) with 10000 stars
;       ranging from 0.5 to 100 solar masses and a Salpeter slope.  Enter:
;
;       RANDOMP,MASS,-2.35,10000,RANGE_X=[0.5,100]
;
; NOTES:
;       Versions 5.1.1 and V5.2 of IDL have a bug in RANDOMU such that the SEED
;       value is initialized to the same value at the start of each session,
;       rather than being initialized by the system clock.    RANDOMP will be
;       affected in a similar manner.
; MODIFICATION HISTORY:
;       Written by R. S. Hill, Hughes STX, July 13, 1995
;       July 14, 1995   SEED keyword added at Landsman's suggestion.
;                    Documentation converted to standard format.  RSH
;       Converted to IDL V5.0   W. Landsman   September 1997
;-
 On_error,2

 if N_params() LT 3 then begin
   print,'Syntax - RANDOMP, x, pow, n, [ RANGE_X = [low,high], SEED= ]'
   return
 endif

 if N_elements(range_x) lt 1 then range_x=[5,100]
 if N_elements(range_x) ne 2 then begin
   message,'Error - RANGE_X keyword must be a 2 element vector',/CON
   return
 endif

 pow1 = pow + 1.0
 lo = range_x[0] & hi = range_x[1]
 if lo GT hi then begin
  temp=lo & lo=hi & hi=tmp
 endif

 r = randomu(s, n )
 if pow NE -1.0 then begin
   norm = 1.0d0/(hi^pow1 - lo^pow1)
   expo = alog10(r/norm + lo^pow1)/pow1
   x = 10.0^expo
 endif else begin
   norm = 1.0d0/(alog(hi) - alog(lo))
   x = exp(r/norm + alog(lo))
 endelse

 return
 end

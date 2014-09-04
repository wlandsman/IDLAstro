 function pent,p,t,x,m,n
;+
; NAME:
;       PENT
; PURPOSE:
;       Return the information entropy of a time series
; EXPLANATION:
;       This function will return S, the information entropy of a time series
;       for a set of trial periods 
;
; CATEGORY:
;       Time series analysis, period finding, astronomical utilities.
;
; CALLING SEQUENCE:
;       Result = PENT(P, T, X, [N, M ] )
;
; INPUTS:
;       P - array of trial period values.
;       T - array of observation times (same units as P).
;       X - array of observations.
;
; OPTIONAL INPUTS:
;       N   - If  four parameters are given then the 4th parameter is assumed
;               to be N. Then NxN boxes are used to calculate S.
;       M,N - If five parameters are given then parameter 4 is M and parameter
;               5 is N. S is then calculated using MxN boxes - M partitions for the
;               phase and N partitions for the data.
;       
; OUTPUTS:
;       This function returns S, the information entropy of the time series for
;       the periods given in P as defined by Cincotta, Me'ndez & Nu'n~ez
;       (Astrophysical Journal 449, 231-235, 1995). The minima of S occur at
;       values of P where X shows periodicity.
;   
; PROCEDURE:
;       The procedure involves dividing the phase space into N^2 partitions 
;       (NxN boxes) and then calculating:
;       
;               __ N^2
;         S = - \        mu_i . ln(mu_i)  for all mu_i <> 0
;               /_  
;                 i = 1 
;
;       where  mu_i is the number of data points in partition i normalised by 
;       the number of partitions.
;
;       The option of using MxN boxes is an additional feature of this routine.
;
; EXAMPLE:
;
;       To generate a similar synthetic data set to Cincotta et al. we
;        do the following:
;
;       IDL> P0 = 173.015                        ; Fundamental period
;       IDL> T = randomu(seed,400)*15000         ; 400 random observation times
;       IDL> A0 = 14.0                           ; Mean magnitude
;       IDL> M0 = -0.5  * sin(2*!pi*T/P0)        ; Fundamental mode
;       IDL> M1 = -0.15 * sin(4*!pi*T/P0)        ; 1st harmonic
;       IDL> M2 = -0.05 * sin(6*!pi*T/P0)        ; 2nd harmonic
;       IDL> sig = randomu(seed,400)*0.03        ; noise
;       IDL> U = A0 + M0 + M1 + M2 + sig         ; Synthetic data
;       IDL> Ptest = 100. + findgen(2000)/2.     ; Trial periods 
;       IDL> S = pent(Ptest,T,U)                 ; Calculate S
;               ... this takes a few seconds ...
;       IDL> plot,Ptest,S,xtitle="P",ytitle="S"  ; plot S v. P
;       IDL> print,Ptest(where(S eq min(S)))     ; Print best period (+/- 0.5)
;
;       The plot produced should be similar to Fig. 2 of Cincotta et al.
;
; RESTRICTIONS:
;
;       My own (limited) experience with this routine suggests that it is not
;       as good as other techniques for finding  weak,  multi-periodic signals in 
;       poorly sampled  data, but is good for establishing periods of eclipsing
;       binary stars when M is quite large (try MxN = 64x16, 128x16 or even 
;       256x16).  This suggests it may be good for other periodic light curves 
;       (Cepheids, RR Lyrae etc.).
;       I would be glad to receive reports of other peoples experience with
;       this technique (e-mail pflm@bro730.astro.ku.dk).
;
; MODIFICATION HISTORY:
;       Written by:   Pierre Maxted, 14Sep95
;       Modifications:
;       Normalisation of S corrected, T-min(T) taken out of loop.
;               -  Pierre Maxted, 15Sep95
;       Converted to IDL V5.0   W. Landsman   September 1997
;-

 on_error,2 ; return to caller

; Check suitable no. of parameters have been entered.

  case N_params() of 
   3 : begin
        n = 8.0 
        m = 8.0
       end
   4 : begin
        n = float(fix(m)) 
        m = n
       end
   5 : begin
        m = float(fix(m)) 
        n = float(fix(n))
       end
  else : message,/noname,' Syntax - Result = ( P, T, X [ [,M ] ,N ])'
  endcase

  nbox = m*n
  np = n_elements(p)
  npts = n_elements(x)

  if n_elements(t) ne  npts  then message , $
     'Input arrays T and X  must have same number of elements'

  if npts lt 3 then message,' Insufficient data in input arrays'

  npts = float(npts)

  S = fltarr(np)
 
   norm = (X - min(X))/(max(x) - min(x))   ; normalised data
   norm = norm - (norm eq 1.0)*(0.1/n) ; norm = 1 -> norm = 0.99..
   ni = 1 + n*(floor(norm*n))

   Tplus = T-min(T)  ; take this operation out of the loop
 
  for j = 0l,np - 1l do begin
 
   phi = ( Tplus / P[j] ) mod  1.0
 
   mu = histogram(floor(phi*m) + ni,max=nbox,min=0.0)/(npts)
 
   mu = mu[where(mu gt 0.0)]
   S[j] = -total(mu*alog(mu))
   
  endfor
  
  S = S/alog(nbox) ; normalise S

  return,S

end   ; That's all folks



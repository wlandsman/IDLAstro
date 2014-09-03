;function to detect type of error array input
function errtype, err, bad_err_msg
sz = size(err)
  case sz[0] of
    0: errtype = 'sigma'
    1: errtype = 'sigmas'
    3: errtype = 'pdfs'
    else: message,bad_err_msg
  endcase
return,errtype
end

;function to check for consistent error array input
pro vet_err, err, errtype, n, bad_err_msg
  sz = size(err)
  
  badinput = 0 ;turn this switch on if input is bad
  ;check that dimensions are good
  ;if errtype eq 'sigma' -- no action needed for scalar
  if errtype eq 'sigmas' and sz[1] ne n then badinput = 1
  if errtype eq 'pdfs' and (sz[1] ne n or sz[2] ne 2) then badinput = 1
  
  ;print error if bad dimensions
  if badinput then message,bad_err_msg
end

;function to generate simulated data based on values and error array
function generate_data, v, err, type, n, nsim, dbl, seed
  r = type eq 'pdfs' ? randomU(seed, n, nsim, double=dbl) : randomN(seed, n, nsim, double=dbl)
  case type of
    ;v # replicate(1,n) uses matrix multiplication to create an array where the
    ;nth column is filled with v[n]
    'sigma': simdata = r*err + (v # replicate(1,nsim))
    'sigmas': simdata = r*(err # replicate(1,nsim)) + (v # replicate(1,nsim))
    'pdfs': begin
      simdata = dbl ? dblarr(n, nsim) : fltarr(n, nsim)
      for i = 0,n-1 do begin
        pdfx = err[i,0,*]
        pdfy = err[i,1,*]
        
        ;first compute the cdf from the pdf using trapezoidal integration
        trapezoid_areas = 0.5*(pdfy[1:-1] + pdfy[0:-2])*(pdfx[1:-1] - pdfx[0:-2])
        f = TOTAL(trapezoid_areas,/CUMULATIVE)
        f = f/f[-1] ;ensure it is normalized
        
        ;modify x vector have one pt centered at each trapezoidal element
        pdfx = (pdfx[1:-1] + pdfx[0:-2])/2.
        
        ;transform uniform to input distribution via interpolation from the cdf
        simdata[i,*] = INTERPOL(pdfx, f, r[i,*])
      endfor
    end
  endcase
  return,simdata
end

;;;;; THE MAIN FUNCTION ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
function safe_correlate, x, y, xerr, yerr, nsim=nsim, seed=seed
;+
;
; NAME:
;       SAFE_CORRELATE
;
; PURPOSE:
;       This function computes the probability by which the null hypothesis of 
;       uncorrelated data may be rejected while accounting for uncertainty in 
;       the data values.
;
; EXPLANATION:
;       This function generates NSIM simulated X,Y datasets based on the
;       provided points and their erros. These are then used to compute
;       the probability that uncorrelated data could explain the arrangement
;       of the points, the probability-to-exceed or PTE, using Spearman's rank 
;       correlation test. Each simulated dataset is assigned a probability of 
;       1/NSIM of occuring. Thus, for a given dataset, the probability that the 
;       true data (given the uncertainties) are arranged as simulated AND
;       that this particular arrangment of data can be explained without an 
;       underlying correlation is PTE/NSIM. These values are summed to compute 
;       the overall probability that the data represent an uncorrelated 
;       arrangement of points (in other words, the p-value or PTE for the null 
;       hypothesis of uncorrelated data).
;
;       A tutorial on SAFE_CORRELATE is available at 
;       http://parkeloyd.com/output/code/safe_correlate/
;
; CALLING SEQUENCE:
;       Result = SAFE_CORRELATE(X, Y, XERR, YERR, [NSIM=1e4, SEED=SEED])
;
; INPUTS:
;       X,Y:       N-element vectors of the data points. These are ignored if 
;                  PDF input is supplied for X or Y (see below).
;       
;       XERR,YERR: The data point errors. These may be supplied as a scalar, 
;                  N-element vector, 2xM array, or Nx2xM array.
;                  scalar: The identical Gaussian 1-sigma error for all 
;                          points.
;                  N vector: The Gaussian 1-sigma error for each respective 
;                            point.
;                  Nx2xM array: M points sampling the probability distribution 
;                               function (PDF) for each data point. The values 
;                               are contained in [N,0,*] and probability 
;                               densities in [N,1,*]. This is useful for 
;                               non-Gaussian errors, especially upper limits.
;
; KEYWORD PARAMETERS:
;       NSIM: The number of X,Y datasets to simulate. Default = 1e4.
;       SEED: Random number seed for use with RANDOMN and RANDOMU. Useful for 
;             ensuring reproducible results. Can either be an input value or 
;             a variable into which the used value will be stored.
;
; EXAMPLES:
;       Data with identical errors:
;         xerr = 2.0
;         yerr = 3.0
;          
;         ;generate linear data with errors
;         N = 10
;         x = findgen(N) + randomn(seed,N)*xerr
;         y = findgen(N) + randomn(seed,N)*yerr
;          
;         ;plot
;         ep = errorplot(x,y,replicate(xerr,N),replicate(yerr,N),'o')
;          
;         ;corrrelate
;         print,safe_correlate(x,y,xerr,yerr)
;          
;       Data with differing errors, 5e3 simulations:
;         ;generate nonuniform errors
;         N = 10
;         xerr = randomu(seed,N) + 1.0
;         yerr = randomu(seed,N)*1.5 + 1.0
;         
;         ;generate linear data with errors
;         x = findgen(N) + randomn(seed,N)*xerr
;         y = findgen(N) + randomn(seed,N)*yerr
;         
;         ;plot
;         ep = errorplot(x,y,xerr,yerr,'o')
;         
;         ;correlate
;         print,safe_correlate(x,y,xerr,yerr,nsim=5e3)
;         
;       Data with non-gaussian errors
;         ;generate linear data with some scatter
;         N = 10
;         x = findgen(N) + 5 + 2*randomn(seed,N)
;         y = findgen(N) + 5 + 3*randomn(seed,N)
;         
;         ;assign uniform pdfs to the x data and gamma distributions to the
;         ;y data (just for example, since the data were actaully generated
;         ;from a Gaussian PDF)
;         ;note that the PDFs do not have to be normalized
;         M = 1000 ;number of points sampling pdfs
;         xerr = fltarr(N,2,M)
;         yerr = fltarr(N,2,M)
;         t = 0.7 ;gamma distribution scale parameter
;         for i = 0,N-1 do begin &$
;           xvalues = findgen(M)/(M-1) + x[i] - 0.5 &$ ;width = 1.0
;           xprobs = replicate(1.0, M) &$
;           xerr[i,0,*] = xvalues &$
;           xerr[i,1,*] = xprobs &$
;           yvalues = findgen(M)/(M-1)*y[i]*2.0 &$
;           k = y[i]/t + 1 &$
;           yprobs = yvalues^(k-1)*exp(-yvalues/t)/t^k/gamma(k) &$
;           yerr[i,0,*] = yvalues &$
;           yerr[i,1,*] = yprobs &$
;         endfor
;         
;         ;correlate
;         print,safe_correlate(x,y,xerr,yerr)
;
; REFERENCE:
;       See Numerical Recipes by Press et al. for information on the 
;       Spearman Rank correlation test.
;
; MODIFICATION HISTORY:
;       Written by:  R. O. Parke Loyd, 2014-07
;-

;;;;; GROOM AND VET THE INPUT ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

bad_err_msg = 'Bad input shape for xerr or yerr, see code header.'

;determine type of error array supplied (sigma, sigmas, pdfs)
xerrtype = errtype(xerr, bad_err_msg)
yerrtype = errtype(yerr, bad_err_msg)

;check if x and y are going to be used and, if so, make sure they have the same
;length
if xerrtype eq 'pdfs' then begin
  temp = size(xerr)
  n = temp[1]
endif else begin
  if yerrtype eq 'pdfs' then begin
    temp = size(yerr)
    n = temp[1]
  endif else begin
    n = n_elements(x)
    if n ne n_elements(y) then begin
      message, 'The x and y vectors must have the same number of points.'
    endif
  endelse
endelse

;check that error input is good and determine its type
vet_err,xerr,xerrtype,n,bad_err_msg
vet_err,yerr,yerrtype,n,bad_err_msg

;record whether double precision is used
dbl = isa(x,'double') or isa(y,'double')

;set default number of simulations
if ~keyword_set(nsim) then nsim = 1e4

;;;;; GENERATE SIMULATED DATA ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
xsim = generate_data(x, xerr, xerrtype, n, nsim, dbl, seed)
ysim = generate_data(y, yerr, yerrtype, n, nsim, dbl, seed)

;;;;; COMPUTE PROBABILITY TO EXCEED FOR NULL HYPOTHESIS ;;;;;;;;;;;;;;;;;;;;;;; 

pte = 0.0d
for i = 0,nsim-1 do begin
  result = r_correlate(xsim[*,i], ysim[*,i])
  pte += result[1]
endfor
pte = pte/nsim

return,pte

end

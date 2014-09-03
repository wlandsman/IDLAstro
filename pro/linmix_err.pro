;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;+
;   NAME:
;     LINMIX_ERR
;   PURPOSE:
;      Bayesian approach to linear regression with errors in both X and Y
;   EXPLANATION:
;     Perform linear regression of y on x when there are measurement
;     errors in both variables. the regression assumes :
;
;                 ETA = ALPHA + BETA * XI + EPSILON
;                 X = XI + XERR
;                 Y = ETA + YERR
;
;
; Here, (ALPHA, BETA) are the regression coefficients, EPSILON is the
; intrinsic random scatter about the regression, XERR is the
; measurement error in X, and YERR is the measurement error in
; Y. EPSILON is assumed to be normally-distributed with mean zero and
; variance SIGSQR. XERR and YERR are assumed to be
; normally-distributed with means equal to zero, variances XSIG^2 and
; YSIG^2, respectively, and covariance XYCOV. The distribution of XI
; is modelled as a mixture of normals, with group proportions PI,
; mean MU, and variance TAUSQR. Bayesian inference is employed, and
; a structure containing random draws from the posterior is
; returned. Convergence of the MCMC to the posterior is monitored
; using the potential scale reduction factor (RHAT, Gelman et
; al.2004). In general, when RHAT < 1.1 then approximate convergence
; is reached.
;
; Simple non-detections on y may also be included.
;
; CALLING SEQUENCE:
;
;     LINMIX_ERR, X, Y, POST, XSIG=, YSIG=, XYCOV=, DELTA=, NGAUSS=, /SILENT, 
;                /METRO, MINITER= , MAXITER= 
;
;
; INPUTS :
;
;   X - THE OBSERVED INDEPENDENT VARIABLE. THIS SHOULD BE AN
;       NX-ELEMENT VECTOR.
;   Y - THE OBSERVED DEPENDENT VARIABLE. THIS SHOULD BE AN NX-ELEMENT
;       VECTOR.
;
; OPTIONAL INPUTS :
;
;   XSIG - THE 1-SIGMA MEASUREMENT ERRORS IN X, AN NX-ELEMENT VECTOR.
;   YSIG - THE 1-SIGMA MEASUREMENT ERRORS IN Y, AN NX-ELEMENT VECTOR.
;   XYCOV - THE COVARIANCE BETWEEN THE MEASUREMENT ERRORS IN X AND Y,
;           AND NX-ELEMENT VECTOR.
;   DELTA - AN NX-ELEMENT VECTOR INDICATING WHETHER A DATA POINT IS
;           CENSORED OR NOT. IF DELTA[i] = 1, THEN THE SOURCE IS
;           DETECTED, ELSE IF DELTA[i] = 0 THE SOURCE IS NOT DETECTED
;           AND Y[i] SHOULD BE AN UPPER LIMIT ON Y[i]. NOTE THAT IF
;           THERE ARE CENSORED DATA POINTS, THEN THE
;           MAXIMUM-LIKELIHOOD ESTIMATE (THETA) IS NOT VALID. THE
;           DEFAULT IS TO ASSUME ALL DATA POINTS ARE DETECTED, IE,
;           DELTA = REPLICATE(1, NX).
;   METRO - IF METRO = 1, THEN THE MARKOV CHAINS WILL BE CREATED USING
;           THE METROPOLIS-HASTINGS ALGORITHM INSTEAD OF THE GIBBS
;           SAMPLER. THIS CAN HELP THE CHAINS CONVERGE WHEN THE SAMPLE
;           SIZE IS SMALL OR IF THE MEASUREMENT ERRORS DOMINATE THE
;           SCATTER IN X AND Y.
;   SILENT - SUPPRESS TEXT OUTPUT.
;   MINITER - MINIMUM NUMBER OF ITERATIONS PERFORMED BY THE GIBBS
;             SAMPLER OR METROPOLIS-HASTINGS ALGORITHM. IN GENERAL,
;             MINITER = 5000 SHOULD BE SUFFICIENT FOR CONVERGENCE. THE
;             DEFAULT IS MINITER = 5000. THE MCMC IS STOPPED AFTER 
;             RHAT < 1.1 FOR ALL PARAMETERS OF INTEREST, AND THE
;             NUMBER OF ITERATIONS PERFORMED IS GREATER THAN MINITER.
;   MAXITER - THE MAXIMUM NUMBER OF ITERATIONS PERFORMED BY THE
;             MCMC. THE DEFAULT IS 1D5. THE MCMC IS STOPPED
;             AUTOMATICALLY AFTER MAXITER ITERATIONS.
;   NGAUSS - THE NUMBER OF GAUSSIANS TO USE IN THE MIXTURE
;            MODELLING. THE DEFAULT IS 3. IF NGAUSS = 1, THEN THE
;            PRIOR ON (MU, TAUSQR) IS ASSUMED TO BE UNIFORM.
;
; OUTPUT :
;
;    POST - A STRUCTURE CONTAINING THE RESULTS FROM THE MCMC. EACH
;           ELEMENT OF POST IS A DRAW FROM THE POSTERIOR DISTRIBUTION
;           FOR EACH OF THE PARAMETERS.
;
;             ALPHA - THE CONSTANT IN THE REGRESSION.
;             BETA - THE SLOPE OF THE REGRESSION.
;             SIGSQR - THE VARIANCE OF THE INTRINSIC SCATTER.
;             PI - THE GAUSSIAN WEIGHTS FOR THE MIXTURE MODEL.
;             MU - THE GAUSSIAN MEANS FOR THE MIXTURE MODEL.
;             TAUSQR - THE GAUSSIAN VARIANCES FOR THE MIXTURE MODEL.
;             MU0 - THE HYPERPARAMETER GIVING THE MEAN VALUE OF THE
;                   GAUSSIAN PRIOR ON MU. ONLY INCLUDED IF NGAUSS >
;                   1.
;             USQR - THE HYPERPARAMETER DESCRIBING FOR THE PRIOR
;                    VARIANCE OF THE INDIVIDUAL GAUSSIAN CENTROIDS
;                    ABOUT MU0. ONLY INCLUDED IF NGAUSS > 1.
;             WSQR - THE HYPERPARAMETER DESCRIBING THE `TYPICAL' SCALE
;                    FOR THE PRIOR ON (TAUSQR,USQR). ONLY INCLUDED IF
;                    NGAUSS > 1.
;             XIMEAN - THE MEAN OF THE DISTRIBUTION FOR THE
;                      INDEPENDENT VARIABLE, XI.
;             XISIG - THE STANDARD DEVIATION OF THE DISTRIBUTION FOR
;                     THE INDEPENDENT VARIABLE, XI.
;             CORR - THE LINEAR CORRELATION COEFFICIENT BETWEEN THE
;                    DEPENDENT AND INDEPENDENT VARIABLES, XI AND ETA.
;
; CALLED ROUTINES :
;
;    RANDOMCHI, MRANDOMN, RANDOMGAM, RANDOMDIR, MULTINOM
;
; REFERENCES :
;
;   Carroll, R.J., Roeder, K., & Wasserman, L., 1999, Flexible
;     Parametric Measurement Error Models, Biometrics, 55, 44
;
;   Kelly, B.C., 2007, Some Aspects of Measurement Error in
;     Linear Regression of Astronomical Data, The Astrophysical
;     Journal, 665, 1489 (arXiv:0705.2774)
;
;   Gelman, A., Carlin, J.B., Stern, H.S., & Rubin, D.B., 2004,
;     Bayesian Data Analysis, Chapman & Hall/CRC
;
; REVISION HISTORY
;
;     AUTHOR : BRANDON C. KELLY, STEWARD OBS., JULY 2006
;   - MODIFIED PRIOR ON MU0 TO BE UNIFORM OVER [MIN(X),MAX(X)] AND
;     PRIOR ON USQR TO BE UNIFORM OVER [0, 1.5 * VARIANCE(X)]. THIS
;     TENDS TO GIVE BETTER RESULTS WITH FEWER GAUSSIANS. (B.KELLY, MAY
;     2007)
;   - FIXED BUG SO THE ITERATION COUNT RESET AFTER THE BURNIN STAGE
;     WHEN SILENT = 1 (B. KELLY, JUNE 2009)
;   - FIXED BUG WHEN UPDATING MU VIA THE METROPOLIS-HASTING
;     UPDATE. PREVIOUS VERSIONS DID NO INDEX MUHAT, SO ONLY MUHAT[0]
;     WAS USED IN THE PROPOSAL DISTRIBUTION. THANKS TO AMY BENDER FOR
;     POINTING THIS OUT. (B. KELLY, DEC 2011)
;-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;routine to compute the hyperbolic arctangent
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

function linmix_atanh, x

z = 0.5d * ( alog(1 + x) - alog(1 - x) )

return, z
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;routine to compute a robust estimate for the standard deviation of a
;data set, based on the inter-quartile range
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

function linmix_robsig, x

nx = n_elements(x)
                                ;get inter-quartile range of x
sorted = sort(x)
iqr = x[sorted[3 * nx / 4]] - x[sorted[nx / 4]]
sdev = stddev(x, /nan)
sigma = min( [sdev, iqr / 1.34] ) ;use robust estimate for sigma
if sigma eq 0 then sigma = sdev

return, sigma
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;routine to compute the log-likelihood of the data
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

function loglik_mixerr, x, y, xvar, yvar, xycov, delta, theta, pi, mu, tausqr, Glabel

alpha = theta[0]
beta = theta[1]
sigsqr = theta[2]

nx = n_elements(x)
ngauss = n_elements(pi)

Sigma11 = dblarr(nx, ngauss)
Sigma12 = dblarr(nx, ngauss)
Sigma22 = dblarr(nx, ngauss)
determ = dblarr(nx, ngauss)

for k = 0, ngauss - 1 do begin

    Sigma11[0,k] = beta^2 * tausqr[k] + sigsqr + yvar
    Sigma12[0,k] = beta * tausqr[k] + xycov
    Sigma22[0,k] = tausqr[k] + xvar

    determ[0, k] = Sigma11[*,k] * Sigma22[*,k] - Sigma12[*,k]^2

endfor

det = where(delta eq 1, ndet, comp=cens, ncomp=ncens) ;any non-detections?

loglik = dblarr(nx)

if ndet gt 0 then begin
                                ;compute contribution to
                                ;log-likelihood from the detected
                                ;sources
    for k = 0, ngauss - 1 do begin
        
        gk = where(Glabel[det] eq k, nk)

        if nk gt 0 then begin

            zsqr = (y[det[gk]] - alpha - beta * mu[k])^2 / Sigma11[det[gk],k] + $
              (x[det[gk]] - mu[k])^2 / Sigma22[det[gk],k] - $
              2d * Sigma12[det[gk],k] * (y[det[gk]] - alpha - beta * mu[k]) * $
              (x[det[gk]] - mu[k]) / (Sigma11[det[gk],k] * Sigma22[det[gk],k])
            
            corrz = Sigma12[det[gk],k] / sqrt( Sigma11[det[gk],k] * Sigma22[det[gk],k] )
            
            loglik[det[gk]] = -0.5d * alog(determ[det[gk],k]) - 0.5 * zsqr / (1d - corrz^2)

        endif

    endfor

endif

if ncens gt 0 then begin
                                ;compute contribution to the
                                ;log-likelihood from the
                                ;non-detections
    for k = 0, ngauss - 1 do begin

        gk = where(Glabel[cens] eq k, nk)

        if nk gt 0 then begin
            
            loglikx = -0.5 * alog(Sigma22[cens[gk],k]) - $
              0.5 * (x[cens[gk]] - mu[k])^2 / Sigma22[cens[gk],k]
            
                                ;conditional mean of y, given x and
                                ;G=k
            cmeany = alpha + beta * mu[k] + Sigma12[cens[gk],k] / Sigma22[cens[gk],k] * $
              (x[cens[gk]] - mu[k])
                                ;conditional variance of y, given x
                                ;and G=k
            cvary = Sigma11[cens[gk],k] - Sigma12[cens[gk],k]^2 / Sigma22[cens[gk],k]

                                ;make sure logliky is finite
            logliky = alog(gauss_pdf( (y[cens[gk]] - cmeany) / sqrt(cvary) )) > (-1d300) 
            
            loglik[cens[gk]] = loglikx + logliky
        
        endif

    endfor
    
endif

loglik = total(loglik)

return, loglik
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;routine to compute the log-prior of the data
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

function logprior_mixerr, mu, mu0, tausqr, usqr, wsqr

ngauss = n_elements(mu)

if ngauss gt 1 then begin

    logprior_mu = -0.5 * alog(usqr) - 0.5 * (mu - mu0)^2 / usqr
    logprior_mu = total(logprior_mu)
    
    logprior_tausqr = 0.5 * alog(wsqr) - 1.5 * alog(tausqr) - 0.5 * wsqr / tausqr
    logprior_tausqr = total(logprior_tausqr)

    logprior = logprior_mu + logprior_tausqr

endif else logprior = 0d ;if ngauss = 1 then uniform prior

return, logprior
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;routine to perform the Metropolis update for the scale parameter in
;the Gibbs sampler
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

function linmix_metro_update, logpost_new, logpost_old, seed, log_jrat

lograt = logpost_new - logpost_old

if n_elements(log_jrat) gt 0 then lograt = lograt + log_jrat

accept = 0

if lograt gt 0 then accept = 1 else begin
    
    u = randomu(seed)

    if alog(u) le lograt then accept = 1
    
endelse

return, accept
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;routine to acceptance rates for metropolis-hastings algorithm
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

pro linmix_metro_results, arate, ngauss

print, ''
print, 'Metropolis-Hastings Acceptance Rates:'

print, '(ALPHA, BETA) : ' + strtrim(arate[0], 1)
print, 'SIGMA^2 : ' + strtrim(arate[1], 1)
print, ''
for k = 0, ngauss - 1 do begin

    print, 'GAUSSIAN ' + strtrim(k+1,1)
    print, '   MEAN : ' + strtrim(arate[2+k], 1)
    print, '   VARIANCE : ' + strtrim(arate[2+k+ngauss], 1)

endfor

if ngauss gt 1 then begin

    print, ''
    print, 'Mu0 : ' + strtrim(arate[2+2*ngauss], 1)
    print, 'u^2 : ' + strtrim(arate[3+2*ngauss], 1)
    print, 'w^2 : ' + strtrim(arate[4+2*ngauss], 1)

endif

print, ''

return
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                     ;
;                             MAIN ROUTINE                            ;
;                                                                     ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

pro linmix_err, x, y, post, xsig=xsig, ysig=ysig, xycov=xycov, delta=delta, $
                ngauss=ngauss, metro=metro, silent=silent, miniter=miniter, $
                maxiter=maxiter

if n_params() lt 3 then begin

    print, 'Syntax- LINMIX_ERR, X, Y, POST, XSIG=XSIG, YSIG=YSIG, XYCOV=XYCOV,'
    print, '                    DELTA=DELTA, NGAUSS=NGAUSS, /SILENT, /METRO, '
    print, '                    MINITER=MINITER, MAXITER=MAXITER'
    return

endif

;check inputs and setup defaults

nx = n_elements(x)
if n_elements(y) ne nx then begin
    print, 'Y and X must have the same size.'
    return
endif

if n_elements(xsig) eq 0 and n_elements(ysig) eq 0 then begin
    print, 'Must supply at least one of XSIG or YSIG.'
    return
endif

if n_elements(xsig) eq 0 then begin
    xsig = dblarr(nx)
    xycov = dblarr(nx)
endif
if n_elements(ysig) eq 0 then begin
    ysig = dblarr(nx)
    xycov = dblarr(nx)
endif
if n_elements(xycov) eq 0 then xycov = dblarr(nx)

if n_elements(xsig) ne nx then begin
    print, 'XSIG and X must have the same size.'
    return
endif
if n_elements(ysig) ne nx then begin
    print, 'YSIG and X must have the same size.'
    return
endif
if n_elements(xycov) ne nx then begin
    print, 'XYCOV and X must have the same size.'
    return
endif

if n_elements(delta) eq 0 then delta = replicate(1, nx)
if n_elements(delta) ne nx then begin
    print, 'DELTA and X must have the same size.'
    return
endif

bad = where(finite(x) eq 0 or finite(y) eq 0 or finite(xsig) eq 0 or $
            finite(ysig) eq 0 or finite(xycov) eq 0, nbad)

if nbad gt 0 then begin
    print, 'Non-finite input detected.'
    return
endif

det = where(delta eq 1, ndet, comp=cens, ncomp=ncens) ;get detected data points

if ncens gt 0 then begin

    cens_noerr = where(ysig[cens] eq 0, ncens_noerr)
    if ncens_noerr gt 0 then begin
        print, 'NON-DETECTIONS FOR Y MUST HAVE NON-ZERO MEASUREMENT ERROR VARIANCE.'
        return
    endif

endif

 ;find data points without measurement error
xnoerr = where(xsig eq 0, nxnoerr, comp=xerr, ncomp=nxerr)
ynoerr = where(ysig eq 0, nynoerr, comp=yerr, ncomp=nyerr)

if nxerr gt 0 then ynoerr2 = where(ysig[xerr] eq 0, nynoerr2) else nynoerr2 = 0L
if nyerr gt 0 then xnoerr2 = where(xsig[yerr] eq 0, nxnoerr2) else nxnoerr2 = 0L

xvar = xsig^2
yvar = ysig^2
xycorr = xycov / (xsig * ysig)
if nxnoerr gt 0 then xycorr[xnoerr] = 0d
if nynoerr gt 0 then xycorr[ynoerr] = 0d

if not keyword_set(metro) then metro = 0
if metro then gibbs = 0 else gibbs = 1
if not keyword_set(silent) then silent = 0
if n_elements(ngauss) eq 0 then ngauss = 3

if ngauss le 0 then begin
    print, 'NGAUSS must be at least 1.'
    return
endif

if n_elements(miniter) eq 0 then miniter = 5000L ;minimum number of iterations that the 
                                                 ;Markov Chain must perform
if n_elements(maxiter) eq 0 then maxiter = 100000L ;maximum number of iterations that the 
                                                   ;Markov Chain will perform

;; perform MCMC

nchains = 4                    ;number of markov chains
checkiter = 100               ;check for convergence every 100 iterations
iter = 0L

;use BCES estimator for initial guess of theta = (alpha, beta, sigsqr)
beta = ( correlate(x, y, /covar) - mean(xycov) ) / $
  ( variance(x) - mean(xvar) )
alpha = mean(y) - beta * mean(x)

sigsqr = variance(y) - mean(yvar) - beta * (correlate(x,y, /covar) - mean(xycov))
sigsqr = sigsqr > 0.05 * variance(y - alpha - beta * x)

                                ;get initial guess of mixture
                                ;parameters prior
mu0 = median(x)
wsqr = variance(x) - median(xvar)
wsqr = wsqr > 0.01 * variance(x)

;now get MCMC starting values dispersed around these initial guesses

Xmat = [[replicate(1d, nx)], [x]]
Vcoef = invert( Xmat ## transpose(Xmat), /double ) * sigsqr

coef = mrandomn(seed, Vcoef, nchains)
chisqr = randomchi(seed, 4, nchains)

;randomly disperse starting values for (alpha,beta) from a
;multivariate students-t distribution with 4 degrees of freedom
alphag = alpha + coef[*,0] * sqrt(4d / chisqr)
betag = beta + coef[*,1] * sqrt(4d / chisqr)

                                ;draw sigsqr from an Inverse scaled
                                ;chi-square density
sigsqrg = sigsqr * (nx / 2) / randomchi(seed, nx / 2, nchains)

;get starting values for the mixture parameters, first do prior
;parameters
   
                                ;mu0 is the global mean

mu0min = min(x) ;prior for mu0 is uniform over mu0min < mu0 < mu0max
mu0max = max(x)

repeat begin
    
    mu0g = mu0 + sqrt(variance(x) / nx) * randomn(seed, nchains) / $
      sqrt(4d / randomchi(seed, 4, nchains))

    pass = where(mu0g gt mu0min and mu0g lt mu0max, npass)

endrep until npass eq nchains

                                ;wsqr is the global scale
wsqrg = wsqr * (nx / 2) / randomchi(seed, nx / 2, nchains)

usqrg = replicate(variance(x) / 2d, nchains)

;now get starting values for mixture parameters

tausqrg = dblarr(ngauss, nchains) ;initial group variances
for k = 0, ngauss - 1 do tausqrg[k,*] = 0.5 * wsqrg * 4 / $
  randomchi(seed, 4, nchains)

mug = dblarr(ngauss, nchains)   ;initial group means
for k = 0, ngauss - 1 do mug[k,*] = mu0g + sqrt(wsqrg) * randomn(seed, nchains)

;get initial group proportions and group labels

pig = dblarr(ngauss, nchains)
Glabel = intarr(nx, nchains)

if ngauss eq 1 then Glabel = intarr(nx, nchains) else begin

    for i = 0, nchains - 1 do begin
        
        for j = 0, nx - 1 do begin
                                ;classify sources to closest centroid
            dist = abs(mug[*,i] - x[j])
            mindist = min(dist, minind)
            
            pig[minind,i] = pig[minind,i] + 1
            Glabel[j,i] = minind
            
        endfor

    endfor

endelse
                                ;get initial values for pi from a
                                ;dirichlet distribution, with
                                ;parameters based on initial class
                                ;occupancies
if ngauss eq 1 then pig = transpose(replicate(1d, nchains)) else $
  for i = 0, nchains - 1 do pig[*,i] = randomdir(seed, pig[*,i] + 1)

alpha = alphag
beta = betag
sigsqr = sigsqrg
mu = mug
tausqr = tausqrg
pi = pig
mu0 = mu0g
wsqr = wsqrg
usqr = usqrg

eta = dblarr(nx, nchains)
for i = 0, nchains - 1 do eta[*,i] = y ;initial values for eta

nut = 1 ;degrees of freedom for the prior on tausqr
nuu = 1 ;degrees of freedom for the prior on usqr

;number of parameters to monitor convergence on
npar = 6

if metro then begin
;get initial variances for the jumping kernels

    jvar_coef = Vcoef
    log_ssqr = alog( sigsqr[0] * nx / randomchi(seed, nx, 1000) )
    jvar_ssqr = variance(log_ssqr) ;get variance of the jumping density
                                   ;for sigsqr

                                ;get variances for prior variance
                                ;parameters
    jvar_mu0 = variance(x) / ngauss
    jvar_wsqr = variance( alog(variance(x) * nx / randomchi(seed, nx, 1000)) )
    jvar_usqr = jvar_wsqr

    naccept = lonarr(5 + 2 * ngauss)
    
    logpost = dblarr(nchains)
                                ;get initial values of the
                                ;log-posterior
    for i = 0, nchains - 1 do begin
        
        theta = [alpha[i], beta[i], sigsqr[i]]
        loglik = loglik_mixerr( x, y, xvar, yvar, xycov, delta, theta, $
                                pi[*,i], mu[*,i], tausqr[*,i], Glabel[*,i] )
        logprior = logprior_mixerr(mu[*,i], mu0[i], tausqr[*,i], usqr[i], wsqr[i])
        logpost[i] = loglik + logprior
        
    endfor
    
endif

convergence = 0

;stop burn-in phase after BURNSTOP iterations if doing
;Metropolis-Hastings jumps, update jumping kernels every BURNITER
;iterations

burnin = metro ? 1 : 0
burniter = 250
burnstop = 500 < (miniter / 2 > 100)
                                ;start Markov Chains
if not silent then print, 'Simulating Markov Chains...'
if not silent and metro then print, 'Doing Burn-in First...'

ygibbs = y
xi = x
umax = 1.5 * variance(x) ;prior for usqr is uniform over 0 < usqr < umax

if metro then begin
                                ;define arrays now so we don't have to
                                ;create them every MCMC iteration
    Sigma11 = dblarr(nx, ngauss)
    Sigma12 = dblarr(nx, ngauss)
    Sigma22 = dblarr(nx, ngauss)
    determ = dblarr(nx, ngauss)

endif

gamma = dblarr(nx, ngauss)
nk = fltarr(ngauss)

repeat begin
    
    for i = 0, nchains - 1 do begin ;do markov chains one at-a-time
        
        if gibbs then begin
            
            if ncens gt 0 then begin
                                ;first get new values of censored y
                for j = 0, ncens - 1 do begin
                    
                    next = 0
                    repeat ygibbs[cens[j]] = eta[cens[j],i] + $
                      sqrt(yvar[cens[j]]) * randomn(seed) $
                      until ygibbs[cens[j]] le y[cens[j]]
                    
                endfor
                
            endif
            
;need to get new values of Xi and Eta for Gibbs sampler
            
            if nxerr gt 0 then begin
                                ;first draw Xi|theta,x,y,G,mu,tausqr
                xixy = x[xerr] + xycov[xerr] / yvar[xerr] * (eta[xerr,i] - ygibbs[xerr])
                if nynoerr2 gt 0 then xixy[ynoerr2] = x[xerr[ynoerr2]]
                xixyvar = xvar[xerr] * (1 - xycorr[xerr]^2)
                
                for k = 0, ngauss - 1 do begin ;do one gaussian at-a-time
                    
                    group = where(Glabel[xerr,i] eq k, ngroup)
                    
                    if ngroup gt 0 then begin
                        
                        xihvar = 1d / (beta[i]^2 / sigsqr[i] + 1d / xixyvar[group] + $
                                       1d / tausqr[k,i])
                        xihat = xihvar * $
                          (xixy[group] / xixyvar[group] + $
                           beta[i] * (eta[xerr[group],i] - alpha[i]) / sigsqr[i] + $
                           mu[k,i] / tausqr[k,i])
                        
                        xi[xerr[group]] = xihat + sqrt(xihvar) * randomn(seed, ngroup)
                        
                    endif
                    
                endfor

            endif
            
            if nyerr gt 0 then begin
                                ;now draw Eta|Xi,x,y,theta
                etaxyvar = yvar[yerr] * (1d - xycorr[yerr]^2)
                etaxy = ygibbs[yerr] + xycov[yerr] / xvar[yerr] * (xi[yerr] - x[yerr])
                if nxnoerr2 gt 0 then etaxy[xnoerr2] = ygibbs[yerr[xnoerr2]]
                etahvar = 1d / (1d / sigsqr[i] + 1d / etaxyvar)
                etahat = etahvar * (etaxy / etaxyvar + $
                                    (alpha[i] + beta[i] * xi[yerr]) / sigsqr[i])
                
                eta[yerr,i] = etahat + sqrt(etahvar) * randomn(seed, nyerr)

            endif

        endif

                                ;now draw new class labels
        if ngauss eq 1 then Glabel[*,i] = 0 else begin
            
            if gibbs then begin
                                ;get unnormalized probability that
                                ;source i came from Gaussian k, given
                                ;xi[i]
                for k = 0, ngauss - 1 do $  
                  gamma[0,k] = pi[k,i] / sqrt(2d * !pi * tausqr[k,i]) * $
                  exp(-0.5 * (xi - mu[k,i])^2 / tausqr[k,i])
                
            endif else begin
                
                for k = 0, ngauss - 1 do begin
                    
                    Sigma11[0,k] = beta[i]^2 * tausqr[k,i] + sigsqr[i] + yvar
                    Sigma12[0,k] = beta[i] * tausqr[k,i] + xycov
                    Sigma22[0,k] = tausqr[k,i] + xvar
                    
                    determ[0, k] = Sigma11[*,k] * Sigma22[*,k] - Sigma12[*,k]^2
                    
                endfor
                
                if ndet gt 0 then begin
                                ;get unnormalized probability that
                                ;source i came from Gaussian k, given
                                ;x[i] and y[i]
                    for k = 0, ngauss - 1 do begin
                        
                        zsqr = (y[det] - alpha[i] - beta[i] * mu[k,i])^2 / Sigma11[det,k] + $
                          (x[det] - mu[k,i])^2 / Sigma22[det,k] - $
                          2d * Sigma12[det,k] * (y[det] - alpha[i] - beta[i] * mu[k,i]) * $
                              (x[det] - mu[k,i]) / (Sigma11[det,k] * Sigma22[det,k])
                        
                        corrz = Sigma12[det,k] / sqrt( Sigma11[det,k] * Sigma22[det,k] )
                        
                        lognorm = -0.5d * alog(determ[det,k]) - 0.5 * zsqr / (1d - corrz^2)
                        
                        gamma[det,k] = pi[k,i] * exp(lognorm) / (2d * !pi)
                            
                    endfor
                    
                endif
                
                if ncens gt 0 then begin
                                ;get unnormalized probability that
                                ;source i came from Gaussian k, given
                                ;x[i] and y[i] > y0[i]
                    for k = 0, ngauss - 1 do begin
                        
                        gamma[cens,k] = pi[k,i] / sqrt(2d * !pi * Sigma22[cens,k]) * $
                          exp(-0.5 * (x[cens] - mu[k,i])^2 / Sigma22[cens,k])
                        
                                ;conditional mean of y, given x
                        cmeany = alpha[i] + beta[i] * mu[k,i] + Sigma12[cens,k] / Sigma22[cens,k] * $
                          (x[cens] - mu[k,i])
                                ;conditional variance of y, given x
                        cvary = Sigma11[cens,k] - Sigma12[cens,k]^2 / Sigma22[cens,k]
                                ;make sure logliky is finite
                        gamma[cens,k] = gamma[cens,k] * gauss_pdf( (y[cens] - cmeany) / sqrt(cvary) )
                        
                    endfor
                    
                endif
                
            endelse
            
            norm = total(gamma, 2)
            
            for j = 0, nx - 1 do begin
                
                gamma0 = reform(gamma[j,*]) / norm[j] ;normalized probability that the i-th data point 
                                                      ;is from the k-th Gaussian, given the observed
                                                      ;data point 
                Gjk = multinom(1, gamma0, seed=seed)
                Glabel[j,i] = where(Gjk eq 1)
                
            endfor
            
        endelse
        
;now draw new values of regression parameters, theta = (alpha, beta,
;sigsqr)
        
        if gibbs then begin
                                ;use gibbs sampler to draw alpha,beta|Xi,Eta,sigsqr
            Xmat = [[replicate(1d, nx)], [xi]]
            Vcoef = invert( Xmat ## transpose(Xmat), /double ) * sigsqr[i]
            
            coefhat = linfit( xi, eta[*,i] )
            coef = coefhat + mrandomn(seed, Vcoef)
            
            alpha[i] = coef[0]
            beta[i] = coef[1]
            
        endif else begin

            theta = [alpha[i], beta[i], sigsqr[i]]

            loglik = loglik_mixerr( x, ygibbs, xvar, yvar, xycov, delta, theta, $
                                    pi[*,i], mu[*,i], tausqr[*,i], Glabel[*,i] )
            logprior = logprior_mixerr(mu[*,i], mu0[i], tausqr[*,i], usqr[i], wsqr[i])

            logpost[i] = loglik + logprior ;log-posterior for current parameter values

                                ;use metropolis update to get new
                                ;values of the coefficients
            coef = [alpha[i], beta[i]] + mrandomn(seed, jvar_coef)
            
            theta = [coef[0], coef[1], sigsqr[i]]
            loglik_new = loglik_mixerr( x, ygibbs, xvar, yvar, xycov, delta, theta, $
                                        pi[*,i], mu[*,i], tausqr[*,i], Glabel[*,i] )
            logprior_new = logprior_mixerr(mu[*,i], mu0[i], tausqr[*,i], usqr[i], wsqr[i])
            
            logpost_new = loglik_new + logprior_new
            
            accept = linmix_metro_update( logpost_new, logpost[i], seed )
            
            if accept then begin
                
                naccept[0] = naccept[0] + 1L
                alpha[i] = coef[0]
                beta[i] = coef[1]
                logpost[i] = logpost_new
                
            endif
            
        endelse
                                ;now get sigsqr
        if gibbs then begin
            
            ssqr = total( (eta[*,i] - alpha[i] - beta[i] * xi)^2 ) / (nx - 2)
            sigsqr[i] = (nx - 2) * ssqr / randomchi(seed, nx - 2.0)
            
        endif else begin
                                ;do metropolis update
            log_ssqr = alog(sigsqr[i]) + sqrt(jvar_ssqr) * randomn(seed)
            ssqr = exp(log_ssqr)
            
            theta = [alpha[i], beta[i], ssqr]
            
            loglik_new = loglik_mixerr( x, ygibbs, xvar, yvar, xycov, delta, theta, $
                                        pi[*,i], mu[*,i], tausqr[*,i], Glabel[*,i] )
            logprior_new = logprior_mixerr(mu[*,i], mu0[i], tausqr[*,i], usqr[i], wsqr[i])
            
            logpost_new = loglik_new + logprior_new + log_ssqr
            logpost_old = logpost[i] + alog(sigsqr[i])
            
            accept = linmix_metro_update( logpost_new, logpost_old, seed )
            
            if accept then begin
                
                naccept[1] = naccept[1] + 1L
                sigsqr[i] = ssqr
                logpost[i] = loglik_new + logprior_new
                
            endif
            
        endelse 

;now do mixture model parameters, psi = (pi,mu,tausqr)
        
        if gibbs then begin

            for k = 0, ngauss - 1 do begin 

                group = where(Glabel[*,i] eq k, ngroup)
                nk[k] = ngroup

                if ngroup gt 0 then begin

                                ;get mu|Xi,G,tausqr,mu0,usqr

                    if ngauss gt 1 then begin

                        muhat = ngroup * mean(xi[group]) / tausqr[k,i] + mu0[i] / usqr[i]
                        
                        muvar = 1d / (1d / usqr[i] + ngroup / tausqr[k,i])

                    endif else begin

                        muhat = ngroup * mean(xi[group]) / tausqr[k,i]

                        muvar = tausqr[k,i] / ngroup

                    endelse

                    muhat = muvar * muhat
                    
                    mu[k,i] = muhat + sqrt(muvar) * randomn(seed)
                    
                                ;get tausqr|Xi,G,mu,wsqr,nut

                    if ngauss gt 1 then begin
                        
                        nuk = ngroup + nut
                        tsqr = (nut * wsqr[i] + total( (xi[group] - mu[k,i])^2 )) / nuk
                        
                    endif else begin

                        nuk = ngroup
                        tsqr = total( (xi[group] - mu[k,i])^2 ) / nuk

                    endelse
                
                    tausqr[k,i] = tsqr * nuk / randomchi(seed, nuk)

                endif else begin

                    mu[k,i] = mu0[i] + sqrt(usqr[i]) * randomn(seed)
                    tausqr[k,i] = wsqr[i] * nut / randomchi(seed, nut)

                endelse

            endfor
                                ;get pi|G
            if ngauss eq 1 then pi[*,i] = 1d else $
              pi[*,i] = randomdir(seed, nk + 1)

        endif else begin
                                ;do metropolis-hastings updating using
                                ;approximate Gibbs sampler

            for k = 0, ngauss - 1 do begin
                
                group = where(Glabel[*,i] eq k, ngroup)
                nk[k] = ngroup

                if ngroup gt 0 then begin
                                ;get proposal for mu[k], do
                                ;approximate Gibbs sampler
                    muprop = mu[*,i]

                    muvarx = (tausqr[k,i] + mean(xvar[group]))

                    muvar = ngauss gt 1 ? 1d / (1d / usqr[i] + ngroup / muvarx) : $
                      muvarx / ngroup

                    muhat = muprop
                    
                    chisqr = randomchi(seed, 4)
                                ;draw proposal for mu from Student's t
                                ;with 4 degrees of freedom
                    muprop[k] = muhat[k] + sqrt(muvar * 4 / chisqr) * randomn(seed)

                endif else begin
                    
                    muprop = mu[*,i]
                    muprop[k] = mu[k,i] + sqrt(usqr[i]) * randomn(seed)

                endelse

                theta = [alpha[i], beta[i], sigsqr[i]]
                
                loglik_new = loglik_mixerr( x, ygibbs, xvar, yvar, xycov, delta, theta, $
                                            pi[*,i], muprop, tausqr[*,i], Glabel[*,i] )
                logprior_new = logprior_mixerr(muprop, mu0[i], tausqr[*,i], usqr[i], wsqr[i])
                
                logpost_new = loglik_new + logprior_new
                
                accept = linmix_metro_update( logpost_new, logpost[i], seed )
                
                if accept then begin
                    
                    naccept[2+k] = naccept[2+k] + 1L
                    mu[k,i] = muprop[k]
                    logpost[i] = logpost_new
                    
                endif

                                ;get proposal for tausqr[k], do
                                ;approximate Gibbs sampler
                tsqrprop = tausqr[*,i]
 
                dof = ngroup > 1

                tsqrprop[k] = tausqr[k,i] * dof / randomchi(seed, dof)

                log_jrat = (dof + 1d) * alog(tsqrprop[k] / tausqr[k,i]) + $
                  dof / 2d * (tausqr[k,i] / tsqrprop[k] - tsqrprop[k] / tausqr[k,i])

                loglik_new = loglik_mixerr( x, ygibbs, xvar, yvar, xycov, delta, theta, $
                                            pi[*,i], mu[*,i], tsqrprop, Glabel[*,i] )
                logprior_new = logprior_mixerr(mu[*,i], mu0[i], tsqrprop, usqr[i], wsqr[i])
                
                logpost_new = loglik_new + logprior_new
                
                accept = linmix_metro_update( logpost_new, logpost[i], seed, log_jrat)
                
                if accept then begin
                    
                    naccept[2 + k + ngauss] = naccept[2 + k + ngauss] + 1L
                    tausqr[k,i] = tsqrprop[k]
                    logpost[i] = logpost_new
                    
                endif
                
            endfor
                                ;get pi|G, can do exact Gibbs sampler
                                ;for this
            if ngauss eq 1 then pi[*,i] = 1d else $
              pi[*,i] = randomdir(seed, nk + 1)

        endelse
        
;finally, update parameters for prior distribution, only do this if
;more than one gaussian
        
        if ngauss gt 1 then begin

            if gibbs then begin
                
                repeat mu0[i] = mean(mu[*,i]) + sqrt(usqr[i] / ngauss) * randomn(seed) $
                  until (mu0[i] gt mu0min) and (mu0[i] lt mu0max)

            endif else begin
                
                loglik = loglik_mixerr( x, ygibbs, xvar, yvar, xycov, delta, theta, $
                                        pi[*,i], mu[*,i], tausqr[*,i], Glabel[*,i] )
                
                muprop = mu0[i] + sqrt(jvar_mu0) * randomn(seed)

                if muprop gt mu0min and muprop lt mu0max then begin

                    logprior_old = logprior_mixerr(mu[*,i], mu0[i], tausqr[*,i], usqr[i], wsqr[i])
                    logprior_new = logprior_mixerr(mu[*,i], muprop, tausqr[*,i], usqr[i], wsqr[i])
                    
                    logpost_new = loglik + logprior_new
                    logpost_old = loglik + logprior_old
                    
                    accept = linmix_metro_update( logpost_new, logpost_old, seed )
                    
                    if accept then begin
                        
                        naccept[2 + 2 * ngauss] = naccept[2 + 2 * ngauss] + 1L
                        mu0[i] = muprop
                        logpost[i] = loglik + logprior_new
                        
                    endif
                
                endif

            endelse

            if gibbs then begin
                
                nu = ngauss + nuu
                usqr0 = (nuu * wsqr[i] + total( (mu[*,i] - mu0[i])^2 )) / nu
                
                repeat usqr[i] = usqr0 * nu / randomchi(seed, nu) $
                  until usqr[i] le umax
                
            endif else begin
                                ;do metropolis update

                log_usqr = alog(usqr[i]) + sqrt(jvar_usqr) * randomn(seed)
                usqr0 = exp(log_usqr)
                
                if usqr0 le umax then begin

                    logprior_old = logprior_mixerr(mu[*,i], mu0[i], tausqr[*,i], usqr[i], wsqr[i])
                    
                    logpost[i] = loglik + logprior_old ;update posterior after gibbs step for mu0
                    
                    logprior_new = logprior_mixerr(mu[*,i], mu0[i], tausqr[*,i], usqr0, wsqr[i])
                    
                    logpost_new = loglik + logprior_new
                    logpost_old = loglik + logprior_old
                    
                    log_jrat = log_usqr - alog(usqr[i])
                    
                    accept = linmix_metro_update( logpost_new, logpost_old, seed, log_jrat )
                    
                    if accept then begin
                        
                        naccept[3 + 2 * ngauss] = naccept[3 + 2 * ngauss] + 1L
                        usqr[i] = usqr0
                        logpost[i] = loglik + logprior_new
                        
                    endif

                endif
                
            endelse
            
            if gibbs then begin
                
                alphaw = ngauss * nut / 2d + 1
                betaw = 0.5 * nut * total(1d / tausqr[*,i])
                
                wsqr[i] = randomgam(seed, alphaw, betaw)
                
            endif else begin
                
                log_wsqr = alog(wsqr[i]) + sqrt(jvar_wsqr) * randomn(seed)
                wsqr0 = exp(log_wsqr)
                
                logprior_old = logprior_mixerr(mu[*,i], mu0[i], tausqr[*,i], usqr[i], wsqr[i])
                logprior_new = logprior_mixerr(mu[*,i], mu0[i], tausqr[*,i], usqr[i], wsqr0)
                
                logpost_new = loglik + logprior_new + log_wsqr
                logpost_old = loglik + logprior_old + alog(wsqr[i])
                
                accept = linmix_metro_update( logpost_new, logpost_old, seed )
                
                if accept then begin
                    
                    naccept[4 + 2 * ngauss] = naccept[4 + 2 * ngauss] + 1L
                    wsqr[i] = wsqr0
                    logpost[i] = loglik + logprior_new
                    
                endif
                
            endelse

        endif

    endfor

                                ;save Markov Chains
    if iter eq 0 then begin
        
        alphag = alpha
        betag = beta
        sigsqrg = sigsqr
        
        pig = pi
        mug = mu
        tausqrg = tausqr

        if ngauss gt 1 then begin

            mu0g = mu0
            usqrg = usqr
            wsqrg = wsqr
        
        endif

        if metro then logpostg = logpost

    endif else begin
        
        alphag = [alphag, alpha]
        betag = [betag, beta]
        sigsqrg = [sigsqrg, sigsqr]

        pig = [[pig], [pi]]
        mug = [[mug], [mu]]
        tausqrg = [[tausqrg], [tausqr]]
        
        if ngauss gt 1 then begin
        
            mu0g = [mu0g, mu0]
            usqrg = [usqrg, usqr]
            wsqrg = [wsqrg, wsqr]

        endif
        
        if metro then logpostg = [logpostg, logpost]

    endelse
        
    iter = iter + 1L
    
;check for convergence
    
    if iter ge 4 and iter eq checkiter and not burnin then begin

        if not silent and metro then linmix_metro_results, $
          float(naccept) / (nchains * iter), ngauss

        Bvar = dblarr(npar)     ;between-chain variance
        Wvar = dblarr(npar)     ;within-chain variance
        
        psi = dblarr(iter, nchains, npar)
        
        psi[*,*,0] = transpose(reform(alphag, nchains, iter))
        psi[*,*,1] = transpose(reform(betag, nchains, iter))
        psi[*,*,2] = transpose(reform(sigsqrg, nchains, iter))

        pig2 = reform(pig, ngauss, nchains, iter)
        mug2 = reform(mug, ngauss, nchains, iter)
        tausqrg2 = reform(tausqrg, ngauss, nchains, iter)

        psi[*,*,3] = transpose( total(pig2 * mug2, 1) ) ;mean of xi
                                ;variance of xi
        psi[*,*,4] = transpose( total(pig2 * (tausqrg2 + mug2^2), 1) ) - psi[*,*,3]^2
                                ;linear correlation coefficient
                                ;between xi and eta
        psi[*,*,5] = psi[*,*,1] * sqrt(psi[*,*,4] / (psi[*,*,1]^2 * psi[*,*,4] + psi[*,*,2]))
                                ;do normalizing transforms before
                                ;monitoring convergence
        psi[*,*,2] = alog(psi[*,*,2])
        psi[*,*,4] = alog(psi[*,*,4])
        psi[*,*,5] = linmix_atanh(psi[*,*,5])

        psi = psi[iter/2:*,*,*] ;discard first half of MCMC

        ndraw = iter / 2
                                ;calculate between- and within-sequence
                                ;                  variances
        for j = 0, npar - 1 do begin
            
            psibarj = total( psi[*,*,j], 1 ) / ndraw
            psibar = mean(psibarj)
            
            sjsqr = 0d
            for i = 0, nchains - 1 do $
              sjsqr = sjsqr + total( (psi[*, i, j] - psibarj[i])^2 ) / (ndraw - 1.0)
            
            Bvar[j] = ndraw / (nchains - 1.0) * total( (psibarj - psibar)^2 )
            Wvar[j] = sjsqr / nchains
            
        endfor
        
        varplus = (1.0 - 1d / ndraw) * Wvar + Bvar / ndraw
        Rhat = sqrt( varplus / Wvar ) ;potential variance scale reduction factor
        
        if total( (Rhat le 1.1) ) eq npar and iter ge miniter then convergence = 1 $
        else if iter ge maxiter then convergence = 1 else begin
            
            if not silent then begin
                print, 'Iteration: ', iter
                print, 'Rhat Values for ALPHA, BETA, log(SIGMA^2), mean(XI), ' + $
                  'log(variance(XI), atanh(corr(XI,ETA)) ): '
                print, Rhat
            endif
            
            checkiter = checkiter + 100L
            
        endelse

    endif
    
    if (burnin) and (iter eq burniter) then begin
;still doing burn-in stage, get new estimates for jumping kernel
;parameters
        
        jvar_ssqr = linmix_robsig( alog(sigsqrg) )^2

                                ;now modify covariance matrix for
                                ;coefficient jumping kernel
        coefg = [[alphag], [betag]]
        
        jvar_coef = correlate( transpose(coefg), /covar)

        if ngauss gt 1 then begin

            jvar_mu0 = linmix_robsig(mu0g)^2 * 2.4^2

            jvar_usqr = linmix_robsig( alog(usqrg) )^2 * 2.4^2
    
            jvar_wsqr = linmix_robsig( alog(wsqrg) )^2 * 2.4^2

        endif
        
        if iter eq burnstop then burnin = 0
        
        if not burnin then begin

           if not silent then print, 'Burn-in Complete'

           iter = 0L

        endif

        naccept = lonarr(5 + 2 * ngauss)
        burniter = burniter + 250L
        
    endif
    
endrep until convergence

ndraw = iter * nchains / 2

;save posterior draws in a structure

if ngauss gt 1 then begin

    post = {alpha:0d, beta:0d, sigsqr:0d, pi:dblarr(ngauss), mu:dblarr(ngauss), $
            tausqr:dblarr(ngauss), mu0:0d, usqr:0d, wsqr:0d, ximean:0d, xisig:0d, $
            corr:0d}

endif else begin
    
    post = {alpha:0d, beta:0d, sigsqr:0d, pi:dblarr(ngauss), mu:dblarr(ngauss), $
            tausqr:dblarr(ngauss), ximean:0d, xisig:0d, corr:0d}

endelse

post = replicate(post, ndraw)

post.alpha = alphag[(iter*nchains+1)/2:*]
post.beta = betag[(iter*nchains+1)/2:*]
post.sigsqr = sigsqrg[(iter*nchains+1)/2:*]
post.pi = pig[*,(iter*nchains+1)/2:*]
post.mu = mug[*,(iter*nchains+1)/2:*]
post.tausqr = tausqrg[*,(iter*nchains+1)/2:*]

if ngauss gt 1 then begin

    post.mu0 = mu0g[(iter*nchains+1)/2:*]
    post.usqr = usqrg[(iter*nchains+1)/2:*]
    post.wsqr = wsqrg[(iter*nchains+1)/2:*]

endif

post.ximean = total(post.pi * post.mu, 1) ;mean of xi   
post.xisig = total(post.pi * (post.tausqr + post.mu^2), 1) - post.ximean^2
post.xisig = sqrt(post.xisig)   ;standard deviation of xi

                                ;get linear correlation coefficient
                                ;between xi and eta
post.corr = post.beta * post.xisig / sqrt(post.beta^2 * post.xisig^2 + post.sigsqr)

return
end

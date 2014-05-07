;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;+
;   NAME:
;     MLINMIX_ERR
;   PURPOSE:
;      Bayesian approach to multiple linear regression with errors in  X and Y
;   EXPLANATION:
; PERFORM LINEAR REGRESSION OF Y ON X WHEN THERE ARE MEASUREMENT
; ERRORS IN BOTH VARIABLES. THE REGRESSION ASSUMES :
;
;                 ETA = ALPHA + BETA ## XI + EPSILON
;                 X = XI + XERR
;                 Y = ETA + YERR
;
; HERE, (ALPHA, BETA) ARE THE REGRESSION COEFFICIENTS, EPSILON IS THE
; INTRINSIC RANDOM SCATTER ABOUT THE REGRESSION, XERR IS THE
; MEASUREMENT ERROR IN X, AND YERR IS THE MEASUREMENT ERROR IN
; Y. EPSILON IS ASSUMED TO BE NORMALLY-DISTRIBUTED WITH MEAN ZERO AND
; VARIANCE SIGSQR. XERR AND YERR ARE ASSUMED TO BE
; NORMALLY-DISTRIBUTED WITH MEANS EQUAL TO ZERO, COVARIANCE MATRICES
; XVAR^2 FOR X, VARIANCES YSIG^2 FOR Y, AND COVARIANCE VECTORS
; XYCOV. THE DISTRIBUTION OF XI IS MODELLED AS A MIXTURE OF NORMALS,
; WITH GROUP PROPORTIONS PI, MEANS MU, AND COVARIANCES T. BAYESIAN
; INFERENCE IS EMPLOYED, AND A STRUCTURE CONTAINING RANDOM DRAWS FROM
; THE POSTERIOR IS RETURNED. CONVERGENCE OF THE MCMC TO THE POSTERIOR
; IS MONITORED USING THE POTENTIAL SCALE REDUCTION FACTOR (RHAT,
; GELMAN ET AL.2004). IN GENERAL, WHEN RHAT < 1.1 THEN APPROXIMATE
; CONVERGENCE IS REACHED.
;
; SIMPLE NON-DETECTIONS ON Y MAY ALSO BE INCLUDED
;
; AUTHOR : BRANDON C. KELLY, STEWARD OBS., JULY 2006
;
; INPUTS :
;
;   X - THE OBSERVED INDEPENDENT VARIABLES. THIS SHOULD BE AN
;       [NX, NP]-ELEMENT ARRAY.
;   Y - THE OBSERVED DEPENDENT VARIABLE. THIS SHOULD BE AN NX-ELEMENT
;       VECTOR.
;
; OPTIONAL INPUTS :
;
;   XVAR - THE COVARIANCE MATRIX OF THE X ERRORS, AND
;          [NX,NP,NP]-ELEMENT ARRAY. XVAR[I,*,*] IS THE COVARIANCE
;          MATRIX FOR THE ERRORS ON X[I,*]. THE DIAGONAL OF
;          XVAR[I,*,*] MUST BE GREATER THAN ZERO FOR EACH DATA POINT.
;   YVAR - THE VARIANCE OF THE Y ERRORS, AND NX-ELEMENT VECTOR. YVAR
;          MUST BE GREATER THAN ZERO.
;   XYCOV - THE VECTOR OF COVARIANCES FOR THE MEASUREMENT ERRORS
;           BETWEEN X AND Y.
;   DELTA - AN NX-ELEMENT VECTOR INDICATING WHETHER A DATA POINT IS
;           CENSORED OR NOT. IF DELTA[i] = 1, THEN THE SOURCE IS
;           DETECTED, ELSE IF DELTA[i] = 0 THE SOURCE IS NOT DETECTED
;           AND Y[i] SHOULD BE AN UPPER LIMIT ON Y[i]. NOTE THAT IF
;           THERE ARE CENSORED DATA POINTS, THEN THE
;           MAXIMUM-LIKELIHOOD ESTIMATE (THETA) IS NOT VALID. THE
;           DEFAULT IS TO ASSUME ALL DATA POINTS ARE DETECTED, IE,
;           DELTA = REPLICATE(1, NX).
;   SILENT - SUPPRESS TEXT OUTPUT.
;   MINITER - MINIMUM NUMBER OF ITERATIONS PERFORMED BY THE GIBBS
;             SAMPLER. IN GENERAL, MINITER = 5000 SHOULD BE SUFFICIENT
;             FOR CONVERGENCE. THE DEFAULT IS MINITER = 5000. THE
;             GIBBS SAMPLER IS STOPPED AFTER RHAT < 1.1 FOR ALPHA,
;             BETA, AND SIGMA^2, AND THE NUMBER OF ITERATIONS
;             PERFORMED IS GREATER THAN MINITER.
;   MAXITER - THE MAXIMUM NUMBER OF ITERATIONS PERFORMED BY THE
;             MCMC. THE DEFAULT IS 1D5. THE GIBBS SAMPLER IS STOPPED
;             AUTOMATICALLY AFTER MAXITER ITERATIONS.
;   NGAUSS - THE NUMBER OF GAUSSIANS TO USE IN THE MIXTURE
;            MODELLING. THE DEFAULT IS 3. 
;
; OUTPUT :
;
;    POST - A STRUCTURE CONTAINING THE RESULTS FROM THE GIBBS
;           SAMPLER. EACH ELEMENT OF POST IS A DRAW FROM THE POSTERIOR
;           DISTRIBUTION FOR EACH OF THE PARAMETERS.
;
;             ALPHA - THE CONSTANT IN THE REGRESSION.
;             BETA - THE SLOPES OF THE REGRESSION.
;             SIGSQR - THE VARIANCE OF THE INTRINSIC SCATTER.
;             PI - THE GAUSSIAN WEIGHTS FOR THE MIXTURE MODEL.
;             MU - THE GAUSSIAN MEANS FOR THE MIXTURE MODEL.
;             T - THE GAUSSIAN COVARIANCE MATRICES FOR THE MIXTURE
;                 MODEL.
;             MU0 - THE HYPERPARAMETER GIVING THE MEAN VALUE OF THE
;                   GAUSSIAN PRIOR ON MU.
;             U - THE HYPERPARAMETER DESCRIBING FOR THE PRIOR
;                 COVARIANCE MATRIX OF THE INDIVIDUAL GAUSSIAN
;                 CENTROIDS ABOUT MU0.
;             W - THE HYPERPARAMETER DESCRIBING THE `TYPICAL' SCALE
;                 MATRIX FOR THE PRIOR ON (T,U).
;             XIMEAN - THE MEAN OF THE DISTRIBUTION FOR THE
;                      INDEPENDENT VARIABLE, XI.
;             XIVAR - THE STANDARD COVARIANCE MATRIX FOR THE
;                     DISTRIBUTION OF THE INDEPENDENT VARIABLE, XI.
;             XICORR - SAME AS XIVAR, BUT FOR THE CORRELATION MATRIX.
;             CORR - THE LINEAR CORRELATION COEFFICIENT BETWEEN THE
;                    DEPENDENT AND INDIVIDUAL INDEPENDENT VARIABLES,
;                    XI AND ETA.
;             PCORR - SAME AS CORR, BUT FOR THE PARTIAL CORRELATIONS.
;
; CALLED ROUTINES :
;
;    RANDOMCHI, MRANDOMN, RANDOMWISH, RANDOMDIR, MULTINOM
;
; REFERENCES :
;
;   Carroll, R.J., Roeder, K., & Wasserman, L., 1999, Flexible
;     Parametric Measurement Error Models, Biometrics, 55, 44
;
;   Kelly, B.C., 2007, Some Aspects of Measurement Error in
;     Linear Regression of Astronomical Data, ApJ, In press
;     (astro-ph/0705.2774)
;
;   Gelman, A., Carlin, J.B., Stern, H.S., & Rubin, D.B., 2004,
;     Bayesian Data Analysis, Chapman & Hall/CRC
;-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;routine to compute the inverse of the lower triangular matrix output
;from the Cholesky decomposition
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

function mlinmix_chol_invert, L

n = n_elements(L[*,0])

X = dblarr(n, n) ;X is the matrix inverse of L

for i = 0, n - 1 do begin

    X[i,i] = 1d / L[i,i]

    if i lt n - 1 then begin

        for j = i + 1, n - 1 do begin

            sum = 0d
            for k = i, j - 1 do sum = sum - L[k,j] * X[i,k]
            X[i,j] = sum / L[j,j]

        endfor

    endif

endfor

return, X
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;routine to compute the inverse of a symmetric positive-definite
;matrix via the Cholesky decomposition
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

pro mlinmix_posdef_invert, A

dim = n_elements(A[*,0])
diag = lindgen(dim) * (dim + 1L)

choldc, A, P, /double

for j = 0, dim - 1 do for k = j, dim - 1 do A[k,j] = 0d

A[diag] = P

A = mlinmix_chol_invert(A)

A = transpose(A) ## A

return
end


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                     ;
;                             MAIN ROUTINE                            ;
;                                                                     ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

pro mlinmix_err, x, y, post, xvar=xvar, yvar=yvar, xycov=xycov, silent=silent, $
                 delta=delta, miniter=miniter, maxiter=maxiter, ngauss=ngauss

if n_params() lt 3 then begin

    print, 'Syntax- MLINMIX_ERR, X, Y, POST, XVAR=XVAR, YVAR=YVAR, XYCOV=XYCOV,'
    print, '                    NGAUSS=NGAUSS, /SILENT, DELTA=DELTA, '
    PRINT, '                    MINITER=MINITER, MAXITER=MAXITER'
    return

endif

;check inputs and setup defaults

nx = size(x)

if nx[0] ne 2 then begin
    print, 'X must be an [NX,NP]-element array.'
    return
endif

np = nx[2]
nx = nx[1]

if n_elements(y) ne nx then begin
    print, 'Y and X must have the same size.'
    return
endif

if n_elements(xvar) eq 0 and n_elements(yvar) eq 0 then begin
    print, 'Must supply at least one of XVAR or YVAR.'
    return
endif

xvar_size = size(xvar)

if (xvar_size[0] ne 3) or (xvar_size[1] ne nx) or (xvar_size[2] ne np) or $
  (xvar_size[3] ne np) then begin
    print, 'XVAR must be an [NX,NP,NP]-element array.'
    return
endif

if n_elements(yvar) ne nx then begin
    print, 'YVAR and Y must have the same size.'
    return
endif

if n_elements(xycov) eq 0 then xycov = dblarr(nx, np)

if n_elements(xycov[*,0]) ne nx or n_elements(xycov[0,*]) ne np then begin
    print, 'XYCOV must be an [NX,NP]-element array.'
    return
endif

if n_elements(delta) eq 0 then delta = replicate(1, nx)
if n_elements(delta) ne nx then begin
    print, 'DELTA and X must have the same size.'
    return
endif

diag = lindgen(np) * (np + 1)
diag2 = lindgen(np+1) * (np + 2)

zero = where(xvar[diag] eq 0 or yvar eq 0, nzero)
if nzero gt 0 then begin
    print, 'Measurement Errors in X and Y have to have non-zero variance.'
    return
endif

det = where(delta eq 1, ndet, comp=cens, ncomp=ncens) ;get detected data points

if not keyword_set(silent) then silent = 0
if n_elements(miniter) eq 0 then miniter = 5000 ;minimum number of iterations that the 
                                                ;Markov Chain must perform
if n_elements(maxiter) eq 0 then maxiter = 100000L ;maximum number of iterations that the 
                                                   ;Markov Chains will perform

if n_elements(ngauss) eq 0 then ngauss = 3

if ngauss le 0 then begin
    print, 'NGAUSS must be at least 1.'
    return
endif

;store covariance matrices for (x,y) measurement errors

xyvar = dblarr(nx,np+1,np+1)

xyvar[*,0,0] = yvar
xyvar[*,1:*,0] = xycov
xyvar[*,0,1:*] = xycov
xyvar[*,1:*,1:*] = xvar

;; perform MCMC

nchains = 4                     ;number of markov chains to use
checkiter = 100            ;check for convergence every 100 iterations
iter = 0L

;;;;;;;;;;;; get initial guesses for the MCMC

;; first use moment correction method to estimate regression
;; coefficients and intrinsic dispersion

Xmat = [[replicate(1d, nx)], [x]]
denom = matrix_multiply(Xmat, Xmat, /atranspose)
Vcoef = denom
denom[1:*,1:*] = denom[1:*,1:*] - median(xvar, dim=1)

denom_diag = (denom[1:*,1:*])[diag]
denom_diag = denom_diag > 0.025 * (Vcoef[1:*,1:*])[diag]
denom[diag2[1:*]] = denom_diag
numer = y ## transpose(Xmat) - [0d, median(xycov, dim=1)]

choldc, denom, P, /double ;solve by cholesky decomposition
coef = cholsol( denom, P, numer, /double )

alpha = coef[0]
beta = coef[1:*]

sigsqr = variance(y) - mean(yvar) - $
  beta ## (correlate(transpose(x), /covar) - median(xvar, dim=1)) ## transpose(beta)
sigsqr = sigsqr[0] > 0.05 * variance(y - alpha - beta ## x)

; randomly disperse starting values for (alpha, beta) from a
; multivariate students-t distribution with 4 degrees of freedom

mlinmix_posdef_invert, Vcoef
Vcoef = Vcoef * sigsqr * 4d

coef = mrandomn(seed, Vcoef, nchains)
chisqr = randomchi(seed, 4, nchains)

alphag = alpha + coef[*,0] * sqrt(4d / chisqr)
betag = dblarr(np, nchains)
for i = 0, nchains - 1 do betag[*,i] = beta + coef[i,1:*] * sqrt(4d / chisqr[i])

;draw sigsqr from an Inverse scaled chi-square density
sigsqrg = sigsqr * (nx / 2) / randomchi(seed, nx / 2, nchains)

;; now get initial guesses for the mixture and prior parameters, do
;; this one chain at a time

pig = dblarr(ngauss, nchains)
mug = dblarr(np, ngauss, nchains)
Tg = dblarr(np, np, ngauss, nchains)
mu0g = dblarr(np, nchains)
Ug = dblarr(np, np, nchains)
Wg = dblarr(np, np, nchains)

dist = dblarr(nx, ngauss)
Glabel = intarr(nx, nchains)

for i = 0, nchains - 1 do begin
    
                                ;randomly choose NGAUSS data points,
                                ;set these to the group means
    ind = lindgen(nx)
    unif = randomu(seed, nx)
    ind = (ind[sort(unif)])[0:ngauss-1]

    mug[*,*,i] = transpose(x[ind,*])
    
    if ngauss gt 1 then begin
                                ;get distance of data points to each
                                ;centroid
        for k = 0, ngauss - 1 do $
          dist[0,k] = total((x - mug[*,k,i] ## replicate(1d, nx))^2, 2)
        
        mindist = min(dist, Glabel0, dim=2) ;classify to closest centroid
        
        Glabel0 = Glabel0 / nx

    endif else Glabel0 = intarr(nx)

    Glabel[0,i] = Glabel0

;now get initial guesses for PI and T

    for k = 0, ngauss - 1 do begin

        gk = where(Glabel0 eq k, nk)
        
        if nk gt np then begin

            pig[k,i] = float(nk) / nx
            Tg[*,*,k,i] = correlate(transpose(x[gk,*]), /covar)

        endif else begin

            pig[k,i] = (1d > nk) / nx
            Tg[*,*,k,i] = correlate(transpose(x), /covar)

        endelse

    endfor

    pig[*,i] = pig[*,i] / total(pig[*,i]) ;make sure Pi sums to unity

;now get initial guesses for prior parameters

    mu0g[*,i] = ngauss eq 1 ? mug[*,0,i] : total(mug[*,*,i], 2) / ngauss
    Smat = correlate(transpose(x), /covar)
    Ug[*,*,i] = randomwish(seed, nx, Smat / nx)

    Wg[*,*,i] = randomwish(seed, nx, Smat / nx)

endfor

alpha = alphag
beta = betag
sigsqr = sigsqrg
pi = pig
mu = mug
T = Tg
mu0 = mu0g
U = Ug
W = Wg
                                ;get inverses of XYVAR
xyvar_inv = xyvar
for i = 0, nx - 1 do begin
    
    xyvar_inv0 = reform(xyvar[i,*,*])
    mlinmix_posdef_invert, xyvar_inv0
    xyvar_inv[i,*,*] = xyvar_inv0
    
endfor
                                ;get staring values for eta
eta = dblarr(nx, nchains)
for i = 0, nchains - 1 do eta[*,i] = y

nut = np ;degrees of freedom for the prior on T
nuu = np ;degrees of freedom for the prior on U

npar = 2 + np ;number of parameters to monitor convergence on

convergence = 0
                                ;start Markov Chains
if not silent then print, 'Simulating Markov Chains...'

ygibbs = y
                                ;define arrays now so we don't have to
                                ;create them every MCMC iteration
xi = dblarr(nx, np, nchains)
for i = 0, nchains - 1 do xi[*,*,i] = x
xstar = dblarr(nx, np)
mustar = dblarr(nx, np)
gamma = dblarr(nx, ngauss)
nk = fltarr(ngauss)
Tk_inv = dblarr(np, np, ngauss, nchains)
U_inv = dblarr(np, np, nchains)

                                ;get various matrix inverses before
                                ;staring markov chain
for i = 0, nchains - 1 do begin

    for k = 0, ngauss - 1 do begin
        
        Tk_inv0 = T[*,*,k,i]
        mlinmix_posdef_invert, Tk_inv0
        
        Tk_inv[*,*,k,i] = Tk_inv0
        
    endfor
    
    U_inv0 = U[*,*,i]
    mlinmix_posdef_invert, U_inv0
    U_inv[*,*,i] = U_inv0

endfor

repeat begin
 
    for i = 0, nchains - 1 do begin ;do markov chains one at-a-time

        W_inv = W[*,*,i]
        mlinmix_posdef_invert, W_inv

;do Gibbs sampler
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
        
                                ;now draw Xi|mu,covar,x, do this for
                                ;each covariate at a time
        
        for j = 0, np - 1 do begin
            
            case j of
                
                0 : inactive = indgen(np - 1) + 1L
                np - 1 : inactive = indgen(np - 1)
                else : inactive = [indgen(j), indgen(np - j - 1) + j + 1]
                
            endcase
            
            xstar[*,j] = x[*,j]
            xstar[*,inactive] = x[*,inactive] - xi[*,inactive,i]
            
            zstar = [[ygibbs - eta[*,i]], [xstar]]
            
            zmu = total(xyvar_inv[*,*,j+1] * zstar, 2)
            
            for k = 0, ngauss - 1 do begin ;do one gaussian at-a-time
                
                gk = where(Glabel[*,i] eq k, ngk)
                
                if ngk gt 0 then begin
                    
                    mustar[gk,j] = mu[j,k,i]
                    for l = 0, np - 2 do mustar[gk,inactive[l]] = $
                      mu[inactive[l],k,i] - xi[gk,inactive[l],i]
                    
                    mmu = Tk_inv[*,j,k,i] ## mustar[gk,*]
                    
                    etamu = eta[gk,i] - alpha[i] - beta[inactive,i] ## xi[gk,inactive,i]
                    
                    xihvar = 1d / (xyvar_inv[gk,j+1,j+1] + Tk_inv[j,j,k,i] + $
                                   beta[j,i]^2 / sigsqr[i])
                    
                    xihat = xihvar * (zmu[gk] + mmu + beta[j,i] * etamu / (sigsqr[i]))
                    
                    xi[gk,j,i] = xihat + sqrt(xihvar) * randomn(seed, nx)
                    
                endif
                
            endfor
            
        endfor
                                ;now draw Eta|Xi,alpha,beta,sigsqr,y
        zstar = [[ygibbs], [x - xi[*,*,i]]]
        
        zmu = total(xyvar_inv[*,*,0] * zstar, 2)
        
        ximu = (alpha[i] + beta[*,i] ## xi[*,*,i]) / sigsqr[i]
        
        etahvar = 1d / (xyvar_inv[*,0,0] + 1d / sigsqr[i])
        etahat = etahvar * (zmu + ximu)
        
        eta[*,i] = etahat + sqrt(etahvar) * randomn(seed, nx)
        
                                ;now draw new class labels
        if ngauss eq 1 then Glabel[*,i] = 0 else begin
                                ;get unnormalized probability that
                                ;source i came from Gaussian k, given
                                ;xi[i]
            for k = 0, ngauss - 1 do begin
                
                xicent = xi[*,*,i] - mu[*,k,i] ## replicate(1, nx)
                gamma[0,k] = $
                  pi[k,i] / ((2d*!pi)^(np/2d) * determ(T[*,*,k,i], /double)) * $
                  exp(-0.5 * total(xicent * (Tk_inv[*,*,k,i] ## xicent), 2))
                
            endfor
            
            norm = total(gamma, 2)
            
            for j = 0, nx - 1 do begin
                
                gamma0 = reform(gamma[j,*]) / norm[j] ;normalized probability that the i-th 
                                                      ;data point is from the k-th Gaussian, 
                                                      ;given the observed data point 
                Gjk = multinom(1, gamma0, seed=seed)
                
                Glabel[j,i] = where(Gjk eq 1)
                
            endfor
            
        endelse

;; now draw new values of alpha, beta, and sigsqr
        
                                ;first do alpha,beta|Xi,Eta,sigsqr
        
        Xmat[*,1:*] = xi[*,*,i]
        
        hatmat = matrix_multiply(Xmat, Xmat, /atranspose)
        Vcoef = hatmat
        
        choldc, hatmat, P, /double ;solve by cholesky decomposition
        coefhat = cholsol( hatmat, P, eta[*,i] ## transpose(Xmat), /double )
        
        mlinmix_posdef_invert, Vcoef
        Vcoef = Vcoef * sigsqr[i]
        
        coef = coefhat + mrandomn(seed, Vcoef)
        
        alpha[i] = coef[0]
        beta[*,i] = coef[1:*]
        
                                ;now do sigsqr|xi,eta,alpha,beta, 
                                ;draw sigsqr from a scaled
                                ;Inverse-chi-square density
        resid = eta[*,i] - alpha[i] - beta[*,i] ## xi[*,*,i]
        ssqr = total( resid^2 ) / (nx - 2d)
        
        sigsqr[i] = ssqr * (nx - 2d) / randomchi(seed, nx - 2)

;; now do mixture model parameters, psi = (pi,mu,tausqr)

        for k = 0, ngauss - 1 do begin 
            
            gk = where(Glabel[*,i] eq k, ngk)
            nk[k] = ngk
           
            if ngk gt 0 then begin
                                ;get mu|Xi,G,tausqr,mu0,U
                
                muvar = U_inv[*,*,i] + ngk * Tk_inv[*,*,k,i]
                mlinmix_posdef_invert, muvar
                
                xibar = total(xi[gk,*,i], 1) / ngk
                
                muhat = (mu0[*,i] ## U_inv[*,*,i] + $
                         ngk * (xibar ## Tk_inv[*,*,k,i])) ## muvar
                
                mu[*,k,i] = muhat + mrandomn(seed, muvar)
                
            endif else mu[*,k,i] = mu0[*,i] + mrandomn(seed, U[*,*,i])

                                ;get T|Xi,G,mu,W,nut
            nuk = ngk + nut

            if ngk gt 0 then begin
                
                xicent = xi[gk,*,i] - mu[*,k,i] ## replicate(1d, ngk)
                
                Smat = W[*,*,i] + xicent ## transpose(xicent)
                
                Smat_inv = Smat
                mlinmix_posdef_invert, Smat_inv

            endif else begin

                Smat = W
                Smat_inv = W_inv

            endelse

            Tmat = randomwish(seed, nuk, Smat_inv)
            
            Tk_inv[*,*,k,i] = Tmat
            mlinmix_posdef_invert, Tmat
            T[*,*,k,i] = Tmat
            
        endfor
                                ;get pi|G
        if ngauss eq 1 then pi[*,i] = 1d else $
          pi[*,i] = randomdir(seed, nk + 1)
        
;; now, finally update the prior parameters    
        
                                ;first update mean of gaussian
                                ;centroids
        mu0[*,i] = ngauss eq 1 ? mu[*,0,i] + mrandomn(seed, U[*,*,i]) : $
          total(mu[*,*,i], 2) / ngauss + mrandomn(seed, U[*,*,i] / ngauss)

                                ;update centroid covariance matrix, U
        nu = ngauss + nuu
        
        mucent = ngauss eq 1 ? transpose(mu[*,0,i] - mu0[*,i]) : $
          transpose(mu[*,*,i]) - mu0[*,i] ## replicate(1d, ngauss)
        
        Uhat = W[*,*,i] + mucent ## transpose(mucent)
        
        mlinmix_posdef_invert, Uhat
        Umat = randomwish(seed, nu, Uhat)
        
        U_inv[*,*,i] = Umat
        mlinmix_posdef_invert, Umat
        U[*,*,i] = Umat
 
                                ;update the common scale matrix, W
        nuw = (ngauss + 2) * np + 1
        What = ngauss eq 1 ? U_inv[*,*,i] + Tk_inv[*,*,0,i] : $
          U_inv[*,*,i] + total(Tk_inv[*,*,*,i], 3)
        
        mlinmix_posdef_invert, What
        
        W[*,*,i] = randomwish(seed, nuw, What)

    endfor
                                ;save Markov Chains
    if iter eq 0 then begin
        
        alphag = alpha
        betag = beta[*]
        sigsqrg = sigsqr
        
        pig = pi[*]
        mug = mu[*]
        Tg = T[*]
        
        mu0g = mu0[*]
        Ug = U[*]
        Wg = W[*]
    
    endif else begin
        
        alphag = [alphag, alpha]
        betag = [betag, beta[*]]
        sigsqrg = [sigsqrg, sigsqr]

        pig = [pig, pi[*]]
        mug = [mug, mu[*]]
        Tg = [Tg, T[*]]

        mu0g = [mu0g, mu0[*]]
        Ug = [Ug, U[*]]
        Wg = [Wg, W[*]]

    endelse
    
    iter = iter + 1L
    
;check for convergence
    
    if iter ge 4 then begin
        
        Bvar = dblarr(npar)     ;between-chain variance
        Wvar = dblarr(npar)     ;within-chain variance
        
        ndraw = n_elements(alphag) / nchains
        
        psi = dblarr(npar, nchains, ndraw)
        psi[0,*,*] = reform(alphag, nchains, ndraw)
        psi[1:np,*,*] = reform(betag, np, nchains, ndraw)
        psi[np+1,*,*] = alog(reform(sigsqrg, nchains, ndraw))
        
        psi = psi[*,*,(ndraw+1)/2:*]
        ndraw = ndraw / 2
                                ;calculate between- and within-sequence
                                ;                  variances
        for j = 0, npar - 1 do begin
            
            psibarj = total( psi[j,*,*], 3 ) / ndraw
            psibar = mean(psibarj)
            
            sjsqr = 0d
            for i = 0, nchains - 1 do $
              sjsqr = sjsqr + total( (psi[j, i, *] - psibarj[i])^2 ) / (ndraw - 1.0)
            
            Bvar[j] = ndraw / (nchains - 1.0) * total( (psibarj - psibar)^2 )
            Wvar[j] = sjsqr / nchains
            
        endfor
        
        varplus = (1.0 - 1d / ndraw) * Wvar + Bvar / ndraw
        Rhat = sqrt( varplus / Wvar ) ;potential variance scale reduction factor
        
    endif
    
    if iter eq checkiter then begin 
;maximum iterations reached, now assess convergence

        if (total( (Rhat le 1.1) ) eq npar and iter ge miniter) or $
          iter ge maxiter then convergence = 1 $
        else begin
            
            if not silent then begin
                print, 'Iteration: ', iter
                print, 'Rhat Values (ALPHA, BETA, SIGSQR) : '
                print, Rhat
            endif
            
            checkiter = checkiter + 100L
            
        endelse

    endif
    
endrep until convergence

ndraw = n_elements(alphag) / nchains

alphag = reform(alphag, nchains, ndraw)
betag = reform(betag, np, nchains, ndraw)
sigsqrg = reform(sigsqrg, nchains, ndraw)

pig = reform(pig, ngauss, nchains, ndraw)
mug = reform(mug, np, ngauss, nchains, ndraw)
Tg = reform(Tg, np, np, ngauss, nchains, ndraw)

mu0g = reform(mu0g, np, nchains, ndraw)
Ug = reform(Ug, np, np, nchains, ndraw)
Wg = reform(Wg, np, np, nchains, ndraw)

;only keep second half of markov chains
alphag = alphag[*,(ndraw+1)/2:*]
betag = betag[*,*,(ndraw+1)/2:*]
sigsqrg = sigsqrg[*,(ndraw+1)/2:*]
pig = pig[*,*,(ndraw+1)/2:*]
mug = mug[*,*,*,(ndraw+1)/2:*]
Tg = Tg[*,*,*,*,(ndraw+1)/2:*]
mu0g = mu0g[*,*,(ndraw+1)/2:*]
Ug = Ug[*,*,*,(ndraw+1)/2:*]
Wg = Wg[*,*,*,(ndraw+1)/2:*]

if not silent then begin
    print, 'Iteration: ', iter
    print, 'Rhat Values (ALPHA, BETA, SIGSQR) : ', Rhat
endif

;save posterior draws in a structure
ndraw = ndraw / 2


if ngauss gt 1 then $
  post = {alpha:0d, beta:dblarr(np), sigsqr:0d, pi:dblarr(ngauss), mu:dblarr(np,ngauss), $
          T:dblarr(np,np,ngauss), mu0:dblarr(np), U:dblarr(np,np), W:dblarr(np,np), $
          ximean:dblarr(np), xivar:dblarr(np,np), xicorr:dblarr(np,np), corr:dblarr(np), $
          pcorr:dblarr(np)} $
else $
  post = {alpha:0d, beta:dblarr(np), sigsqr:0d, pi:0d, mu:dblarr(np), $
          T:dblarr(np,np), mu0:dblarr(np), U:dblarr(np,np), W:dblarr(np,np), $
          ximean:dblarr(np), xivar:dblarr(np,np), xicorr:dblarr(np,np), corr:dblarr(np), $
          pcorr:dblarr(np)}

post = replicate(post, ndraw * nchains)

post.alpha = alphag[*]
post.beta = reform(betag, np, ndraw * nchains)
post.sigsqr = sigsqrg[*]

if ngauss gt 1 then begin

    post.pi = reform(pig, ngauss, ndraw * nchains)
    post.mu = reform(mug, np, ngauss, ndraw * nchains)
    post.T = reform(Tg, np, np, ngauss, ndraw * nchains)

endif else begin

    post.pi = reform(pig, ndraw * nchains)
    post.mu = reform(mug, np, ndraw * nchains)
    post.T = reform(Tg, np, np, ndraw * nchains)

endelse

post.mu0 = reform(mu0g, np, ndraw * nchains)
post.U = reform(Ug, np, np, ndraw * nchains)
post.W = reform(Wg, np, np, ndraw * nchains)

;get posterior draws of moments of distribution

if not silent then print, 'Getting Posterior Draws for Various Moments...'

corrmat = dblarr(np+1,np+1)

for i = 0, ndraw * nchains - 1 do begin
                                ;average value of Xi
    post[i].ximean = ngauss gt 1 ? post[i].pi ## post[i].mu : post[i].mu

    if ngauss eq 1 then post[i].xivar = post[i].T else begin

        for k = 0, ngauss - 1 do post[i].xivar = post[i].xivar + $
          post[i].pi[k] * (post[i].T[*,*,k] + transpose(post[i].mu[*,k]) ## post[i].mu[*,k])
                                ;covariance matrix of Xi
        post[i].xivar = post[i].xivar - transpose(post[i].ximean) ## post[i].ximean

    endelse       
    
    xivar = post[i].xivar
    
                                ;variance in Eta
    etavar = post[i].beta ## post[i].xivar ## transpose(post[i].beta) + post[i].sigsqr
                                ;correlation coefficients between Eta
                                ;and Xi
    post[i].corr = post[i].beta ## post[i].xivar / $
      sqrt( etavar[0] * post[i].xivar[diag] )
                                ;correlation matrix of the covariates
    post[i].xicorr = xivar * ( transpose(1d / sqrt(xivar[diag])) ## (1d / sqrt(xivar[diag])) )
                                ;now get partial correlations, need
                                ;full correlation matrix first
    corrmat[0,0] = 1d
    corrmat[1:*,0] = post[i].corr
    corrmat[0,1:*] = post[i].corr
    corrmat[1:*,1:*] = post[i].xicorr

    mlinmix_posdef_invert, corrmat

    post[i].pcorr = -1d * corrmat[1:*,0] / sqrt(corrmat[0,0] * corrmat[diag2[1:*]])

endfor

return
end

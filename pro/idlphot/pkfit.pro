pro pkfit,f,scale,x,y,sky,radius,ronois,phpadu,gauss,psf, $
                  errmag,chi,sharp,niter, DEBUG= debug
;+
; NAME:
;	PKFIT
; PURPOSE:
;	Subroutine of  GETPSF to perform a one-star least-squares fit 
; EXPLANATION:
;	Part of the DAOPHOT PSF photometry sequence
;
; CALLING SEQUENCE:
;	PKFIT, f, scale, x, y, sky, radius, ronois, phpadu, gauss, psf, 
;				errmag, chi, sharp, Niter, /DEBUG 
; INPUTS:
;	F      - NX by NY array containing actual picture data.           
;	X, Y   - the initial estimates of the centroid of the star relative
;		to the corner (0,0) of the subarray.  Upon return, the
;		final computed values of X and Y will be passed back to the
;		calling routine.
;	SKY  -   the local sky brightness value, as obtained from APER
;	RADIUS-  the fitting radius-- only pixels within RADIUS of the
;		instantaneous estimate of the star's centroid will be
;		included in the fit, scalar
;	RONOIS - readout noise per pixel, scalar
;	PHPADU - photons per analog digital unit, scalar
;	GAUSS -  vector containing the values of the five parameters defining
;		the analytic Gaussian which approximates the core of the PSF.
;	PSF   -  an NPSF by NPSF look-up table containing corrections from
;		the Gaussian approximation of the PSF to the true PSF.
;
; INPUT-OUTPUT:
;	SCALE  - the initial estimate of the brightness of the star,
;		expressed as a fraction of the brightness of the PSF.
;		Upon return, the final computed value of SCALE will be
;		passed back to the calling routine.
; OUTPUTS:
;	ERRMAG - the estimated standard error of the value of SCALE
;		returned by this routine.
;	CHI    - the estimated goodness-of-fit statistic:  the ratio
;		of the observed pixel-to-pixel mean absolute deviation from
;		the profile fit, to the value expected on the basis of the
;		noise as determined from Poisson statistics and the 
;		readout noise.
;	SHARP  - a goodness-of-fit statistic describing how much broader  
;		the actual profile of the object appears than the
;		profile of the PSF.
;	NITER -  the number of iterations the solution required to achieve
;		convergence.  If NITER = 25, the solution did not converge.
;		If for some reason a singular matrix occurs during the least-
;		squares solution, this will be flagged by setting NITER = -1.
;
; RESTRICTIONS:
;	No parameter checking is performed
; REVISON HISTORY:
;	Adapted from the official DAO version of 1985 January 25
;	Version 2.0 W. Landsman STX             November 1988
;	Converted to IDL V5.0   W. Landsman   September 1997
;-
 s = size(f)	;Get array dimensions
 nx = s[1] & ny = s[2]
;               ;Initialize a few things for the solution
 redo = 0B		
 pkerr = 0.027/(gauss[3]*gauss[4])^2
 clamp = fltarr(3) + 1.
 dtold = fltarr(3) 
 niter = 0                                
 chiold = 1.

 if keyword_set(DEBUG) then $
     print,'PKFIT: ITER  X      Y      SCALE    ERRMAG   CHI     SHARP'

BIGLOOP:  		        ;Begin the big least-squares loop
 niter = niter+1

 ixlo = fix(x-radius) > 0	;Choose boundaries of subarray containing
 iylo = fix(y-radius) > 0        ;points inside the fitting radius
 ixhi = fix(x+radius) +1 < (nx-1)
 iyhi = fix(y+radius) +1 < (ny-1)
 ixx  = ixhi-ixlo+1
 iyy  = iyhi-iylo+1
 dy   =  findgen(iyy) + iylo - y    ;X distance vector from stellar centroid
 dysq = dy^2
 dx   = findgen(ixx) + ixlo - x
 dxsq = dx^2
 rsq  = fltarr(ixx,iyy)  ;RSQ - array of squared 

 for J = 0,iyy-1 do rsq[0,j] = (dxsq+dysq[j])/radius^2

 ; The fitting equation is of the form
 ;
 ; Observed brightness =
 ;      SCALE + delta(SCALE)  *  PSF + delta(Xcen)*d(PSF)/d(Xcen) +
 ;                                           delta(Ycen)*d(PSF)/d(Ycen)
 ;
 ; and is solved for the unknowns delta(SCALE) ( = the correction to
 ; the brightness ratio between the program star and the PSF) and
 ; delta(Xcen) and delta(Ycen) ( = corrections to the program star's
 ; centroid).
 ;
 ; The point-spread function is equal to the sum of the integral under
 ; a two-dimensional Gaussian profile plus a value interpolated from            
 ; a look-up table.

 good = where(rsq lt 1.,ngood)
 ngood = ngood > 1

 t = fltarr(ngood,3)
 dx = dx[good mod ixx]
 dy = dy[good/ixx]
 model = dao_value(dx, dy, gauss, psf, dvdx, dvdy)

 if keyword_set(DEBUG) then begin print,'model created ' & stop & end

 t[0,0] = model
 t[0,1] = -scale*dvdx  
 t[0,2] = -scale*dvdy
 fsub = f[ixlo:ixhi,iylo:iyhi]
 fsub = fsub[good]
 rsq = rsq[good]
 df = fsub - scale*model - sky     ;Residual of the brightness from the PSF fit

 ; The expected random error in the pixel is the quadratic sum of
 ; the Poisson statistics, plus the readout noise, plus an estimated
 ; error of 0.75% of the total brightness for the difficulty of flat-
 ; fielding and bias-correcting the chip, plus an estimated error of 
 ; of some fraction of the fourth derivative at the peak of the profile,
 ; to account for the difficulty of accurately interpolating within the
 ; point-spread function.  The fourth derivative of the PSF is
 ; proportional to H/sigma**4 (sigma is the Gaussian width parameter for
 ; the stellar core); using the geometric mean of sigma(x) and sigma(y),
 ; this becomes H/ sigma(x)*sigma(y) **2.  The ratio of the fitting
 ; error to this quantity is estimated from a good-seeing CTIO frame to
 ; be approximately 0.027 (see definition of PKERR above.)

 fpos = (fsub-df) > 0	;Raw data - residual = model predicted intensity
 sigsq = fpos/phpadu + ronois + (0.0075*fpos)^2 + (pkerr*(fpos-sky))^2
 sig = sqrt(sigsq)
 relerr = df/sig

 ; SIG is the anticipated standard error of the intensity
 ; including readout noise, Poisson photon statistics, and an estimate
 ; of the standard error of interpolating within the PSF.  

 rhosq = fltarr(ixx,iyy)

 for j = 0,iyy-1 do rhosq[0,j] = (dxsq/gauss[3]^2+dysq[j]/gauss[4]^2)

 rhosq = rhosq[good]
 if (niter GE 2) then begin    ;Reject any pixel with 10 sigma residual
        badpix = where( ABS(relerr/chiold) GE 10.,nbad ) 
        if nbad GT 0 then begin 
            remove, badpix, fsub, df, sigsq, sig
            remove, badpix, relerr, rsq, rhosq
            ngood = ngood-badpix
        endif
 endif

 wt = 5./(5.+rsq/(1.-rsq))
 lilrho = where(rhosq LE 36.)	;Include only pixels within 6 sigma of centroid
 rhosq[lilrho] = 0.5*rhosq[lilrho]
 dfdsig = exp(-rhosq[lilrho])*(rhosq[lilrho]-1.)
 fpos = ( fsub[lilrho]-sky) >0 + sky

 ; FPOS-SKY = raw data minus sky = estimated value of the stellar
 ; intensity (which presumably is non-negative).

 sig  = fpos/phpadu + ronois + (0.0075*fpos)^2 + (pkerr*(fpos-sky))^2
 numer = total(dfdsig*df/sig)
 denom = total(dfdsig^2/sig)

 ; Derive the weight of this pixel.  First of all, the weight depends
 ; upon the distance of the pixel from the centroid of the star-- it
 ; is determined from a function which is very nearly unity for radii
 ; much smaller than the fitting radius, and which goes to zero for
 ;  radii very near the fitting radius.  

 chi = total(wt*abs(relerr))
 sumwt = total(wt)

 wt = wt/sigsq   ;Scale weight to inverse square of expected mean error
 if niter GE 2 then $	;Reduce weight of a bad pixel
         wt = wt/(1.+(0.4*relerr/chiold)^8)

 v = fltarr(3)       ;Compute vector of residuals and the normal matrix. 
 c = fltarr(3,3)

 for kk = 0,2 do begin   
    v[kk] = TOTAL(df*t[*,kk]*wt)
    for ll = 0,2 do C[kk,ll] = TOTAL(t[*,kk]*t[*,ll]*wt)
 end 

 ; Compute the (robust) goodness-of-fit index CHI.
 ; CHI is pulled toward its expected value of unity before being stored
 ; in CHIOLD to keep the statistics of a small number of pixels from
 ; completely dominating the error analysis.

 if sumwt GT 3.0 then begin  
   chi = 1.2533*chi*sqrt(1./(sumwt*(sumwt-3.)))
   chiold = ((sumwt-3.)*chi+3.)/sumwt
 endif

 C = INVERT(C)	;Invert the normal matrix
 dt = c#v 	;Compute parameter corrections

; In the beginning, the brightness of the star will not be permitted
; to change by more than two magnitudes per iteration (that is to say,
; if the estimate is getting brighter, it may not get brighter by
; more than 525% per iteration, and if it is getting fainter, it may
; not get fainter by more than 84% per iteration).  The x and y
; coordinates of the centroid will be allowed to change by no more
; than one-half pixel per iteration.  Any time that a parameter
; correction changes sign, the maximum permissible change in that
; parameter will be reduced by a factor of 2.

 div = where( dtold*dt LT -1.e-38, nbad )
 if nbad GT 0 then clamp[div] = clamp[div]/2.
 dtold = dt
 adt = abs(dt)

 scale = scale+dt[0]/    $
  (1.+(( dt[0]/(5.25*scale)) > (-1*dt[0]/(0.84*scale)) )/clamp[0])
 x = x + dt[1]/(1.+adt[1]/(0.5*clamp[1]))
 y = y + dt[2]/(1.+adt[2]/(0.5*clamp[2]))
 redo = 0B

; Convergence criteria:  if the most recent computed correction to the
; brightness is larger than 0.1% or than 0.05 * sigma(brightness),
; whichever is larger, OR if the absolute change in X or Y is
; greater than 0.01 pixels, convergence has not been achieved.

 sharp = 2.*gauss[3]*gauss[4]*numer/(gauss[0]*scale*denom)
 errmag = chiold*sqrt(c[0,0])
 if ( adt[0] GT ( 0.05*errmag > 0.001*scale )) then redo = 1b
 if ((adt[1] > adt[2] ) GT 0.01) then redo = 1b

 if keyword_set(DEBUG) then print,format='(1H  ,I9,2F7.2,2F9.3,F8.2,F9.2)', $ 
                       niter,x,y,scale,errmag,chiold,sharp
 if niter LT 3 then goto, BIGLOOP 	;At least 3 iterations required

; If the solution has gone 25 iterations, OR if the standard error of 
; the brightness is greater than 200%, give up.

 if (redo and (errmag LE 1.9995) and (niter LT 25) ) then goto, BIGLOOP
 sharp = sharp>(-99.999)<99.999

 return
 end

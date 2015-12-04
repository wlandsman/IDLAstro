;+
; NAME:
;    SOLVE_ASTRO
; PURPOSE:
;    Solve for an TANgent-plane astrometric plate solution with optional 
;    distortion terms
; EXPLANATION:
;    SOLVE_ASTRO takes an input matched xpixel/ypixel/ra/dec list, and returns
;    an IDL astrometry structure containing the astrometric
;    solution. Optional sigma clipping on the fit. Similar in
;    principle to IRAF's ccmap.
; CALLING SEQUENCE:
;    astr  = SOLVE_ASTRO( ra_degrees, dec_degrees, xpixel, ypixel,
;    [XIORDER=, ETAORDER=, XTERMS= DISTORT=, CRVAL= NITER=, REJECT=,
;    XIRMS=, ETARMS= SUCCESS=,VERBOSE= NORTERMS= NREJ=, N_TPVTERMS=,
;    XIRESID=, ETARESID=, WFIT= ])
; INPUT ARGUMENT:
;    ra_degrees  - the right ascensions in degrees of the matched objects
;    dec_degrees - the declinations in degrees
;    xpixel      - the x pixel values
;    ypixel      - the y pixel values
; RETURNS:
;    astr        - an IDL astrometric structure containing the plate solution
; OPTIONAL INPUT KEYWORDS:
;    distort     - type of distortion to fit. Options: 'none': just
;                  fit the linear solution, 'tpv' fit the TPV
;                  convention, 'tnx': fit the iraf tnx
;                  convention. default: 'none'. See:
;                  http://iraf.noao.edu/projects/ccdmosaic/tnx.html
;                  for TNX
;                  http://iraf.noao.edu/projects/ccdmosaic/tpv.html
;                  for TPV
;    crval       - the ra/dec of the reference pixel in
;                  degrees. Default is mean of the input coordinates
;    nrej        - Number of rejection iterations
;    reject      - the sigma of the rejection (input to RESISTANT_MEAN)
;    etaorder    - The order of the TNX coefficients in eta
;    xiorder     - The order of the TNX coefficients in xi
;    xterms      - TNX cross-terms type (0: none, 1: full, 2: half)
;    n_tpvterms  - Number of TPV coefficients to include. Always
;                  includes rterms even if /NORTERMS is passed. Useful
;                  values include: 7 (quadratic), 12 (cubic), 17
;                  (quartic).
;    norterms    - Do not fit the "r" terms if using a TPV distortion
;    naxis1      - xpixel size of image being fit. If passed this is
;                  inserted into the final astrometry structure, but
;                  are not required.
;    naxis2      - ypixel size of image being fit. If passed this is
;                  inserted into the final astrometry structure, but
;                  are not required.
;    verbose     - Give verbose output
; OPTIONAL OUTPUT KEYWORD:
;    xirms       - the xi fit rms in arcseconds
;    etarms      - the eta fit rms in arcseconds
;    xiresid     - Residuals of each point in xi entering the final
;                  (fit indices of input points given by wfit).
;    etaresid    - Residuals of each point in eta entering the final
;                  (fit indices of input points given by wfit).
;    wfit        - index of input datapoints that were used in the
;                  final fit
;    success     - Did the code run successfully?
; NOTES:
;  The implmentation is slightly limited. It can only fit the
;  polynomial terms in TNX and not the legendre or chebyshev. The x/y
;  pixels must be previously matched with the ra/decs. The use of
;  MPFIT2DFUN is probably not needed and could be re-written to run
;  faster with LAPACK libraries. For tnx projections, orders in eta and xi cannot currently
;  be specified separately.
;  There is code duplication between tpv_eval, tnx_eval and this routine
;  Input/Output is always assumed to be FK5/equinox 2000. Could be changed!
; PROCEDURES USED:
;       STRN() - in astrolib
;       RESISTANT_MEAN - in astrolib
;       MPFIT2DFUN(), MPFIT - in MPFIT library, can be downloaded from either
;               from   http://cow.physics.wisc.edu/~craigm/idl/fitting.html
;               or     http://idlastro.gsfc.nasa.gov/ftp/pro/markwardt/
;       LA_LEAST_SQUARES() - IDL built-in
; REVISION HISTORY:
;       Written, M. Sullivan, March 2014
;       Modified to better handle fields that straddle RA=0
;       Modified to populate final astrometry structure with naxis1/2
;         if these are passed
;
;-

FUNCTION xi_solve_tpv,xpixel,ypixel,pv1,TPVINFO=tpvinfo
COMPILE_OPT IDL2, STRICTARRSUBS

xin=tpvinfo.CD11*(xpixel-tpvinfo.crpix1)+tpvinfo.cd12*(ypixel-tpvinfo.crpix2)
yin=tpvinfo.CD21*(xpixel-tpvinfo.crpix1)+tpvinfo.cd22*(ypixel-tpvinfo.crpix2)


npv1=N_ELEMENTS(pv1)

xin2=xin*xin
yin2=yin*yin
r=SQRT(xin2+yin2)

xp=pv1[0] + pv1[1]*xin + pv1[2]*yin
IF(npv1 GT 3 && pv1[3] NE 0.0)THEN xp += pv1[3]*r
IF(npv1 GT 4 && pv1[4] NE 0.0)THEN xp += pv1[4]*xin2
IF(npv1 GT 5 && pv1[5] NE 0.0)THEN xp += pv1[5]*xin*yin
IF(npv1 GT 6 && pv1[6] NE 0.0)THEN xp += pv1[6]*yin2
IF(npv1 GT 7)THEN BEGIN
   IF(pv1[7] NE 0.0)THEN xp += pv1[7]*xin^3
   IF(Npv1 GT 8 && pv1[8] NE 0.0)THEN xp += pv1[8]*xin2*yin
   IF(Npv1 GT 9 && pv1[9] NE 0.0)THEN xp += pv1[9]*xin*yin2
   IF(Npv1 GT 10 && pv1[10] NE 0.0)THEN xp += pv1[10]*yin2*yin
   IF(Npv1 GT 11 && pv1[11] NE 0.0)THEN xp += pv1[11]*r^3
   IF(Npv1 GT 12) THEN BEGIN
      IF(pv1[12] NE 0.0) THEN xp += pv1[12]*xin2*xin2
      IF(npv1 GT 13 && pv1[13] NE 0.0) THEN xp += pv1[13]*xin2*xin*yin
      IF(npv1 GT 14 && pv1[14] NE 0.0) THEN xp += pv1[14]*xin2*yin2
      IF(npv1 GT 15 && pv1[15] NE 0.0) THEN xp += pv1[15]*xin*yin2*yin
      IF(npv1 GT 16 && pv1[16] NE 0.0) THEN xp += pv1[16]*yin2*yin2
   ENDIF
ENDIF      

RETURN,xp
END


FUNCTION eta_solve_tpv,xpixel,ypixel,pv2,TPVINFO=tpvinfo
COMPILE_OPT IDL2, STRICTARRSUBS

xin=tpvinfo.CD11*(xpixel-tpvinfo.crpix1)+tpvinfo.cd12*(ypixel-tpvinfo.crpix2)
yin=tpvinfo.CD21*(xpixel-tpvinfo.crpix1)+tpvinfo.cd22*(ypixel-tpvinfo.crpix2)


npv2=N_ELEMENTS(pv2)

xin2=xin*xin
yin2=yin*yin
r=SQRT(xin2+yin2)

yp=pv2[0] + pv2[1]*yin + pv2[2]*xin
IF(npv2 GT 3 && pv2[3] NE 0.0)THEN yp += pv2[3]*r
IF(npv2 GT 4 && pv2[4] NE 0.0)THEN yp += pv2[4]*yin2
IF(npv2 GT 5 && pv2[5] NE 0.0)THEN yp += pv2[5]*yin*xin
IF(npv2 GT 6 && pv2[6] NE 0.0)THEN yp += pv2[6]*xin2
IF(npv2 GT 7)THEN BEGIN
   IF(pv2[7] NE 0.0)THEN yp += pv2[7]*yin^3
   IF(Npv2 GT 8 && pv2[8] NE 0.0)THEN yp += pv2[8]*yin2*xin
   IF(Npv2 GT 9 && pv2[9] NE 0.0)THEN yp += pv2[9]*yin*xin2
   IF(Npv2 GT 10 && pv2[10] NE 0.0)THEN yp += pv2[10]*xin2*xin
   IF(Npv2 GT 11 && pv2[11] NE 0.0)THEN yp += pv2[11]*r^3
   IF(Npv2 GT 12) THEN BEGIN
      IF(pv2[12] NE 0.0) THEN yp += pv2[12]*yin2*yin2
      IF(npv2 GT 13 && pv2[13] NE 0.0) THEN yp += pv2[13]*yin2*yin*xin
      IF(npv2 GT 14 && pv2[14] NE 0.0) THEN yp += pv2[14]*yin2*xin2
      IF(npv2 GT 15 && pv2[15] NE 0.0) THEN yp += pv2[15]*yin*xin2*xin
      IF(npv2 GT 16 && pv2[16] NE 0.0) THEN yp += pv2[16]*xin2*xin2
   ENDIF
ENDIF      

RETURN,yp
END


FUNCTION eta_solve_tnx,xpixel,ypixel,params,TNXINFO=tnxinfo
COMPILE_OPT IDL2, STRICTARRSUBS

xin=tnxinfo.CD11*(xpixel-tnxinfo.crpix1)+tnxinfo.cd12*(ypixel-tnxinfo.crpix2)
yin=tnxinfo.CD21*(xpixel-tnxinfo.crpix1)+tnxinfo.cd22*(ypixel-tnxinfo.crpix2)

yp=0.d0
icount=0L
IF(tnxinfo.latcor.xterms EQ 1)THEN BEGIN
   ;; full cross-terms
   FOR n=0,tnxinfo.latcor.etaorder-1 DO BEGIN
      FOR m=0,tnxinfo.latcor.xiorder-1 DO BEGIN
         yp=yp + xin^m * yin^n * params[icount]
         icount++
      ENDFOR
   ENDFOR
ENDIF ELSE IF(tnxinfo.latcor.xterms EQ 0)THEN BEGIN
   ;; no cross-terms
   FOR m=0,tnxinfo.latcor.xiorder-1 DO BEGIN
      yp=yp + xin^m * params[icount]
      icount++
   ENDFOR
   FOR n=0,tnxinfo.latcor.etaorder-1 DO BEGIN
      yp=yp + yin^n * params[icount]
      icount++
   ENDFOR
ENDIF ELSE IF(tnxinfo.latcor.xterms EQ 2)THEN BEGIN
   ;; half cross terms
   maxxt=MAX([tnxinfo.latcor.xiorder,tnxinfo.latcor.etaorder])-1
   FOR n=0,tnxinfo.latcor.etaorder-1 DO BEGIN
      FOR m=0,tnxinfo.latcor.xiorder-1 DO BEGIN
         IF(m+n GT maxxt)THEN CONTINUE
;         print,m,n,m+n,icount
         yp=yp + xin^m * yin^n * params[icount]
         icount++
      ENDFOR
   ENDFOR   
ENDIF

etamodel=yin+yp

RETURN,etamodel
END

FUNCTION xi_solve_tnx,xpixel,ypixel,params,TNXINFO=tnxinfo
COMPILE_OPT IDL2, STRICTARRSUBS

xin=tnxinfo.CD11*(xpixel-tnxinfo.crpix1)+tnxinfo.cd12*(ypixel-tnxinfo.crpix2)
yin=tnxinfo.CD21*(xpixel-tnxinfo.crpix1)+tnxinfo.cd22*(ypixel-tnxinfo.crpix2)

xp=0.d0
icount=0L
IF(tnxinfo.lngcor.xterms EQ 1)THEN BEGIN
   ;; full cross-terms
   FOR n=0,tnxinfo.lngcor.etaorder-1 DO BEGIN
      FOR m=0,tnxinfo.lngcor.xiorder-1 DO BEGIN
         xp=xp + xin^m * yin^n * params[icount]
         icount++
      ENDFOR
   ENDFOR
ENDIF ELSE IF(tnxinfo.lngcor.xterms EQ 0)THEN BEGIN
   ;; no cross-terms
   FOR m=0,tnxinfo.lngcor.xiorder-1 DO BEGIN
      xp=xp + xin^m * params[icount]
      icount++
   ENDFOR
   FOR n=0,tnxinfo.lngcor.etaorder-1 DO BEGIN
      xp=xp + yin^n * params[icount]
      icount++
   ENDFOR
ENDIF ELSE IF(tnxinfo.lngcor.xterms EQ 2)THEN BEGIN
   ;; half cross terms
   maxxt=MAX([tnxinfo.lngcor.xiorder,tnxinfo.lngcor.etaorder])-1
   FOR n=0,tnxinfo.lngcor.etaorder-1 DO BEGIN
      FOR m=0,tnxinfo.lngcor.xiorder-1 DO BEGIN
         IF(m+n GT maxxt)THEN CONTINUE
;         print,m,n,m+n,icount
         xp=xp + xin^m * yin^n * params[icount]
         icount++
      ENDFOR
   ENDFOR   
ENDIF

ximodel=xin+xp

RETURN,ximodel
END

FUNCTION solve_astro,radeg,decdeg,xpixel,ypixel,XIORDER=xiorder,ETAORDER=etaorder,XTERMS=xterms,$
                     DISTORT=distort,CRVAL=crval,NITER=niter,REJECT=reject,XIRMS=xirms,ETARMS=etarms,$
                     SUCCESS=success,VERBOSE=verbose,NORTERMS=norterms,NREJ=nrej,n_tpvterms=n_tpvterms,$
                     XIRESID=xiresid,ETARESID=etaresid,WFIT=wfit,NAXIS1=naxis1,NAXIS2=naxis2
COMPILE_OPT IDL2, STRICTARRSUBS

success=0b
IF(N_ELEMENTS(verbose) EQ 0)THEN verbose=0b
IF(N_ELEMENTS(distort) EQ 0)THEN mydistort='none' ELSE mydistort=STRLOWCASE(distort)
IF(N_ELEMENTS(reject) EQ 0)THEN reject=3.0
IF(N_ELEMENTS(niter) EQ 0)THEN niter=1S

IF(N_ELEMENTS(radeg) NE N_ELEMENTS(decdeg))THEN BEGIN
   PRINT,'ERROR in solve_astro: xpixel/ypixel/radeg/decdeg must contain the same number of elements'
   RETURN,0
ENDIF

IF(N_ELEMENTS(xpixel) NE N_ELEMENTS(ypixel))THEN BEGIN
   PRINT,'ERROR in solve_astro: xpixel/ypixel/radeg/decdeg must contain the same number of elements'
   RETURN,0
ENDIF

IF(N_ELEMENTS(xpixel) NE N_ELEMENTS(radeg))THEN BEGIN
   PRINT,'ERROR in solve_astro: xpixel/ypixel/radeg/decdeg must contain the same number of elements'
   RETURN,0
ENDIF

IF(N_ELEMENTS(xpixel) EQ 0)THEN BEGIN
   PRINT,'ERROR in solve_astro: xpixel/ypixel/radeg/decdeg must contain at least 6 objects.'
   RETURN,0
ENDIF

data=REPLICATE({index:0L,xpixel:0.d0,ypixel:0.d0,radeg:0.d0,decdeg:0.d0,eta:0.d0,xi:0.d0},N_ELEMENTS(xpixel))
data.xpixel=xpixel
data.ypixel=ypixel
data.radeg=radeg
data.decdeg=decdeg
ndata=N_ELEMENTS(data)
data.index=LINDGEN(ndata)

IF (ndata LE 5)THEN BEGIN
   PRINT,'ERROR in solve_astro: xpixel/ypixel/radeg/decdeg must contain at least 6 objects.'
   RETURN,0
ENDIF

CASE mydistort OF 
   'tnx' : BEGIN
      IF(N_ELEMENTS(xterms) EQ 0)THEN xterms=2S
      IF(N_ELEMENTS(etaorder) EQ 0)THEN etaorder=3S
      IF(N_ELEMENTS(xiorder) EQ 0)THEN xiorder=3S
      IF(verbose GE 1)THEN PRINT,'Solving astrometry using '+STRN(ndata)+' points and TNX distortion, xiorder='+STRN(xiorder)+', etaorder='+STRN(etaorder)
   END
   'tpv' : BEGIN
      IF(N_ELEMENTS(norterms) EQ 0)THEN norterms=0b
      IF(N_ELEMENTS(n_tpvterms) EQ 0)THEN n_tpvterms=7S
      IF(verbose GE 1)THEN BEGIN
         IF(norterms)THEN PRINT,'Solving astrometry using '+STRN(ndata)+' points and TPV distortion, nterms='+STRN(n_tpvterms)+', no radial terms'
         IF(~norterms)THEN PRINT,'Solving astrometry using '+STRN(ndata)+' points and TPV distortion, nterms='+STRN(n_tpvterms)+', with radial terms.'
      ENDIF
   END
   'none': BEGIN
      IF(verbose GE 1)THEN PRINT,'Solving astrometry using '+STRN(ndata)+' points and no distortion.'
   END
   ELSE : BEGIN
      PRINT,'ERROR in solve_astro: distortion term '+mydistort+' not known.'
      RETURN,0
   ENDELSE
ENDCASE



;; set reference ra/dec to mean of input coords if not specified
IF(N_ELEMENTS(crval) EQ 0)THEN BEGIN
   ;; we need an ugly hack here to protect against fields that straddle ra=0
   wlow=WHERE(data.radeg GE 0.0 AND data.radeg LT 10.0,nlow)
   whigh=WHERE(data.radeg GE 350 AND data.radeg LT 360.0,nhigh)
   IF(nlow EQ 0 || nhigh EQ 0)THEN BEGIN
      crval=[MEAN(data.radeg,/DOUBLE),MEAN(data.decdeg,/DOUBLE)]
   ENDIF ELSE BEGIN
      ;; wrap around region
      ratmp=data.radeg
      ratmp[whigh]=ratmp[whigh]-360d0
      ratmpmean=MEAN(ratmp,/DOUBLE)
      IF(ratmpmean LT 0.0)THEN ratmpmean=ratmpmean+360d0
      crval=[ratmpmean,MEAN(data.decdeg,/DOUBLE)]
   ENDELSE
ENDIF
crpix=DBLARR(2)


;; apply tangent plane projection
WCSSPH2XY,data.radeg,data.decdeg,xi,eta,CRVAL=crval,CTYPE=['RA---TAN','DEC--TAN']
data.xi=xi
data.eta=eta

ndataorig=ndata
dataorig=data
FOR iter=0,niter-1 DO BEGIN
   ndatastart=ndata
;         xi = CD1_1 * (x - CRPIX1) + CD1_2 * (y - CRPIX2)
;         eta = CD2_1 * (x - CRPIX1) + CD2_2 * (y - CRPIX2)

;xi = a + b * x + c * y
;eta = d + e * x + f * y

   lhs1=[[REPLICATE(1d0,ndata)],[data.xpixel],[data.ypixel]]
   lhs1=TRANSPOSE(lhs1)
   rhs1=data.xi
   soln1=LA_LEAST_SQUARES(lhs1,rhs1,/DOUBLE)
   
   CD11=soln1[1]
   CD12=soln1[2]
   
   lhs2=[[REPLICATE(1d0,ndata)],[data.xpixel],[data.ypixel]]
   lhs2=TRANSPOSE(lhs2)
   rhs2=data.eta
   soln2=LA_LEAST_SQUARES(lhs2,rhs2,/DOUBLE)
   
   CD21=soln2[1]
   CD22=soln2[2]

   crpix[1]=(soln2[0]-CD21*soln1[0]/CD11 ) / (CD21/CD11*CD12-CD22)
   crpix[0]=(soln1[0]+CD12*crpix[1])/((-1.0)*CD11)

   IF(mydistort EQ 'tnx')THEN BEGIN
      
      ximin=MIN(data.xi)
      ximax=MAX(data.xi)
      etamin=MIN(data.eta)
      etamax=MAX(data.eta)

      IF(xterms EQ 0)THEN BEGIN
         nparams=etaorder+xiorder ;; none
      ENDIF ELSE IF(xterms EQ 1) THEN BEGIN
         nparams=etaorder*xiorder ;; full
      ENDIF ELSE IF(xterms EQ 2) THEN BEGIN
         nparams=0
         maxxt=MAX([xiorder,etaorder])-1
         FOR n=0,etaorder-1 DO BEGIN
            FOR m=0,xiorder-1 DO BEGIN
               IF(m+n GT maxxt)THEN CONTINUE
               nparams++
            ENDFOR
         ENDFOR   
      ENDIF
      
      lngcor={functype:3,xiorder:xiorder,etaorder:etaorder,xterms:xterms,ximin:ximin,ximax:ximax,etamin:etamin,etamax:etamax,coeff:DBLARR(nparams)}
      latcor={functype:3,xiorder:xiorder,etaorder:etaorder,xterms:xterms,ximin:ximin,ximax:ximax,etamin:etamin,etamax:etamax,coeff:DBLARR(nparams)}
      
      tnxinfo={crpix1:crpix[0],crpix2:crpix[1],cd11:cd11,cd12:cd12,cd21:cd21,cd22:cd22,$
               lngcor:{etaorder:etaorder,xiorder:xiorder,xterms:xterms},$
               latcor:{etaorder:etaorder,xiorder:xiorder,xterms:xterms}}

      functargs={tnxinfo:tnxinfo}
      start_params=DBLARR(nparams)
      res=MPFIT2DFUN('xi_solve_tnx',data.xpixel,data.ypixel,data.xi,REPLICATE(0.2d0/3600,ndata),start_params,FUNCTARGS=functargs,YFIT=ximodel,QUIET=verbose LT 2)
      lngcor.coeff=res
      start_params=DBLARR(nparams)
      res=MPFIT2DFUN('eta_solve_tnx',data.xpixel,data.ypixel,data.eta,REPLICATE(0.2d0/3600,ndata),start_params,FUNCTARGS=functargs,YFIT=etamodel,QUIET=verbose LT 2)
      latcor.coeff=res
   ENDIF ELSE IF (mydistort EQ 'tpv')THEN BEGIN
      
      tpvinfo={crpix1:crpix[0],crpix2:crpix[1],cd11:cd11,cd12:cd12,cd21:cd21,cd22:cd22}
      functargs={tpvinfo:tpvinfo}

      IF(norterms)THEN BEGIN
         pi=REPLICATE({value:0.d0, fixed:0},n_tpvterms)
         IF(n_tpvterms GE 4)THEN pi[3].fixed=1
         IF(n_tpvterms GE 10)THEN pi[11].fixed=1
      ENDIF
 
      start_params=DBLARR(n_tpvterms)
      pv1=MPFIT2DFUN('xi_solve_tpv',data.xpixel,data.ypixel,data.xi,REPLICATE(0.2d0/3600,ndata),start_params,FUNCTARGS=functargs,YFIT=ximodel,QUIET=verbose LT 2,PARINFO=pi)
      
      start_params=DBLARR(n_tpvterms)
      pv2=MPFIT2DFUN('eta_solve_tpv',data.xpixel,data.ypixel,data.eta,REPLICATE(0.2d0/3600,ndata),start_params,FUNCTARGS=functargs,YFIT=etamodel,QUIET=verbose LT 2,PARINFO=pi)
     
      
   ENDIF ELSE IF(mydistort EQ 'none')THEN BEGIN
      ximodel=CD11*(data.xpixel-crpix[0])+cd12*(data.ypixel-crpix[1])
      etamodel=CD21*(data.xpixel-crpix[0])+cd22*(data.ypixel-crpix[1])
   ENDIF ELSE BEGIN
      PRINT,'ERROR in solve_astro: distortion type '+mydistort+' not known.'
      RETURN,0
   ENDELSE

   xiresid=(data.xi-ximodel)*3600d0 ;; in "
   etaresid=(data.eta-etamodel)*3600d0 ; in "
   xirms=SQRT(TOTAL(xiresid^2,/DOUBLE)/ndata)
   etarms=SQRT(TOTAL(etaresid^2,/DOUBLE)/ndata)

   ;; sigma clipping on all but final loops
   IF(iter NE niter-1)THEN BEGIN
      resistant_mean,xiresid,reject,m,s,nrejxi,GOODVEC=wgoodxi,/DOUBLE
      data=data[wgoodxi]
      resistant_mean,etaresid[wgoodxi],reject,m,s,nrejeta,GOODVEC=wgoodeta,/DOUBLE
      data=data[wgoodeta]
      ndata=N_ELEMENTS(data)
   ENDIF
   IF(ndata EQ ndatastart)THEN BREAK
   
ENDFOR
nrej=ndataorig-ndata
wfit=data.index

;; construct the astro structure
cd=DBLARR(2,2)
cd[0,0]=cd11
cd[1,0]=cd21
cd[0,1]=cd12
cd[1,1]=cd22

make_astr,astro, CRPIX=crpix, CRVAL=crval, CD=cd, EQUINOX=2000., RADECSYS='FK5', CTYPE=['RA---TAN','DEC--TAN']

IF(N_ELEMENTS(naxis1) GT 0)THEN astro.naxis[0]=naxis1
IF(N_ELEMENTS(naxis2) GT 0)THEN astro.naxis[1]=naxis2

IF(mydistort EQ 'tnx')THEN BEGIN

   distorttmp = {name:'TNX', lngcor:lngcor, latcor:latcor}
   astro = create_struct(temporary(astro), 'distort', distorttmp)
   
   astro.ctype[0]='RA---TNX'
   astro.ctype[1]='DEC--TNX'
   
ENDIF ELSE IF(mydistort EQ 'tpv')THEN BEGIN
   
   distorttmp = {name:'TPV', a:0.0d, b:0.0d, ap:0.0d, bp:0.0d}
   astro = create_struct(temporary(astro), 'distort', distorttmp)
   
   astro.ctype[0]='RA---TPV'
   astro.ctype[1]='DEC--TPV'

   ;; remove PV1/PV2 tags if make_astr wrote them
   tags=tag_names(astro)
   ntags=N_ELEMENTS(tags)
   newstruct=create_struct(tags[0], astro.(0))
   FOR i=1L,ntags-1 DO BEGIN
      IF(tags[i] EQ 'PV1' || tags[i] EQ 'PV2')THEN CONTINUE
      newstruct=create_struct(newstruct, tags[i], astro.(i))
   ENDFOR
   astro=newstruct
   
   astro= create_struct(temporary(astro), 'pv1', pv1)
   astro= create_struct(temporary(astro), 'pv2', pv2)
   
ENDIF
IF(verbose GE 1)THEN BEGIN
   PRINT,'Solution has xi rms '+STRN(xirms)+' and eta rms '+STRN(etarms)+' with '+STRN(ndata)+' points.'
ENDIF

success=1b
RETURN,astro
END

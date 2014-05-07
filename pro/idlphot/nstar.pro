pro nstar,image,id,xc,yc,mags,sky,group,phpadu,readns,psfname,DEBUG=debug, $  
          errmag,iter,chisq,peak,PRINT=print,SILENT=silent, VARSKY = varsky
;+
; NAME:
;       NSTAR
; PURPOSE:
;       Simultaneous point spread function fitting (adapted from DAOPHOT)
; EXPLANATION:
;       This PSF fitting algorithm is based on a very old (~1987) version of 
;       DAOPHOT, and much better algorithms (e.g. ALLSTAR) are now available
;       -- though not in IDL.
;       
; CALLING SEQUENCE:
;       NSTAR, image, id, xc, yc, mags, sky, group, [ phpadu, readns, psfname,
;               magerr, iter, chisq, peak, /PRINT , /SILENT, /VARSKY, /DEBUG ]
;
; INPUTS:
;       image - image array
;       id    - vector of stellar ID numbers given by FIND
;       xc    - vector containing X position centroids of stars (e.g. as found
;               by FIND)
;       yc    - vector of Y position centroids
;       mags  - vector of aperture magnitudes (e.g. as found by APER)
;               If 9 or more parameters are supplied then, upon output
;               ID,XC,YC, and MAGS will be modified to contain the new
;               values of these parameters as determined by NSTAR.
;               Note that the number of output stars may be less than 
;               the number of input stars since stars may converge, or 
;               "disappear" because they are too faint.
;       sky   - vector of sky background values (e.g. as found by APER)
;       group - vector containing group id's of stars as found by GROUP
;
; OPTIONAL INPUT:
;       phpadu - numeric scalar giving number of photons per digital unit.  
;               Needed for computing Poisson error statistics.   
;       readns - readout noise per pixel, numeric scalar.   If not supplied, 
;               NSTAR will try to read the values of READNS and PHPADU from
;               the PSF header.  If still not found, user will be prompted.
;       psfname - name of FITS image file containing the point spread
;               function residuals as determined by GETPSF, scalar string.  
;               If omitted, then NSTAR will prompt for this parameter.
;
; OPTIONAL OUTPUTS:
;       MAGERR - vector of errors in the magnitudes found by NSTAR
;       ITER - vector containing the number of iterations required for
;               each output star.  
;       CHISQ- vector containing the chi square of the PSF fit for each
;               output star.
;       PEAK - vector containing the difference of the mean residual of
;               the pixels in the outer half of the fitting circle and
;               the mean residual of pixels in the inner half of the
;               fitting circle
;
; OPTIONAL KEYWORD INPUTS:
;       /SILENT - if set and non-zero, then NSTAR will not display its results
;               at the terminal
;       /PRINT - if set and non-zero then NSTAR will also write its results to
;               a file nstar.prt.   One also can specify the output file name
;               by setting PRINT = 'filename'.
;       /VARSKY - if this keyword is set and non-zero, then the sky level of
;               each group is set as a free parameter.
;       /DEBUG - if this keyword is set and non-zero, then the result of each
;               fitting iteration will be displayed.
;
; PROCEDURES USED:
;       DAO_VALUE(), READFITS(), REMOVE, SPEC_DIR(), STRN(), SXPAR()
;
; COMMON BLOCK:
;       RINTER - contains pre-tabulated values for cubic interpolation
; REVISION HISTORY
;       W. Landsman                 ST Systems Co.       May, 1988
;       Adapted for IDL Version 2, J. Isensee, September, 1990
;       Minor fixes so that PRINT='filename' really prints to 'filename', and
;       it really silent if SILENT is set.  J.Wm.Parker HSTX 1995-Oct-31
;       Added /VARSKY option   W. Landsman   HSTX      May 1996
;       Converted to IDL V5.0   W. Landsman   September 1997
;       Replace DATATYPE() with size(/TNAME)  W. Landsman November 2001
;       Assume since V5.5, remove VMS calls W. Landsman September 2006
;-
 compile_opt idl2
 common rinter,c1,c2,c3,init            ;Save time in RINTER()
 npar = N_params()
 if npar LT 7 then begin
   print,'Syntax - NSTAR, image, id, xc, yc, mags, sky, group, [phpadu, '
   print, $
     '   [readns, psfname, magerr, iter, chisq, peak, /SILENT, /PRINT, /VARSKY]'
   return
 endif                                                        

 if ( N_elements(psfname) EQ 0 ) then begin
   psfname=''
   read,'Enter name of FITS file containing PSF: ',psfname
 endif else zparcheck,'PSFNAME',psfname,10,7,0,'PSF disk file name'

 psf_file = file_search( psfname, COUNT = n)
 if n EQ 0 then message, $
      'ERROR - Unable to locate PSF file ' + spec_dir(psfname)

 if npar LT 9 then begin                                                     
   ans = ''
   read, $
     'Do you want to update the input vectors with the results of NSTAR? ',ans
   if strmid(strupcase(ans),0,1) EQ 'Y' then npar = 9
 endif

 if npar LT 9 then $
    message,'Input vectors ID,XC,YC and MAGS will not be updated by NSTAR',/INF

; Read in the FITS file containing the PSF

 s = size(image)
 icol = s[1]-1 & irow = s[2]-1  ;Index of last row and column
 psf = readfits(psfname, hpsf)
 if  N_elements(phpadu) EQ 0 then begin 
    par = sxpar(hpsf,'PHPADU', Count = N_phpadu)
    if N_phpadu eq 0 $
        then read, 'Enter photons per analog digital unit: ',phpadu $
        else phpadu = par
endif

 if ( N_elements(readns) EQ 0 ) then begin 
    par = sxpar(hpsf,'RONOIS', Count = N_ronois)
    if N_ronois EQ 0 $
        then read, 'Enter the readout noise per pixel: ',readns $
        else readns = par
 endif

 gauss = sxpar(hpsf,'GAUSS*')
 psfmag = sxpar(hpsf,'PSFMAG')
 psfrad = sxpar(hpsf,'PSFRAD')
 fitrad = sxpar(hpsf,'FITRAD')
 npsf = sxpar(hpsf,'NAXIS1')
;                               Compute RINTER common block arrays
 p_1 = shift(psf,1,0) & p1 = shift(psf,-1,0) & p2 = shift(psf,-2,0)
 c1 = 0.5*(p1 - p_1)
 c2 = 2.*p1 + p_1 - 0.5*(5.*psf + p2)
 c3 = 0.5*(3.*(psf-p1) + p2 - p_1)
 init = 1

 ronois = readns^2
 radsq = fitrad^2   &  psfrsq = psfrad^2
 sepmin = 2.773*(gauss[3]^2+gauss[4]^2)

;      PKERR will be used to estimate the error due to interpolating PSF
;      Factor of 0.027 is estimated from good-seeing CTIO frames

 pkerr = 0.027/(gauss[3]*gauss[4])^2     
 sharpnrm = 2.*gauss[3]*gauss[4]/gauss[0]
 if (N_elements(group) EQ 1) then groupid = group[0] else $
     groupid = where(histogram(group,min=0))    ;Vector of distinct group id's

 mag = mags                        ;Save original magnitude vector
 bad = where( mag GT 99, nbad )     ;Undefined magnitudes assigned 99.9
 if nbad GT 0 then mag[bad] = psfmag + 7.5
 mag = 10.^(-0.4*(mag-psfmag)) ;Convert magnitude to brightness, scaled to PSF
 fmt = '(I6,2F9.2,3F9.3,I4,F9.2,F9.3)'

 SILENT = keyword_set(SILENT)
 VARSKY = keyword_set(VARSKY)

 if keyword_set(PRINT) then begin
     if ( size(print,/TNAME) NE 'STRING' ) then file = 'nstar.prt' $
                                           else file = print
    message,'Results will be written to a file '+ file,/INF
     openw,lun,file,/GET_LUN
     printf,lun,'NSTAR:    '+ getenv('USER') + ' '+ systime()
     printf,lun,'PSF File:',psfname
 endif 
 PRINT = keyword_set(PRINT)

 hdr='   ID      X       Y       MAG     MAGERR   SKY   NITER     CHI     SHARP'
 if not(SILENT) then print,hdr
 if PRINT then printf,lun,hdr

 for igroup = 0, N_elements(groupid)-1 do begin

 index = where(group EQ groupid[igroup],nstr) 
 if not SILENT then print,'Processing group ', $
               strtrim(groupid[igroup],2),'    ',strtrim(nstr,2),' stars'
 if nstr EQ 0 then stop
 magerr = fltarr(nstr)
 chiold = 1.0
 niter = 0
 clip = 0b
 nterm = nstr*3 + varsky
 xold = dblarr(nterm)
 clamp = replicate(1.,nterm)
 xb = double(xc[index])  &   yb = double(yc[index])
 magg = double(mag[index]) & skyg = double(sky[index])
 idg = id[index]
 skybar = total(skyg)/nstr
 reset = 0b
;
START_IT : 
   niter = niter+1
RESTART: 
 case 1 of              ;Set up critical error for star rejection
   niter GE 4 : wcrit = 1
   niter GE 8 : wcrit = 0.4444444
   niter GE 12: wcrit = 0.25             
   else       : wcrit = 400                                                   
 endcase

 if reset EQ 1b then begin
     xb = xg + ixmin & yb = yg + iymin
 endif

 reset = 1b
 xfitmin = fix(xb - fitrad) > 0
 xfitmax = fix(xb + fitrad)+1 < (icol-1)
 yfitmin = fix(yb - fitrad) > 0
 yfitmax = fix(yb + fitrad)+1 < (irow-1)
 nfitx = xfitmax - xfitmin + 1
 nfity = yfitmax - yfitmin + 1
 ixmin = min(xfitmin)& iymin = min(yfitmin)
 ixmax = max(xfitmax)& iymax = max(yfitmax)
 nx = ixmax-ixmin+1 & ny = iymax-iymin+1
 dimage = image[ixmin:ixmax,iymin:iymax]
 xfitmin = xfitmin -ixmin & yfitmin = yfitmin-iymin
 xfitmax = xfitmax -ixmin & yfitmax = yfitmax-iymin
;                                        Offset to the subarray
 xg = xb-ixmin & yg = yb-iymin
 j = 0

 while (j LT nstr-1) do begin
   sep = (xg[j] - xg[j+1:*])^2 + (yg[j] - yg[j+1:*])^2
   bad = where(sep LT sepmin,nbad)
   if nbad GT 0 then begin      ;Do any star overlap?
      for l = 0,nbad-1 do begin
      k = bad[l] + j + 1
      if magg[k] LT magg[j] then imin = k else imin = j ;Identify fainter star
      if ( sep[l] LT 0.14*sepmin) or  $
         ( magerr[imin]/magg[imin]^2 GT wcrit ) then begin
      if  imin EQ j then imerge = k else imerge = j
      nstr = nstr - 1
      if not SILENT then print, $
       'Star ',strn(idg[imin]),' has merged with star ',strn(idg[imerge])
      totmag = magg[imerge] + magg[imin]
      xg[imerge] = (xg[imerge]*magg[imerge] + xg[imin]*magg[imin])/totmag
      yg[imerge] = (yg[imerge]*magg[imerge] + yg[imin]*magg[imin])/totmag
      magg[imerge] = totmag     
      remove,imin,idg,xg,yg,magg,skyg,magerr    ;Remove fainter star from group
      nterm = nstr*3 + varsky                   ;Update matrix size
      xold = dblarr(nterm) 
      clamp = replicate(1.,nterm)               ;Release all clamps
      clip = 0b
      niter = niter-1                           ;Back up iteration counter
      goto, RESTART 
      endif
   endfor
   endif
   j = j+1
 endwhile

 xpsfmin = (fix (xg - psfrad+1)) > 0
 xpsfmax = (fix (xg + psfrad  )) < (nx-1)
 ypsfmin = (fix (yg - psfrad+1)) > 0
 ypsfmax = (fix (yg + psfrad  )) < (ny-1)
 npsfx = xpsfmax-xpsfmin+1 & npsfy = ypsfmax-ypsfmin+1
 wt = fltarr(nx,ny)
 mask = bytarr(nx,ny)
 nterm = 3*nstr + varsky
 chi = fltarr(nstr) & sumwt = chi & numer = chi & denom = chi
 c = fltarr(nterm,nterm) & v = fltarr(nterm)

 for j = 0,nstr-1 do begin   ;Mask of pixels within fitting radius of any star
     x1 = xfitmin[j]  &  y1 = yfitmin[j]
     x2 = xfitmax[j]  &  y2 = yfitmax[j]
     rpixsq = fltarr(nfitx[j],nfity[j])
     xfitgen2 = (findgen(nfitx[j]) + x1 - xg[j])^2
     yfitgen2 = (findgen(nfity[j]) + y1 - yg[j])^2
     for k=0,nfity[j]-1 do rpixsq[0,k] = xfitgen2 + yfitgen2[k]
     temp = (rpixsq LE 0.999998*radsq)
     mask[x1,y1] = mask[x1:x2,y1:y2] or temp
     good = where(temp)
     rsq = rpixsq[good]/radsq
     temp1 = wt[x1:x2,y1:y2] 
     temp1[good] = temp1[good] > (5./(5.+rsq/(1.-rsq)) )
     wt[x1,y1] = temp1
 endfor

 igood = where(mask, ngoodpix)
 x = dblarr(ngoodpix,nterm)
 if varsky then x[0, nterm-1] = replicate(-1.0d, ngoodpix)

 psfmask = bytarr(ngoodpix,nstr)
 d = dimage[igood] - skybar
 for j = 0,nstr-1 do begin ;Masks of pixels within PSF radius of each star
     x1 = xpsfmin[j]   &    y1 = ypsfmin[j]
     x2 = xpsfmax[j]   &    y2 = ypsfmax[j]
     xgen = lindgen(npsfx[j]) + x1 - xg[j]
     ygen = lindgen(npsfy[j]) + y1 - yg[j]
     xgen2 = xgen^2 & ygen2 = ygen^2
     rpxsq = fltarr( npsfx[j],npsfy[j] )
     for k = 0,npsfy[j]-1 do rpxsq[0,k] = xgen2 + ygen2[k]
     temp =  mask[x1:x2,y1:y2] and (rpxsq LT psfrsq)
     temp1 = bytarr(nx,ny)
     temp1[x1,y1] = temp 
     goodfit = where(temp1[igood])
     psfmask[goodfit+ngoodpix*j] = 1b
     good = where(temp)
     xgood = xgen[good mod npsfx[j]] & ygood = ygen[good/npsfx[j]]
     model = dao_value(xgood,ygood,gauss,psf,dvdx,dvdy)
     d[goodfit] = d[goodfit] - magg[j]*model
     x[goodfit + 3*j*ngoodpix] = -model
     x[goodfit + (3*j+1)*ngoodpix] = magg[j]*dvdx
     x[goodfit + (3*j+2)*ngoodpix] = magg[j]*dvdy
 endfor

 wt = wt[igood] & idimage = dimage[igood]
 dpos = (idimage-d) > 0
 sigsq = dpos/phpadu + ronois + (0.0075*dpos)^2 + (pkerr*(dpos-skybar))^2

 relerr = abs(d)/sqrt(sigsq)
 if clip then begin   ;Reject pixels with 20 sigma errors (after 1st iteration)
        bigpix = where(relerr GT 20.*chiold, nbigpix)
        if ( nbigpix GT 0 ) then begin
              keep = indgen(ngoodpix)
              for i = 0,nbigpix-1 do keep = keep[ where( keep NE bigpix[i]) ]
              wt= wt[keep] & d = d[keep] & idimage = idimage[keep] 
              igood= igood[keep]  & relerr = relerr[keep]
              psfmask = psfmask[keep,*]   &  x = x[keep,*]
        endif
 endif

 sumres = total(relerr*wt)
 grpwt = total(wt)

 dpos = ((idimage-skybar) > 0) + skybar
 sig = dpos/phpadu + ronois + (0.0075*dpos)^2 + (pkerr*(dpos-skybar))^2
 for j = 0,nstr-1 do begin
     goodfit = where(psfmask[*,j])
     chi[j] = total(relerr[goodfit]*wt[goodfit])
     sumwt[j] = total(wt[goodfit])
     xgood = igood[goodfit] mod nx & ygood = igood[goodfit]/nx
     rhosq = ((xg[j] - xgood)/gauss[3])^2  +  ((yg[j] - ygood)/gauss[4])^2
     goodsig = where(rhosq LT 36)     ;Include in sharpness index only
     rhosq = 0.5*rhosq[goodsig]       ;pixels within 6 sigma of centroid
     dfdsig = exp(-rhosq)*(rhosq-1.)
     sigpsf = sig[goodfit[goodsig]] & dsig = d[goodfit[goodsig]]
     numer[j] = total(dfdsig*dsig/sigpsf)
     denom[j] = total(dfdsig^2/sigpsf)
 endfor

 wt = wt/sigsq
 if clip then $  ;After 1st iteration, reduce weight of a bad pixel
   wt = wt/(1.+(0.4*relerr/chiold)^8) 

 v = d * wt # x
 c = transpose(x) # ( ( wt # replicate(1.,nterm) ) * x )

 if grpwt GT 3 then begin
        chiold = 1.2533*sumres*sqrt(1./(grpwt*(grpwt-3.)))
        chiold = ((grpwt-3.)*chiold+3.)/grpwt
 endif

 i = where(sumwt GT 3, ngood)
 if ngood GT 0 then begin 
     chi[i] = 1.2533*chi[i]*sqrt(1./((sumwt[i]-3.)*sumwt[i]))
     chi[i] = ((sumwt[i]-3.)*chi[i]+3.)/sumwt[i]
 endif

chibad = where(sumwt LE 3, ngood)
if ngood GT 0 then chi[chibad] = chiold

 c = invert(c)
 x = c # transpose(v)

 if (not clip) or (niter LE 1) then redo = 1b else redo = 0b 
 if varsky then begin
        skybar = skybar - x[nterm-1]
        if abs(x[nterm-1]) GT  0.01 then redo = 1b
 endif
 clip = 1b

 j = 3*indgen(nstr) & k = j+1 & l=j+2
 sharp = sharpnrm*numer/(magg*denom)
 if not redo then begin
   redo = max(abs(x[j]) GT ( (0.05*chi*sqrt(c[j+nterm*j])) > 0.001*magg) )
   if redo EQ 0 then redo = max( abs([x[k], x[l]]) GT 0.01)
 endif

 sgn = where( xold[j]*x[j]/magg^2 LT -1.E-37, Nclamp )  
 if Nclamp GT 0 then clamp[j[sgn]] = 0.5*clamp[j[sgn]]
 sgn = where( xold[k]*x[k]        LT -1.E-37, Nclamp )
 if Nclamp GT 0 then clamp[k[sgn]] = 0.5*clamp[k[sgn]]
 sgn = where( xold[l]*x[l]        LT -1.E-37, Nclamp )
 if Nclamp GT 0 then clamp[l[sgn]] = 0.5*clamp[l[sgn]]

 magg = magg-x[j] / (1.+ ( (x[j]/(0.84*magg)) > (-x[j]/(5.25*magg)) )/ clamp[j] )
 xg = xg - x[k]   /(1.+abs(x[k])/( clamp[k]*0.5))
 yg = yg - x[l]   /(1.+abs(x[l])/( clamp[l]*0.5))
 xold = x

 magerr = c[j+nterm*j]*(nstr*chi^2 + (nstr-1)*chiold^2)/(2.*nstr-1.)

 dx = (-xg) > ( (xg - nx) > 0.) ;Find stars outside subarray
 dy = (-yg) > ( (yg-  ny) > 0.)
 badcen = where(    $                     ;Remove stars with bad centroids
     (dx GT 0.001) or (dy GT 0.001) or ( (dx+1)^2 + (dy+1)^2 GE radsq ), nbad)
 if nbad GT 0 then begin
        nstr = nstr - nbad
        print,strn(nbad),' stars eliminated by centroid criteria'
        if nstr LE 0 then goto, DONE_GROUP 
        remove, badcen, idg, xg, yg, magg, skyg, magerr
        nterm = nstr*3 + varsky
        redo = 1b
 endif

 faint = 1        
 toofaint =  where (magg LE 1.e-5,nfaint) 
                              ;Number of stars 12.5 mags fainter than PSF star
 if nfaint GT 0 then begin        
         faint = min( magg[toofaint], min_pos )
         ifaint = toofaint[ min_pos ]
         magg[toofaint] = 1.e-5
         goto, REM_FAINT                ;Remove faintest star
 endif else begin
         faint = 0.
         ifaint = -1
         if (not redo) or (niter GE 4) then $
            faint = max(magerr/magg^2, ifaint) else $ 
            goto,START_IT 
 endelse

 if keyword_set(DEBUG) then begin 
   err = 1.085736*sqrt(magerr)/magg
    for i=0,nstr-1 do  print,format=fmt,idg[i],xg[i]+ixmin,yg[i]+iymin, $
          psfmag-1.085736*alog(magg[i]),err[i],skyg[i],niter,chi[i],sharp[i]
 endif

 if redo and (niter LE 50) and (faint LT wcrit) then goto,START_IT   
REM_FAINT: 
 if (faint GE 0.25) or (nfaint GT 0) then begin
         if not SILENT then $
               message,'Star '+ strn(idg[ifaint]) + ' is too faint',/INF
         nstr = nstr-1
         if nstr LE 0 then goto,DONE_GROUP  
         remove,ifaint,idg,xg,yg,magg,skyg,magerr
         nterm = nstr*3 + varsky
         xold = dblarr(nterm)
         clamp = replicate(1.,nterm)
         clip = 0b
         niter = niter-1
         goto,RESTART  
 endif

 err = 1.085736*sqrt(magerr)/magg
 magg = psfmag - 1.085736*alog(magg)
 sharp = sharp > (-99.999) < 99.999
 xg = xg+ixmin & yg = yg+iymin

; Print results to terminal and/or file

 if not SILENT then for i = 0,nstr-1 do print,format=fmt, $
     idg[i],xg[i],yg[i],magg[i],err[i],skyg[i],niter,chi[i],sharp[i]
 if PRINT then for i = 0,nstr-1 do printf,lun,format=fmt, $
     idg[i],xg[i],yg[i],magg[i],err[i],skyg[i],niter,chi[i],sharp[i]

 if ( npar GE 9 ) then begin                  ;Create output vectors?
   if ( N_elements(newid) EQ 0 ) then begin  ;Initialize output vectors?
       newid = idg &  newx = xg  &  newy = yg & newmag = magg
       iter = replicate(niter,nstr) & peak = sharp & chisq = chi
       errmag = err
   endif else begin           ;Append current group to output vector
       newid = [newid,idg] & newx = [newx ,xg] & newy = [newy,yg]
       newmag = [newmag,magg] & iter = [iter,replicate(niter,nstr)]
       peak = [peak,sharp]     & chisq = [chisq,chi] & errmag = [errmag,err]
   endelse
 endif

DONE_GROUP: 
 endfor

 if  ( npar GE 9 ) then begin
    if N_elements(newid) GT 0 then begin
       id = newid &  xc = newx &  yc = newy  & mags = newmag
    endif else $
     message,'ERROR - There are no valid stars left, variables not updated',/CON
 endif

 if PRINT then free_lun,lun

 return
 end

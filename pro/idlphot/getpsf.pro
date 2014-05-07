pro getpsf,image,xc,yc,apmag,sky,ronois,phpadu, gauss,psf,idpsf,psfrad, $
            fitrad,psfname, DEBUG = debug
;+
; NAME:
;	GETPSF
; PURPOSE:
;	To generate a point-spread function (PSF) from observed stars. 
; EXPLANATION:
;	The PSF is represented as a 2-dimensional Gaussian
;	(integrated over each pixel) and a lookup table of residuals.
;	The lookup table and Gaussian parameters are output in a FITS
;	image file.   The PSF FITS file created by GETPSF can be
;	read with the procedure RDPSF.      Adapted from the 1986 STSDAS 
;	version of DAOPHOT
;
; CALLING SEQUENCE:
;	GETPSF, image, xc, yc, apmag, sky, [ronois, phpadu, gauss, psf, 
;			idpsf, psfrad, fitrad, psfname, /DEBUG ]
;
; INPUTS:
;	IMAGE  - input image array
;	XC     - input vector of x coordinates (from FIND), these should be
;		IDL (first pixel is (0,0)) convention.
;	YC     - input vector of y coordinates (from FIND)
;	APMAG  - vector of magnitudes (from APER), used for initial estimate
;		of gaussian intensity.  If APMAG is multidimensional, (more
;		than 1 aperture was used in APER) then the first aperture
;		is used.
;	SKY    - vector of sky values (from APER)                
;
; OPTIONAL INPUTS:
;	The user will be prompted for the following parameters if not supplied.
;
;	RONOIS - readout noise per pixel, (in electrons, or equivalent photons)
;	PHPADU - photons per analog digital unit, used to scale the data
;		numbers in IMAGE into photon units
;	IDPSF  - subscripts of the list of stars created by 
;		APER which will be used to define the PSF.   Stars whose
;		centroid does not fall within PSFRAD of the edge of the frame,
;		or for which a Gaussian fit requires more than 25 iterations,
;		will be ignored when creating the final PSF.
;	PSFRAD - the scalar radius, in pixels, of the circular area within
;		which the PSF will be defined.   This should be slightly larger
;		than the radius of the brightest star that one will be
;		interested in.
;	FITRAD - the scalar radius, in pixels of the circular area used in the
;		least-square star fits.  Stetson suggest that FITRAD should
;		approximately equal to the FWHM, slightly less for crowded
;		fields.  (FITRAD must be smaller than PSFRAD.)
;	PSFNAME- Name of the FITS file that will contain the table of residuals,
;		and the best-fit Gaussian parameters.    This file is 
;		subsequently required for use by NSTAR.  
;
; OPTIONAL OUTPUTS:
;	GAUSS  - 5 element vector giving parameters of gaussian fit to the 
;		first PSF star
;		GAUSS(0) - height of the gaussian (above sky)
;		GAUSS(1) - the offset (in pixels) of the best fitting gaussian
;			and the original X centroid
;		GAUSS(2) - similiar offset from the Y centroid 
;		GAUSS(3) - Gaussian sigma in X
;		GAUSS(4) - Gaussian sigma in Y
;	PSF    - 2-d array of PSF residuals after a Gaussian fit.
;
; PROCEDURE:
;	GETPSF fits a Gaussian profile to the core of the first PSF star 
;	and generates a look-up table of the residuals of the
;	actual image data from the Gaussian fit.  If desired, it will then
;	fit this PSF to another star (using PKFIT) to determine its precise 
;	centroid, scale the same Gaussian to the new star's core, and add the
;	differences between the actual data and the scaled Gaussian to the
;	table of residuals.   (In other words, the Gaussian fit is performed
;       only on the first star.)
;
; OPTIONAL KEYWORD INPUT:
;	DEBUG - if this keyword is set and non-zero, then the result of each
;		fitting iteration will be displayed.
;
; PROCEDURES CALLED
;	DAOERF, MAKE_2D, MKHDR, RINTER(), PKFIT, STRNUMBER(), STRN(), WRITEFITS
;
; REVISON HISTORY:
;	Adapted from the 1986 version of DAOPHOT in STSDAS
;	IDL Version 2  W Landsman           November 1988
;	Use DEBUG keyword instead of !DEBUG  W. Landsman       May 1996
;	Converted to IDL V5.0   W. Landsman   September 1997
;-                                              
 On_error,2                      ;Return to caller

 common rinter,c1,c2,c3,init	;Save time in RINTER
 init = 0                        ;Initialize the common blocks

 npar = N_params()

 if npar LT 5 then begin	 	;Enough parameters passed?
   print,'Syntax -  GETPSF, image, x, y, mags, sky, '
   print,'       [ronois, phpadu, gauss, psf, idpsf, psfrad, fitrad, ' + $
         'psfname, /DEBUG]'
   return
 endif

 s = size(image)    		;Get number of rows and columns in image
 ncol = s[1] & nrow = s[2]
 nstar = N_elements(xc)	        ;Total # of stars identified in image

 if N_elements(idpsf) LT 1 then begin	;Array of PSF id's defined?
   idpsf = intarr(25)
   i = 0 &  id = ''
   print,"GETPSF: Enter index of stars to be used for PSF, one index per line"
   RD_ID:   
   print,'Enter a stellar ID ( [RETURN] when finished) '
   read,id
   if id EQ '' then begin             ;Did User hit the [RETURN] key
             if i EQ 0 then return    ;No stellar ID's supplied
             idpsf = idpsf[0:i-1]
             goto, GOT_ID     
   endif else result = strnumber(id,val)

   if not result then print,string(7b),'INVALID INPUT:' else $
         if (val GE nstar) or (val LT 0) then $ 
                print,string(7b),'INVALID ID NUMBER' else begin
             idpsf[i] = fix(val) 
             i = i+1
         endelse
   goto,RD_ID
 endif 

GOT_ID:  

 if N_elements(psfrad) NE 1 then read, $
   'Enter radius (in pixels) of circular area defining the PSF: ',psfrad
 if N_elements(fitrad) NE 1 then read, $
   'Enter radius (in pixels) to be used for Gaussian fitting: ',fitrad
 if fitrad GE psfrad then $
    message,'ERROR - Fitting radius must be smaller than radius defining PSF'

 if N_elements(ronois) NE 1 then read, $
   'Enter readout noise per pixel: ',ronois
 if N_elements(phpadu) NE 1 then read, $
   'Enter photons per analog digital unit: ',phpadu

 numpsf = N_elements(idpsf)      ;# of stars used to create the PSF

 smag = size(apmag)     ;Is APMAG multidimensional?
 if N_elements(apmag) NE smag[1] then mag = apmag[0,*] else mag = apmag[*]

 n = 2*fix(psfrad+0.5)+1  ;(Odd) width of box that contains PSF circle
 npsf = 2*n+7             ;Lookup table has half pixel interpolation
 nbox = n+7		 ;(Even) Width of subarray to be extracted from image
 nhalf = nbox/2           

 if keyword_set(DEBUG) then begin
    print,'GETPSF: Fitting radius - ',string(float(fitrad),'(F5.1)')
    print,'        PSF Radius     - ',string(float(psfrad),'(F5.1)')
    print,'        Stellar IDs: ',idpsf   & print,' '
 endif

 boxgen = findgen(nbox)
 make_2d, boxgen, boxgen, xgen, ygen

;               Find the first PSF star in the star list.
 nstrps = -1	;Counter for number of stars used to create PSF
GETSTAR: 

 nstrps = nstrps + 1       
 if nstrps GE numpsf then $
     message,'ERROR - No valid PSF stars were supplied'

 istar = idpsf[nstrps]       ;ID number of first PSF star
 ixcen = fix(xc[istar])      
 iycen = fix(yc[istar])

;  Now a subarray F will be read in from the big image, given by 
;  IXCEN-NBOX/2+1 <= x <= IXCEN+NBOX/2, IYCEN-NBOX/2+1 <= y <= IYCEN+NBOX/2.  
;  (NBOX is an even number.)  In the subarray, the coordinates of the centroid
;  of the star will lie between NBOX/2 and NBOX/2+1 in each coordinate.

 lx = ixcen-nhalf+1  &  ux = ixcen + nhalf  ;Upper & lower bounds in X
 ly = iycen-nhalf+1  &  uy = iycen + nhalf
 if ((lx LT 0)   or (ly LT 0) or $     ;Star too close to edge?
   (ux GE ncol) or (uy GE nrow)) then begin    
   print,'GETPSF: Star ',strn(istar),' too near edge of frame.'
   goto, GETSTAR
 endif                      

 f = image[lx:ux,ly:uy] - sky[istar]  ;Read in subarray, subtract off sky

; An integrated Gaussian function will be fit to the central part of the
; stellar profile.  Initially, a 5x5 box centered on the centroid of the 
; star is used, but if the sigma in one coordinate drops to less than
; 1 pixel, then the box width of 3 will be used in that coordinate.
; If the sigma increases to over 3 pixels, then a box width of 7 will be 
; used in that coordinate

 x = xc[istar] - lx    ;X coordinate of stellar centroid in subarray F
 y = yc[istar] - ly    ;Y coordinate of stellar centroid in subarray F
 ix = fix(x+0.5)       ;Index of pixel containing centroid
 iy = fix(y+0.5) 
;                     ;Begin least squares
 h = max(f)  	      ;Initial guess for peak intensity
 sigx = 2.0 & sigy = 2.0                                       
 dxcen=0.  &  dycen=0.
;
 niter = 0                    ;Beginning of big iteration loop
 v = fltarr(5)
 c = fltarr(5,5)
;                            Print the current star
 fmt1 = "(/17X, 'STAR', 5X, 'X', 8X, 'Y', 5X, 'MAG  1', 5X, 'SKY')"
 fmt2 = "(15X, I5, 2F9.2, 12F9.3)"
 if keyword_set(DEBUG) then begin
    print,format=fmt1          
    print,format=fmt2,istar, xc[istar], yc[istar], mag[istar], sky[istar]
 endif

 if keyword_set(DEBUG) then print,'GETPSF: Gaussian Fit Iteration'

 REPEAT BEGIN		     ;Begin the iterative loop

 niter = niter + 1
 if niter GT 100 then begin   ;No convergence after 100 iterations?
    message,'No convergence after 100 iterations for star ' + strn(istar),/INF
    goto, GETSTAR 
 endif

      if sigx LE 1 then nx = 1  $  ;A default box width 
 else if sigx GT 3 then nx = 3  $
 else                   nx = 2

      if sigy LE 1 then ny = 1  $
 else if sigy GT 3 then ny = 3  $
 else                   ny = 2

 a = [H, x+dxcen,y+dycen,sigx,sigy]
 xin = (findgen(2*nx+1)-nx) + ix
 yin = (findgen(2*ny+1)-ny) + iy
 make_2d, xin, yin
 DAOERF, xin, yin, a, g, t

;  The T's are the first derivatives of the model profile with respect
;  to the five fitting parameters H, DXCEN, DYCEN, SIGX, and SIGY.
;  Note that the center of the best-fitting Gaussian profile is
;  expressed as an offset from the centroid of the star.  In the case of
;  a general, asymmetric stellar profile, the center of symmetry of the
;  best-fitting Gaussian profile will not necessarily coincide with the
;  centroid determined by any arbitrary centroiding algorithm.  

 dh = f[ ix-nx:ix+nx, iy-ny:iy+ny] - g ;Subtract best fit Gaussian from subarray
 for kk = 0,4 do begin
      tk = t[*,kk]
      v[kk] = total( dh * tk )
      for ll = 0,4 do c[kk,ll] = total( tk * t[*,ll] )
 endfor

 c = invert(c,status)	;IDL version assumes INVERT is successful

 if status EQ 1 then begin
     message,'Singular matrix encountered fitting star ' + strn(istar),/INF
     goto, GETSTAR
 endif 

 z = c#v         ;Multiply by vector of residuals

 h = h + z[0]/(1.0+4.0*abs(z[0]/h))	;Correct the fitting parameters
 dxcen = dxcen+z[1]/(1.0+3.0*abs(z[1]))
 dycen = dycen+z[2]/(1.0+3.0*abs(z[2]))
 sigx = sigx+z[3]/(1.0+4.0*abs(z[3]/sigx))
 sigy = sigy+z[4]/(1.0+4.0*abs(z[4]/sigy))

 if keyword_set(DEBUG) then print,niter,h,dxcen,dycen,sigx,sigy

 endrep until $ 				;Test for convergence  
       (abs(z[0]/h)+abs(z[3]/sigx)+abs(z[4]/sigy) LT 0.0001)

;  Now that the solution has converged, we can generate an
;  array containing the differences between the actual stellar profile
;  and the best-fitting Gaussian analytic profile.

 a = [H, x+dxcen, y+dycen, sigx,sigy]  ;Parameters for Gaussian fit
 DAOERF,xgen,ygen,a,g                  ;Compute Gaussian
 f = f - g                             ;Residuals (Real profile - Gaussian)

 psfmag = mag[istar]
 xpsf1 = xc[istar] & ypsf1 = yc[istar]

; The look-up table is obtained by interpolation within the array of
; fitting residuals.  We need to interpolate because we want the look-up
; table to be centered accurately on the centroid of the star, which of 
; course is at some fractional-pixel position in the original data.

 ncen = (npsf-1)/2.
 psfgen = (findgen(npsf) - ncen)/2.         ;Index function for PSF array
 YY = psfgen + Y   &  XX = psfgen + X
 make_2d,xx,yy
 psf = RINTER(F, XX, YY)            ;Interpolate residuals onto current star
 gauss = [h,dxcen,dycen,sigx,sigy]
 goodstar = nstrps                   ;Index of first good star

; For each additional star, determine the precise  coordinates of the 
; centroid and the relative brightness of the star
; by least-squares fitting to the current version of the point-spread
; function.  Then subtract off the appropriately scaled integral under
; the analytic Gaussian function  and add the departures of the actual 
; data from the analytic Gaussian function to the look-up table.

GETMORE:            ;Loop for additional PSF stars begins here                 
 nstrps = nstrps+1
 if nstrps GE numpsf then goto,WRITEOUT	;Have all the stars been done?

 istar = idpsf[nstrps]
 ixcen = fix(xc[istar])
 iycen = fix(yc[istar])                  
 scale = 10.^(-0.4*(mag[istar]-psfmag))

; Fit the current version of the point-spread function to the data for
; this star.

 lx = ixcen-nhalf+1 & ux =ixcen + nhalf
 ly = iycen-nhalf+1 & uy =iycen + nhalf
 if ( (lx LT 0) or (ly LT 0) or $             ;Star too close to edge?
    (ux GE ncol) or (uy GE nrow)) then begin  
   print,'GETPSF: Star ',strn(istar),' too near edge of frame.'
   goto,GETMORE
 endif                      

 if keyword_set(DEBUG) then begin
   print,format=fmt1
   print,format=fmt2, istar, xc[istar], yc[istar], mag[istar], sky[istar]
 endif

 f = image[lx:ux,ly:uy]
 x = xc[istar]-lx   &   y = yc[istar]-ly   

 pkfit, f, scale, x, y, sky[istar], fitrad, ronois, phpadu, $
		gauss, psf, errmag, chi, sharp, niter, DEBUG = debug

 if niter EQ 25 then begin	;Convergence in less than 25 iterations?
      print,'GETPSF: No convergence after 25 iterations for star',istar
      goto, GETMORE 
 endif

 a = [gauss[0], x+dxcen,y+dycen,sigx,sigy]  ;Parameters of successful fit
 daoerf,xgen,ygen,a,e
 f = f - scale*e -sky[istar]	           ;Compute array of residuals

; Values of the array of residuals are now interpolated to an NPSF by
; NPSF (NPSF is an odd number) array centered on the centroid of the
; star, and added to the existing look-up table of corrections to the 
; analytic profile 

 xx = psfgen + x
 yy = psfgen + y 
 make_2d,xx,yy
 psf = psf + RINTER(f,xx,yy)    

; Now correct both the height of the analytic Gaussian, and the value
; of the aperture-magnitude of the point-spread function for the
; inclusion of the additional star.

 psfmag = -2.5*alog10((1.+scale)*10^(-0.4*psfmag))
 gauss[0] = gauss[0]*(1.+scale)
 goodstar = [ goodstar, nstrps]
 goto, GETMORE  

WRITEOUT:   

; Create FITS file containing the PSF created.

 if ( N_elements(psfname) EQ  0 ) then begin
   psfname=''
   read,'Enter name of FITS file to contain final PSF ([RETURN] to exit): ',psfname
 endif

if ( psfname EQ  '' ) then return

 mkhdr, hdr, psf         ;Create a minimal FITS header
 sxaddpar, hdr, 'PHPADU', phpadu, 'Photons per Analog Digital Unit'
 sxaddpar, hdr, 'RONOIS', ronois, 'Readout Noise'
 sxaddpar, hdr, 'PSFRAD', psfrad, 'Radius where PSF is defined (pixels)'
 sxaddpar, hdr, 'FITRAD', fitrad, 'Fitting Radius'
 sxaddpar, hdr, 'PSFMAG', psfmag, 'PSF Magnitude'
 sxaddpar, hdr, 'GAUSS1', gauss[0], 'Gaussian Scale Factor'
 sxaddpar, hdr, 'GAUSS2', gauss[1], 'Gaussian X Position'
 sxaddpar, hdr, 'GAUSS3', gauss[2], 'Gaussian Y Position'
 sxaddpar, hdr, 'GAUSS4', gauss[3], 'Gaussian Sigma: X Direction'
 sxaddpar, hdr, 'GAUSS5', gauss[4], 'Gaussian Sigma: Y Direction'

 ngood = N_elements(goodstar)
 sxaddhist,'GETPSF: '+ systime() + ' ' + strn(ngood) +  $
           ' Stars Used to Create PSF',hdr

  sxaddhist,'GETPSF: ID - '+ string(idpsf[goodstar[0:12<ngood-1]], $
    format='(13i5)'),hdr

 if ngood gt 13 then $
     sxaddhist,'GETPSF: ID - '+ string(idpsf[goodstar[13:*]], $
     format='(13i5)'),hdr

 sxaddhist,'PSF Coordinates:'+ $
    string(xpsf1, format='(F7.2)') + $
    string(ypsf1, format='(F7.2)'), hdr

 writefits,psfname,psf,hdr

 return
 end

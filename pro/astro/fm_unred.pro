pro fm_unred, wave, flux, ebv, funred, R_V = R_V, gamma = gamma, x0 = x0, $
              c1 = c1, c2 = c2, c3 = c3, c4 = c4,avglmc=avglmc, lmc2 = lmc2, $
              ExtCurve=ExtCurve
;+
; NAME:
;     FM_UNRED
; PURPOSE:
;     Deredden a flux vector using the Fitzpatrick (1999) parameterization
; EXPLANATION:
;     The R-dependent Galactic extinction curve is that of Fitzpatrick & Massa 
;     (Fitzpatrick, 1999, PASP, 111, 63; astro-ph/9809387 ).    
;     Parameterization is valid from the IR to the far-UV (3.5 microns to 0.1 
;     microns).    UV extinction curve is extrapolated down to 912 Angstroms.
;
; CALLING SEQUENCE:
;     FM_UNRED, wave, flux, ebv, [ funred, R_V = , /LMC2, /AVGLMC, ExtCurve= 
;                       gamma =, x0=, c1=, c2=, c3=, c4= ]
; INPUT:
;      WAVE - wavelength vector (Angstroms)
;      FLUX - calibrated flux vector, same number of elements as WAVE
;               If only 3 parameters are supplied, then this vector will
;               updated on output to contain the dereddened flux.
;      EBV  - color excess E(B-V), scalar.  If a negative EBV is supplied,
;               then fluxes will be reddened rather than dereddened.
;
; OUTPUT:
;      FUNRED - unreddened flux vector, same units and number of elements
;               as FLUX
;
; OPTIONAL INPUT KEYWORDS
;      R_V - scalar specifying the ratio of total to selective extinction
;               R(V) = A(V) / E(B - V).    If not specified, then R = 3.1
;               Extreme values of R(V) range from 2.3 to 5.3
;
;      /AVGLMC - if set, then the default fit parameters c1,c2,c3,c4,gamma,x0 
;             are set to the average values determined for reddening in the 
;             general Large Magellanic Cloud (LMC) field by Misselt et al. 
;            (1999, ApJ, 515, 128)
;      /LMC2 - if set, then the fit parameters are set to the values determined
;             for the LMC2 field (including 30 Dor) by Misselt et al.
;             Note that neither /AVGLMC or /LMC2 will alter the default value 
;             of R_V which is poorly known for the LMC. 
;             
;      The following five input keyword parameters allow the user to customize
;      the adopted extinction curve.    For example, see Clayton et al. (2003,
;      ApJ, 588, 871) for examples of these parameters in different interstellar
;      environments.
;
;      x0 - Centroid of 2200 A bump in microns (default = 4.596)
;      gamma - Width of 2200 A bump in microns (default  =0.99)
;      c3 - Strength of the 2200 A bump (default = 3.23)
;      c4 - FUV curvature (default = 0.41)
;      c2 - Slope of the linear UV extinction component 
;           (default = -0.824 + 4.717/R)
;      c1 - Intercept of the linear UV extinction component 
;           (default = 2.030 - 3.007*c2
;            
; OPTIONAL OUTPUT KEYWORD:
;      ExtCurve - Returns the E(wave-V)/E(B-V) extinction curve, interpolated
;                 onto the input wavelength vector
;
; EXAMPLE:
;       Determine how a flat spectrum (in wavelength) between 1200 A and 3200 A
;       is altered by a reddening of E(B-V) = 0.1.   Assume an "average"
;       reddening for the diffuse interstellar medium (R(V) = 3.1)
;
;       IDL> w = 1200 + findgen(40)*50      ;Create a wavelength vector
;       IDL> f = w*0 + 1                    ;Create a "flat" flux vector
;       IDL> fm_unred, w, f, -0.1, fnew  ;Redden (negative E(B-V)) flux vector
;       IDL> plot,w,fnew                   
;
; NOTES:
;       (1) The following comparisons between the FM curve and that of Cardelli, 
;           Clayton, & Mathis (1989), (see ccm_unred.pro):
;           (a) - In the UV, the FM and CCM curves are similar for R < 4.0, but
;                 diverge for larger R
;           (b) - In the optical region, the FM more closely matches the
;                 monochromatic extinction, especially near the R band.
;       (2)  Many sightlines with peculiar ultraviolet interstellar extinction 
;               can be represented with the FM curve, if the proper value of 
;               R(V) is supplied.
;       (3) Use the 4 parameter calling sequence if you wish to save the 
;               original flux vector.
; PROCEDURE CALLS:
;       CSPLINE(), POLY()
; REVISION HISTORY:
;       Written   W. Landsman        Raytheon  STX   October, 1998
;       Based on FMRCurve by E. Fitzpatrick (Villanova)
;       Added /LMC2 and /AVGLMC keywords,  W. Landsman   August 2000
;       Added ExtCurve keyword, J. Wm. Parker   August 2000
;       Assume since V5.4 use COMPLEMENT to WHERE  W. Landsman April 2006
;       Fix calculation of EXTCurve A. Sarkisyan/W. Landsman  Jan 2014
;
;-
 On_error, 2
 compile_opt idl2

 if N_params() LT 3 then begin
     print,'Syntax: FM_UNRED, wave, flux, ebv, funred,[ R_V =, /LMC2, /AVGLMC '
     print,'                  gamma =, x0 =, c1 =, c2 = ,c3 = ,c4 =, ExtCurve=]'
     return
 endif

 if N_elements(R_V) EQ 0 then R_V = 3.1

 x = 10000./ wave                ; Convert to inverse microns 
 curve = x*0.

; Set default values of c1,c2,c3,c4,gamma and x0 parameters

 if keyword_set(LMC2) then  begin
         if N_elements(x0) EQ 0 then x0    =  4.626
         if N_elements(gamma) EQ 0 then gamma =  1.05	
         if N_elements(c4) EQ 0 then c4   =  0.42   
         if N_elements(c3) EQ 0 then c3    =  1.92	
         if N_elements(c2) EQ 0 then c2    = 1.31
         if N_elements(c1) EQ 0 then c1    =  -2.16
 endif else if keyword_set(AVGLMC) then begin
         if N_elements(x0) EQ 0 then x0 = 4.596  
         if N_elements(gamma) EQ 0 then gamma = 0.91
         if N_elements(c4) EQ 0 then c4   =  0.64  
         if N_elements(c3) EQ 0 then c3    =  2.73	
         if N_elements(c2) EQ 0 then c2    = 1.11
         if N_elements(c1) EQ 0 then c1    =  -1.28
  endif else begin
         if N_elements(x0) EQ 0 then x0    =  4.596  
         if N_elements(gamma) EQ 0 then gamma =  0.99	
         if N_elements(c3) EQ 0 then c3    =  3.23	
         if N_elements(c4) EQ 0 then c4   =  0.41    
         if N_elements(c2) EQ 0 then c2    = -0.824 + 4.717/R_V
         if N_elements(c1) EQ 0 then c1    =  2.030 - 3.007*c2
 endelse

; Compute UV portion of A(lambda)/E(B-V) curve using FM fitting function and 
; R-dependent coefficients
 
 xcutuv = 10000.0/2700.0
 xspluv = 10000.0/[2700.0,2600.0]
 iuv = where(x ge xcutuv, N_UV, complement = iopir, Ncomp = Nopir)
 IF (N_UV GT 0) THEN xuv = [xspluv,x[iuv]] ELSE  xuv = xspluv

    yuv = c1  + c2*xuv
    yuv = yuv + c3*xuv^2/((xuv^2-x0^2)^2 +(xuv*gamma)^2)
    yuv = yuv + c4*(0.5392*((xuv>5.9)-5.9)^2+0.05644*((xuv>5.9)-5.9)^3)
    yuv = yuv + R_V
    yspluv  = yuv[0:1]                  ; save spline points

 IF (N_UV GT 0) THEN curve[iuv] = yuv[2:*]      ; remove spline points
 
; Compute optical portion of A(lambda)/E(B-V) curve
; using cubic spline anchored in UV, optical, and IR

 xsplopir = [0,10000.0/[26500.0,12200.0,6000.0,5470.0,4670.0,4110.0]]
 ysplir   = [0.0,0.26469,0.82925]*R_V/3.1 
 ysplop   = [poly(R_V, [-4.22809e-01, 1.00270, 2.13572e-04] ), $
             poly(R_V, [-5.13540e-02, 1.00216, -7.35778e-05] ), $
             poly(R_V, [ 7.00127e-01, 1.00184, -3.32598e-05] ), $
             poly(R_V, [ 1.19456, 1.01707, -5.46959e-03, 7.97809e-04, $ 
                     -4.45636e-05] ) ]
  
 ysplopir = [ysplir,ysplop]

 if (Nopir GT 0) then $
          curve[iopir] = CSPLINE([xsplopir,xspluv],[ysplopir,yspluv],x[iopir])

 ; Now apply extinction correction to input flux vector

   curve = ebv*curve 
   if N_params() EQ 3 then flux = flux * 10.^(0.4*curve) else $
        funred = flux * 10.^(0.4*curve)       ;Derive unreddened flux

   ExtCurve = Curve/ebv - R_V     ;Updated Jan 2014

 end

pro ccm_UNRED, wave, flux, ebv, funred, R_V = r_v
;+
; NAME:
;     CCM_UNRED
; PURPOSE:
;     Deredden a flux vector using the CCM 1989 parameterization 
; EXPLANATION:
;     The reddening curve is that of Cardelli, Clayton, and Mathis (1989 ApJ.
;     345, 245), including the update for the near-UV given by O'Donnell 
;     (1994, ApJ, 422, 158).   Parameterization is valid from the IR to the 
;     far-UV (3.5 microns to 0.1 microns).    
;
;     Users might wish to consider using the alternate procedure FM_UNRED
;     which uses the extinction curve of Fitzpatrick (1999).
; CALLING SEQUENCE:
;     CCM_UNRED, wave, flux, ebv, funred, [ R_V = ]      
;             or 
;     CCM_UNRED, wave, flux, ebv, [ R_V = ]      
; INPUT:
;     WAVE - wavelength vector (Angstroms)
;     FLUX - calibrated flux vector, same number of elements as WAVE
;             If only 3 parameters are supplied, then this vector will
;             updated on output to contain the dereddened flux.
;     EBV  - color excess E(B-V), scalar.  If a negative EBV is supplied,
;             then fluxes will be reddened rather than deredenned.
;
; OUTPUT:
;     FUNRED - unreddened flux vector, same units and number of elements
;             as FLUX
;
; OPTIONAL INPUT KEYWORD
;     R_V - scalar specifying the ratio of total selective extinction
;             R(V) = A(V) / E(B - V).    If not specified, then R_V = 3.1
;             Extreme values of R(V) range from 2.75 to 5.3
;
; EXAMPLE:
;     Determine how a flat spectrum (in wavelength) between 1200 A and 3200 A
;     is altered by a reddening of E(B-V) = 0.1.   Assume an "average"
;     reddening for the diffuse interstellar medium (R(V) = 3.1)
;
;       IDL> w = 1200 + findgen(40)*50      ;Create a wavelength vector
;       IDL> f = w*0 + 1                    ;Create a "flat" flux vector
;       IDL> ccm_unred, w, f, -0.1, fnew  ;Redden (negative E(B-V)) flux vector
;       IDL> plot,w,fnew                   
;
; NOTES:
;     (1) The CCM curve shows good agreement with the Savage & Mathis (1979)
;             ultraviolet curve shortward of 1400 A, but is probably
;             preferable between 1200 and 1400 A.
;     (2)  Many sightlines with peculiar ultraviolet interstellar extinction 
;             can be represented with a CCM curve, if the proper value of 
;             R(V) is supplied.
;     (3)  Curve is extrapolated between 912 and 1000 A as suggested by
;             Longo et al. (1989, ApJ, 339,474)
;     (4) Use the 4 parameter calling sequence if you wish to save the 
;               original flux vector.
;     (5) Valencic et al. (2004, ApJ, 616, 912) revise the ultraviolet CCM
;             curve (3.3 -- 8.0 um-1).    But since their revised curve does
;             not connect smoothly with longer and shorter wavelengths, it is
;             not included here.
;
; REVISION HISTORY:
;       Written   W. Landsman        Hughes/STX   January, 1992
;       Extrapolate curve for wavelengths between 900 and 1000 A   Dec. 1993
;       Use updated coefficients for near-UV from O'Donnell   Feb 1994
;       Allow 3 parameter calling sequence      April 1998
;       Converted to IDLV5.0                    April 1998
;-

 On_error, 2

 if N_params() LT 3 then begin
     print,'Syntax: CCM_UNRED, wave, flux, ebv, funred,[ R_V = ]'
     return
 endif

 if not keyword_set(R_V) then R_V = 3.1

 x = 10000./ wave                ; Convert to inverse microns 
 npts = N_elements( x )
 a = fltarr(npts)  
 b = fltarr(npts)
;******************************

 good = where( (x GT 0.3) and (x  LT 1.1), Ngood )       ;Infrared
 if Ngood GT 0 then begin
      a[good] =  0.574 * x[good]^(1.61)
      b[good] = -0.527 * x[good]^(1.61)
 endif

;******************************

 good = where( (x GE 1.1) and (x LT 3.3) ,Ngood)           ;Optical/NIR
 if Ngood GT 0 then begin             ;Use new constants from O'Donnell (1994)
     y = x[good] - 1.82
;     c1 = [ 1. , 0.17699, -0.50447, -0.02427,  0.72085,    $ ;Original
;                 0.01979, -0.77530,  0.32999 ]               ;coefficients
;     c2 = [ 0.,  1.41338,  2.28305,  1.07233, -5.38434,    $ ;from CCM89
;                -0.62251,  5.30260, -2.09002 ]
      c1 = [ 1. , 0.104,   -0.609,    0.701,  1.137,    $    ;New coefficients
                 -1.718,   -0.827,    1.647, -0.505 ]        ;from O'Donnell
      c2 = [ 0.,  1.952,    2.908,   -3.989, -7.985,    $    ;(1994)
                 11.102,    5.491,  -10.805,  3.347 ]

     a[good] = poly( y, c1)
     b[good] = poly( y, c2)
 endif
;******************************

 good = where( (x GE 3.3) and (x LT 8) ,Ngood)           ;Mid-UV
 if Ngood GT 0 then begin

    y = x[good]
    F_a = fltarr(Ngood)    & F_b = fltarr(Ngood)
    good1 = where( (y GT 5.9), Ngood1 )
    if Ngood1 GT 0 then begin
       y1 = y[good1] - 5.9
       F_a[ good1] = -0.04473 * y1^2 - 0.009779 * y1^3
       F_b[ good1] =   0.2130 * y1^2  +  0.1207 * y1^3
    endif
    
   a[good] =  1.752 - 0.316*y - (0.104 / ( (y-4.67)^2 + 0.341 )) + F_a
   b[good] = -3.090 + 1.825*y + (1.206 / ( (y-4.62)^2 + 0.263 )) + F_b
 endif

;   *******************************

 good = where( (x GE 8) and (x LE 11), Ngood )         ;Far-UV
 if Ngood GT 0 then begin
    y = x[good] - 8.
    c1 = [ -1.073, -0.628,  0.137, -0.070 ]
    c2 = [ 13.670,  4.257, -0.420,  0.374 ]
    a[good] = poly(y, c1)
    b[good] = poly(y, c2)
 endif

;   *******************************

; Now apply extinction correction to input flux vector

  A_V = R_V * EBV
  A_lambda = A_V * (a + b/R_V)
  if N_params() EQ 3 then flux = flux * 10.^(0.4*A_lambda) else $
        funred = flux * 10.^(0.4*A_lambda)       ;Derive unreddened flux

 return     
 end                               

pro uvbybeta,xby,xm1,xc1,xHbeta,xn,Te,MV,eby,delm0,radius,TEXTOUT=textout, $
    eby_in = eby_in, name = name, prompt=prompt,print=print
;+
; NAME:
;       UVBYBETA
; PURPOSE:
;       Derive dereddened colors, metallicity, and Teff from Stromgren colors.
; EXPLANATION:
;       Adapted from FORTRAN routine of same name published by T.T. Moon, 
;       Communications of University of London Observatory, No. 78. Parameters 
;       can either be input interactively (with /PROMPT keyword) or supplied 
;       directly.   
;
; CALLING SEQUENCE:
;       uvbybeta, /PROMPT               ;Prompt for all parameters
;       uvbybeta,by,m1,c1,Hbeta,n        ;Supply inputs, print outputs
;       uvbybeta, by, m1, c1, Hbeta, n, Te, Mv, Eby, delm0, radius, 
;                       [ TEXTOUT=, Eby_in =, Name =  ]
;
; INPUTS:
;       by - Stromgren b-y color, scalar or vector
;       m1 - Stromgren line-blanketing parameter, scalar or vector
;       c1 - Stromgren Balmer discontinuity parameter, scalar or vector
;       Hbeta - H-beta line strength index.  Set  Hbeta to 0 if it is not 
;            known, and UVBYBETA will estimate a value based on by, m1,and c1.
;            Hbeta is not used for stars in group 8.
;       n -  Integer (1-8), scalar or vector,  giving approximate stellar 
;            classification
;
;       (1) B0 - A0, classes III - V, 2.59 < Hbeta < 2.88,-0.20 <   c0  < 1.00
;       (2) B0 - A0, class   Ia     , 2.52 < Hbeta < 2.59,-0.15 <   c0  < 0.40
;       (3) B0 - A0, class   Ib     , 2.56 < Hbeta < 2.61,-0.10 <   c0  < 0.50
;       (4) B0 - A0, class   II     , 2.58 < Hbeta < 2.63,-0.10 <   c0  < 0.10
;       (5) A0 - A3, classes III - V, 2.87 < Hbeta < 2.93,-0.01 < (b-y)o< 0.06
;       (6) A3 - F0, classes III - V, 2.72 < Hbeta < 2.88, 0.05 < (b-y)o< 0.22
;       (7) F1 - G2, classes III - V, 2.60 < Hbeta < 2.72, 0.22 < (b-y)o< 0.39
;       (8) G2 - M2, classes  IV _ V, 0.20 < m0   < 0.76, 0.39 < (b-y)o< 1.00
;
;
; OPTIONAL INPUT KEYWORD:
;       Eby_in - numeric scalar specifying E(b-y) color to use.   If not
;             supplied, then E(b-y) will be estimated from the Stromgren colors
;       NAME - scalar or vector string giving name(s) of star(s).  Used only 
;               when writing to  disk for identification purposes.
;       /PROMPT - if set, then uvbybeta.pro will prompt for Stromgren indicies
;                interactively
;       TEXTOUT  -  Used to determine output device.  If not present, the
;               value of the !TEXTOUT system variable is used (see TEXTOPEN)
;               textout=1       Terminal with /MORE (if a tty)
;               textout=2       Terminal without /MORE
;               textout=3       uvbybeta.prt   (output file)
;               textout=4       Laser Printer 
;               textout=5       User must open file         
;               textout=7       Append to existing uvbybeta.prt file
;               textout = filename (default extension of .prt)
;      /PRINT - if set, then force display output information to the device 
;               specified by !TEXTOUT.    By default, UVBYBETA does not display
;               information if output variables are supplied (and TEXTOUT is
;               not set). 
;
; OPTIONAL OUTPUTS:
;       Te - approximate effective temperature
;       MV - absolute visible magnitude
;       Eby - Color excess E(b-y)
;       delm0 - metallicity index, delta m0, (may not be calculable for early
;               B stars).
;       radius - Stellar radius (R/R(solar))
; EXAMPLE:
;       Suppose 5 stars have the following Stromgren parameters
;
;       by = [-0.001 ,0.403, 0.244, 0.216, 0.394 ]
;       m1 = [0.105, -0.074, -0.053, 0.167, 0.186 ]
;       c1 = [0.647, 0.215, 0.051, 0.785, 0.362] 
;       hbeta = [2.75, 2.552, 2.568, 2.743, 0 ]
;       nn = [1,2,3,7,8]              ;Processing group number
;
;       Determine stellar parameters and write to a file uvbybeta.prt
;       IDL> uvbybeta, by,m1,c1,hbeta, nn, t=3
;            ==> E(b-y) = 0.050    0.414   0.283  0.023  -0.025
;                Teff =   13060    14030   18420  7250    5760
;                M_V =    -0.27    -6.91   -5.94  2.23    3.94
;                radius=  2.71     73.51    39.84 2.02    1.53
; SYSTEM VARIABLES:
;       The non-standard system variables !TEXTOUT and !TEXTUNIT will be  
;       automatically defined if they are not already present.   
;
;       DEFSYSV,'!TEXTOUT',1
;       DEFSYSV,'!TEXTUNIT',0
;
; NOTES:
;       (1) **This procedure underwent a major revision in January 2002
;       and the new calling sequence may not be compatible with the old** (NAME
;       is now a keyword rather than a parameter.)
;
;       (2) Napiwotzki et al. (1993, A&A, 268, 653) have written a FORTRAN
;           program that updates some of the Moon (1985) calibrations.  These
;           updates are *not* included in this IDL procedure.
; PROCEDURES USED:
;       DEREDD, TEXTOPEN, TEXTCLOSE
; REVISION HISTORY:                                           
;       W. Landsman          IDL coding              February, 1988
;       Keyword textout added, J. Isensee, July, 1990
;       Made some constants floating point.   W. Landsman    April, 1994
;       Converted to IDL V5.0   W. Landsman   September 1997
;       Added Eby_in, /PROMPT keywords, make NAME a keyword and not a parameter
;                 W. Landsman      January 2002
;-
 npar = N_params()
 if (npar EQ 0) and ( not keyword_set(PROMPT)) then begin
     print,'Syntax - UVBYBETA, by, m1, c1, beta, n,     ;Input parameters'
     print,'                   Te,MV,eby,delm0,radius   ;Output parameters'
     print,'Input Keywords: Eby_in=, /PROMPT, NAME=, TEXTOUT ='
     return
 endif

 defsysv,'!textout',exists = i
 if i EQ 0 then astrolib

 if N_elements( TEXTOUT ) EQ 0 then textout = !TEXTOUT  ;default output dev.
 do_print =  (npar LT 6) || (TEXTOUT GT 2) || keyword_set(PRINT)

 Rm1 = -0.33 & Rc1 = 0.19 & Rub = 1.53          ;Parameter values
 init = 0

 READ_PAR:  if keyword_set(PROMPT) then begin 
  ans = ''
  print,'Enter (b-y), m1, c1, and Hbeta in that order ([RETURN] to exit)'
  read,ans
  if ans eq '' then begin               ;Normal Exit
    if ( init EQ 1 ) then textclose, TEXTOUT = textout
    return 
  endif else ans = getopt(ans)
  if ( N_elements(ans) NE 4 ) then begin
    message, 'INPUT ERROR - Expecting 4 scalar values', /CON
    print, 'Enter 0.0 for Hbeta if it is not known: '
    goto, READ_PAR 
  endif else begin
    xby = ans[0] & xm1 = ans[1] & xc1 = ans[2]  & xhbeta = ans[3]
    endelse
 endif 
    
 nstar  = N_elements(xby)
 xub = xc1 + 2*(xm1+xby)
 xflag1 = (xHbeta EQ 0.)

 
 READ_GROUP:  if ( npar LT 5 )then begin

   print,' The following group of stars are available'
   print, $ 
     '(1) B0 - A0, classes III - V, 2.59 < Hbeta < 2.88,-0.20 <   c0  < 1.00'
   print, $
     '(2) B0 - A0, class   Ia     , 2.52 < Hbeta < 2.59,-0.15 <   c0  < 0.40'
   print, $
     '(3) B0 - A0, class   Ib     , 2.56 < Hbeta < 2.61,-0.10 <   c0  < 0.50'
   print, $ 
     '(4) B0 - A0, class   II     , 2.58 < Hbeta < 2.63,-0.10 <   c0  < 0.10'
   print, $ 
     '(5) A0 - A3, classes III - V, 2.87 < Hbeta < 2.93,-0.01 < (b-y)o< 0.06'
   print, $
     '(6) A3 - F0, classes III - V, 2.72 < Hbeta < 2.88, 0.05 < (b-y)o< 0.22'
   print,$ 
     '(7) F1 - G2, classes III - V, 2.60 < Hbeta < 2.72, 0.22 < (b-y)o< 0.39'
   print, $ 
     '(8) G2 - M2, classes  IV _ V, 0.20 < m0   < 0.76, 0.39 < (b-y)o< 1.00'
   xn = 0                   
   read,'Enter group number to which star belongs: ',xn

   if N_elements(name) Eq 0 then begin
   if (TEXTOUT ne 1) and (npar lt 6) then begin ;Prompt for star name?
     name = ''
     read,'Enter name of star: ',name
   endif
   endif
 endif

 do_eby = N_elements(eby_in) EQ 0
 te = fltarr(nstar) & MV = te & delm0 = te & radius = te
 if N_elements(name) EQ 0 then name = strtrim( indgen(nstar)+1,2)
 if not do_eby then eby = replicate(eby_in,nstar) else eby = te

 for i=0,Nstar -1 do begin
   by = xby[i] & m1 = xm1[i] & c1 = xc1[i] & hbeta = xhbeta[i] & n = fix(xn[i])
   ub = xub[i] & flag1 = xflag1[i] 
   flag2 = 0
   warn = ''

 case n of

 1: BEGIN

;   For group 1, beta is a luminosity indicator and c0 is a temperature
;   indicator. (u-b) is also a suitable temperature indicator.

;   For dereddening a linear relation between the intrinsic (b-y)
;   and (u-b) colors is used (Crawford 1978, AJ 83, 48)

    if do_eby then Eby[i] = ( 13.608*by-ub+1.467 ) / (13.608-Rub)
    DEREDD, Eby[i], by, m1, c1, ub, by0, m0, c0, ub0

; If beta is not given it is estimated using a cubic fit to the
; c0-beta relation for luminosity class V given in Crawford (1978).
    IF flag1 EQ 1 then Hbeta = $
                   poly(c0, [2.61033, 0.132557, 0.161463, -0.027352] )
; Calculation of the absolute magnitude by applying the calibration
; of Balona & Shobbrock (1974, MNRAS 211, 375)   
   g = ALOG10(Hbeta - 2.515) - 1.6*ALOG10(c0 +0.322)
   MV[i] = 3.4994 + 7.2026*ALOG10(Hbeta - 2.515) -2.3192*g + 2.9375*g^3
   Te[i] = 5040/(0.2917*c0 + 0.2)  

; The ZAMS value of m0 is calculated from a fit to the data of 
; Crawford (1978), modified by Hilditch, Hill & Barnes (1983, 
; MNRAS 204, 241)
   m0zams = poly(c0, [0.07473, 0.109804, -0.139003, 0.0957758] )
   delm0[i] = m0zams - m0
   flag2 = 1
   END

 2: BEGIN
    if do_eby then begin
; For dereddening the linear relations between c0 and (u-b)
; determined from Zhang (1983, AJ 88, 825) is used.
       Eub = ( 1.5*c1 - ub + 0.035) / (1.5/(Rub/Rc1)-1)
       Eby[i] = Eub/Rub
    endif
    DEREDD, Eby[i], by, m1, c1, ub, by0, m0, c0, ub0
    if ( flag1 EQ 1 ) then Hbeta = 0.037*c0 + 2.542
    END

 3: BEGIN
; For dereddening the linear relations between c0 and (u-b)
; determined from Zhang (1983, AJ 88, 825) is used.
    if do_Eby then begin
       Eub = (1.36*c1-ub+0.004) / (1.36/(Rub/Rc1)-1)
       Eby[i] = Eub/Rub
    endif
    DEREDD, Eby[i], by, m1, c1, ub, by0, m0, c0, ub0
; If beta is not given it is derived from a fit of the c0-beta
; relation of Zhang (1983).
    if flag1 then Hbeta = 0.047*c0 +2.578
    END

 4: BEGIN
; For dereddening the linear relations between c0 and (u-b)
; determined from Zhang (1983, AJ 88, 825) is used.
    if do_Eby then begin
       Eub = ( 1.32*c1 - ub - 0.056) / ( 1.32 / (Rub/Rc1)-1 )
       Eby[i] = Eub/Rub
    endif
    DEREDD, Eby[i], by, m1, c1, ub, by0, m0, c0, ub0
; If beta is not given it is derived from a fit of the c0-beta
; relation of Zhang (1983).
    if ( flag1 EQ 1 ) then Hbeta = 0.066*c0+2.59
    END

 5: BEGIN
; For group 5, the hydrogen Balmer lines are at maximum; hence two
; new parameters, a0 = f{(b-y),(u-b)} and r = f{beta,[c1]} are defined
; in order to calculate absolute magnitude and metallicity.

    if do_eby then begin
      m = m1 - Rm1*by
      by0 = 4.2608*m^2 - 0.53921*m - 0.0235
      REPEAT BEGIN
         bycorr = by0
         m0 = m1 - Rm1*(by-bycorr)
         by0 = 14.0881*m0^2 - 3.36225*m0 + 0.175709
      ENDREP UNTIL ( abs(bycorr - by0) LT 0.001)
       Eby[i] = by - by0
    endif
    DEREDD, Eby[i], by, m1, c1, ub, by0, m0, c0, ub0
    if flag1 eq 1 then Hbeta = 2.7905 - 0.6105*by + 0.5*m0 + 0.0355*c0
    r = 0.35*(c1-Rc1*by) - (Hbeta-2.565)
    a0 = by0+ 0.18*(ub0-1.36)
; MV is calculated according to Stroemgren (1966, ARA&A 4, 433)
; with corrections by Moon & Dworetsky (1984, Observatory 104, 273)
    MV[i] = 1.5 + 6.0*a0 - 17.0*r
    Te[i] =  5040. /(0.7536 *a0 +0.5282)
    m0zams = -3.95105*by0^2 + 0.86888*by0 + 0.1598
    delm0[i] = m0zams - m0
   end

 6: begin
    if flag1 then begin
        warn = ' Estimate of Hbeta only valid if star is unreddened'
        Hbeta = 3.06 - 1.221*by - 0.104*c1
    endif
    m1zams = -2.158*Hbeta^2 +12.26*Hbeta-17.209
    if ( Hbeta LE 2.74 ) then begin

        c1zams = 3.0*Hbeta - 7.56
        MVzams = 22.14 - 7*Hbeta

   endif else if ( ( Hbeta GT 2.74 ) and ( Hbeta LE 2.82 ) ) then begin

        c1zams = 2.0*Hbeta - 4.82
        MVzams = 11.16-3*Hbeta

   endif else begin
        c1zams = 2.0*Hbeta-4.83
        MVzams =-88.4*Hbeta^2+497.2*Hbeta-696.41

   endelse        
   if do_eby then begin
     delm1 = m1zams - m1
     delc1 = c1-c1zams
     if delm1 lt 0. then $
          by0 = 2.946 - Hbeta - 0.1*delc1 - 0.25*delm1 else $
          by0 = 2.946 - Hbeta - 0.1*delc1
     Eby[i] = by - by0
   endif 
   Deredd, eby[i], by, m1, c1, ub, by0, m0, c0, ub0
   delm0[i] = m1zams - m0
   delc0 = c0 - c1zams
   MV[i] = MVzams -9.0*delc0
   Te[i] = 5040 / (0.771453*by0 + 0.546544)
 end

 7: begin

;  For group 7 c1 is the luminosity indicator for a particular beta,
;  while beta {or (b-y)0} indicates temperature.
;  Where beta is not available iteration is necessary to evaluate
;  a corrected (b-y) from which beta is then estimated.

   if flag1 then begin 
        byinit = by
        m1init = m1
        for ii = 1,1000 do begin
          m1by = 2.5*byinit^2 - 1.32*byinit + 0.345
          bycorr = byinit + (m1by-m1init) / 2.0
          if ( abs(bycorr-byinit) LE 0.0001 ) then goto,T71
          byinit = bycorr
          m1init = m1by
        endfor
        T71: Hbeta = 1.01425*bycorr^2 - 1.32861*bycorr + 2.96618 
    endif

; m1(ZAMS) and MV(ZAMS) are calculated according to Crawford (1975)
;	with corrections suggested by Hilditch, Hill & Barnes (1983,
;	MNRAS 204, 241) and Olson (1984, A&AS 57, 443).

    m1zams = poly(Hbeta, [ 46.4167, -34.4538, 6.41701] )
    MVzams = poly(Hbeta, [324.482, -188.748, 11.0494, 5.48012])

;c1(ZAMS) calculated according to Crawford (1975)
    if Hbeta le 2.65 then $
        c1zams = 2*Hbeta - 4.91 else $
        c1zams = 11.1555*Hbeta^2-56.9164*Hbeta+72.879

     if do_eby then begin
       delm1 = m1zams - m1
       delc1 = c1 - c1zams
       dbeta = 2.72 - Hbeta
       by0 = 0.222+1.11*dbeta +2.7*dbeta^2-0.05*delc1-(0.1+3.6*dbeta)*delm1
       Eby[i] = by - by0
     endif
     Deredd,Eby[i],by,m1,c1,ub,by0,m0,c0,ub0
     delm0[i] = m1zams - m0
     delc0 = c0 - c1zams
     f = 9.0 + 20.0*dbeta
     MV[i] = MVzams - f*delc0
     Te[i] = 5040/(0.771453*by0 + 0.546544)
 end

 8:   begin
     if ( flag1 EQ 1 ) then flag1 = 2
;  Dereddening is done using color-color relations derived from 
;  Olson 1984, A&AS 57, 443)
     if ( by LE 0.65 ) then $
           Eby[i] = (5.8651*by - ub -0.8975) / (5.8651 - Rub) $

     else if ( ( by GT 0.65 ) and ( by LT 0.79 ) ) then begin 

           Eby[i] = (-0.7875*by - c1 +0.6585)/(-0.7875 - Rc1)
           by0 = by - Eby[i]
        if ( by0 LT 0.65 ) then $
           Eby[i] = (5.8651*by - ub -0.8975) / (5.8651-Rub)

     endif else begin 

        Eby[i] = ( 0.5126*by - c1 - 0.3645 ) / (0.5126-Rc1)
        by0 = by - Eby[i]
        if ( by0 LT 0.79 ) then $ 
                  Eby[i] = (-0.7875*by - c1 + 0.6585) / (-0.7875-Rc1)
        by0  = by - Eby[i]
        if ( by0 LT 0.65 ) then $ 
                  Eby[i] = ( 5.8651*by - ub - 0.8975) / (5.8651-Rub)

     endelse 
    

        DEREDD,Eby[i],by,m1,c1,ub,by0,m0,c0,ub0
; m1(ZAMS), c1(ZAMS), and MV(ZAMS) are calculated according to Olson (1984)
        m1zams = poly( by0, [7.18436, -49.43695, 122.1875, -122.466, 42.93678]) 
         IF by0 lt 0.65 THEN BEGIN
                c1zams = poly(by0, [3.78514, -21.278, 42.7486, -28.7056 ] )
                MVzams =  $
                  poly(by0, [-59.2095, 432.156, -1101.257, 1272.503, -552.48])
        ENDIF ELSE IF (by0 GE 0.65) and (by0 lt 0.79) THEN BEGIN
                c1zams = -0.631821*by0^2+0.116031*by0+0.33657
                MVzams = 1.37632*by0^2 + 4.97911*by0+3.4305
        ENDIF ELSE BEGIN
                c1zams = -0.010028*by0^2 + 0.530426*by0 - 0.37237
                MVzams =  1.18298*by0^2  + 3.92776*by0 + 4.37507
        ENDELSE
        delm0[i] = m1zams - m0
        delc0 =c0 - c1zams
; Teff and MV calibration of Olson (1984)
        IF (by0 LE 0.505) THEN BEGIN
                f = 10. - 80.*(by0-0.38)
                Te[i] = 10^(-0.416*by0+3.924)
        ENDIF ELSE BEGIN
                f = 0.0
                Te[i] = 10^(-0.341*by0+3.869)
        ENDELSE
        MV[i] = MVzams - f*delc0 + 3.2*delm0[i] - 0.07
      END 
 ELSE: BEGIN
      print,'A stellar group of',n,' is not available'
      npar = npar<4
      goto, READ_GROUP 
      end

 endcase
 if (n GE 2) and ( n LE 4 ) then begin
; c0-beta relation for ZAMS stars according to Crawford (1978,
; AJ 83, 48), modified by Hilditch, Hill & Barnes (1983, MNRAS 204, 241).
     betaza = poly(c0, [2.62745, 0.228638, -0.099623, 0.277363, -0.160402 ] )
     B = betaza - 2.5
; MV(ZAMS) calculated according to Balona & Shobbrock (1984, MNRAS 211, 375)
     MVzams =203.704*B^3 - 206.98*B^2 + 77.18*b - 9.563
; MV is calculated from the d(beta)-d(MV) relation of Zhang (1983)
     dbeta = betaza - Hbeta
     dMV = -121.6*dbeta^2 +61.0*dbeta + 0.08
     MV[i] = MVzams - dMV
; Estimate of Teff by coupling the relations of Boehm-Vitense 
; (1981, ARA&A 19, 295) and Zhang (1983)     
     Te[i] = 5040 / (0.35866*ub0 + 0.27346)
     flag2 = 2
endif

; Transformation according to the FV-(b-y)0 relation of Moon 
; (1984, MNRAS 211, 21P)
 if ( by0 LE 0.335 ) then $
            FV = -6.759*by0^3 + 3.731*by0^2 - 1.092*by0 + 3.981 $
       else FV = -0.534*by0 + 3.959
 radius[i] = 10^(2.*(4.236-0.1*MV[i] - FV))
  if do_print then begin
 if ( flag2 EQ 2 )then metal = 'no delta(m0)' else metal = 'delta(m0) = '
 Hbeta = round(Hbeta*1000)/1000.
 Teff = long(round(Te[i]/10.)*10.)
 if !TEXTUNIT eq 0 then textopen,'uvbybeta',textout=textout
 init = 1                          ;First star has been done
 printf,!TEXTUNIT,'        Star is: ',strtrim(name[i],2), $
        '                Processed in group ' + strtrim(n,2) 
 fmt = '(2x,A, f6.3,7x, A, f6.3, 10x,A, F6.3,A,F5.3)'
 if strlen(warn) GT 0 then printf, !TEXTUNIT, warn
 nohbeta = '      Hbeta is not used'

 case flag1 of 
    2: printf, !TEXTUNIT, 'b-y   = ',by, 'm1 = ', m1,'c1 = ',c1, f=fmt, $
               nohbeta
    1: printf, !TEXTUNIT, f = fmt, $
       'b-y   = ',by, 'm1 = ', m1,'c1 = ',c1,' estimated Hbeta  = ', Hbeta 
    0: printf,!TEXTUNIT, f = fmt, $
       'b-y   = ',by, 'm1 = ', m1,'c1 = ',c1,'         Hbeta  = ', Hbeta 
 endcase

 fmt = '(1x,A, F6.3,7X, A,F6.3,10X,A,F6.3, 8x, A, F6.3,/)' 
 printf,!TEXTUNIT,f=fmt, '(b-y)0 = ', by0, 'm0 = ',m0,'c0 = ', c0, $
         'E(b-y) = ',Eby[i]

 printf,!TEXTUNIT,form="(1X,'Absolute Magnitude (Mv) = ',F6.2,5x," + $
       "'Radius  (R/R[solar]) = ',F7.2)",MV[i],radius[i]

 fmt1 = "(1X,A12,25X,'Effective Temperature (Teff) = ',I5,1X,'K'//)"
 fmt2 = "(1X,A12,F6.3,20X,'Effective Temperature (Teff) = ',I5,1X,'K'//)"

 if ( flag2 EQ 2 ) then printf,!TEXTUNIT,form=fmt1,metal,Teff else  $
                       printf,!TEXTUNIT,form=fmt2,metal,delm0[i],Teff

 endif
 endfor
 if keyword_set(PROMPT) then goto, READ_PAR 
 if do_print then textclose, textout = textout
 return
 end

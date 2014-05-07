function ismeuv,wave,Hcol,HeIcol,HeIIcol,Fano=fano
;+
; NAME:
;       ISMEUV
; PURPOSE:
;       Compute the continuum interstellar EUV optical depth 
;
; EXPLANATION:
;       The EUV optical depth is computed from the photoionization of
;       hydrogen and helium.
;
; CALLING SEQUENCE:
;       tau = ISMEUV( wave, Hcol, [ HeIcol, HeIIcol, /Fano ]
;
; INPUTS:
;       wave - Vector of wavelength values (in Angstroms).   Useful range is
;               40 - 912 A; at shorter wavelengths metal opacity should be
;               considered, at longer wavelengths there is no photoionization.
;       Hcol - Scalar specifying interstellar hydrogen column density in cm-2.
;                 Typical values are 1E17 to 1E20.
;
; OUTPUT:
;       tau - Vector giving resulting optical depth, same number of elements 
;               as wave, non-negative values.   To obtain the attenuation of 
;               an input spectrum, multiply by exp(-tau).
;
; OPTIONAL INPUTS:
;       HeIcol - Scalar specifying neutral helium column density in cm-2.    
;               Default is 0.1*Hcol (10% of hydrogen column)
;       HeIIcol - Scalar specifying ionized helium column density in cm-2
;               Default is 0 (no HeII)
;
; OPTIONAL INPUT KEYWORDS:
;       /FANO - If this keyword is set and non-zero, then the 4 strongest 
;               auto-ionizing resonances of He I are included.   The shape 
;               of these resonances is given by a Fano profile - see Rumph, 
;               Bowyer, & Vennes 1994, AJ, 107, 2108.  If these resonances are
;               included then the input wavelength vector should have
;               a fine (>~0.01 A) grid between 190 A and 210 A, since the
;               resonances are very narrow.
; EXAMPLE:
;       (1) One has a model EUV spectrum with wavelength, w (in Angstroms) and 
;       flux,f .  Plot the model flux after attenuation by 1e18 cm-2 of HI, 
;       with N(HeI)/N(HI) = N(HeII)/N(HI) = 0.05
;
;       IDL> Hcol = 1e18
;       IDL> plot, w, f*exp(-ismeuv(w, Hcol, .05*Hcol, .05*Hcol))
;
;       (2)  Plot the cross-section of HeI from 180 A to 220 A for 1e18 cm-2
;               of HeI, showing the auto-ionizing resonances.   This is 
;               Figure 1 in Rumph et al. (1994)
;
;       IDL> w = 180 + findgen(40000)*0.001        ;Need a fine wavelength grid
;       IDL> plot, w, ismeuv(w, 0, 1e18, /Fano)          
;
; NOTES:
;       (1) The more complete program  ismtau.pro at 
;           http://hea-www.harvard.edu/PINTofALE/pro/ extends this work
;           to shorter wavelengths and includes metal and molecular hydrogen
;           opacities
;       (2) This program only compute continuum opacities, and for example,
;           the He ionization edges at 504 A  and 228 A are blurred by
;           converging line absorptions (Dupuis et al. 1995. ApJ, 455, 574)
;           
; HISTORY:
;       Written,    W. Landsman                  October, 1994
;       Adapted from ism.c at anonymous ftp site cea-ftp.cea.berkeley.edu
;       by Pat Jelinsky, Todd Rumph & others.
;       Avoid underflow messages, support double prec.  W. Landsman October 2003
;       Fix error in He II optical Depth  J. Slavin/WL   Sep 2013
;-
 On_error,2

 if N_params() LT 2 then begin
        print,'Syntax - tau = ISMEUV( wave, Hcol, [ HeIcol, HeIIcol, /FANO] )'
        return,-1
 endif

 if N_elements( HeIcol) EQ 0 then HeIcol = 0.1*Hcol
 if N_elements( HeIIcol) EQ 0 then HeIIcol = 0.0*Hcol

; Compute attenuation due to photoionization of hydrogen.   See Spitzer
; (Physical processes in the interstellar medium), page 105

 if (size(wave,/TNAME) EQ 'DOUBLE') then begin 
          pi = !dpi 
          double  = 1b
  endif else  begin 
          pi = !pi
          double = 0b
 endelse
 ratio = wave/911.75
 tauh = wave*0.
 good = where(ratio LT 1, Ngood)
 minexp = alog((machar(double=double)).xmin) ;Min exponent to avoid underflow
 if Ngood GT 0 then begin
        r = ratio[good]
        z = sqrt( r/(1.0-r) )
        denom = replicate(1.0, Ngood)
        y = -2.*pi*z
        good1 = where(y GT minexp, Ngood1)
             if Ngood1 GT 0 then denom[good1] = (1.0 - exp(y[good1]))        
        tauh[good] = Hcol * 3.44e-16 * (r^4)*exp(-4.0*z*atan(1/z)) /  denom
  endif

; Now compute photoionization cross-section of He II; just like hydrogen but 
; with a nuclear charge Z = 2

 tauheII = wave*0.
 ratio = 4. * wave/911.75
 good = where(ratio LT 1, Ngood)
 if Ngood GT 0 then begin
        r = ratio[good]
        z = sqrt( r/(1.0-r) )
        denom = replicate(4.0, Ngood)    ;Z^2  Bug fix Sep 13
        y = -2*PI*z
        good1 = where(y GT minexp, Ngood1)
           if Ngood1 GT 0 then denom[good1] *= (1.0 - exp(y[good1]))
        tauheII[good] = heiicol * 3.44e-16 * (r^4)*exp(-4.0*z*atan(1/z)) / denom
            
 endif

; Polynomial coefficients for He I cross-section taken from experimental
; data by Marr & West (1976)
; c1 for wavelengths greater than 46 A

 c1 = [-2.953607d+01, 7.083061d+00, 8.678646d-01,-1.221932d+00,  $
       4.052997d-02, 1.317109d-01, -3.265795d-02, 2.500933d-03 ]

; c2 for wavelengths less than 46 A.

 c2 = [ -2.465188d+01, 4.354679d+00, -3.553024d+00, 5.573040d+00, $
       -5.872938d+00, 3.720797d+00, -1.226919d+00, 1.576657d-01 ]

; parameters of autoionization resonances for 4 strongest He I resonances
; Numbers are from Oza (1986), Phys Rev. A, 33, 824 -- nu and gamma
; and Fernley et al., J. Phys. B., 20, 6457, 1987 -- q

        q  = [2.81d, 2.51d, 2.45d, 2.44d ]
        nu = [1.610d, 2.795d, 3.817d, 4.824d ]
        fano_gamma = [2.64061d-03, 6.20116d-04, 2.56061d-04, 1.320159d-04 ]
        esubi = 3.0d - 1.0d/nu^2 + 1.807317d

 tauHeI = wave*0.
 good = where( wave LT 503.97, Ngood )
 if Ngood GT 0 then begin

        x = alog10(wave[good])
        y = x*0.

        good1 = where(wave LT 46.0, Ngood1 )
        if Ngood1 GT 0 then y[good1] = poly( x[good1], c2)      

        good2 = where(wave GE 46.0, Ngood2 )
        if Ngood2 GT 0 then begin 

                y[good2] = poly( x[good2], c1)

        if keyword_set(fano) then begin
                epsilon = 911.2671/wave
                for i=0,3 do begin       ;Loop over first four HeI resonances
                        x = 2.0 * ((epsilon-esubi[i] )/ fano_gamma[i] ) 
                        y = y + alog10( (x - q[i])^2/ (1 + x*x ) )
                endfor
        endif
        endif

  tauHeI[good] = HeIcol * 10^y

 endif

; Total optical depth from HI, HeII and HeI

 return, tauH + tauHeII + tauHeI

 end

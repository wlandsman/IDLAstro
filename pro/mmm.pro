pro mmm, sky_vector, skymod, sigma , skew, HIGHBAD = highbad, DEBUG = debug, $
           ReadNoise = readnoise, Nsky = nsky, INTEGER = discrete, $
	   MAXITER = mxiter, SILENT = silent, MINSKY = minsky
;+
; NAME:
;       MMM
; PURPOSE: 
;       Estimate the sky background in a stellar contaminated field.
; EXPLANATION:  
;       MMM assumes that contaminated sky pixel values overwhelmingly display 
;       POSITIVE departures from the true value.  Adapted from DAOPHOT 
;       routine of the same name.
;
; CALLING SEQUENCE:
;       MMM, sky, [ skymod, sigma, skew, HIGHBAD = , READNOISE=, /DEBUG, 
;                  MINSKY=, NSKY=, /INTEGER,/SILENT]
;
; INPUTS:
;       SKY - Array or Vector containing sky values.  This version of
;               MMM does not require SKY to be sorted beforehand.  SKY
;               is unaltered by this program.
;
; OPTIONAL OUTPUTS:
;       skymod - Scalar giving estimated mode of the sky values (float)
;       SIGMA -  Scalar giving standard deviation of the peak in the sky
;               histogram.  If for some reason it is impossible to derive
;               skymod, then SIGMA = -1.0
;       SKEW -   Scalar giving skewness of the peak in the sky histogram
;
;               If no output variables are supplied or if /DEBUG is set
;               then the values of skymod, SIGMA and SKEW will be printed.
;
; OPTIONAL KEYWORD INPUTS:
;       HIGHBAD - scalar value of the (lowest) "bad" pixel level (e.g. cosmic 
;                rays or saturated pixels) If not supplied, then there is 
;                assumed to be no high bad pixels.
;       MINSKY - Integer giving mininum number of sky values to be used.   MMM
;                will return an error if fewer sky elements are supplied.
;                Default = 20.
;       MAXITER - integer giving maximum number of iterations allowed,default=50
;       READNOISE - Scalar giving the read noise (or minimum noise for any 
;                pixel).     Normally, MMM determines the (robust) median by 
;                averaging the central 20% of the sky values.     In some cases
;                where the noise is low, and pixel values are quantized a
;                larger fraction may be needed.    By supplying the optional
;                read noise parameter, MMM is better able to adjust the
;                fraction of pixels used to determine the median.                
;       /INTEGER - Set this keyword if the  input SKY vector only contains
;                discrete integer values.    This keyword is only needed if the
;                SKY vector is of type float or double precision, but contains 
;                only discrete integer values.     (Prior to July 2004, the
;                equivalent of /INTEGER was set for all data types)
;       /DEBUG - If this keyword is set and non-zero, then additional 
;               information is displayed at the terminal.
;       /SILENT - If set, then error messages will be suppressed when MMM
;                cannot compute a background.    Sigma will still be set to -1
; OPTIONAL OUTPUT KEYWORD:
;      NSKY - Integer scalar giving the number of pixels actually used for the
;             sky computation (after outliers have been removed).
; NOTES:
;       (1) Program assumes that low "bad" pixels (e.g. bad CCD columns) have
;       already been deleted from the SKY vector.
;       (2) MMM was updated in June 2004 to better match more recent versions
;       of DAOPHOT.
;       (3) Does not work well in the limit of low Poisson integer counts
;       (4) MMM may fail for strongly skewed distributions.
; METHOD:
;       The algorithm used by MMM consists of roughly two parts:
;       (1) The average and sigma of the sky pixels is computed.   These values
;       are used to eliminate outliers, i.e. values with a low probability
;       given a Gaussian with specified average and sigma.   The average
;       and sigma are then recomputed and the process repeated up to 20
;       iterations:
;       (2) The amount of contamination by stars is estimated by comparing the 
;       mean and median of the remaining sky pixels.   If the mean is larger
;       than the median then the true sky value is estimated by
;       3*median - 2*mean
;         
; REVISION HISTORY:
;       Adapted to IDL from 1986 version of DAOPHOT in STSDAS, 
;       W. Landsman, STX Feb 1987
;       Added HIGHBAD keyword, W. Landsman January, 1991
;       Fixed occasional problem with integer inputs    W. Landsman  Feb, 1994
;       Avoid possible 16 bit integer overflow   W. Landsman  November 2001
;       Added READNOISE, NSKY keywords,  new median computation   
;                          W. Landsman   June 2004
;       Added INTEGER keyword W. Landsman July 2004
;       Improve numerical precision  W. Landsman  October 2004
;       Fewer aborts on strange input sky histograms W. Landsman October 2005
;       Added /SILENT keyword  November 2005
;       Fix too many /CON keywords to MESSAGE  W.L. December 2005
;       Fix bug introduced June 2004 removing outliers when READNOISE not set
;         N. Cunningham/W. Landsman  January 2006
;       Make sure that MESSAGE never aborts  W. Landsman   January 2008
;       Add mxiter keyword and change default to 50  W. Landsman August 2011
;       Added MINSKY keyword W.L. December 2011
;       Always return floating point sky mode  W.L.  December 2015
;-
 compile_opt idl2
 On_error,2               ;Return to caller
 if N_params() EQ 0 then begin          
        print,'Syntax:  MMM, sky, skymod, sigma, skew, [/INTEGER, /SILENT' 
        print,'              [HIGHBAD = , READNOISE =, /DEBUG, MXITER=, NSKY=] '
        return
 endif

 silent = keyword_set(SILENT)
              ;Maximum number of iterations allowed
 if N_elements(mxiter) EQ 0 then mxiter = 50
 if N_elements(minsky) Eq 0 then minsky = 20   ;Minimum number of legal sky elements
 nsky = N_elements( sky_vector )            ;Get number of sky elements 
 
  if nsky LT minsky then begin
      sigma=-1.0 &  skew = 0.0
      message,/CON, NoPrint= Silent, $
    'ERROR -Input vector must contain at least '+strtrim(minsky,2)+' elements'
      return
 endif
 
 nlast = nsky-1                        ;Subscript of last pixel in SKY array
 if keyword_set(DEBUG) then $
     message,'Processing '+strtrim(nsky,2) + ' element array',/INF
 sz_sky = size(sky_vector,/structure)
 integer = keyword_set(discrete)
 if ~integer then integer = (sz_sky.type LT 4) or (sz_sky.type GT 11) 
 sky = sky_vector[ sort( sky_vector ) ]    ;Sort SKY in ascending values

 skymid = 0.5*sky[(nsky-1)/2] + 0.5*sky[nsky/2] ;Median value of all sky values  
       
 cut1 = min( [skymid-sky[0],sky[nsky-1] - skymid] ) 
 if N_elements(highbad) EQ 1 then cut1 = cut1 < (highbad - skymid)
 cut2 = skymid + cut1
 cut1 = skymid - cut1
         
; Select the pixels between Cut1 and Cut2

 good = where( (sky LE cut2) and (sky GE cut1), Ngood ) 
 if ( Ngood EQ 0 ) then begin
      sigma=-1.0 &  skew = 0.0   
      message,/CON, NoPrint=Silent, $ 
           'ERROR - No sky values fall within ' + strtrim(cut1,2) + $
	   ' and ' + strtrim(cut2,2)           
      return
   endif
  
 delta = sky[good] - skymid  ;Subtract median to improve arithmetic accuracy
 sum = total(delta,/double)                     
 sumsq = total(delta^2,/double)

 maximm = max( good,MIN=minimm )  ;Highest value accepted at upper end of vector
 minimm = minimm -1               ;Highest value reject at lower end of vector

; Compute mean and sigma (from the first pass).

 skymed = 0.5*sky[(minimm+maximm+1)/2] + 0.5*sky[(minimm+maximm)/2 + 1] ;median 
 skymn = float(sum/(maximm-minimm))                            ;mean       
 sigma = sqrt(sumsq/(maximm-minimm)-skymn^2)             ;sigma          
 skymn = skymn + skymid         ;Add median which was subtracted off earlier 


;    If mean is less than the mode, then the contamination is slight, and the
;    mean value is what we really want.
skymod =  (skymed LT skymn) ? 3.*skymed - 2.*skymn : skymn

; Rejection and recomputation loop:

 niter = 0
 clamp = 1
 old = 0                            
START_LOOP:
   niter = niter + 1                     
   if ( niter GT mxiter ) then begin
      sigma=-1.0 &  skew = 0.0   
      message,/CON, NoPrint=Silent, $ 
           'ERROR - Too many ('+strtrim(mxiter,2) + ') iterations,' + $
           ' unable to compute sky'
      return
   endif

   if ( maximm-minimm LT minsky ) then begin    ;Error? 

      sigma = -1.0 &  skew = 0.0   
      message,/CON,NoPrint=Silent, $
            'ERROR - Too few ('+strtrim(maximm-minimm,2) +  $
           ') valid sky elements, unable to compute sky'
      return
   endif 

; Compute Chauvenet rejection criterion.

    r = alog10( float( maximm-minimm ) )      
    r = max( [ 2., ( -0.1042*r + 1.1695)*r + 0.8895 ] )

; Compute rejection limits (symmetric about the current mode).

    cut = r*sigma + 0.5*abs(skymn-skymod)   
    if integer then cut = cut > 1.5 
    cut1 = skymod - cut   &    cut2 = skymod + cut
; 
; Recompute mean and sigma by adding and/or subtracting sky values
; at both ends of the interval of acceptable values.
      
    redo = 0B
    newmin = minimm             
    tst_min = sky[newmin+1] GE cut1      ;Is minimm+1 above current CUT?
    done = (newmin EQ -1) and tst_min    ;Are we at first pixel of SKY?
    if ~done then  $
        done =  (sky[newmin>0] LT cut1) and tst_min
    if ~done then begin
        istep = 1 - 2*fix(tst_min)
        repeat begin
                newmin = newmin + istep
                done = (newmin EQ -1) || (newmin EQ nlast)
                if ~done then $
                    done = (sky[newmin] LE cut1) and (sky[newmin+1] GE cut1)
        endrep until done
        if tst_min then delta = sky[newmin+1:minimm] - skymid $
                   else delta = sky[minimm+1:newmin] - skymid
        sum = sum - istep*total(delta,/double)
        sumsq = sumsq - istep*total(delta^2,/double)
        redo = 1b
        minimm = newmin
     endif
;       
   newmax = maximm
   tst_max = sky[maximm] LE cut2           ;Is current maximum below upper cut?
   done = (maximm EQ nlast) and tst_max    ;Are we at last pixel of SKY array?
   if ~done then $   
       done = ( tst_max ) && (sky[(maximm+1)<nlast] GT cut2) 
    if ~done then begin                 ;Keep incrementing NEWMAX
       istep = -1 + 2*fix(tst_max)         ;Increment up or down?
       Repeat begin
          newmax = newmax + istep
          done = (newmax EQ nlast) or (newmax EQ -1)
          if ~done then $
                done = ( sky[newmax] LE cut2 ) and ( sky[newmax+1] GE cut2 )
       endrep until done
       if tst_max then delta = sky[maximm+1:newmax] - skymid $
               else delta = sky[newmax+1:maximm] - skymid
       sum = sum + istep*total(delta,/double)
       sumsq = sumsq + istep*total(delta^2,/double)
       redo = 1b
       maximm = newmax
    endif
;       
; Compute mean and sigma (from this pass).
;
   nsky = maximm - minimm
   if ( nsky LT minsky ) then begin    ;Error? 
       sigma = -1.0 &  skew = 0.0   
       message,NoPrint=Silent, /CON, $ 
               'ERROR - Outlier rejection left too few sky elements'
       return		 		 
   endif 

   skymn = float(sum/nsky)       
   sigma = float( sqrt( (sumsq/nsky - skymn^2)>0 ))
    skymn = skymn + skymid 
                

;  Determine a more robust median by averaging the central 20% of pixels.
;  Estimate the median using the mean of the central 20 percent of sky
;  values.   Be careful to include a perfectly symmetric sample of pixels about
;  the median, whether the total number is even or odd within the acceptance
;  interval
    
        center = (minimm + 1 + maximm)/2.
        side = round(0.2*(maximm-minimm))/2.  + 0.25
        J = round(CENTER-SIDE)
        K = round(CENTER+SIDE)

;  In case  the data has a large number of of the same (quantized) 
;  intensity, expand the range until both limiting values differ from the 
;  central value by at least 0.25 times the read noise.

        if keyword_set(readnoise) then begin        
          L = round(CENTER-0.25)
          M = round(CENTER+0.25)
          R = 0.25*readnoise
          while ((J GT 0) && (K LT Nsky-1) && $
            ( ((sky[L] - sky[J]) LT R) || ((sky[K] - sky[M]) LT R))) do begin
             J--
             K++
        endwhile
        endif 
   skymed = total(sky[j:k])/(k-j+1)
   
;  If the mean is less than the median, then the problem of contamination
;  is slight, and the mean is what we really want.

   dmod = skymed LT skymn ?  3.*skymed-2.*skymn-skymod : skymn - skymod

; prevent oscillations by clamping down if sky adjustments are changing sign
   if dmod*old LT 0 then clamp = 0.5*clamp
   skymod = skymod + clamp*dmod 
   old = dmod     
   if redo then goto, START_LOOP

;       
 skew = float( (skymn-skymod)/max([1.,sigma]) )
 nsky = maximm - minimm 

 if keyword_set(DEBUG) or ( N_params() EQ 1 ) then begin
        print, '% MMM: Number of unrejected sky elements: ', strtrim(nsky,2), $
              '    Number of iterations: ',  strtrim(niter,2)
        print, '% MMM: Mode, Sigma, Skew of sky vector:', skymod, sigma, skew   
 endif

 return
 end

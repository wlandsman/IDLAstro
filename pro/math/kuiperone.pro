 pro kuiperone, data, func_name, d, prob, PLOT = plot, WINDOW=window, $
     _EXTRA = extra
;+
; NAME:
;       KUIPERONE
; PURPOSE:
;       Compute the one-sided Kuiper statistic (invariant Kolmogorov-Smirnov)
; EXPLANATION:
;       Returns the Kuiper statistic and associated probability
;       for an array of data values and a user-supplied cumulative distribution
;       function (CDF) of a single variable.   Algorithm adapted from KSONE
;       in "Numerical Recipes" by Press et al. 2nd edition (1992)
;
;       Kuiper's test is especially useful for data defined on a circle or 
;       to search for periodicity (see Paltani 2004, A&A, 420, 789). 
; CALLING SEQUENCE:
;       kuiperone, data, func_name, D, prob, [ /PLOT ]
;
; INPUT PARAMETERS:
;       data -  vector of data values, must contain at least 4 elements for the
;               Kuiper statistic to be meaningful
;       func_name - scalar string giving the name of the cumulative distribution
;               function.    The function must be defined to accept the data
;               vector as its only input (see example).
;
; OUTPUT PARAMETERS:
;       D - floating scalar giving the Kuiper statistic.   It
;               specifies the sum of positive and negative deviations between the
;               cumulative distribution of the data and the supplied function
;       prob - floating scalar between 0 and 1 giving the significance level of
;               the Kuiper statistic.   Small values of PROB show that the
;               cumulative distribution function of DATA is significantly
;               different from FUNC_NAME.
;
; OPTIONAL INPUT KEYWORD:
;       /PLOT - If this keyword is set and non-zero, then KUIPERONE will display a
;               plot of the CDF of the data with the supplied function
;               superposed.   The data values where the Kuiper statistic is
;               computed (i.e. at the maximum difference between the data CDF
;               and the function) are indicated by vertical dashed lines.
;               KUIPERONE accepts the _EXTRA keyword, so that most plot keywords
;               (e.g. TITLE, XTITLE, XSTYLE) can also be passed to KUIPERONE.
;
; EXAMPLE:
;       Determine if a vector created by the RANDOMN function is really
;       consistent with a Gaussian distribution.
;       The CDF of a Gaussian is the error function except that a factor
;       of 2 is included in the error function.   So we must create a special
;       function:
;
;       function gauss_cdf, x
;       return, errorf( x/sqrt(2) )
;       end
;
;       IDL> data = randomn(seed, 50)          ;create data array to be tested
;       IDL> kuiperone, data, 'gauss_pdf', D, prob, /PLOT     ;Use Kuiper test
;
;       A small value of PROB indicates that the cumulative distribution of
;       DATA is significantly different from a Gaussian
;
; NOTES:
;       Note that the 2nd (1992) edition of Numerical Recipes includes
;       a more accurate computation of the K-S significance for small
;       values of N.
;
; PROCEDURE CALLS
;       procedure PROB_KUIPER - computes significance of Kuiper distribution
;
; REVISION HISTORY:
;       Written     W. Landsman                   August, 1992
;       Accept _EXTRA keywords   W. Landsman      September, 1995
;       Fixed possible bug in plot display showing position maximum difference
;       in histogram   M. Fardal/ W. Landsman      March, 1997
;       Adapted from KSONE      J. Ballet     July 2003
;       Use Coyote graphics   W. Landsman     Feb 2011
;-
 On_error, 2
 compile_opt idl2

 if ( N_params() LT 3 ) then begin
    print,'Syntax - kuiperone, data, func_name, D, [prob ,/PLOT]'
    return
 endif

 N = N_elements( data )
 if N LT 3 then message, $
   'ERROR - Input data values (first param) must contain at least 3 values'

 sortdata = data[ sort( data ) ]

 f0 = findgen(N)/ N
 fn = ( findgen( N ) +1. ) / N
 ff = call_function( func_name, sortdata )

; Maximum distance above the reference
 D1 = max( fn-ff, subn )

; Maximum distance below the reference
 D2 = max( ff-f0, sub0 )

 D = D1 + D2

 if keyword_set(plot) || keyword_set(WINDOW) then begin

; Prepare the step function
     xx = REBIN(sortdata,2*N,/SAMPLE)
     yy = REBIN(f0,2*N,/SAMPLE)
     yy = [yy[1:*],1.]

     cgplot, xx,yy,_EXTRA = extra, WINDOW=window
     cgplots, [sortdata[sub0], sortdata[sub0]], [0,ff[sub0]], linestyle=2, $
         WINDOW=window
     cgplots, [sortdata[subn], sortdata[subn]], [ff[subn],1], linestyle=2, $
        WINDOW=window

; Plot the expected cumulative distribution
     n2 = n > 100
     x2 = FINDGEN(n2+1)*(!X.CRANGE[1]-!X.CRANGE[0])/n2 + !X.CRANGE[0]
     y2 = call_function( func_name, x2 )
     cgplot,/over, x2,y2,lines=1,thick=2, WINDOW=window
 endif

 prob_kuiper, D, N, prob           ;Compute significance of Kuiper statistic

 return
 end

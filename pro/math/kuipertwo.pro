 pro kuipertwo, data1, data2, D, prob, PLOT = plot, _EXTRA = extra,WINDOW=window
;+
; NAME:
;       KUIPERTWO
; PURPOSE:
;       Compute the two-sided Kuiper statistic (invariant Kolmogorov-Smirnov)
; EXPLANATION:
;       Returns the Kuiper statistic and associated probability 
;       that two arrays of data values are drawn from the same distribution
;       Algorithm adapted from KSTWO in "Numerical
;       Recipes" by Press et al., 2nd edition (1992), Chapter 14
;
; CALLING SEQUENCE:
;       kuipertwo, data1, data2, D, prob, [ /PLOT ]
;
; INPUT PARAMETERS:
;       data1 -  vector of data values, at least 4 data values must be included
;               for the Kuiper statistic to be meaningful
;       data2 -  second set of data values, does not need to have the same 
;               number of elements as data1
;
; OUTPUT PARAMETERS:
;       D - floating scalar giving the Kuiper statistic.   It
;               specifies the sum of positive and negative deviations between
;               the cumulative distributions of the two data sets
;       prob - floating scalar between 0 and 1 giving the significance level of
;               the Kuiper statistic.   Small values of PROB show that the 
;               cumulative distribution function of DATA1 is significantly 
;               different from DATA2
;
; OPTIONAL INPUT KEYWORD:
;       /PLOT - If this keyword is set and non-zero, then KUIPERTWO will display
;               a plot of the CDF of the two data sets.
;               The data values where the Kuiper statistic is
;               computed (i.e. at the maximum difference between the CDF of
;               the two data sets) are indicated by vertical dashed lines.
;               KUIPERTWO accepts the _EXTRA keyword, so that most plot keywords
;               (e.g. TITLE, XTITLE, XSTYLE) can also be passed to KUIPERTWO.
;       /WINDOW - If set the plot to a resizeable graphics window.
; EXAMPLE:
;       Test whether two vectors created by the RANDOMN function likely came
;       from the same distribution
;
;       IDL> data1 = randomn(seed,40)        ;Create data vectors to be 
;       IDL> data2 = randomn(seed,70)        ;compared
;       IDL> kuipertwo, data1, data2, D, prob   & print,D,prob
;
; PROCEDURE CALLS
;       procedure PROB_KUIPER - computes significance of Kuiper distribution
;
; REVISION HISTORY:
;       Written     W. Landsman                August, 1992
;       FP computation of N_eff      H. Ebeling/W. Landsman  March 1996
;       Fix for arrays containing equal values J. Ballet/W. Landsman
;       Oct. 2001
;       Adapted from KSTWO, added PLOT keyword  J. Ballet     July 2004
;       Use Coyote Graphics W. Landsman   Feb 2011
;-
  On_error, 2
  compile_opt idl2

 if ( N_params() LT 4 ) then begin
    print,'Syntax - KUIPERTWO, data1, data2, d, prob [, /PLOT]'
    return
 endif

 n1 = N_elements( data1 )
 if ( N1 LE 3 ) then message, $
   'ERROR - Input data values (first param) must contain at least 4 values'

 n2 = N_elements( data2 )
 if ( n2 LE 3 ) then message, $
   'ERROR - Input data values (second param) must contain at least 4 values'

 sortdata1 = data1[ sort( data1 ) ]        ;Sort input arrays into 
 sortdata2 = data2[ sort( data2 ) ]        ;ascending order

 fn1 = ( findgen( n1 )  ) / n1
 fn2 = ( findgen( n2 )  ) / n2

 j1 = 0l & j2 = 0l
 id1 = lonarr(n1+n2)  & id2 = id1
 i = 0l

; Form the two cumulative distribution functions, marking points where one
; must test their difference

 while ( j1 LT n1 ) and ( j2 LT n2 ) do begin

     d1 = sortdata1[j1]
     d2 = sortdata2[j2]
     if d1 LE d2 then j1 = j1 +1
     if d2 LE d1 then j2 = j2 +1
            
     id1[i] = j1   & id2[i] = j2
     i = i+1

 endwhile

 id1 = id1[0:i-1]   &  id2 = id2[0:i-1]

; The Kuiper statistic D is the sum of the maximum positive and
; negative differences between the two distribution functions

 D1 = max(fn1[id1] - fn2[id2], sub1, MIN=D2, SUBSCRIPT_MIN=sub2)
 D = D1 - D2
 N_eff =  n1*n2/ float(n1 + n2)              ;Effective # of data points
 PROB_KUIPER, D, N_eff, prob                 ;Compute significance of statistic

 if keyword_set(plot) || keyword_set(Window) then begin

; Prepare the step functions
     xx1 = REBIN(sortdata1,2*n1,/SAMPLE)
     yy1 = REBIN(fn1,2*n1,/SAMPLE)
     yy1 = [yy1[1:*],1.]

     xx2 = REBIN(sortdata2,2*n2,/SAMPLE)
     yy2 = REBIN(fn2,2*n2,/SAMPLE)
     yy2 = [yy2[1:*],1.]

     cgplot, xx1, yy1, _EXTRA = extra, WINDOW=window
     cgplot, /over, xx2, yy2, lines=1, thick=2, WINDOW=window
     j1 = id1[sub1] - 1
     j2 = id1[sub2]
     cgplots, [sortdata1[j2], sortdata1[j2]], [0,fn2[id2[sub2]]], linestyle=2,$
         WINDOW=window
     cgplots, [sortdata1[j1], sortdata1[j1]], [fn2[id2[sub1]],1], linestyle=2,$
        WINDOW=window
 endif

 return
 end

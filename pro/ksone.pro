 pro ksone, data, func_name, d, prob, xval,  $
     PLOT=plot, _EXTRA=extra, Window=window, INTEGER=int, ABOVE=msub, BREAK=ib
;+
; NAME:
;       KSONE
; PURPOSE:
;       Compute the one-sided Kolmogorov-Smirnov statistic
; EXPLANATION:
;       Returns the Kolmogorov-Smirnov statistic and associated probability for 
;       for an array of data values and a user-supplied cumulative distribution
;       function (CDF) of a single variable.   Algorithm from the procedure of
;       the same name in "Numerical Recipes" by Press et al. 2nd edition (1992)
;
; CALLING SEQUENCE:
;       ksone, data, func_name, D, prob, xval, [ /PLOT ]
;
; INPUT PARAMETERS:
;       data -  vector of data values, must contain at least 4 elements for the
;               K-S statistic to be meaningful 
;       func_name - scalar string giving the name of the cumulative distribution
;               function.    The function must be defined to accept the data
;               vector as its only input (see example), though keywords may be
;               passed via the _EXTRA facility.
;
; OUTPUT PARAMETERS:
;       D - floating scalar giving the Kolmogorov-Smirnov statistic.   It 
;               specified the maximum deviation between the cumulative 
;               distribution of the data and the supplied function 
;       prob - floating scalar between 0 and 1 giving the significance level of
;               the K-S statistic.   Small values of PROB show that the 
;               cumulative distribution function of DATA is significantly 
;               different from FUNC_NAME.
;       xval - data value at which the maximum deviation occurs
;
; OPTIONAL INPUT KEYWORD:
;       /PLOT - If this keyword is set and non-zero, then KSONE will display a
;               plot of the CDF of the data with the supplied function 
;               superposed.   The data value xval where the K-S statistic is 
;               computed (i.e. at the maximum difference between the data CDF 
;               and the function) is indicated by a vertical line.
;               KSONE accepts the _EXTRA keyword, so that most plot keywords
;               (e.g. TITLE, XTITLE, XSTYLE) can also be passed to KSONE.
;       /WINDOW - If set, the plot to a resizeable graphics window
;      /INTEGER - Set if func_name is a discrete function taking only integer values
;                 like the Poisson distribution.
;                 func_name must be defined down to min(data) - 1
;                 If the maximum distance is below the function, xval will return
;                 one of the data - 1
; OPTIONAL OUTPUT KEYWORD:
;       ABOVE - Returns 0 (false) is the maximum distance is below the expected CDF
;               and 1 (true) if it is above
; EXAMPLE:
;       Determine if a vector created by the RANDOMN function is really 
;       consistent with a Gaussian distribution with unit variance.
;       The CDF of a Gaussian is the error function except that a factor
;       of 2 is included in the error function.   So we must create a special
;       function:
;
;       function gauss_cdf, x
;       return, errorf( x/sqrt(2) )
;       end
;
;       IDL> data = randomn(seed, 50)          ;create data array to be tested
;       IDL> ksone, abs(data), 'gauss_cdf', D, prob, /PLOT     ;Use K-S test
;      
;       A small value of PROB indicates that the cumulative distribution of 
;        DATA is significantly different from a Gaussian
;
; NOTES:
;       The code for PROB_KS is from the 2nd (1992) edition of Numerical 
;       Recipes which includes a more accurate computation of the K-S 
;       significance for small values of N than the first edition.
;
;       Since _EXTRA is used to pass extra parameters both to the user-supplied
;       function, and to the cgPLOT command, the user-supplied function should
;       not accept "cgPLOT" keyword names (e.g. XTITLE).
;
; PROCEDURE CALLS
;       procedure PROB_KS - computes significance of K-S distribution
;       TAG_EXIST() 
;
; REVISION HISTORY:
;       Written     W. Landsman                   August, 1992
;       Accept _EXTRA keywords   W. Landsman      September, 1995          
;       Fixed possible bug in plot display showing position maximum difference
;       in histogram   M. Fardal/ W. Landsman      March, 1997
;       Documentation updates   W. Landsman   June 2003
;       Pass _EXTRA to func_name  M. Fitzgerald    April, 2005
;       Work for functions that do not accept keywords W. Landsman July 2009
;       Use Coyote graphics for plotting           Feb 2011
;       Return xval                                    J. Ballet  July 2019
;       Fix plotting, return msub, add DISCRETE option J. Ballet  August 2019
;-
 On_error, 2
 compile_opt idl2

 if ( N_params() LT 3 ) then begin
    print,'Syntax - ksone, data, func_name, D, [prob , xval, /PLOT]'
    return
 endif

 if not keyword_set(int) then  int = 0
 if not keyword_set(ib) then  ib = 0

 dsize = SIZE(data)
 id = dsize[0]
 if int*dsize[id+1] gt 3 then message, $
   'ERROR - If INTEGER keyword is set data values must be integer'
 
 N = dsize[id+2]
 if N LT 3 then message, $
   'ERROR - Input data values (first param) must contain at least 3 values'

 sortdata = data[ sort( data ) ]
 
 if int gt 0 then begin
    ; Mark duplicates
    singles = [0,where(sortdata lt sortdata[1:*])+1]
    ; Create secondary points shifted by 1 from input
    sortdata = rebin(sortdata,2*N,/sample)
    ; Do not shift duplicates
    sortdata[2*singles] -= 1
 endif

 ; We need to determine if the user-supplied function accepts keyword 
 ; arguments.    If it does not then passing the _EXTRA keyword will signal
 ; an error.
 resolve_routine, func_name,/is_function
 r = routine_info(func_name,/parameter,/function)
 if tag_exist(r,'KW_ARGS') then $
      ff = call_function( func_name, sortdata, _EXTRA = extra) else $
      ff = call_function( func_name, sortdata)

 if int eq 0 then begin
    sortdata = rebin(sortdata,2*N,/sample)
    ff = rebin(ff,2*N,/sample)    
 endif

 f0 = findgen(N)/ N
 f0 = [0,rebin(f0[1:*],2*(N-1),/sample),1]

 D = max( abs(f0-ff), sub0 )
 xval = sortdata[sub0]
 msub = f0[sub0] ge ff[sub0]

 if keyword_set(plot) || keyword_set(WINDOW) then begin
 ; Slight horizontal offset to avoid overlaying vertical line with CDF
    eps = 3E-3 * (sortdata[2*N-1]-sortdata[0])
    if msub EQ 0 then  eps *= -1
    cgplot, sortdata, f0,_EXTRA = extra, window=window
    cgplots, (xval+eps)*[1,1], [0,1], window=window
    cgplot, /over, sortdata, ff, lines=1, window=window
 endif

 PROB_KS, D, N, prob           ;Compute significance of K-S statistic

 if ib gt 0 then  stop
 return
 end

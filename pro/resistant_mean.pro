PRO RESISTANT_Mean,Y,CUT,Mean,Sigma,Num_Rej,goodvec = goodvec, $
                  dimension=dimension, double=double,sumdim=sumdim, $
		  wused=wused, Silent = silent
;+
; NAME:
;    RESISTANT_Mean  
;
; PURPOSE:
;    Outlier-resistant determination of the mean and standard deviation. 
; 
; EXPLANATION:
;    RESISTANT_Mean trims away outliers using the median and the median 
;    absolute deviation.    An approximation formula is used to correct for
;    the truncation caused by trimming away outliers
;
; CALLING SEQUENCE:
;    RESISTANT_Mean, ARRAY, Sigma_CUT, Mean, Sigma_Mean, Num_RejECTED
;                         [/DOUBLE, DIMENSION= , GOODVEC = ]
; INPUT ARGUMENT:
;       ARRAY    = Vector or array to average, NaN values will be ignored
;       Sigma_CUT = Data more than this number of standard deviations from the
;               median is ignored. Suggested values: 2.0 and up.
;
; OUTPUT ARGUMENT:
;       Mean  = the mean of the input array, numeric scalar,    If the 
;            DIMENSION keyword is set, then MEAN will be an array with one
;            less dimension than the input.
; OPTIONAL OUTPUTS:
;	Sigma_Mean = the approximate standard deviation of the mean, numeric 
;            scalar.  This is the Sigma of the distribution divided by sqrt(N-1)
;            where N is the number of unrejected points. The larger
;            SIGMA_CUT, the more accurate. It will tend to underestimate the 
;            true uncertainty of the mean, and this may become significant for 
;            cuts of 2.0 or less. 
;       Num_RejECTED = the number of points trimmed, integer scalar
; OPTIONAL INPUT KEYWORDS:
;      /DOUBLE - If set, then all calculations are performed internally 
;            in double precision.  
;      DIMENSION - for a multi-dimensional array, the dimension over which to
;            take the mean, starting at 1. If not set, then the scalar mean
;            over all elements is used. If this argument is present, the result
;            is an array with one less dimension than Array. For example, if 
;            the dimensions of Array are N1, N2, N3, and Dimension is 2, then 
;            the dimensions of the result are (N1, N3)    
;      /SILENT - Set to suppress error messages, e.g.if all values in the array
;            are NaN
;      SUMDIM - Obsolete synonym for DIMENSION
; OPTIONAL OUTPUT KEYWORD:
;       Goodvec -  Indices of non-trimmed elements of the input vector
;       Wused - synonym for Goodvec (for solarsoft compatibility)
; EXAMPLE:
;       IDL> a = randomn(seed, 10000)    ;Normal distribution with 10000 pts
;       IDL> RESISTANT_Mean,a, 3, mean, meansig, num    ;3 Sigma clipping    
;       IDL> print, mean, meansig,num
; 
;       The mean should be near 0, and meansig should be near 0.01 ( =
;        1/sqrt(10000) ).     
; PROCEDURES USED:
;       MEAN() - compute simple mean, in Exelis library
; REVISION HISTORY:
;       Written, H. Freudenreich, STX, 1989; Second iteration added 5/91.
;       Use MEDIAN(/EVEN)    W. Landsman   April 2002
;       Correct conditional test, higher order truncation correction formula
;                R. Arendt/W. Landsman   June 2002
;       New truncation formula for sigma H. Freudenriech  July 2002
;       Divide Sigma_mean by Num_good rather than Npts W. Landsman/A. Conley
;                          January 2006
;       Use of double precision S. Bianchi February 2008
;       More double precision B. Carcich December 2009
;       Added DIMENSION keyword (from M. Desnoyer) B. Carcich December 2009
;       Use IDL's MEAN() function instead of AVG() W. Landsman Jan 2012
;       Use of Dimension keyword yielded transpose of correct value
;                     W. Landsman  July 2012
;       Added NaN keyword to MEAN() call N. Crouzet/WL  April 2013
;       Allow a row/column to be all NaN values N. Crouzet/WL  April 2013
;       Use of DIMENSION keyword yielded wrong answer for non-square arrays
;                       D. Cottingham  December 2014
;-

 On_Error,2
 compile_opt idl2
 if N_params() LT 3 then begin
     print,'Syntax - Resistant_Mean, Vector, Sigma_cut, Mean, [ Sigma_mean, ' 
     print,'                                  Num_Rejected,  GOODVEC=,'
     print,'                                  DIMEN=, /DOUBLE]'
     return
 endif

 sz = size(Y)
 indouble = size(Y,/tname) EQ 'DOUBLE'          ;Is input double precision?
 
; Average over a single dimension?
   if N_elements(DIMENSION)  then DIM = long(DIMENSION[0]) $
   else if n_elements(SUMDIM) then DIM = long(SUMDIM[0]) 
 if (sz[0] gt 1L) && (sz[0] lt 5L) && (N_elements(DIM) EQ 1) then begin
   if (DIM lt 1L) || (dim gt sz[0]) then begin
     message,/continue, 'Invalid dimension number'
     print,'Syntax - Resistant_Mean, Vector, Sigma_cut, Mean'
     print,'        , [ Sigma_mean, Num_Rejected, Dimension={1|2} ]'
     return
   endif
   ;;;
   od=[ sz[0:dim-1], sz[dim+1:sz[0]+1] ]  ;;; [buffer, i,j,k,m, buffer]
   od=[ od[1:sz[0]-1], 1, 1, 1]         ;;; [i,j,k,m]
   rowlen = sz[dim]
   colhgt = sz[sz[0]+2]/rowlen
   sd = size([0d0])
   Num_Rej = make_array(od[0],od[1],od[2],od[3],val=0L)
   if keyword_set(double) || indouble then v=0d0 else v=0.
   Mean = make_array(od[0],od[1],od[2],od[3],val=v)
   Sigma = Mean
   ;;;
   if n_elements(CUT) eq colhgt then iwCUT = lindgen(colhgt) $
   else iwCUT = make_array(colhgt,val=0L)
   ;;;
   ijkL=0L
  
   for L=0L,od[3]-1L do begin
   for k=0L,od[2]-1L do begin
   for j=0L,od[1]-1L do begin
   for i=0L,od[0]-1L do begin
     thisCut = CUT[iwCUT[ijkL]]
     case dim of
     1: RESISTANT_Mean,Y[*,i,j,k,L],thisCUT,M,S,N,double=double,/Silent
     2: RESISTANT_Mean,Y[i,*,j,k,L],thisCUT,M,S,N,double=double,/Silent
     3: RESISTANT_Mean,Y[i,j,*,k,L],thisCUT,M,S,N,double=double,/Silent
     4: RESISTANT_Mean,Y[i,j,k,*,L],thisCUT,M,S,N,double=double,/Silent
     5: RESISTANT_Mean,Y[i,j,k,L,*],thisCUT,M,S,N,double=double,/Silent
     endcase
     
     ;;;
     Mean[ijkL] = M
     Sigma[ijkL] = S
     Num_Rej[ijkL] = N
     ijkL++
   endfor
   endfor
   endfor
   endfor
   return
 endif

 MADscale = 0.6745d0
 MADscale2 = 0.8d0
 MADlim = 1d-24
 Sigcoeff = [ -0.15405d0, +0.90723d0, -0.23584d0, +0.020142d0 ]
 One = 1d0
 if ~keyword_set(double) && ~indouble then begin
   MADscale = float(MADscale)
   MADscale2 = float(MADscale2)
   MADlim = float(MADlim)
   SIGcoeff = float(SIGcoeff)
   One = float(One)
 endif

 Npts    = N_Elements(Y)
 YMed    = MEDIAN(Y,/EVEN, DOUBLE=double)
 AbsDev  = ABS(Y-YMED)
 MedAbsDev = MEDIAN(AbsDev,/EVEN, DOUBLE=double)/MADscale
 IF MedAbsDev LT MADlim THEN $
      MedAbsDev = MEAN(AbsDev, DOUBLE=double, /NaN)/MADscale2

 Cutoff    = Cut*MedAbsDev

 goodvec = where( AbsDev LE Cutoff, Num_Good) 
 if Num_Good LE 0 then begin
     if ~keyword_set(SILENT) then $
           message,'Unexpected error -- Unable to compute mean',/Con
     mean = !Values.F_NaN & sigma = !VALUES.F_NAN & Num_rej = 0
     return
 endif    
 GoodPts = Y[ goodvec]
 Mean    = mean( GoodPts, DOUBLE=double)
 Sigma   = SQRT( TOTAL((GoodPts-Mean)^2, DOUBLE=double)/Num_Good )
 Num_Rej = Npts - Num_Good

; Compensate Sigma for truncation (formula by HF):
 SC = Cut > 1.0
 IF SC LE 4.50 THEN SIGMA=SIGMA/poly(SC, SIGcoeff)

 Cutoff = Cut*Sigma 

 goodvec = where( AbsDev LE Cutoff, Num_Good) 

 Num_Rej = Npts - Num_Good
 GoodPts = Y[ goodvec ]
 if arg_present(wused) then wused = goodvec
 Mean    = mean( GoodPts, DOUBLE= double)
 if N_params() LT 4 then return     ;Skip sigma calculation?
 

 Sigma   = SQRT( TOTAL((GoodPts-Mean)^2)/Num_Good )

; Fixed bug (should check for SC not Sigma) & add higher order correction
 SC = Cut > 1.0
 IF SC LE 4.50 THEN SIGMA=SIGMA/poly(SC, SIGcoeff)

; Now the standard deviation of the mean:
 Sigma = Sigma/SQRT(Num_Good-One)

 RETURN
 END

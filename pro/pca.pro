PRO PCA, data, eigenval, eigenvect, percentages, proj_obj, proj_atr, $
      MATRIX=AM,TEXTOUT=textout,COVARIANCE=cov,SSQ=ssq,SILENT=silent
     
;+
; NAME:
;    PCA
;
; PURPOSE:
;    Carry out a Principal Components Analysis (Karhunen-Loeve Transform)
; EXPLANATION:
;    Results can be directed to the screen, a file, or output variables
;    See notes below for comparison with the intrinsic IDL function PCOMP.
;
;    Harris Geospatial has a video/blog post  on using pca.pro at 
;    http://tinyurl.com/h6ky6qy .     Also see David Fanning's discussion of
;    PCA analysis with IDL ( http://www.idlcoyote.com/code_tips/pca.html )
;
; CALLING SEQUENCE:
;    PCA, data, eigenval, eigenvect, percentages, proj_obj, proj_atr, 
;             [MATRIX =, TEXTOUT = ,/COVARIANCE, /SSQ, /SILENT ]
;
; INPUT PARAMETERS:
;     data -  2-d data matrix, data(i,j) contains the jth attribute value
;               for the ith object in the sample.    If N_OBJ is the total
;               number of objects (rows) in the sample, and N_ATTRIB is the 
;               total number of attributes (columns) then data should be
;               dimensioned N_OBJ x N_ATTRIB.         
;
; OPTIONAL INPUT KEYWORD PARAMETERS:
;     /COVARIANCE - if this keyword is set, then the PCA will be carried out
;              on the covariance matrix (rare), the default is to use the
;              correlation matrix
;     /SILENT - If this keyword is set, then no output is printed
;     /SSQ - if this keyword is set, then the PCA will be carried out on
;               on the sums-of-squares & cross-products matrix (rare)
;     TEXTOUT - Controls print output device, defaults to !TEXTOUT
;
;              textout=1       TERMINAL using /more option
;              textout=2       TERMINAL without /more option
;              textout=3       <program>.prt
;              textout=4       laser.tmp
;              textout=5      user must open file
;              textout = filename (default extension of .prt)
;
; OPTIONAL OUTPUT PARAMETERS:
;     eigenval -  N_ATTRIB element vector containing the sorted eigenvalues
;     eigenvect - N_ATRRIB x N_ATTRIB matrix containing the corresponding 
;               eigenvectors
;     percentages - N_ATTRIB element containing the cumulative percentage 
;             variances associated with the principal components
;     proj_obj - N_OBJ by N_ATTRIB matrix containing the projections of the 
;             objects on the principal components
;     proj_atr - N_ATTRIB by N_ATTRIB matrix containing the projections of 
;               the attributes on the principal components
;
; OPTIONAL OUTPUT PARAMETER
;      MATRIX   = analysed matrix, either the covariance matrix if /COVARIANCE
;              is set, the "sum of squares and cross-products" matrix if
;              /SSQ is set, or the (by default) correlation matrix.    Matrix
;              will have dimensions N_ATTRIB x N_ATTRIB
;
; NOTES:
;      This procedure performs Principal Components Analysis (Karhunen-Loeve
;      Transform) according to the method described in "Multivariate Data 
;      Analysis" by Murtagh & Heck [Reidel : Dordrecht 1987], pp. 33-48.
;      See  http://www.classification-society.org/csna/mda-sw/pca.f
;
;      Keywords /COVARIANCE and /SSQ are mutually exclusive.
;
;      The printout contains only (at most) the first seven principle 
;      eigenvectors.    However, the output variables EIGENVECT contain 
;      all the eigenvectors
;       
;      Different authors scale the covariance matrix in different ways.
;      The eigenvalues output by PCA may have to be scaled by 1/N_OBJ or
;      1/(N_OBJ-1) to agree with other calculations when /COVAR is set.
;
;      PCA uses the non-standard system variables !TEXTOUT and !TEXTUNIT.
;      These are automatically added if not originally present.
;
;      The intrinsic IDL function PCOMP duplicates most of the 
;      functionality of PCA, but uses different conventions and
;      normalizations.   Note the following:
;
;   (1) PCOMP requires a N_ATTRIB x N_OBJ input array; this is the transpose
;         of what PCA expects
;   (2) PCA uses standardized variables for the correlation matrix:  the input 
;        vectors are set to a  mean of zero and variance of one and divided by 
;        sqrt(n); use the /STANDARDIZE keyword to PCOMP for a direct comparison.
;   (3) PCA (unlike PCOMP) normalizes the eigenvectors by the square root
;         of the eigenvalues.
;   (4) PCA returns cumulative percentages; the VARIANCES keyword of PCOMP
;         returns the variance in each variable
;   (5) PCOMP divides the eigenvalues by (1/N_OBJ-1) when the covariance matrix
;          is used.
;
; EXAMPLE:
;      Perform a PCA analysis on the covariance matrix of a data matrix, DATA,
;      and write the results to a file
;
;      IDL> PCA, data, /COVAR, t = 'pca.dat'
;
;      Perform a PCA analysis on the correlation matrix.   Suppress all 
;      printing, and save the eigenvectors and eigenvalues in output variables
;
;      IDL> PCA, data, eigenval, eigenvect, /SILENT
;
; PROCEDURES CALLED:
;      TEXTOPEN, TEXTCLOSE
;
; REVISION HISTORY:
;      Immanuel Freedman (after Murtagh F. and Heck A.).     December 1993
;      Wayne Landsman, modified I/O              December 1993
;      Fix MATRIX output, remove GOTO statements   W. Landsman August 1998      
;      Changed some index variable to type LONG    W. Landsman March 2000
;      Fix error in computation of proj_atr, see Jan 1990 fix in 
;       http://www.classification-society.org/csna/mda-sw/pca.f   W. Landsman Feb 2008
;- 
  compile_opt idl2

; Constants
  TOLERANCE = 1.0E-5       ; are array elements near-zero ?

; Dispatch table

 IF N_PARAMS() EQ 0  THEN BEGIN
  print,'Syntax  - PCA, data, [eigenval, eigenvect, percentages, proj_obj, proj_atr,'
  print,'               [MATRIX =, /COVARIANCE, /SSQ, /SILENT, TEXTOUT=]'
  RETURN
 ENDIF 
 
 ; Constants
  TOLERANCE = 1.0E-5       ; are array elements near-zero ?


 Catch, theError
 IF theError NE 0 then begin
     Catch,/Cancel
     void = cgErrorMsg(/quiet)
     RETURN
     ENDIF


  if size(data,/N_dimen)  NE 2 THEN BEGIN 
    HELP,data
    MESSAGE,'ERROR - Data matrix is not two-dimensional'
  ENDIF

  dimen = size(data,/dimen) 
  Nobj = dimen[0]   &  Mattr = dimen[1]      ;Number of objects and attributes


  IF KEYWORD_SET(cov) THEN BEGIN
        msg = 'Covariance matrix will be analyzed'
; form column-means
        column_mean = total( data,1 )/Nobj
	temp = replicate(1.0, Nobj)
        X = (data - temp # transpose(column_mean))
  ENDIF ELSE $
  IF KEYWORD_SET(ssq) THEN BEGIN

        msg = 'Sum-of-squares & cross-products matrix will be analyzed'
        X = data 

   ENDIF ELSE BEGIN
        msg = 'Default: Correlation matrix will be analyzed' 
; form column-means
        temp = replicate( 1.0, Nobj )
        column_mean = (temp # data)/ Nobj
        X = (data - temp # transpose(column_mean))
        S = sqrt(temp # (X*X)) & X = X/(temp # S)
         
   ENDELSE

 A = transpose(X) # X
 if arg_present(AM) then AM = A

; Carry out eigenreduction
 trired, A, D, E              ; D contains diagonal, E contains off-diagonal
 triql, D, E, A               ; D contains the eigen-values, A(*,i) -vectors

; Use TOLERANCE to decide if eigenquantities are sufficiently near zero

 index = where(abs(D) LE TOLERANCE*MAX(abs(D)),count) 
 if count NE 0 THEN D[index]=0
 index = where(abs(A) LE TOLERANCE*MAX(abs(A)),count) 
 if count NE 0 THEN A[index]=0

 index = sort(D)                   ; Order by increasing eigenvalue
 D = D[index] & E=E[index]
 A = A[*,index]

; Eigenvalues expressed as percentage variance and ...
 W1 = 100.0 * reverse(D)/total(D)

;... Cumulative percentage variance
 W = total(W1, /cumul)

;Define returned parameters
 eigenval = reverse(D)
 eigenvect = reverse(transpose(A))
 percentages = W

; Output eigen-values and -vectors 

  if ~keyword_set(SILENT) then begin
;       Open output file 
        textopen,'PCA', TEXTOUT = textout
        printf,!TEXTUNIT,'PCA: ' + systime()
        sz1 = strtrim( Nobj,2) & sz2 = strtrim( Mattr, 2 )
        printf,!TEXTUNIT, 'Data  matrix has '+ sz1 + ' objects with up to ' + $
                 sz2 + ' attributes'
        printf,!TEXTUNIT, msg 
        printf,!TEXTUNIT, " "
        printf,!TEXTUNIT, $ 
                '   Eigenvalues     As Percentages       Cumul. percentages'
        for i = 0L, Mattr-1 do $
        printf,!TEXTUNIT, eigenval[i], W1[i], percentages[i] ,f = '(3f15.4)'
        printf,!TEXTUNIT," "
        printf,!TEXTUNIT, 'Corresponding eigenvectors follow...'
        Mprint = Mattr < 7
        header = ' VBLE  '
        for i = 1, Mprint do header = header + '  EV-' + strtrim(i,2) + '   '
        printf,!TEXTUNIT, header
        for i = 1L, Mattr do printf,!TEXTUNIT, $
                 i, eigenvect[0:Mprint-1,i-1],f='(i4,7f9.4)'
  endif

; Obtain projection of row-point on principal axes  (Murtagh & Heck convention)
 projx = X # A

; Use TOLERANCE again...
 index = where(abs(projx) LE TOLERANCE*MAX(abs(projx)),count)
 if count NE 0 THEN projx[index]=0
 proj_obj = reverse( transpose(projx) )

 if ~keyword_set( SILENT ) then begin
         printf,!TEXTUNIT,' '
         printf,!TEXTUNIT, 'Projection of objects on principal axes ...'
         printf,!TEXTUNIT,' '
         header = ' VBLE  '
         for i = 1, Mprint do header = header + 'PROJ-' + strtrim(i,2) + '   '
         printf,!TEXTUNIT, header 
         for i = 0L, Nobj-1 do printf,!TEXTUNIT, $
                i+1, proj_obj[0:Mprint-1,i], f='(i4,7f9.4)'
 endif

; Obtain projection of column-points on principal axes
 projy = transpose(projx)#X

; Use TOLERANCE again...
 index = where(abs(projy) LE TOLERANCE*MAX(abs(projy)),count)
 if count NE 0 THEN projy[index] = 0

; scale by square root of eigenvalues...
 temp = replicate( 1.0, Mattr )
 proj_atr = reverse(projy)/(sqrt(eigenval)#temp)

 if ~keyword_set( SILENT ) then begin
        printf,!TEXTUNIT,' '
        printf,!TEXTUNIT,'Projection of attributes on principal axes ...'
        printf,!TEXTUNIT,' '
        printf,!TEXTUNIT, header
        for i = 0L, Mattr-1 do printf,!TEXTUNIT, $
                i+1, proj_atr[0:Mprint-1,i], f='(i4,7f9.4)'
         textclose, TEXTOUT = textout           ; Close output file  
 endif

 RETURN
 END

PRO AUTOHIST,V, ZX,ZY,XX,YY, NOPLOT=whatever,_EXTRA = _extra
;
;+
; NAME:
;       AUTOHIST
;
; PURPOSE:
;       Draw a histogram using automatic bin-sizing.
; EXPLANATION
;       AUTOHIST chooses a number of bins (initially, SQRT(2*N). If this leads 
;       to a histogram in which > 1/5 of the central 50% of the bins are empty,
;       it decreases the number of bins and tries again. The minimum # bins is 
;       5. The max=199.     Called by HISTOGAUSS and HALFAGAUSS.
;
; CALLING SEQUENCE:
;       AUTOHIST, Sample, XLines, Ylines, XCenters, YCenters, [/NOPLOT, ]
;                             ...Plotting Keywords
; INPUT:
;       Sample = the vector to be histogrammed
;
; OUTPUT:
;       XLINES = vector of x coordinates of the points that trace the rectangular 
;               histogram bins
;       YLINES = vector of y coordinates. To draw the histogram plot YLINES vs 
;                 XLINES
;       XCENTERS = the x values of the bin centers
;       YCENTERS = the corresponding y values
;
; OPTIONAL INPUT KEYWORDS:
;       /NOPLOT  If set, nothing is drawn
;
;       Any plotting keywords (e.g. XTITLE) may be supplied to AUTOHIST through
;       the _EXTRA facility. 
; REVISION HISTORY:
;       Written,   H. Freudenreich, STX, 1/91
;       1998 March 17 - Changed shading of histogram.  RSH, RSTX
;       V5.0 update, _EXTRA keywords  W. Landsman    April 2002
;       Added NOCLIP keyword for POLYFILL call C. Paxson/W. Landsman July 2003
;       Use Coyote graphics   W. Landsman  Feb 2011
;-

 ON_ERROR,2
 compile_opt idl2 
 
 if N_params() LT 1 then begin
    print,'Syntax - AUTOHIST, Sample, XLines, Ylines, XCenters, YCenters, [ '
    print,'                           /NOPLOT, Plotting keywords... ]'
    return
 endif

 MINBIN=5

 N = N_ELEMENTS(V)
 NB = FIX(SQRT(2.*N)) < 199
 NB = NB > MINBIN

 X1 = MIN(V, MAX = X2)

tryagain:

 DX = (X2-X1)/NB
 XX = FINDGEN(NB)*DX + DX/2. + X1

 IND = (V-X1)/DX > 0 <(NB-1)

;  Compute the histogram for the current binning 

 YY = HISTOGRAM(IND,MIN=0,MAX = NB-1)

; Count the fraction of empty bins in the middle half of the histogram:
 X14 = (XX[NB-1]-XX[0])/4.+X1
 X34 = XX[NB-1]-(XX[NB-1]-XX[0])/4.
 Q=WHERE( (YY EQ 0.) AND (XX GT X14) AND (XX LT X34), COUNT )
 IF (COUNT GT NB/10) AND (NB GT MINBIN) THEN BEGIN  ; 20% EMPTY
   NB = 3*NB/4
   IF NB LT (2*N) THEN GOTO,tryagain
ENDIF

; Fill in ZX,ZY:
 MB = 2*NB+2
 ZX = FLTARR(MB)  &  ZY = FLTARR(MB)
 IT = INDGEN(NB)*2 + 1

 ZY[IT] = YY   &  ZY[IT+1] = YY

 ZX[0] = X1       
 ZX[IT] = XX - DX/2. &   ZX[IT+1] = XX + DX/2.
 ZX[MB-1] = X2 

IF KEYWORD_SET(WHATEVER) THEN RETURN

; Plot, then fill, the bins:
 YTOP = MAX(YY[1:NB-2])
 YY[0] = YY[0] < YTOP
 YY[NB-1] = YY[NB-1] < YTOP
 cgPLOT,XX,YY,XRAN=[X1-DX,X2+DX],YRAN=[0.,1.1*YTOP],PSYM=10,_EXTRA=_extra
 FOR J=0,NB-1 DO BEGIN
  IF YY[J] GT 0 THEN BEGIN
     A=[XX[J]-DX/2.,XX[J]+DX/2.,XX[J]+DX/2.,XX[J]-DX/2.] 
     B=[0.,0.,YY[J],YY[J]]
     cgcolorFILL,A,B,orientation=45,noclip=0
  ENDIF
ENDFOR

RETURN
END

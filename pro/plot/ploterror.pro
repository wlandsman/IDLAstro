PRO ploterror, x, y, xerr, yerr, NOHAT=hat, HATLENGTH=hln, ERRTHICK=eth, $
      ERRSTYLE=est, TYPE=itype, XRANGE = xrange, XLOG=xlog, YLOG=ylog, $
      NSKIP = nskip, NOCLIP = noclip, ERRCOLOR= ecol, YRANGE = yrange, $
      NSUM = nsum, WINDOW=window, _EXTRA = pkey

;+
; NAME:
;     PLOTERROR
; PURPOSE:
;     Plot data points with accompanying X or Y error bars.
; EXPLANATION:
;     This is a greatly enhanced version of the standard IDL Library routine
;     PLOTERR
;
;     Note that since December 2013 a similar error plotting capablity is 
;     available in CGPLOT (http://www.idlcoyote.com/programs/cgplot.pro).
;
; CALLING SEQUENCE:
;     ploterror, [ x,]  y, [xerr], yerr [, TYPE=, /NOHAT, HATLENGTH= , NSUM =
;                  ERRTHICK=, ERRSTYLE=, ErrcolOR=, NSKIP=, .. PLOT keywords]
;
; INPUTS:
;     X = array of abscissas.
;     Y = array of Y values.
;     XERR = array of error bar values (along X)
;     YERR = array of error bar values (along Y)
;
; OPTIONAL INPUT KEYWORD PARAMETERS:
;     TYPE = type of plot produced.  The possible types are:
;              TYPE = 0 :       X Linear - Y Linear  (default)
;              TYPE = 1 :       X Linear - Y Log
;              TYPE = 2 :       X Log    - Y Linear
;              TYPE = 3 :       X Log    - Y Log
;              Actually, if 0 is specified, the XLOG and YLOG keywords
;              are used.  If these aren't specified, then a linear-linear
;              plot is produced.  This keyword is available to maintain
;              compatibility with the previous version of PLOTERROR.
;     /NOHAT     = if specified and non-zero, the error bars are drawn
;              without hats.
;     HATLENGTH = the length of the hat lines in device units used to cap the 
;              error bars.   Defaults to !D.X_VSIZE / 100).
;     ERRTHICK  = the thickness of the error bar lines.  Defaults to the
;              THICK plotting keyword.
;     ERRSTYLE  = the line style to use when drawing the error bars.  Uses
;              the same codes as LINESTYLE.
;     ERRCOLOR =  String (e.g. 'red') or scalar integer (0 - !D.N_TABLE)
;              specifying the color to use for the error bars.   See CGCOLOR()
;              for a list of possible color names.  See 
;              http://www.idlcoyote.com/cg_tips/legcolor.php
;              for a warning about the use of indexed color
;     NSKIP = Integer specifying the error bars to be plotted.   For example,
;              if NSKIP = 2 then every other error bar is plotted; if NSKIP=3
;              then every third error bar is plotted.   Default is to plot
;              every error bar (NSKIP = 1)
;     NSUM =  Number of points to average over before plotting, default=!P.NSUM
;             The errors are also averaged, and then divided by sqrt(NSUM).   
;             This  approximation is meaningful only when the neighboring error
;             bars have similar sizes.    PLOTERROR does not pass the NSUM 
;             keyword to the PLOT command, but rather computes the binning 
;             itself using the  FREBIN function.
;     TRADITIONAL - If set to 0 then a black plot is drawn on a white background
;             in the graphics window.   The default value is 1, giving the
;             traditional black background for a graphics window.
;     WINDOW - Set this keyword to plot to a resizeable graphics window
;            
;
;     Any valid keywords to the cgPLOT command (e.g. PSYM, YRANGE, AXISCOLOR  
;     SYMCOLOR, ASPECT) are also accepted by PLOTERROR via the _EXTRA facility.
;
; RESTRICTIONS:
;       Arrays must not be of type string, and there must be at least 1 point.
;       If only three parameters are input, they will be taken as X, Y and
;       YERR respectively.
;
;       PLOTERROR cannot be used for asymmetric error bars.   Instead use
;       OPLOTERROR with the /LOBAR and /HIBAR keywords.
;
;       Any data points with NAN values in the X, Y, or error vectors are 
;       ignored.
; EXAMPLE:
;       Suppose one has X and Y vectors with associated errors XERR and YERR
;
;       (1) Plot Y vs. X with both X and Y errors and no lines connecting
;           the points
;                  IDL> ploterror, x, y, xerr, yerr, psym=3
;
;       (2) Like (1) but plot only the Y errors bars and omits "hats"
;                  IDL> ploterror, x, y, yerr, psym=3, /NOHAT
;
; WARNING:
;       This an enhanced version of the procedure PLOTERR in the standard IDL
;       distribution.    It was renamed from PLOTERR to PLOTERROR in June 1998
;       in the IDL Astronomy Library to avoid conflict with the RSI procedure.
;
; PROCEDURE:
;       A plot of X versus Y with error bars drawn from Y - YERR to Y + YERR
;       and optionally from X - XERR to X + XERR is written to the output device
;
; PROCEDURE CALLS:
;     cgPlot, cgPlots
;     FREBIN - used to compute binning if NSUM keyword is present
; MODIFICATION HISTORY:
;     William Thompson        Applied Research Corporation  July, 1986
;     DMS, April, 1989        Modified for Unix
;     Michael R. Greason      ST Systems
;     May, 1991               Added most of the plotting keywords, put hats
;                               on the error bars.
;     K. Venkatakrishna       Added option to plot xerr, May, 1992
;     Michael R. Greason      Corrected handling of reversed axes.  Aug. 1992
;     W. Landsman             Use _EXTRA keyword                    July 1995
;     W. Landsman             Plot more than 32767 points           Feb 1996
;     W. Landsman     Fix Y scaling when only XRANGE supplied       Nov 1996
;     W. Landsman     Added NSKIP keyword                           Dec 1996
;     W. Landsman     Use XLOG, YLOG instead of XTYPE, YTYPE        Jan 1998
;     W. Landsman     Rename to PLOTERROR, OPLOTERROR               Jun 1998
;     W. Landsman  Better default scaling when NSKIP supplied       Oct 1998 
;     W. Landsman  Ignore !P.PSYM when drawing error bars           Jan 1999
;     W. Landsman  Handle NSUM keyword correctly                    Aug 1999
;     W. Landsman  Fix case of /XLOG but no X error bars            Oct 1999
;     W. Landsman  Work in the presence of NAN values               Nov 2000
;     W. Landsman  Improve logic when NSUM or !P.NSUM is set        Jan 2001
;     W. Landsman  Only draw error bars with in XRANGE (for speed)  Jan 2002
;     W. Landsman  Fix Jan 2002 update to work with log plots       Jun 2002
;     W. Landsman  Added _STRICT_EXTRA                              Jul 2005
;     W. Landsman/D.Nidever Fixed case of logarithmic axes reversed Mar 2009
;     W. Landsman/S. Koch  Allow input to be a single point         Jan 2010
;     W. Landsman  Add Coyote Graphics                              Feb 2011
;     W. Landsman Make keyword name ERRCOLOR instead of ECOLOR 
;                 Speedup when no ERRCOLOR defined                  Feb 2011
;     D. Fanning Use PLOTS instead of CGPLOTS for speed             Jan 2012
;-
;                       Check the parameters.
 On_error, 2
 compile_opt idl2

 np = N_params()
 IF (np LT 2) THEN BEGIN
        print, "PLOTERROR must be called with at least two parameters."
        print, "Syntax: ploterror, [x,] y, [xerr], yerr"
        RETURN
 ENDIF
 
IF Keyword_Set(window) THEN BEGIN

   currentWindow = cgQuery(/CURRENT, COUNT=wincnt)
   IF wincnt EQ 0 THEN replaceCmd = 0 ELSE replaceCmd=1
   cgWindow, 'ploterror', x, y, xerr, yerr, NOHAT=hat, HATLENGTH=hln, ERRTHICK=eth, $
      ERRSTYLE=est, TYPE=itype, XRANGE = xrange, XLOG=xlog, YLOG=ylog, $
      NSKIP = nskip, NOCLIP = noclip, ERRCOLOR= ecol, YRANGE = yrange, $
      NSUM = nsum, _EXTRA = pkey, REPLACECMD=replaceCmd
   RETURN
   
ENDIF

; Error bar keywords (except for HATLENGTH; this one will be taken care of 
; later, when it is time to deal with the error bar hats).

 hat =  ~keyword_set(hat)
 setdefaultvalue, eth, !P.thick
 setdefaultvalue, est, 0
 setdefaultvalue, ecol, 'Opposite'
 setdefaultvalue, noclip, 0
 setdefaultvalue, nskip, 1
 setdefaultvalue, nsum, !p.nsum
 setdefaultvalue, traditional, 0
 
;				Other keywords.

 IF (keyword_set(itype)) THEN BEGIN
	CASE (itype) OF
		   1 :  ylog = 1	; X linear, Y log
		   2 :  xlog = 1	; X log, Y linear
		   3 :  BEGIN		; X log, Y log
			xlog = 1
			ylog = 1
			END
		ELSE : 
	ENDCASE
 ENDIF
 setdefaultvalue,xlog, 0
 setdefaultvalue,ylog, 0
 ;			If no x array has been supplied, create one.  Make
;			sure the rest of the procedure can know which parameter
;			is which.

 IF np EQ 2 THEN BEGIN			; Only Y and YERR passed.
	yerr = y
	yy = x
	xx = lindgen(n_elements(yy))
        xerr = make_array(size=size(xx))

 ENDIF ELSE IF np EQ 3 THEN BEGIN 	; X, Y, and YERR passed.
        yerr = xerr
        yy = y
        xx = x

 ENDIF ELSE BEGIN                        ; X, Y, XERR and YERR passed.
	yy = y
        g = where(finite(xerr))
        xerr[g] = abs(xerr[g])
	xx = x
 ENDELSE

 g = where(finite(yerr))               ;Don't take absolute value of NAN values
 yerr[g] = abs(yerr[g])

;			Determine the number of points being plotted.  This
;			is the size of the smallest of the three arrays
;			passed to the procedure.  Truncate any overlong arrays.

 n = N_elements(xx) < N_elements(yy)

 IF np GT 2 then n = n < N_elements(yerr)   
 IF np EQ 4 then n = n < N_elements(xerr)

 IF n LT 1 THEN $
	message,'ERROR - No data points to plot.'

 xx = xx[0:n-1]
 yy = yy[0:n-1]
 yerr = yerr[0:n-1]
 IF np EQ 4 then xerr = xerr[0:n-1]

; If NSUM is greater than one, then we need to smooth ourselves (using FREBIN)

 if nsum GT 1 then begin
      n1 = float(n) / nsum
      n  = long(n1)
      xx = frebin(xx, n1)
      yy = frebin(yy, n1)
      yerror = frebin(yerr,n1)/sqrt(nsum)
      if NP EQ 4 then xerror = frebin(xerr,n1)/sqrt(nsum)
  endif else begin
      yerror = yerr
      if NP EQ 4 then xerror = xerr
  endelse


; If no y-range was passed via keyword or system variable, force one large 
; enough to display all the data and the entire error bars.     
; If a reversed y-range was passed, switch ylo and yhi.

 ylo = yy - yerror
 yhi = yy + yerror

 setdefaultvalue, yrange, !Y.RANGE
 IF yrange[0] EQ yrange[1] THEN BEGIN
	if keyword_set( XRANGE ) then  begin
		good = where( (xx GT min(xrange)) and (xx LT max(xrange)), Ng )
		if Ng EQ 0 then message, $
		   'ERROR - No X data within specified X range'
		yrange = [min(ylo[good],/NAN), max(yhi[good], /NAN)]
	endif else yrange = [min(ylo,/NAN), max(yhi, /NAN)]
 ENDIF 
;        Similarly for x-range
 setdefaultvalue, xrange, !X.RANGE
 if NP EQ 4 then begin
   xlo = xx - xerror
   xhi = xx + xerror
   IF xrange[0] EQ xrange[1] THEN xrange = [min(xlo,/NAN), max(xhi,/NAN)]
 endif

; Plot the positions.    Always set NSUM = 1 since we already took care of 
; smoothing with FREBIN

 cgPlot, xx, yy, XRANGE = xrange, YRANGE = yrange, XLOG = xlog, YLOG = ylog, $
         _EXTRA = pkey, NOCLIP = noclip, NSum= 1, TRADITIONAL=traditional

;	Plot the error bars.   Compute the hat length in device coordinates
;       so that it remains fixed even when doing logarithmic plots.

    data_low = convert_coord(xx,ylo,/TO_DEVICE)
    data_hi = convert_coord(xx,yhi,/TO_DEVICE)
    if NP EQ 4 then begin
       x_low = convert_coord(xlo,yy,/TO_DEVICE)
       x_hi = convert_coord(xhi,yy,/TO_DEVICE)
    endif
    ycrange = !Y.crange
    xcrange = !x.crange
    sv_psym = !P.PSYM & !P.PSYM = 0
    
    if ylog EQ 1 then ylo = ylo > 10^min(ycrange)    
    if (xlog EQ 1) && (np EQ 4) then  xlo = xlo > 10^min(xcrange)    
	                   
; Only draw error bars for X values within XCRANGE
    if xlog EQ 1 then xcrange = 10^xcrange
    g = where((xx GT xcrange[0]) and (xx LE xcrange[1]), Ng)

    if (Ng GT 0) && (Ng NE n) then begin  
          istart = min(g, max = iend)  
    endif else begin
          istart = 0L & iend = n-1
    endelse
    
    ecol = cgDefaultColor(ecol, Default='opposite')
    IF Size(ecol, /TNAME) EQ 'STRING' THEN ecol = cgColor(ecol)
					 
 FOR i = istart, iend, Nskip DO BEGIN     

    Plots, [xx[i],xx[i]], [ylo[i],yhi[i]], LINESTYLE=est,THICK=eth,  $
		NOCLIP = noclip, COLOR = ecol
;                                                         Plot X-error bars 
    if np EQ 4 then Plots, [xlo[i],xhi[i]],[yy[i],yy[i]],LINESTYLE=est, $
		THICK=eth, COLOR = ecol, NOCLIP = noclip
	IF (hat NE 0) THEN BEGIN
		IF (N_elements(hln) EQ 0) THEN hln = !D.X_VSIZE/100. 
		exx1 = data_low[0,i] - hln/2.
		exx2 = exx1 + hln
		
		Plots, [exx1,exx2], [data_low[1,i],data_low[1,i]], $  
		  COLOR=ecol, $
      LINESTYLE=est,THICK=eth,/DEVICE, noclip = noclip
		Plots, [exx1,exx2], [data_hi[1,i],data_hi[1,i]], $
		 COLOR = ecol, $
     LINESTYLE=est,THICK=eth,/DEVICE, noclip = noclip

;                                                        Plot Y-error bars

                IF np EQ 4 THEN BEGIN
                   IF (N_elements(hln) EQ 0) THEN hln = !D.Y_VSIZE/100.
                   eyy1 = x_low[1,i] - hln/2.
                   eyy2 = eyy1 + hln
                   Plots, [x_low[0,i],x_low[0,i]], [eyy1,eyy2],COLOR = ecol, $
                         LINESTYLE=est,THICK=eth,/DEVICE, NOCLIP = noclip
                   Plots, [x_hi[0,i],x_hi[0,i]], [eyy1,eyy2],COLOR = ecol, $
                         LINESTYLE=est,THICK=eth,/DEVICE, NOCLIP = noclip
                ENDIF
	ENDIF
    NOPLOT:
 ENDFOR
 !P.PSYM = sv_psym
;
 RETURN
 END

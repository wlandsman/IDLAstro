PRO plothist, arr, xhist,yhist, BIN=bin,  NOPLOT=NoPlot, $
                 OVERPLOT=Overplot, PSYM = psym, Peak=Peak, $
                 Fill=Fill, FCOLOR=Fcolor, FLINE=FLINE, $
                 FTHICK=FThick, FSPACING=Fspacing, FPATTERN=Fpattern, $
                 FORIENTATION=Forientation, NAN = NAN, $
                 _EXTRA = _extra, Halfbin = halfbin, AUTOBin = autobin, $
                 Boxplot = boxplot, xlog = xlog, ylog = ylog, $
                 yrange = yrange, Color = color,axiscolor=axiscolor, $
                 rotate = rotate, WINDOW=window,XSTYLE=xstyle, YSTYLE = ystyle,$
		 THICK= thick, LINESTYLE = linestyle
;+
; NAME:
;      PLOTHIST
; PURPOSE:
;      Plot the histogram of an array with the corresponding abscissa.
;
; CALLING SEQUENCE:
;      plothist, arr, xhist, yhist, [, BIN=, /FILL, /NOPLOT, /OVERPLOT, PEAK=,
;                                     /AUTOBIN,  ...plotting keywords]
; INPUTS:
;      arr - The array to plot the histogram of.   It can include negative
;            values, but non-integral values will be truncated.              
;
; OPTIONAL OUTPUTS:
;      xhist - X vector used in making the plot  
;              ( = lindgen( N_elements(h)) * bin + min(arr) )
;      yhist - Y vector used in making the plot  (= histogram(arr/bin))
;
; OPTIONAL INPUT KEYWORDS:
;      /AUTOBIN - Automatically determines bin size of the histogram as the
;                 square root of the number of samples. Only valid when BIN
;                 is not set.
;      AXISCOLOR - Color (string or number) of the plotting axes.  
;      BIN -  The size of each bin of the histogram,  scalar (not necessarily
;             integral).  If not present (or zero), the bin size is set to 1.
;      /BOXPLOT - If set, then each histogram data value will be plotted
;             "box style" with vertical lines drawn from Y=0 at each end of 
;              the bin width
;      COLOR - Color (number or string) of the plotted data.    See CGCOLOR
;              for a list of available color names. 
;      /HALFBIN - Set this keyword to a nonzero value to shift the binning by
;              half a bin size.     This is useful for integer data, where e.g.
;              the bin for values of 6 will go from 5.5 to 6.5.   The default
;              is to set the HALFBIN keyword for integer data, and not for
;              non-integer data.     
;      /NAN - If set, then check for the occurence of IEEE not-a-number values
;      /NOPLOT - If set, will not plot the result.  Useful if intention is to
;             only get the xhist and yhist outputs.
;      /OVERPLOT - If set, will overplot the data on the current plot.  User
;            must take care that only keywords valid for OPLOT are used.
;      PEAK - if non-zero, then the entire histogram is normalized to have
;             a maximum value equal to the value in PEAK.  If PEAK is
;             negative, the histogram is inverted.
;      /FILL - if set, will plot a filled (rather than line) histogram.
;      /ROTATE - if set, the plot is rotated onto it's side, meaning the bars 
;             extend from left to right.  Xaxis corresponds to the count within 
;             in each bin.      Useful for placing a histogram plot
;             at the side of a scatter plot, as shown at the bottom of
;               http://www.dur.ac.uk/j.r.mullaney/pages/software.php
;       WINDOW - Set this keyword to plot to a resizeable graphics window
;
;
; The following keywords take effect only if the FILL keyword is set:
;      FCOLOR - color (string or number) to use for filling the histogram
;      /FLINE - if set, will use lines rather than solid color for fill (see
;              the LINE_FILL keyword in the cgcolorfill routine)
;      FORIENTATION - angle of lines for fill (see the ORIENTATION keyword
;              in the cgcolorfill routine)
;      FPATTERN - the pattern to use for the fill (see the PATTERN keyword
;              in the cgcolorfill routine)
;      FSPACING - the spacing of the lines to use in the fill (see the SPACING
;              keyword in the cgcolorfill routine)
;      FTHICK - the thickness of the lines to use in the fill (see the THICK
;              keyword in the cgcolorfill routine)
;
; Any input keyword that can be supplied to the cgPLOT procedure (e.g. XRANGE,
;    AXISCOLOR, LINESTYLE, /XLOG, /YLOG) can also be supplied to PLOTHIST.
;
; EXAMPLE:
;       (1) Create a vector of random 1000 values derived from a Gaussian of 
;       mean 0, and sigma of 1.    Plot the histogram of these values with a 
;       binsize of 0.1, and use a box plotting style.
;
;       IDL> a = randomn(seed,1000)
;       IDL> plothist,a, bin = 0.1, /boxplot
;
;       (2) As before, but fill the plot with diagonal lines at a 45 degree 
;           angle
;
;       IDL> plothist,a, bin=0.1, /fill, /fline, forient=45
;
; NOTES:
;       David Fanning has written a similar program CGHISTOPLOT with more graphics
;       options:   See http://www.idlcoyote.com/programs/cghistoplot.pro
; MODIFICATION HISTORY:
;        Written     W. Landsman            January, 1991
;        Add inherited keywords W. Landsman        March, 1994
;        Use ROUND instead of NINT  W. Landsman   August, 1995
;        Add NoPlot and Overplot keywords.   J.Wm.Parker  July, 1997
;        Add Peak keyword.   J.Wm.Parker  Jan, 1998
;        Add FILL,FCOLOR,FLINE,FPATTERN,FSPACING keywords. J.Wm.Parker Jan, 1998
;        Add /NAN keyword        W. Landsman October 2001
;        Don't plot out of range with /FILL, added HALFBIN keyword, make
;        half bin shift default for integer only W. Landsman/J. Kurk May 2002
;        Add BOXPLOT keyword, use exact XRANGE as default W.L.  May 2006
;        Allow use of /XLOG and /YLOG keywords  W.L. June 2006
;        Adjust Ymin when /YLOG is used  W. L.  Sep 2007
;        Added AXISCOLOR keyword, fix color problem with overplots WL Nov 2007
;        Check when /NAN is used and all elements are NAN  S. Koposov Sep 2008
;        Added /ROTATE keyword to turn plot on its side. J. Mullaney, 2009.
;        Added FTHICK keyword for thickness of fill lines. L. Anderson Oct. 2010
;        Use Coyote Graphics  W. Landsman Feb 2011
;        Explicit XSTYLE, YSTYLE keywords to avoid _EXTRA confusion WL. Aug 2011
;        Fix PLOT keyword problem with /ROTATE  WL  Dec 2011
;        Fix problems when /XLOG is set A. Kimball/WL April 2013
;        Fix FILL to work when axis is inverted (xcrange[0] >
;          xcrange[1]) T.Ellsworth-Bowers July 2014
;-
;			Check parameters.
 On_error,2
 compile_opt idl2

 if N_params() LT 1 then begin   
	print,'Syntax - plothist, arr, [xhist,yhist, ' 
        print, '         [/AUTOBIN, BIN=, /BOXPLOT, HALFBIN=, PEAK=, /NOPLOT,'
	print, '         /OVERPLOT, /FILL...plotting keywords]' 
        print,'Fill keywords: FCOLOR=, /FLINE, FORIENTATION=, FPATTERN=,' + $
              'FSPACING= '
	return
 endif

 if N_elements( arr ) LT 2 then message, $
      'ERROR - Input array must contain at least 2 elements'
 arrmin = min( arr, MAX = arrmax)
 if ( arrmin EQ arrmax ) then message, $
       'ERROR - Input array must contain distinct values'

 ;Determining how to calculate bin size:
 if ~keyword_set(BIN) then begin
    if keyword_set(AUTOBIN) then begin
       bin = (max(arr)-min(arr))/sqrt(N_elements(arr))
    endif else begin
       bin = 1.0
    endelse
 endif else begin
    bin = float(abs(bin))
 endelse

; Compute the histogram and abscissa.    
; Determine if a half bin shift is 
; desired (default for integer data)     
 if N_elements(halfbin) EQ 0 then begin 
    dtype = size(arr,/type)
    halfbin = (dtype NE 4) and (dtype NE 5) ;Non-integer data?
 endif 
 
 halfbin = keyword_set(halfbin)
 
 if keyword_set(NAN) then begin
      good = where(finite(arr), ngoods )
      if ngoods eq 0 then $
              message, 'ERROR - Input array contains no finite values'

      if halfbin then y = round( ( arr[good] / bin)) $
                 else y = floor( ( arr[good] / bin))
 endif else if halfbin then y = round( ( arr / bin)) $
                       else y = floor( ( arr/ bin)) 
 
 ;Determine number in each bin:
 yhist = histogram( y )
 N_hist = N_elements( yhist )
 
 ;Positions of each bin:
 xhist = lindgen( N_hist ) * bin + min(y*bin) 
 
 if ~halfbin then xhist = xhist + 0.5*bin

;;;
;   If renormalizing the peak, do so.
;
if keyword_set(Peak) then yhist = yhist * (Peak / float(max(yhist)))

;;;
;   If not doing a plot, exit here.
;
 if keyword_set(NoPlot) then return
 
 ;JRM;;;;;
 xra_set = keyword_set(XRANGE)?1:0
 xst_set = keyword_set(xstyle)?1:0
 yst_set = keyword_set(ystyle)?1:0
;JRM;;;;;
 
 if keyword_set(over) then begin ;if overplotting, was original plot a log?
      if N_elements(ylog) EQ 0 then ylog = !Y.type
      if N_elements(xlog) EQ 0 then xlog = !X.type
 endif     
 if N_elements(PSYM) EQ 0 then psym = 10         ;Default histogram plotting
 if ~keyword_set(XRANGE) then xrange = [ xhist[0]-bin ,xhist[N_hist-1]+bin ]
 if ~keyword_set(xstyle) then xstyle=1
 
 if  keyword_set(ylog) then begin 
     ymin = min(yhist) GT 1 ? 1 : 0.1
     if N_elements(yrange) EQ 2 then ymin = ymin < yrange[0] 
     ;ydata contains the y-positions where the lines should be linked.
     ydata = [ymin, yhist>ymin, ymin] 
  endif else ydata = [0, yhist, 0]
 ;xdata contains the y-positions where the lines should be linked.
 xdata = [xhist[0] - bin, xhist, xhist[n_hist-1]+ bin]
 if keyword_set(xlog) then xrange[0] = xrange[0]>1
 
 ;JRM;;;;;;;;;;;
  IF n_elements(rotate) EQ 1 THEN BEGIN
    old_xdata = xdata
    old_ydata = ydata
    xdata = old_ydata
    ydata = old_xdata
    
    old_xhist=xhist
    old_yhist=yhist
    xhist=old_yhist
    yhist=old_xhist
    
    ;If xrange is not set.
    ;Then the auto x- range by setting xrange to [0,0].
    if ~xra_set then xrange=[0,0]
    if ~xst_set then xstyle=0
    if ~yst_set then ystyle=1
    
 ENDIF
 
 
  if ~keyword_set(Overplot) then begin

     cgplot, xdata , ydata,  $ 
           PSYM = psym, _EXTRA = _extra,xrange=xrange,axiscolor=axiscolor, $
           xstyle=xstyle, xlog = xlog, ylog = ylog, yrange=yrange, $
           ystyle=ystyle, /nodata,window=window
	   if keyword_Set(window) then cgcontrol,execute=0
  endif
;JRM;;;;;;;;;;;;;

;;;
;   If doing a fill of the histogram, then go for it.
;
  if N_elements(color) EQ 0 then color = cgcolor('opposite')
 
 if keyword_set(Fill) then begin
    ;JRM;;;;;;;;;;;
    xcrange = keyword_set(xlog)? 10^!X.CRANGE : !X.CRANGE
    ycrange = keyword_set(ylog)? 10^!Y.CRANGE : !Y.CRANGE
       
    IF n_elements(rotate) EQ 0 THEN BEGIN
       Xfill = transpose([[Xhist-bin/2.0],[Xhist+bin/2.0]])
       Xfill = reform(Xfill, n_elements(Xfill))
       Xfill = [Xfill[0], Xfill, Xfill[n_elements(Xfill)-1]]
       Yfill = transpose([[Yhist],[Yhist]])
       Yfill = reform(Yfill, n_elements(Yfill))
    
       if keyword_set(ylog) then Yfill = [ycrange[0]/10, Yfill, ycrange[0]/10] $
       else yfill = [0, yfill, 0 ]
       
    ENDIF ELSE BEGIN
       Xfill = transpose([[Xhist],[Xhist]])
       Xfill = reform(Xfill, n_elements(Xfill))
       Yfill = transpose([[Yhist-bin/2.0],[Yhist+bin/2.0]])
       Yfill = reform(Yfill, n_elements(Yfill))
       Yfill = [Yfill[0], Yfill, Yfill[n_elements(Yfill)-1]]
    
       if keyword_set(xlog) then Xfill = [xcrange[0]/10, xfill, xcrange[0]/10] $
       else xfill = [0, xfill, 0 ]
    ENDELSE
    ;JRM;;;;;;;;;;;

    ;; TPEB;;;;;;;;;;;
    ;; Check if plot ranges are reversed (i.e. large to small)
    Xfill = (XCRANGE[0] GT XCRANGE[1]) ? Xfill > XCRANGE[1] < XCRANGE[0] : $
            Xfill > XCRANGE[0] < XCRANGE[1] ;Make sure within plot range
    
    Yfill = (YCRANGE[0] GT YCRANGE[1]) ? Yfill > YCRANGE[1] < YCRANGE[0] : $
            Yfill > YCRANGE[0] < YCRANGE[1]
    ;; TPEB;;;;;;;;;;;
    
    if keyword_set(Fcolor) then Fc = Fcolor else Fc = 'Opposite'
    if keyword_set(Fline) then begin
       Fs =  keyword_set(Fspacing) ? Fspacing : 0
       Fo =  keyword_set(Forientation) ? Forientation: 0
       cgcolorfill, Xfill,Yfill, color=Fc, /line_fill, spacing=Fs, orient=Fo, $
         thick = fthick, WINDOW=window
    
    endif else begin
   
       if keyword_set(Fpattern) then begin
          cgcolorfill, Xfill,Yfill, color=Fc, pattern=Fpattern, window=window
       endif else begin
          cgcolorfill, Xfill,Yfill, color=Fc,window=window
       endelse
    endelse
 endif
 
 ;JRM;;;;;;;;;;;
 IF n_elements(rotate) GT 0 THEN BEGIN
    ;Need to determine the positions and use plotS.
    ycrange = keyword_set(ylog)? 10^!Y.CRANGE : !Y.CRANGE
    xcrange = keyword_set(xlog)? 10^!X.CRANGE : !X.CRANGE
    cgplots, xdata[0]<xcrange[1], ycrange[1]<(ydata[0]-bin/2)>ycrange[0], $
           color=color,Thick = thick, LINESTYLE = linestyle, ADDCMD=window
    cgplots, xdata[0]<xcrange[1], ycrange[1]<(ydata[1]-bin/2)>ycrange[0], $
           color=color,THICK = thick, LINESTYLE= linestyle, ADDCMD=window
    FOR i=1, n_elements(xdata)-2 DO BEGIN
       cgplots, xdata[i]<xcrange[1], ycrange[1]<(ydata[i]-bin/2)>ycrange[0], $
              color=color, THICK=thick, LINESTYLE= linestyle, $
	      /CONTINUE,ADDCMD=window
       cgplots, xdata[i]<xcrange[1], ycrange[1]<(ydata[i+1]-bin/2)>ycrange[0], $
              color=color, /CONTINUE,THICK=thick, LINESTYLE=linestyle, $
	       ADDCMD=window
    ENDFOR
    cgplots, xdata[i]<xcrange[1], ycrange[1]<(ydata[i]-bin/2)>ycrange[0], $
           color=color, /CONTINUE, THICK=thick, LINESTYLE = linestyle, $
	   ADDCMD=window
 ENDIF ELSE BEGIN
    cgplot, /over, xdata, ydata, XSTYLE= xstyle, YSTYLE = ystyle, $ 
           PSYM = psym, THICK=thick, LINESTYLE = linestyle, $
	    _EXTRA = _extra,color=color,ADDCMD=window
    ENDELSE
 ;JRM;;;;;;;;;;;
 
 ; Make histogram boxes by drawing lines in data color.
if keyword_set(boxplot) then begin
   ;JRM;;;;;;;;;;;
   IF n_elements(rotate) EQ 0 THEN BEGIN
      ycrange = keyword_set(ylog)? 10^!Y.CRANGE : !Y.CRANGE
      FOR j =0 ,N_Elements(xhist)-1 DO BEGIN
         cgPlotS, [xhist[j], xhist[j]]-bin/2, [YCRange[0], yhist[j], Ycrange[1]], $
                Color=Color,noclip=0, THICK=thick, LINESTYLE = linestyle, $
		_Extra=extra,ADDCMD=window
      ENDFOR 
      
   ENDIF ELSE BEGIN
      xcrange = keyword_set(xlog)? 10^!X.CRANGE : !X.CRANGE
      FOR j =0 ,N_Elements(xhist)-1 DO BEGIN
         cgPlotS, [xcrange[0], xhist[j]<xcrange[1]], [yhist[j], $
	            yhist[j]]-bin/2, ADDCMD=window, THICK=thick, $
                    LINESTYLE = linestyle, Color=Color, noclip=0
      ENDFOR 
   ENDELSE
   ;JRM;;;;;;;;;;;
endif

 if keyword_Set(window) then cgcontrol,execute=1
 return
 end

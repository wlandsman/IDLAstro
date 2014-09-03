pro lineid_plot,wave,flux,wline,text1,text2, extend=extend, $
	lcharthick = lcharthick,lcharsize=lcharsize,window=window, $
	 _EXTRA = extra
;+
; NAME:
;	LINEID_PLOT
; PURPOSE:
;	 Plot spectrum with specified line identifications annotated at the
;	 top of the plot.
;
; CALLING SEQUENCE:
;	lineid_plot, wave, flux, wline, text1, [ text2, 
;			LCHARSIZE=, LCHARTHICK=, EXTEND =, ...plotting keywords]
;
; INPUTS:
;	wave - wavelength vector for the plot
;	flux - flux vector
;	wline - wavelength vector of line identifications.  (only the lines 
;		between	the plot limits will be used)
;	text1 - string array of text to be used to annotate each line
;	text2 - (OPTIONAL) second string array of text to be used for
;		line annotation.  Since the text is written with
;		proportional spaced characters, TEXT2 can be used if
;		you want two sets of annotation to be aligned:
;
;		eg:	Cr IV  1390.009
;			Fe V   1390.049
;			Ni IV  1390.184
;			    instead of
;			Cr IV 1390.009
;			Fe V 1390.049
;			Ni IV 1390.184
;
; OPTIONAL KEYWORD INPUTS:
;	EXTEND - specifies that the annotated lines should have a dotted line 
;		extended to the spectrum to indicate the line position.  
;		EXTEND can be a scalar (applies to all lines) or a vector with
;		a different value for each line.  The value of EXTEND gives 
;		the line IDL plot line thickness for the dotted lines.
;		If EXTEND is a vector each dotted line can have a different 
;		thickness.  A value of 0 indicates that no dotted line is to 
;		be drawn. (default = scalar 0)
;	LCHARSIZE - the character size of the annotation for each line.
;		If can be a vector so that different lines are annotated with 
;		different size characters.  LCHARSIZE can be used to make 
;		stronger lines have a larger annotation. (default = scalar 1.0).
;	LCHARTHICK = the character thickness of the annotation for each line. 
;		It can be a vector so that different lines are annotated with 
;		characters of varying thickness.   LCHARTHICK can be used to 
;		make stronger lines have a bolder annotation. 
;		(default = !p.charthick)
;
;	LINEID_PLOT uses the _EXTRA facility to allow the use of any cgPLOT
;	keywords (e.g. AXISCOLOR, LINESTYLE, CHARSIZE) to be passed to the 
;       plot.
;
; SIDE EFFECTS:
;	Program uses SET_VIEWPORT to set the !P.POSITION parameter to allow
;	room for the annotation.   This system variable can be reset to the 
;	default value by setting !P.POSTION=0 or typing SET_VIEWPORT with no 
;	parameters
;
; OPERATIONAL NOTES:
;	Once the program has completed, You can use OPLOT to draw additional
;	plots on the display. 
;
;	If your annotated characters are not being rotated properly,
;	try setting !P.FONT to a non zero value.
; EXAMPLE:
;	Annotate some interstellar lines between 1240 and 1270 A.
;
;	IDL> w = 1240+ indgen(300)*0.1    ;Make a wavelength vector
;	IDL> f = randomn(seed,300)        ;Random flux vector
;	IDL> id = ['N V','Si II','Si II','Si II']   ;Line IDs
;	IDL> wl = [1242.80,1260.42,1264.74,1265.00] ;Line positions
;	IDL> lineid_plot,w,f,wl,id,wl,/ext
;
;	Note that LINEID_PLOT is smart enough not to overlap the annotation
;	for the two closely spaced lines at 1264.74 and 1265.00	
; HISTORY:
;	version 1  D. Lindler Jan, 1992
;	Sept 27, 1993  DJL  fixed bug in /extend option
;	Apr 19, 1994 DJL corrected bug in sorting of charthick (cthick)
;	Sep 1996, W. Landsman,  added _EXTRA keyword, changed keyword names
;		CHARTHICK==>LCHARTHICK, CHARSIZE==>LCHARSIZE
;       Work with !P.MULTI   W. Landsman   December 2003
;       Use Coyote graphics routines  W. Landsman February 2011
;-
;----------------------------------------------------------------------------
	On_error,2

	if n_params() lt 4 then begin
	   print,'Syntax - LINEID_PLOT, wave, flux, wline, text1 [,text2, '
	   print,'       LCHARTHICK=, EXTEND=, LCHARSIZE= ...plotting keywords]'
	   return
	end
;
; initialization
;

	setdefaultvalue, lcharsize, 1
	n = n_elements(wline)
	setdefaultvalue,text2,strarr(n)
	if n_elements(lcharsize) eq 1 then csize = replicate(lcharsize,n) $
				     else csize = lcharsize
	setdefaultvalue, extend, 0
	if n_elements(extend) eq 1 then ethick = replicate(extend,n) $
				   else ethick = extend
	if n_elements(lcharthick) eq 0 then cthick = !p.charthick $
				      else cthick = lcharthick
	if n_elements(cthick) eq 1 then cthick = replicate(cthick,n)
;
; First make a plot without any data to get the region size.    Then use
; the position keyword to assign a plot area that allows room for the 
; line annotation and plot the data
;       
        plot,wave,flux,xsty=4,ysty=4,/nodata,/noerase
        x0 = !X.region[0]
        y0 = !Y.region[0]
        xsize = !X.region[1] - x0
        ysize = !Y.region[1] - y0
        pos = [x0+xsize*0.13,y0+ysize*0.1, x0+xsize*0.95, y0+ysize*0.65]
	cgplot,wave,flux,_EXTRA=extra,pos = pos, Window=window       
        if keyword_set(window) then cgcontrol,execute=0
;
; get data ranges
;
	xmin = !x.crange[0]
	xmax = !x.crange[1]
	ymin = !y.crange[0]
	ymax = !y.crange[1]
	xrange = xmax-xmin
	yrange = ymax-ymin
;
; find lines within x range and sort them
;
	good = where((wline gt xmin) and (wline lt xmax),nlines)
	if nlines lt 1 then return
	wl = wline[good]
	csize = csize[good] & cthick = cthick[good] & ethick = ethick[good]
	txt1 = text1[good] & txt2 = text2[good] 

	sub = sort(wl)
	wl = wl[sub] & csize = csize[sub] & ethick = ethick[sub]
	cthick = cthick[sub] 
	txt1 = txt1[sub] & txt2 = txt2[sub] 
	maxids = 65/(total(csize)/nlines)   ;maximum number of identifications
	if nlines gt maxids then begin
		print,'Too many lines to mark'
		return
	endif

;
; determine character height in wavelength units
;
	char_height = abs(xrange) / 65 * csize
;
; adjust wavelengths of where to print the line ids
;
	wlp = wl		;wavelength to print text
;
; test to see if we can just equally space the annotated lines
;
	if (nlines gt maxids*0.85) and (n_elements(charsize) eq 1) then begin
		wlp = findgen(nlines) * (xrange/(nlines-1)) + xmin
		goto,print_text
	end
;
; iterate to find room to annotate each line
;
	changed = 1		;flag saying we moved a wlp position
	niter = 0
	factor = 0.35		;size of adjustments in text position
	while changed do begin	;iterate
	    changed = 0
	    for i=0,nlines-1 do begin
;
; determine the difference of the annotation from the lines on the
; left and right of it and the required separation
;
		if i gt 0 then begin
			diff1 = wlp[i]-wlp[i-1]
			separation1 = (char_height[i]+char_height[i-1])/2.0
		    end else begin
			diff1 = wlp[i] - xmin + char_height[i]*1.01
			separation1 = char_height[i]
		end

		if i lt (nlines-1) then begin
			diff2 = wlp[i+1] - wlp[i]
			separation2 = (char_height[i]+char_height[i+1])/2.0
		    end else begin
			diff2 = xmax + char_height[i]*1.01 - wlp[i]
			separation2 = char_height[i]
		end
;
; determine if line annotation should be moved
;
		if (diff1 lt separation1) or (diff2 lt separation2) then begin
		    if wlp[i] eq xmin then diff1 = 0
		    if wlp[i] eq xmax then diff2 = 0
		    if diff2 gt diff1 then $
				wlp[i] = (wlp[i] + separation2*factor) < xmax $
			   else wlp[i] = (wlp[i] - separation1*factor) > xmin
		    changed = 1
		endif

	    end

	    if niter eq 300 then $		; fine adjustment for 
			factor = factor/3	; crowded field
			

	    if niter eq 1000 then changed=0	; stop at 1000 iterations
	    niter = niter + 1

	endwhile

;
; print line id's
;
print_text:
	maxcsize = max(csize)
	start_arrow = ymax + yrange/60
	bend1 = ymax + yrange/30
	bend2 = ymax + (yrange/30)*3
	stop_arrow = ymax + (yrange/30)*4
	start_text1 = stop_arrow + yrange/50*maxcsize
	start_text2 = start_text1 +  $
			max(strlen(strtrim(txt1,1)))*yrange/50*maxcsize
	start_text3 = start_text2 +  $
			max(strlen(strtrim(txt2,1)))*yrange/50*maxcsize

	for i=0,nlines-1 do begin
		cgplots,[wl[i],wl[i],wlp[i],wlp[i]], ADDCMD=window, $
		      [start_arrow,bend1,bend2,stop_arrow]
		cgtext,wlp[i] + char_height[i]/2, start_text1, txt1[i], $
		    orientation = 90, size=csize[i], charthick = cthick[i],$
		    window = window
		cgtext,wlp[i] + char_height[i]/2, start_text2, txt2[i], $
		  orientation = 90, size=csize[i], charthick = cthick[i],$
		  window= window
	endfor
;
; extend selected lines down to the spectrum
;
	good = where((ethick gt 0) and (wl gt xmin) and (wl lt xmax),n)
	if n lt 1 then return
	ww = wl[good]
	ethick = ethick[good]
	linterp,wave,flux,ww,ff
	ymax = !y.crange[1]
	ymin = !y.crange[0]
	offset = (ymax-ymin)/20.0
	for i=0,n-1 do $
	   cgplots,[ww[i],ww[i]],[(ff[i]+offset)<ymax>ymin,ymax], $
				line=2,thick = ethick[i],ADDCMD=window
	if keyword_set(window) then cgcontrol,execute=1			

return
end

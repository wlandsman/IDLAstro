Pro arcbar, hdr, arclen, LABEL = label, SIZE = size, THICK = thick, DATA =data, $
            COLOR = color, POSITION = position, NORMAL = normal, $
            SECONDS=SECONDS, FONT=font
;+
; NAME:
;       ARCBAR
; PURPOSE:
;       Draw an arc bar on an image showing the astronomical plate scale
;
; CALLING SEQUENCE:
;       ARCBAR, hdr, arclen,[  COLOR= , /DATA, LABEL= , /NORMAL, POSITION=, 
;                              /SECONDS, SIZE=, THICK=, FONT= ]
;
; INPUTS:
;       hdr - image FITS header with astrometry, string array
; OPTIONAL INPUT:
;       arclen - numeric scalar giving length of bar in arcminutes (default)
;               or arcseconds (if /SECONDS is set).    Default is 1 arcminute 
;
; OPTIONAL KEYWORD INPUTS:
;       COLOR - name  or integer scalar specifying the color to draw the arcbar 
;               See cgColor for a list of available color names
;       /DATA - if set and non-zero, then the POSITION keyword and the arc 
;               length is given in data units
;       LABEL - string giving user defined label for bar.  Default label is size
;               of bar in arcminutes
;       /NORMAL - if this keyword is set and non-zero, then POSITION is given in
;               normalized units
;       POSITION - 2 element vector giving the (X,Y) position in device units 
;               (or normalized units if /NORMAL is set, or data units if /DATA
;               is set) at which to place the  scale bar.   If not supplied, 
;               then the user will be prompted to place the cursor at the 
;               desired position
;       /SECONDS - if set, then arlen is specified in arcseconds rather than
;               arcminutes
;       SIZE  - scalar specifying character size of label, default = 1.0
;       THICK -  Character thickness of the label, default = !P.THICK
;       FONT - scalar font graphics keyword (-1,0 or 1) for text
;
; EXAMPLE:
;       Suppose one has an image array, IM, and FITS header, HDR, with 
;       astrometry.    Display the image and place a 3' arc minute scale bar 
;       at position 300,200 of the current image window
;
;       IDL> cgimage, IM, /scale,/save   ;Use /SAVE to set data coordinates
;       IDL> arcbar, HDR, 3, pos = [300,200],/data
;
; RESTRICTIONS:
;       When using using a device with scalable pixels (e.g. postscript)
;       the data coordinate system must be established before calling ARCBAR.
;       If data coordinates are not set, then ARCBAR assumes that the displayed
;       image size is given by the NAXIS1 keyword in the FITS header.
; PROCEDURE CALLS:
;       AD2XY, EXTAST, GSSSADXY, SXPAR(), SETDEFAULTVALUE, cgPlot, cgText
; REVISON HISTORY:
;       written by L. Taylor (STX) from ARCBOX (Boothman)
;       modified for Version 2 IDL,                     B. Pfarr, STX, 4/91
;       New ASTROMETRY structures               W.Landsman,  HSTX, Jan 94
;       Recognize a GSSS header                 W. Landsman June 94
;       Added /NORMAL keyword                   W. Landsman Feb. 96
;       Use NAXIS1 for postscript if data coords not set,  W. Landsman Aug 96
;       Fixed typo for postscript W. Landsman   Oct. 96
;       Account for zeropoint offset in postscript  W. Landsman   Apr 97
;       Added /DATA, /SECONDS keywords   W. Landsman    July 1998
;       Use device-independent label offset  W. Landsman   August 2001
;       Allow font keyword to be passed.  T. Robishaw Apr. 2006
;       Remove obsolete TVCURSOR command  W. Landsman Jul 2007
;       Use Coyote Graphics W. Landsman  February 2011
;       Fix problem using data coordinates when not in postscript 
;                 W. Landsman January 2013
;-
;
 compile_opt idl2
 On_error,2                                  ;Return to caller

 if N_params() LT 1 then begin
      print, 'Syntax - ARCBAR, hdr,[ arclen, COLOR= '
      print, '         /DATA, LABEL=, /NORM, POS=, /SECONDS, SIZE=, THICK= ]'
      return
 endif

 extast, hdr, bastr, noparams   ;extract astrom params in deg.

 if N_params() LT 2 then arclen = 1      ;default size = 1 arcmin

 setdefaultvalue, size, 1.0
 setdefaultvalue, thick, !P.THICK
 setdefaultvalue, font, !P.FONT

 a = bastr.crval[0]
 d = bastr.crval[1]
 if keyword_set(seconds) then factor = 3600.0d else factor = 60.0
 d1 = d + (1/factor)             ;compute x,y of crval + 1 arcmin

 proj = strmid(bastr.ctype[0],5,3)
  
 case proj of 
        'GSS': gsssadxy, bastr, [a,a], [d,d1], x, y
        else:  ad2xy, [a,a], [d,d1], bastr, x, y 
 endcase

 dmin = sqrt( (x[1]-x[0])^2 + (y[1]-y[0])^2 ) ;det. size in pixels of 1 arcmin

 if ((!D.FLAGS AND 1) EQ 1) || keyword_set(data) then begin          ;Device have scalable pixels?
        if !X.s[1] NE 0 then begin
                dmin = convert_coord( dmin, 0, /DATA, /TO_DEVICE) - $ 
                       convert_coord(    0, 0, /DATA, /TO_DEVICE)  ;Fixed Apr 97
                dmin = dmin[0]
        endif else dmin = dmin/sxpar(hdr, 'NAXIS1' )     ;Fixed Oct. 96
 endif 

 dmini2 = round(dmin * arclen)

 if ~keyword_set( POSITION) then begin
          print,'Position the cursor where you want the bar to begin'
          print,'Hit right mouse button when ready'
          cursor,xi,yi,1,/device
 endif else begin 
        if keyword_set(NORMAL) then begin
                posn = convert_coord(position,/NORMAL, /TO_DEVICE) 
                xi = posn[0] & yi = posn[1]
        endif else if keyword_set(DATA) then begin
                posn = convert_coord(position,/DATA, /TO_DEVICE) 
                xi = posn[0] & yi = posn[1]
        endif else begin
                xi = position[0]   & yi = position[1]
        endelse         
 endelse

 xf = xi + dmini2
 dmini3 = dmini2/10             ;Height of vertical end bars = total length/10.

 cgPlots,[xi,xf],[yi,yi], COLOR=color, /DEV, THICK=thick
 cgPlots,[xf,xf],[ yi+dmini3, yi-dmini3 ], COLOR=color, /DEV, THICK=thick
 cgPlots,[xi,xi],[ yi+dmini3, yi-dmini3 ], COLOR=color, /DEV, THICK=thick

 if ~keyword_set(Seconds) then begin
 if (!D.NAME EQ 'PS') && (FONT EQ 0) then $        ;Postscript Font?
        arcsym='!9'+string(162B)+'!X' else arcsym = "'" 
 endif else begin
 if (!D.NAME EQ 'PS') && (FONT EQ 0) then $        ;Postscript Font?
        arcsym = '!9'+string(178B)+'!X' else arcsym = "''" 
 endelse
 if ~keyword_set( LABEL) then begin
     if (arclen LT 1) then arcstr = string(arclen,format='(f4.2)') $
        else arcstr = string(arclen)
     label = strtrim(arcstr,2) + arcsym 
 endif

 yoffset = round(!D.Y_CH_SIZE/2.)
 cgTEXT,(xi+xf)/2, yi+yoffset, label, SIZE = size,COLOR=color,/DEV,  $
       alignment=0.5, CHARTHICK=thick, FONT=font

 return
 end

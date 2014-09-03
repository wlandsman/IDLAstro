pro imcontour, im, hdr, TYPE=type, PUTINFO=putinfo, XTITLE=xtitle,  $
      YTITLE=ytitle, SUBTITLE = subtitle, XDELTA = xdelta, YDELTA = ydelta, $
      _EXTRA = extra, XMID = xmid, YMID = ymid, OVERLAY = OVERLAY, $
       NOerase = noerase,window=window
;+
; NAME:
;       IMCONTOUR
; PURPOSE:
;       Make a contour plot labeled with astronomical coordinates.
; EXPLANATION:
;       The type of coordinate display is controlled by the keyword TYPE
;       Set TYPE=0 (default) to measure distances from the center of the image
;       (IMCONTOUR will decide whether the plotting units will be in
;       arc seconds, arc minutes, or degrees depending on image size.)
;       Set /TYPE for standard RA and Dec labeling
;
;       By using the /NODATA keyword, IMCONTOUR can also be used to simply
;       provide astronomical labeling of a previously displayed image.
; CALLING SEQUENCE
;       IMCONTOUR, im, hdr,[ /TYPE, /PUTINFO, XDELTA = , YDELTA =, _EXTRA = 
;                            XMID=, YMID= ]
;
; INPUTS:
;       IM - 2-dimensional image array
;       HDR - FITS header associated with IM, string array, must include
;               astrometry keywords.   IMCONTOUR will also look for the
;               OBJECT and IMAGE keywords, and print these if found and the 
;               PUTINFO keyword is set.
;
; OPTIONAL PLOTTING KEYWORDS:
;       /TYPE - the type of astronomical labeling to be displayed.   Either set
;               TYPE = 0 (default), distance to center of the image is
;               marked in units of Arc seconds, arc minutes, or degrees
;
;               TYPE = 1 astronomical labeling with Right ascension and 
;               declination.
;
;       /PUTINFO - If set, then IMCONTOUR will add information about the image
;               to the right of the contour plot.  Information includes image
;               name, object, image center, image center, contour levels, and
;               date plot was made
;
;       XDELTA, YDELTA - Integer scalars giving spacing of labels for TYPE=1.  
;               Default is to label every major tick (XDELTA=1) but if 
;               crowding occurs, then the user might wish to label every other
;               tick (XDELTA=2) or every third tick (XDELTA=3)
;
;       XMID, YMID - Scalars giving the X,Y position from which offset distances
;               will be measured when TYPE=0.   By default, offset distances 
;               are measured from the center of the image.
;       /OVERLAY - If set, then IMCONTOUR is assumed to overlay an image.
;               This requires 1 extra pixel be included on the X and Y axis,
;               to account for edge effects in the image display.    Setting
;               OVERLAY provide a better match of the contour and underlying
;               image but is not as aesthetically pleasing because the contours
;               will not extend to the axes. 
;               
;
;       Any keyword accepted by CONTOUR may also be passed through IMCONTOUR
;       since IMCONTOUR uses the _EXTRA facility.     IMCONTOUR uses its own
;       defaults for the XTITLE, YTITLE XMINOR, YMINOR, and SUBTITLE keywords
;       but these may be overridden.    Note in particular the /NODATA keyword
;       which can be used if imcontour.pro is to only provide labeling.
;
; NOTES:
;       (1) The contour plot will have the same dimensional ratio as the input
;           image array
;       (2) To contour a subimage, use HEXTRACT before calling IMCONTOUR
;       (3) Use the /NODATA keyword to simply provide astronomical labeling
;           of a previously displayed image.
;       (4) The IMCONTOUR display currently does not indicate the image 
;           rotation in any way, but only specifies coordinates along the 
;           edges of the image 
;
; EXAMPLE:
;       Overlay the contour of an image, im2, with FITS header, h2, on top
;       of the display of a different image, im1.   Use RA, Dec labeling, and
;       seven equally spaced contour levels.    The use of a program like
;       David Fanning's cgImage  http://www.idlcoyote.com/programs/cgimage.pro
;       is suggested to properly overlay plotting and image coordinates.  The
;       /Keep_aspect_ratio keyword must be used.
;
;       IDL> cgimage,im1,/keep_aspect, position = pos
;       IDL> imcontour,im2,h2,nlevels=7,/Noerase,/TYPE,position = pos
;
; PROCEDURES USED:
;       CHECK_FITS, EXTAST, GETROT, TICPOS, TICLABEL, TIC_ONE, TICS, XYAD
;       CONS_RA(), CONS_DEC(), ADSTRING()
;
; REVISION HISTORY:
;       Written   W. Landsman   STX                    May, 1989
;       Fixed RA,Dec labeling  W. Landsman             November, 1991
;       Fix plotting keywords  W.Landsman             July, 1992
;       Recognize GSSS headers  W. Landsman            July, 1994
;       Removed Channel keyword for V4.0 compatibility June, 1995
;       Add _EXTRA CONTOUR plotting keywords  W. Landsman  August, 1995
;       Add XDELTA, YDELTA keywords  W. Landsman   November, 1995
;       Use SYSTIME() instead of !STIME                August, 1997
;       Remove obsolete !ERR system variable W. Landsman   May 2000 
;       Added XMID, YMID keywords to specify central position (default is still
;          center of image)  W. Landsman               March 2002     
;       Recognize Galactic coordinates, fix Levels display when /PUTINFO set
;           W. Landsman                May 2003
;       Correct conversion from seconds of RA to arcmin is 4 not 15.
;       	M. Perrin					July 2003
;       Fix integer truncation which appears with tiny images WL  July 2004
;       Changed some keyword_set() to N_elements WL  Sep 2006
;       Work to 1 pixels level when overlaying an image,added /OVERLAY keyword
;        Use FORMAT_AXIS_VALUES()  W. Landsman   Jan 2008 
;       Make /OVERLAY  always optional   W. Landsman  Feb 2008
;       Check if RA crosses 0 hours  WL  Aug 2008
;       Use Coyote Graphics WL Feb 2011
;-
  On_error,2                                 ;Return to caller
  compile_opt idl2

  if N_params() LT 2 then begin             ;Sufficient parameters?
      print,'Syntax - imcontour, im, hdr, [ /TYPE, /PUTINFO, XDELTA=, YDELT= '
      print,'                               XMID=, YMID = ]'
      print,'         Any CONTOUR keyword is also accepted by IMCONTOUR'  
     return
  endif

  ;Make sure header appropriate to image
  check_fits, im, hdr, dimen, /NOTYPE, ERRMSG = errmsg    
  if errmsg NE '' then message,errmsg

; Set defaults if keywords not set

  if ~keyword_set( TYPE ) then type = 0
  if ~keyword_set( XDELTA ) then xdelta = 1
  if ~keyword_set( YDELTA ) then ydelta = 1
  
  if N_Elements(XMINOR) EQ 0 then $
       xminor = !X.MINOR EQ 0 ? 5 : !X.MINOR

  if N_Elements(YMINOR) EQ 0 then $
       yminor = !Y.MINOR EQ 0 ?  5 : !Y.MINOR

  EXTAST, hdr, astr, noparams      ;Extract astrometry from header
  if noparams LT 0 then $                       ;Does astrometry exist?
      message,'FITS header does not contain astrometry'
  if strmid( astr.ctype[0], 5, 3) EQ 'GSS' then begin
        hdr1 = hdr
        gsss_STDAST, hdr1
        extast, hdr1, astr, noparams
  endif
  sexig = strmid(astr.ctype[0],0,4) EQ 'RA--'
 
; Adjust plotting window so that contour plot will have same dimensional 
; ratio as the image

  xlength = !D.X_VSIZE &  ylength = !D.Y_VSIZE
  xsize = fix( dimen[0] )  &   ysize = fix( dimen[1] )
  xsize1 = xsize-1 & ysize1 = ysize-1
     if keyword_set(OVERLAY) then begin 
         xran  = [0,xsize]-0.5  & yran = [0,ysize]-0.5
    endif else begin
         xran = [0,xsize1] & yran = [0,ysize1]	 
    endelse

 xratio = xsize / float(ysize)
  yratio = ysize / float(xsize)
  if N_elements(XMID) EQ 0 then xmid = (xran[1] -xran[0]-1)/2.
  if N_elements(YMID) EQ 0 then ymid = (yran[1] -yran[0]-1)/2.

  if ( ylength*xratio LT xlength ) then begin

    xmax = 0.15 + 0.8*ylength*xratio/xlength
    pos = [ 0.15, 0.15, xmax, 0.95 ]

  endif else begin

     xmax = 0.95
     pos = [ 0.15, 0.15, xmax, 0.15+ 0.8*xlength*yratio/ylength ]

  endelse

  xtics = !X.TICKS GT 0 ? abs(!X.TICKS) : 8
  ytics = !Y.TICKS GT 0 ? abs(!Y.TICKS) : 8
 
  pixx = float(xsize)/xtics            ;Number of X pixels between tic marks
  pixy = float(ysize)/ytics            ;Number of Y pixels between tic marks

  getrot,hdr,rot,cdelt               ;Get the rotation and plate scale

  xyad,hdr,xsize1/2.,ysize1/2.,ra_cen,dec_cen         ;Get coordinates of image center
  if sexig then ra_dec = adstring(ra_cen,dec_cen,1)       ;Make a nice string

; Determine tic positions and labels for the different type of contour plots

  if type NE 0 then begin                  ;RA and Dec labeling

     xedge = [ xran[0], xran[1], xran[0]]          ;X pixel values of the four corners
     yedge = [ yran[0], yran[0], yran[1] ]          ;Y pixel values of the four corners

     xy2ad, xedge, yedge, astr, a, d
 
     pixx = float(xmid*2)/xtics          ;Number of X pixels between tic marks
     pixy = float(ymid*2)/ytics          ;Number of Y pixels between tic marks

; Find an even increment on each axis, for RA check crossing of 0 hours
     case 1 of 
     ( a[1] GT a[0] ) and (cdelt[0] LT 0 ) : $ 
        tics, a[0], a[1] - 360.0d , xsize, pixx, raincr, RA=sexig 
     ( a[1] LT a[0] ) and (cdelt[0] GT 0 ) : $ 
        tics, a[0], 360.0d + a[1], xsize, pixx, raincr, RA=sexig 
     else: tics, a[0], a[1], xsize, pixx, raincr, RA=sexig 
     endcase
     tics, d[0], d[2], ysize, pixy, decincr    ;Find an even increment for Dec

; Find position of first tic on each axis
     tic_one, a[0], pixx, raincr, botmin, xtic1, RA= sexig  ;Position of first RA tic
     tic_one, d[0], pixy, decincr,leftmin,ytic1       ;Position of first Dec tic

     nx = fix( (xsize1-xtic1)/pixx )             ;Number of X tic marks
     ny = fix( (ysize1-ytic1)/pixy )             ;Number of Y tic marks

     if sexig then ra_grid = (botmin + findgen(nx+1)*raincr/4.) else $ 
                   ra_grid = (botmin + findgen(nx+1)*raincr/60.)
     dec_grid = (leftmin + findgen(ny+1)*decincr/60.)

     ticlabels, botmin, nx+1, raincr, xlab, RA=sexig, DELTA=xdelta
     ticlabels, leftmin, ny+1, decincr, ylab,DELTA=ydelta

     xpos = cons_ra( ra_grid,0,astr )     ;Line of constant RA
     ypos = cons_dec( dec_grid,0,astr)   ;Line of constant Dec

     if sexig then begin 
        xunits = 'Right Ascension'
        yunits = 'Declination'
     endif else begin
        xunits = 'Longitude'
        yunits = 'Latitude'
     endelse                          

  endif else begin ; label with distance from center.
     ticpos, xsize*cdelt[0], xsize, pixx, incrx, xunits     
     numx = fix((xmid-xran[0])/pixx)              ;Number of ticks from left edge
     ticpos, ysize*cdelt[1], ysize, pixy, incry, yunits
     numy = fix((ymid-yran[0])/pixy)             ;Number of ticks from bottom to center
     nx = numx + fix((xran[1]-xmid)/pixx)    ;Total number of X ticks 
     ny = numy + fix((yran[1]-ymid)/pixy)    ;Total number of Y ticks  
     xpos = xmid  + (findgen(nx+1)-numx)*pixx
     ypos = ymid   + (findgen(ny+1)-numy)*pixy
     xlab = format_axis_values( indgen(nx+1)*incrx - incrx*numx)
     ylab = format_axis_values( indgen(ny+1)*incry - incry*numy)
    
     
  endelse

; Get default values of XTITLE, YTITLE, TITLE and SUBTITLE

  putinfo = keyword_set(PUTINFO)

  if N_elements(xtitle) EQ 0 then $
  xtitle = !X.TITLE eq ''? xunits : !X.TITLE

  if N_elements(ytitle) EQ 0 then $
        ytitle = !Y.TITLE eq ''? yunits : !Y.TITLE

  if (~keyword_set( SUBTITLE) ) && (putinfo LT 1) then $
      if sexig then $
      subtitle = 'Center:  R.A. '+ strmid(ra_dec,1,13)+'  Dec ' + $
               strmid(ra_dec,13,13) else $
     subtitle = 'Center:  Longitude '+ strtrim(string(ra_cen,'(f6.2)'),2) + $
                          ' Latitude ' + strtrim(string(dec_cen,'(f6.2)'),2)

  if N_elements( SUBTITLE)  EQ 0 then subtitle = !P.SUBTITLE
  cgContour,im, $
         XTICKS = nx, YTICKS = ny, POSITION=pos, XSTYLE=1, YSTYLE=1,$
         XTICKV = xpos, YTICKV = ypos, XTITLE=xtitle, YTITLE=ytitle, $
         XTICKNAME = xlab, YTICKNAME = ylab, SUBTITLE = subtitle, $
         XMINOR = xminor, YMINOR = yminor, _EXTRA = extra, XRAn=xran, $
	 YRAN = yran,noerase=noerase,WINDOW=window
	 
  
;  Write info about the contour plot if desired

  if putinfo GE 1 then begin

    sv = !D.NAME
    set_plot,'null'
    contour,im, _EXTRA = extra, PATH_INFO = info
    set_plot,sv


  if keyword_set(window) then cgcontrol, execute= 0	
   xmax = xmax + 0.01

     ypos = 0.92
     object = sxpar( hdr, 'OBJECT', Count = N_object )
     if N_object GT 0  then begin 
           cgText, xmax, ypos, object, /NORM, addcmd=window
           ypos = ypos-0.05
     endif

     name = sxpar( hdr, 'IMAGE', Count = N_image )
     if N_image GT 0 then begin 
           cgtext,xmax,ypos,name, /NORM, addcmd= window
           ypos = ypos - 0.05
     endif

     cgText, xmax, ypos,'Center:',/NORM, addcmd=window
     ypos = ypos - 0.05
     if sexig then begin
     cgText, xmax, ypos, 'R.A. '+ strmid(ra_dec,1,13),/NORM,addcmd=window
     cgText, xmax, ypos-0.05, 'Dec '+  strmid(ra_dec,13,13),/NORM,addcmd=window
     endif else begin
     cgText, xmax, ypos, 'Longitude: '+ strtrim(string(ra_cen,'(f6.2)'),2), $
             /NORM, addcmd=window
     cgText, xmax, ypos-0.05, addcmd=window, $
             'Latitude: '+  strtrim(string(dec_cen,'(f6.2)'),2),/NORM
     endelse
     ypos = ypos - 0.1
     cgText, xmax, ypos, 'Image Size', /NORM, addcmd=window
     cgText, xmax, ypos-0.05, 'X: ' + strtrim(xsize,2), /NORM, addcmd=window
     cgText, xmax, ypos-0.1, 'Y: ' + strtrim(ysize,2), /NORM, addcmd=window
     cgText, xmax, ypos- 0.15, strmid(systime(),4,20),/NORM, addcmd=window
     cgText, xmax, ypos - 0.2, 'Contour Levels:',/NORM, addcmd=window


    ypos = ypos - 0.25
    val = info.value
    val = val[uniq(val,sort(val))]
     nlevels = N_elements(val)
     for i = 0,(nlevels < 7)-1 do $
          cgText,xmax,ypos-0.05*i,string(i,'(i2)') + ':' + $
                              string(val[i]), /NORM,addcmd=window
     if keyword_set(window) then cgcontrol, execute=1
 
  endif
  
  return                                          
  end                                         

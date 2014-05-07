pro arrows,h,xcen,ycen,thick=thick,charsize=charsize,arrowlen=arrowlen, $
             color=color,NotVertex=NotVertex,Normal = normal,Data=data,font=font
;+
; NAME:
;      ARROWS
; PURPOSE:
;      To display "weathervane" directional arrows on an astronomical image 
; EXPLANATION:
;      Overlays a graphic showing orientation of North and East.
;
; CALLING SEQUENCE:
;      ARROWS,h, [ xcen, ycen, ARROWLEN= , CHARSIZE=  COLOR= , /DATA
;                              FONT=, /NORMAL, /NOTVERTEX, THICK=  ]
;
; INPUTS:
;       h - FITS header array, must include astrometry
;
; OPTIONAL INPUTS:
;       xcen,ycen - numeric scalars, specifying the center position of
;		arrows.   Position in device units unless the /NORMALIZED 
;		keyword is specified.   If not supplied, then ARROWS
;		will prompt for xcen and ycen
;
; OPTIONAL KEYWORD INPUTS:
;       arrowlen  - length of arrows in terms of normal Y size of vector-drawn
;                     character,  default  = 3.5, floating point scalar
;       charsize  - character size, default = 2.0, floating point scalar
;       color     -  color name or number for the arrows and NE letters.  See
;                 cgCOLOR() for a list of color names.                    
;       Data - if this keyword is set and nonzero, the input center (xcen,
;                 ycen) is understood to be in data coordinates
;       font - IDL vector font number (1-20) to use to display NE letters.
;                 For example, set font=13 to use complex italic font.
;       NotVertex - Normally (historically) the specified xcen,ycen indicated
;                   the position of the vertex of the figure.  If this
;                   keyword is set, the xcen,ycen coordinates refer to a sort
;                   of 'center of mass' of the figure.  This allows the
;                   figure to always appear with the area irregardless of
;                   the rotation angle.
;       Normal - if this keyword is set and nonzero, the input center 
;                (xcen,ycen) is taken to be in normalized coordinates.   The
;                default is device coordinates.
;       thick     - line thickness, default = 2.0, floating point scalar
; OUTPUTS:
;       none
; EXAMPLE:
;       Draw a weathervane at (400,100) on the currently active window, 
;       showing the orientation of the image associated with a FITS header, hdr
;
;       IDL> arrows, hdr, 400, 100
;
; METHOD:
;       Uses EXTAST to EXTract ASTrometry from the FITS header.   The 
;       directions of North and East are computed and the procedure
;       ONE_ARROW called to create the "weathervane".
;
; PROCEDURES USED:
;       GETROT - Computes rotation from the FITS header
;       ONE_ARROW - Draw a labeled arrow	
;       ZPARCHECK
; REVISON HISTORY:
;       written by B. Boothman 2/5/86 
;       Recoded with new procedures ONE_ARROW, ONE_RAY.  R.S.Hill,HSTX,5/20/92
;       Added separate determination for N and E arrow to properly display
;         arrows irregardless of handedness or other peculiarities and added
;         /NotVertex keyword to improve positioning of figure. E.Deutsch 1/10/93
;       Added /DATA and /NORMAL keywords W. Landsman      July 1993
;       Recognize GSSS header    W. Landsman       June 1993
;       Added /FONT keyword W. Landsman           April 1995
;       Modified to work correctly for COLOR=0  J.Wm.Parker, HITC   1995 May 25
;       Work correctly for negative CDELT values   W. Landsman   Feb. 1996
;       Use GETROT to compute rotation   W. Landsman    June 2003
;       Restored /NotVertex keyword which was not working after June 2003 change
;                  W. Landsman  January 2004
;-

  On_error,2                            ;Return to caller

  if (N_params() LT 1) then begin 
    print,'Syntax - ' + $
             'ARROWS, hdr, [ xcen, ycen, ARROWLEN= , CHARSIZE=  COLOR= , /DATA'
    print,'                        FONT=, /NORMAL, /NotVertex, THICK=  ]'
    print,'         hdr - FITS header with astrometry'
    return
  endif else zparcheck,'ARROWS',h,1,7,1,'FITS header array'

  if ( N_params() LT 3 ) then $
    read,'Enter x, y values for center of arrows: ',xcen,ycen

  setdefaultvalue, thick, 2.0
  setdefaultvalue, charsize, 2.0
  setdefaultvalue, arrowlen, 3.5
  setdefaultvalue, NotVertex, 0

;  Derive Position Angles for North and East separately

  getrot,h,npa, cdelt,/SILENT
  sgn = 1 - 2*(cdelt[0]*cdelt[1] GT 0) 
  epa = npa + sgn*90   

;  Make arrows reasonable size depending on device

  arrowlen_dev = arrowlen*!D.y_ch_size
  arrowsize = [arrowlen_dev, arrowlen_dev/3.5, 35.0]  ; See one_arrow.pro

  if keyword_set( NORMAL) then begin
	newcen = convert_coord( xcen, ycen, /NORMAL, /TO_DEVICE)
        xcent = newcen[0]
        ycent = newcen[1]
  endif else if keyword_set( DATA) then begin
	newcen = convert_coord( xcen, ycen, /DATA, /TO_DEVICE)
        xcent = newcen[0]
        ycent = newcen[1]
  endif else begin
         xcent=xcen & ycent=ycen
  endelse 

;  Adjust Center to 'Center of Mass' if NotVertex set
 if NotVertex then begin
    rot = npa/!RADEG
    dRAdX = cdelt[0]*cos(rot)
    dRAdY = cdelt[1]*sin(rot)
    dDECdX = cdelt[0]*sin(rot) 
    dDECdY = cdelt[1]*cos(rot)
    RAnorm = sqrt( dRAdX^2 + dRAdY^2 )
    DECnorm = sqrt(dDECdX^2 + dDECdY^2 )
    xcent = xcen - (dRAdX+dDECdX)/2/RAnorm*arrowsize[0]
    ycent = ycen - (dRAdY+dDECdY)/2/DECnorm*arrowsize[0]
    endif

;  Draw arrows
  one_arrow, xcent, ycent,  90+NPA, 'N', font= font, $
    charsize=charsize, thick=thick, color=color, arrowsize=arrowsize
  one_arrow, xcent, ycent, 90+EPA, 'E', font = font, $
    charsize=charsize, thick=thick, color=color, arrowsize=arrowsize

  return
  end

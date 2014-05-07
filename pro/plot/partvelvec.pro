;+
; NAME:
;      PARTVELVEC
;
; PURPOSE:
;       Plot the velocity vectors of particles at their positions
; EXPLANATION:
;       This procedure plots the velocity vectors of particles (at the
;       positions of the particles).
;
; CATEGORY:
;       Plotting, Two-dimensional.
;
; CALLING SEQUENCE:
;       PARTVELVEC, VELX, VELY, POSX, POSY [, X, Y]
;
; INPUTS:
;       VELX:  An array of any dimension, containing the x-components
;              of the particle velocities.   Can include NaN values
;       VELY:  An array of the same dimension as velx, containing the
;              y-components of the particle velocities. 
;       POSX:  An array of the same dimension as velx, containing the
;              x-components of the particle positions.
;       POSY:  An array of the same dimension as velx, containing the
;              y-components of the particle positions.
;
; OPTIONAL INPUTS:
;       X:   Optional abscissa values. X must be a vector.
;       Y:   Optional ordinate values. Y must be a vector. If only X
;            is specified, then Y is taken equal to be equal to X.
;
; OPTIONAL INPUT KEYWORD PARAMETERS:
;       FRACTION:   The fraction of the vectors to plot. They are
;                   taken at random from the complete sample.    Default is
;              FRACTION = 1.0, use all vectors
;
;       LENGTH:     The maximum vectorlength relative to the plot data
;                   window.   Default = 0.08
;
;       COLOR:      Color for the vectors, axes and titles by string name or
;                   number (see cgCOLOR).   Note that if VECCOLORS is 
;                   supplied, then the COLOR keyword still specifies the 
;                   color of the axes and title.    Default  = 'Opposite'
;
;       OVER:       Plot over the previous plot
;
;       VECCOLORS:  The vector colors. Must be either a scalar, or
;                   a vector (nmeric or string) the same size as VELX. 
;                   Set to COLOR by default.
;       WINDOW - Set this keyword to plot to a resizeable graphics window
;
;       Plot        All other keywords available to cgPlot (e.g. AXISCOLOR,
;       Keywords:   LINESTYLE, XRANGE) are available (via _EXTRA)
;
; OUTPUTS:
;       This procedure plots the velocity vectors (VELX,VELY) at the
;       positions of the particles, (POSX,POSY). If X and Y are not
;       specified, then the size of the plot is such that all vectors
;       just fit within in the plot data window.
;
; SIDE EFFECTS:
;       Plotting on the current device is performed.
;
; EXAMPLE:
;       Generate some particle positions and velocities.
;
;         POSX=RANDOMU(seed,200)
;         POSY=RANDOMU(seed,200)
;         VELX=RANDOMU(seed,200)-0.5
;         VELY=RANDOMU(seed,200)-0.5
;
;       Plot the particle velocities.
;
;         PARTVELVEC, VELX, VELY, POSX, POSY
;
;       Example using vector colors.
;
;         POSX=RANDOMU(seed,200)
;         POSY=RANDOMU(seed,200)
;         VELX=RANDOMU(seed,200)-0.5
;         VELY=RANDOMU(seed,200)-0.5
;         magnitude = SQRT(velx^2 + vely^2)
;         LOADCT, 5, NCOLORS=254, BOTTOM=1 ; Load vector colors
;         colors = BytScl(magnitude, Top=254) + 1B
;         PARTVELVEC, VELX, VELY, POSX, POSY, COLOR='green', VECCOLORS=colors
;
; MODIFICATION HISTORY:
;       Written by:  Joop Schaye (jschaye@astro.rug.nl), Sep 1996.
;       Added /OVER keyword   Theo Brauers (th.brauers@fz-juelich.de) Jul 2002
;       Added VECCOLORS keyword. David Fanning (david@dfanning.com) March, 2005
;       Incorporate the Coyote Graphics (cg) plot programs  WL  January 2011
;       Allow VELX, VELY to include NaN values P. Blitzer/WL March 2013
;-

PRO partvelvec,velx,vely,posx,posy,x,y, OVER = over, VECCOLORS=vecColors, $
               FRACTION=fraction,LENGTH=length,COLOR=color,WINDOW=window, $
	       _EXTRA=extra


;---------------------------------------------
; Various settings, modify these to customize
;---------------------------------------------

c = {customize, $
   length: 0.08, $     ; Maximum vector length relative to plot region. (*)
   lengtharrow: 0.3, $ ; Length of arrowhead legs relative to vectorlength.
   angle: 22.5 }       ; 1/2 times the angle between the arrowhead legs.

; (*) Not used if keyword LENGTH is present


;---------------------
; Some error handling
;---------------------

on_error,2  ; Return to caller if an error occurs.

nparams=n_params()
IF nparams NE 4 THEN BEGIN
    IF (nparams NE 5 AND nparams NE 6) THEN BEGIN
        message,'Wrong number of parameters!',/continue
        message,'Syntax: PARTVELVEC, VELX, VELY, POSX, POSY [, X, Y]', $
          /noname,/noprefix
    ENDIF
    IF nparams EQ 5 THEN y=x
    sizex = size(x)
    sizey = size(y)
    IF (sizex[0] NE 1 || sizey[0] NE 1) THEN $
      message,'X and Y must be vectors!'
ENDIF

sizevelx = size(velx)
sizevely = size(vely)
sizeposx = size(posx)
sizeposy = size(posy)

IF (total(sizevelx[0:sizevelx[0]]-sizevely[0:sizevelx[0]]) NE 0 $
    || total(sizevelx[0:sizevelx[0]]-sizeposx[0:sizevelx[0]]) NE 0 $
    || total(sizevelx[0:sizevelx[0]]-sizeposy[0:sizevelx[0]]) NE 0) THEN $
  message,'All arguments must have the same dimension and size!'

IF n_elements(fraction) GT 0 THEN $
  IF (fraction LT 0.0 || fraction GT 1.0) THEN $
  message,'Fraction has to be between 0.0 and 1.0.'


;--------------
; Prepare plot
;--------------

 nvecs = n_elements(velx)  ; Number of particles.
 vel = sqrt(velx^2+vely^2)  ; Total velocity.
 maxvel = max(vel,/nan)  ; Maximum velocity.

; Compute maximum length of vectors.
IF n_elements(length) LE 0 THEN length=c.length
minposx = min(posx)
maxposx = max(posx)
minposy = min(posy)
maxposy = max(posy)
length = length*((maxposx-minposx) > (maxposy-minposy))

; Convert velocities.
vx = length*velx/maxvel
vy = length*vely/maxvel
vel = length*temporary(vel)/maxvel

; Make sure no vectors extend beyond the plot data window.
x1 = posx+vx  ; End of vector.
y1 = posy+vy
IF nparams EQ 4 THEN BEGIN
    minposx = min(x1)<minposx
    maxposx = max(x1)>maxposx
    minposy = min(y1)<minposy
    maxposy = max(y1)>maxposy
ENDIF

angle = c.angle*!dtor  ; Convert from degrees to radians.
sinangle = sin(angle)  ; Need these.
cosangle = cos(angle)


;-----------
; Plot axes
;-----------

if N_elements(color) EQ 0 then color = cgcolor('opposite')
IF n_elements(veccolors) EQ 0 THEN BEGIN
   veccolors = Replicate(cgcolor('opposite'), nvecs)
ENDIF ELSE BEGIN
   nvc = N_Elements(veccolors)
   CASE nvc OF
      1: veccolors = Replicate(veccolors, nvecs)
      nvecs:
      ELSE: Message, 'Vector color array VECCOLORS must be same size as VELX.'
   ENDCASE
ENDELSE
IF n_elements(over) EQ 0 THEN BEGIN
IF nparams EQ 4 THEN $
  cgPlot,[minposx,maxposx],[minposy,maxposy], axiscolor=color,$
  /nodata,/xstyle,/ystyle,COLOR=color,window=window,_EXTRA=extra $
ELSE cgPlot,x,y,/nodata,/xstyle,/ystyle,COLOR=color,window=window,_EXTRA=extra
ENDIF
if keyword_set(window) then cgcontrol,execute=0
;--------------
; Plot vectors
;--------------

IF (n_elements(fraction) GT 0) && (fraction NE 1.0) THEN BEGIN
   nrgood=long(fraction*nvecs)  ; # of vectors to plot.
    IF nrgood EQ 0 THEN return
    ; Compute indices of vectors to plot. I use two lines to get more
    ; random "random numbers".
    good=long(randomu(seed,nrgood+1)*(nvecs-1.0))
    good=good[1:*]
    vx = temporary(vx[good])
    vy = temporary(vy[good])
    px = posx[good]  ; Can't use temporary if we want to keep the data.
    py = posy[good]
    x1 = temporary(x1[good])
    y1 = temporary(y1[good])
    nvecs=nrgood
ENDIF ELSE BEGIN
    px=posx
    py=posy
ENDELSE

FOR i=0l,nvecs-1l DO BEGIN  ; Loop over particles.
    ; Note that we cannot put the next three lines outside the loop,
    ; because we want the arrow size to be relative to the vector length.
    r = c.lengtharrow*vel[i]  ; Length of arrow head.
    rsin = r*sinangle
    rcos = r*cosangle
    ; Draw basis, arrow leg, same arrow leg, other arrow leg.
    ; One arrow leg is drawn twice, because we need to return to the end
    ; of the vector to draw the other leg.

    cgPlots,[px[i],x1[i],x1[i]-(vx[i]*rcos+vy[i]*rsin)/vel[i], $
           x1[i],x1[i]-(vx[i]*rcos-vy[i]*rsin)/vel[i]], $
          [py[i],y1[i],y1[i]-(vy[i]*rcos-vx[i]*rsin)/vel[i], $
           y1[i],y1[i]-(vy[i]*rcos+vx[i]*rsin)/vel[i]],COLOR=veccolors[i],$
	   ADDCMD = window
	
ENDFOR
 if keyword_set(window) then cgcontrol,execute=1
 return
END  ; End of procedure PARTVELVEC.

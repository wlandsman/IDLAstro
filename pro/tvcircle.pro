Pro Tvcircle, radius, xc, yc, color, COLOR = TheColor, Device=device, $
               DATA= data, FILL=fill,_Extra = _extra
;+
; NAME:
;     TVCIRCLE
; PURPOSE:
;     Draw circle(s) of specified radius at specified position(s) 
; EXPLANATION: 
;     If a position is not specified, and device has a cursor, then a circle
;     is drawn at the current cursor position.    By default, TVCIRCLE now
;     (since Jan 2012) assumes data coordinates if !X.crange is set.
;
; CALLING SEQUENCE:
;     TVCIRCLE, rad, x, y, color, [ /DATA, /FILL, _EXTRA  =  ]         
;
; INPUTS:
;     RAD - radius of circle(s) to be drawn, positive numeric scalar
;
; OPTIONAL INPUT:
;      X - x position for circle center, vector or scalar
;      Y - y position for circle center, vector or scalar
;               If X and Y are not specified, and the device has a cursor, 
;               then program will draw a circle at the current cursor position
;      COLOR -  color name or intensity value(s) (0 - !D.N_COLORS) used to draw
;               the circle(s).   If COLOR is a scalar then all circles are drawn
;               with the same color value.   Otherwise, the Nth circle is drawn
;               with the  Nth value of color.  See cgCOLOR() for a list of color
;               names.  Default = 'opposite' (i.e. color opposite the 
;               background).   
;
; OPTIONAL KEYWORD INPUTS:
;       /DATA - if this keyword is set and non-zero, then the circle width and
;              X,Y position center are interpreted as being in DATA 
;              coordinates.   Note that data coordinates must be previously
;              defined (with a PLOT or CONTOUR call).    TVCIRCLE will
;              internally convert to device coordinates before drawing the
;              circle, in order to maintain optimal smoothness.    The default
;              is to assume data coordinates if !X.CRANGE is set.    Force
;              device coordinates by setting DATA = 0 or /DEVICE
;       /DEVICE - If set, then force use of device coordinates..
;       /FILL  - If set, fill the circle using cgCOLORFILL
;
;               Any keyword recognized by cgPLOTS (or cgCOLORFILL if /FILL is 
;               set) is also recognized by TVCIRCLE.   In particular, the color,
;               linestyle, thickness and clipping of the circles are controlled
;               by the  COLOR, LINESTYLE, THICK and NOCLIP keywords.  (Clipping
;               is turned off by default, set NOCLIP=0 to activate it.)
;               If /FILL is set then available keywords are LINE_FILL and 
;               FILL_PATTERN. 
; OUTPUTS:
;       None
;
; RESTRICTIONS:
;       (1) Some round-off error may occur when non-integral values are 
;           supplied for both the radius and the center coordinates
;       (2) TVCIRCLE does not accept /NORMAL coordinates.
;       (3) TVCIRCLE always draws a circle --- even when in data coordinates 
;           and the X and Y data scales are unequal.    (The X data scale is 
;           used to define the circle radius.)     If this is not the behaviour
;           you want, then use TVELLIPSE instead.
; EXAMPLE:
;       (1) Draw circles of radius 9 pixels at the positions specified by 
;           X,Y vectors, using double thickness lines
;
;           IDL> tvcircle, 9, x, y, THICK = 2
;
;           Now fill in the circles using the LINE_FILL method
;
;           IDL> tvcircle, 9, x, y, /FILL, /LINE_FILL
; METHOD:
;           The method used is that of Michener's, modified to take into account
;           the fact that IDL plots arrays faster than single points.   See
;           "Fundamental of Interactive Computer Graphics" by Foley and Van Dam"
;           p. 445 for the algorithm.
;
; REVISON HISTORY:
;           Original version   written by B. Pfarr  STX   10-88 
;           Major rewrite adapted from CIRCLE by Allyn Saroyan   LNLL
;           Wayne Landsman   STX     Sep. 91
;           Added DATA keyword   Wayne Landsman  HSTX    June 1993
;           Added FILL keyword.  R. S. Hill, HSTX, 4-Nov-1993
;           Always convert to device coords, add _EXTRA keyword, allow vector
;           colors.   Wayne Landsman, HSTX,  May 1995
;           Allow one to set COLOR = 0,   W. Landsman, HSTX, November 1995
;           Check if data axes reversed.  P. Mangifico, W. Landsman  May 1996
;           Use strict_extra to check input keywords W. Landsman  July 2005
;           Update documentation to note NOCLIP=0 option W.L.  Oct. 2006
;           Make all integers default to LONG  W. Landsman  Dec 2006
;           Use Coyote Graphics procedures W. Landsman Feb 2011
;           Default to data coordinates if !X.crange present  WL Jan 2012
;           Add /DEVICE coords, fix Jan 2012 update.   Mar 2012
;-

   On_Error, 2   ; Return to caller
   compile_opt idl2

   if ( N_params() LT 1) then begin
       print, 'Syntax - TVCIRCLE, rad, [ xc, yc, color, /DATA, /FILL, _EXTRA= ]'
       return
   endif

; Default to data coordinates if !X.crange is set (previous plot) 
   if keyword_set(device) then datacoord = 0 else begin
      if N_elements(data) eq 0 then datacoord = !x.crange[0] NE !x.crange[1]  $
                          else datacoord = logical_true(data)
   endelse   			  
		  
   if N_elements(radius) NE 1 then message, $
          'ERROR - Circle radius (first parameter) must be a scalar'

   if N_elements(TheColor) EQ 0 then begin
      IF N_Elements( Color ) EQ 0 THEN Color = cgcolor('opposite')
   endif else color = TheColor


  if N_params() LT 3 then begin
        if (!D.WINDOW EQ -1) then message, $
                'ERROR - Cursor not available for device ' + !D.NAME
        cursor, xc, yc, /DEVICE, /NOWAIT
        if (xc LT 0) || (yc LT 0) then begin
        message,'Position cursor in window ' + strtrim(!D.WINDOW,2) + $
                ' -- then hit mouse button',/INF
        cursor, xc, yc, /DEVICE, /WAIT
        message,'Circle is centered at (' + strtrim(xc,2) + ',' + $
                strtrim(yc,2) + ')',/INF
  endif

  endif 

    N_circle = min( [ N_elements(xc), N_elements(yc) ] )


    if datacoord then begin 
                coord = abs(convert_coord(radius,0,/data,/to_dev) - $
                        convert_coord(0,0,/data,/to_dev)) 
                irad =  round( coord[0] )
    endif else $
               irad = round(radius)	             

   x = 0
   y = irad 
   d = 3 - 2 * irad


   ; Find the x and y coordinates for one eighth of a circle.
   ; The maximum number of these coordinates is the radius of the circle.

   xHalfQuad = Make_Array( irad + 1, /Long, /NoZero )
   yHalfQuad = xHalfQuad

   path = 0

   WHILE x lt y $
   DO BEGIN

      xHalfQuad[path] = x
      yHalfQuad[path] = y

      path++

      IF d lt 0 $
      THEN d += 4*x + 6 $
      ELSE BEGIN

           d +=  4*(x-y) + 10
           y--

           END

      x++

      END

   IF x eq y $
   THEN BEGIN ; Fill in last point

        xHalfQuad[path] = x
        yHalfQuad[path] = y

        path++

        END ; Filling in last point

   ; Shrink the arrays to their correct size

   xHalfQuad = xHalfQuad[ 0:path-1 ]
   yHalfQuad = yHalfQuad[ 0:path-1 ]

   ; Convert the eighth circle into a quadrant

   xQuad = [ xHalfQuad, Rotate(yHalfQuad, 5) ]
   yQuad = [ yHalfQuad, Rotate(xHalfQuad, 5) ]

   ; Prepare for converting the quadrants into a full circle

   xQuadRev = Rotate( xQuad[0:2*path-2], 5 )
   yQuadRev = Rotate( yQuad[0:2*path-2], 5 )

   ; Create full-circle coordinates

   x = [ xQuad, xQuadRev, -xQuad[1:*], -xQuadRev ]
   y = [ yQuad, -yQuadRev, -yQuad[1:*], yQuadRev ]

   ; Plot the coordinates about the given center
   
   if datacoord then begin        ;Convert to device coordinates
        coord = convert_coord( xc, yc, /DATA, /TO_DEVICE)
        xcen = round(coord[0,*]) & ycen = round(coord[1,*])
   endif else begin
        xcen = round(xc) & ycen = round(yc)
   endelse


   Ncolor1 = N_elements(color) -1
   for i = 0l, N_circle-1 do begin
      j = i < Ncolor1
      if keyword_set(fill) then begin
            cgcolorfill, x+xcen[i],  y + ycen[i], COLOR=color[j], /DEV, $
            _STRICT_Extra = _extra
      endif else begin
            cgPlotS, x + xcen[i], y+ ycen[i], COLOR = Color[j], /DEV, $
            _STRICT_Extra = _extra
      endelse

   endfor

   Return
   End; TVcircle

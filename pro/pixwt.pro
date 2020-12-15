;+
; NAME:
;	PIXWT
; PURPOSE: 
;	Circle-rectangle overlap area computation.
; DESCRIPTION:
;	Compute the fraction of a unit pixel that is interior to a circle.
;	The circle has a radius r and is centered at (xc, yc).  The center of
;	the unit pixel (length of sides = 1) is at (x, y).
;
; CATEGORY:
;	CCD data processing
; CALLING SEQUENCE:
;	area = Pixwt( xc, yc, r, x, y )
; INPUTS:
;	xc, yc : Center of the circle, numeric scalars
;	r      : Radius of the circle, numeric scalars
;	x, y   : Center of the unit pixel, numeric scalar or vector
; OPTIONAL INPUT PARAMETERS:
;	None.
; KEYWORD PARAMETERS:
;	None.
; OUTPUTS:
;	Function value: Computed overlap area.
; EXAMPLE:
;       What is the area of overlap of a circle with radius 3.44 units centered
;       on the point 3.23, 4.22 with the pixel centered at [5,7]
;
;       IDL> print,pixwt(3.23,4.22,3.44,5,7)  ==>  0.6502
; COMMON BLOCKS:
;    None.
; PROCEDURE:
;	Divides the circle and rectangle into a series of sectors and
;	triangles.  Determines which of nine possible cases for the
;	overlap applies and sums the areas of the corresponding sectors
;	and triangles.    Called by aper.pro
;
; NOTES:
;      If improved speed is needed then a C version of this routines, with
;      notes on how to linkimage it to IDL is available at   
;       ftp://ftp.lowell.edu/pub/buie/idl/custom/
;
; MODIFICATION HISTORY:
;     Ported by Doug Loucks, Lowell Observatory, 1992 Sep, from the
;    routine pixwt.c, by Marc Buie.
;      Fix problem when radius supplied as short integer > sqrt(32767)
;              W. Landsman June 2018
;-
; ---------------------------------------------------------------------------
; Function Arc( x, y0, y1, r )
;
; Compute the area within an arc of a circle.  The arc is defined by
; the two points (x,y0) and (x,y1) in the following manner:  The circle
; is of radius r and is positioned at the origin.  The origin and each
; individual point define a line which intersects the circle at some
; point.  The angle between these two points on the circle measured
; from y0 to y1 defines the sides of a wedge of the circle.  The area
; returned is the area of this wedge.  If the area is traversed clockwise
; then the area is negative, otherwise it is positive.
; ---------------------------------------------------------------------------
FUNCTION Arc, x, y0, y1, r
RETURN, 0.5 * r*r * ( ATAN( FLOAT(y1)/FLOAT(x) ) - ATAN( FLOAT(y0)/FLOAT(x) ) )
END


; ---------------------------------------------------------------------------
; Function Chord( x, y0, y1 )
;
; Compute the area of a triangle defined by the origin and two points,
; (x,y0) and (x,y1).  This is a signed area.  If y1 > y0 then the area
; will be positive, otherwise it will be negative.
; ---------------------------------------------------------------------------
FUNCTION Chord, x, y0, y1
RETURN, 0.5 * x * ( y1 - y0 )
END


; ---------------------------------------------------------------------------
; Function Oneside( x, y0, y1, r )
;
; Compute the area of intersection between a triangle and a circle.
; The circle is centered at the origin and has a radius of r.  The
; triangle has verticies at the origin and at (x,y0) and (x,y1).
; This is a signed area.  The path is traversed from y0 to y1.  If
; this path takes you clockwise the area will be negative.
; ---------------------------------------------------------------------------
FUNCTION Oneside, x, y0, y1, r

true = 1
size_x  = SIZE( x )

CASE size_x[ 0 ] OF
   0    : BEGIN
      IF x EQ 0 THEN RETURN, x
      IF ABS( x ) GE r THEN RETURN, Arc( x, y0, y1, r )
      yh = SQRT( r*r - x*x )
      CASE true OF
         ( y0 LE -yh ) : BEGIN
            CASE true OF
               ( y1 LE -yh ) : RETURN, Arc( x, y0, y1, r )
               ( y1 LE  yh ) : RETURN, Arc( x, y0, -yh, r ) $
                               + Chord( x, -yh, y1 )
               ELSE          : RETURN, Arc( x, y0, -yh, r ) $
                               + Chord( x, -yh, yh ) + Arc( x, yh, y1, r )
            ENDCASE
         END

         ( y0 LT  yh ) : BEGIN
            CASE true OF
               ( y1 LE -yh ) : RETURN, Chord( x, y0, -yh ) $
                               + Arc( x, -yh, y1, r )
               ( y1 LE  yh ) : RETURN, Chord( x, y0, y1 )
               ELSE          : RETURN, Chord( x, y0, yh ) + Arc( x, yh, y1, r )
            ENDCASE
         END

         ELSE          : BEGIN
            CASE true OF
               ( y1 LE -yh ) : RETURN, Arc( x, y0, yh, r ) $
                               + Chord( x, yh, -yh ) + Arc( x, -yh, y1, r )
               ( y1 LE  yh ) : RETURN, Arc( x, y0, yh, r ) + Chord( x, yh, y1 )
               ELSE          : RETURN, Arc( x, y0, y1, r )
            ENDCASE
         END
      ENDCASE
   END

   ELSE : BEGIN
      ans = x
      t0 = WHERE( x EQ 0, count )
      IF count EQ n_elements( x ) THEN RETURN, ans

      ans = x * 0
      yh = ans
      to = WHERE( ABS( x ) GE r, tocount )
      ti = WHERE( ABS( x ) LT r, ticount )
      IF tocount NE 0 THEN ans[ to ] = Arc( x[to], y0[to], y1[to], r )
      IF ticount EQ 0 THEN RETURN, ans

      yh[ ti ] = SQRT( r*r - x[ti]*x[ti] )

      t1 = WHERE( y0[ti] LE -yh[ti], count )
      IF count NE 0 THEN BEGIN
         i = ti[ t1 ]

         t2 = WHERE( y1[i] LE -yh[i], count )
         IF count NE 0 THEN BEGIN
            j = ti[ t1[ t2 ] ]
            ans[j] =  Arc( x[j], y0[j], y1[j], r )
         ENDIF

         t2 = WHERE( ( y1[i] GT -yh[i] ) AND ( y1[i] LE  yh[i] ), count )
         IF count NE 0 THEN BEGIN
            j = ti[ t1[ t2 ] ]
            ans[j] = Arc( x[j], y0[j], -yh[j], r ) $
                   + Chord( x[j], -yh[j], y1[j] )
         ENDIF

         t2 = WHERE( y1[i] GT yh[i], count )
         IF count NE 0 THEN BEGIN
            j = ti[ t1[ t2 ] ]
            ans[j] = Arc( x[j], y0[j], -yh[j], r ) $
                   + Chord( x[j], -yh[j], yh[j] ) $
                   + Arc( x[j], yh[j], y1[j], r )
         ENDIF
      ENDIF

      t1 = WHERE( ( y0[ti] GT -yh[ti] ) AND ( y0[ti] LT yh[ti] ), count )
      IF count NE 0 THEN BEGIN
         i = ti[ t1 ]

         t2 = WHERE( y1[i] LE -yh[i], count )
         IF count NE 0 THEN BEGIN
            j = ti[ t1[ t2 ] ]
            ans[j] = Chord( x[j], y0[j], -yh[j] ) $
                   + Arc( x[j], -yh[j], y1[j], r )
         ENDIF

         t2 = WHERE( ( y1[i] GT -yh[i] ) AND ( y1[i] LE  yh[i] ), count )
         IF count NE 0 THEN BEGIN
            j = ti[ t1[ t2 ] ]
            ans[j] = Chord( x[j], y0[j], y1[j] )
         ENDIF

         t2 = WHERE( y1[i] GT yh[i], count )
         IF count NE 0 THEN BEGIN
            j = ti[ t1[ t2 ] ]
            ans[j] = Chord( x[j], y0[j], yh[j] ) $
                   + Arc( x[j], yh[j], y1[j], r )
         ENDIF
      ENDIF

      t1 = WHERE( y0[ti] GE yh[ti], count )
      IF count NE 0 THEN BEGIN
         i = ti[ t1 ]

         t2 = WHERE ( y1[i] LE -yh[i], count )
         IF count NE 0 THEN BEGIN
            j = ti[ t1[ t2 ] ]
            ans[j] = Arc( x[j], y0[j], yh[j], r ) $
                   + Chord( x[j], yh[j], -yh[j] ) $
                   + Arc( x[j], -yh[j], y1[j], r )
         ENDIF

         t2 = WHERE( ( y1[i] GT -yh[i] ) AND ( y1[i] LE  yh[i] ), count )
         IF count NE 0 THEN BEGIN
            j = ti[ t1[ t2 ] ]
            ans[j] = Arc( x[j], y0[j], yh[j], r ) $
                   + Chord( x[j], yh[j], y1[j] )
         ENDIF

         t2 = WHERE( y1[i] GT yh[i], count )
         IF count NE 0 THEN BEGIN
            j = ti[ t1[ t2 ] ]
            ans[j] = Arc( x[j], y0[j], y1[j], r )
         ENDIF
      ENDIF

      RETURN, ans
   END
ENDCASE

END


; ---------------------------------------------------------------------------
; Function Intarea( xc, yc, r, x0, x1, y0, y1 )
;
; Compute the area of overlap of a circle and a rectangle.
;    xc, yc  :  Center of the circle.
;    r       :  Radius of the circle.
;    x0, y0  :  Corner of the rectangle.
;    x1, y1  :  Opposite corner of the rectangle.
; ---------------------------------------------------------------------------
FUNCTION Intarea, xc, yc, r, x0, x1, y0, y1
;
; Shift the objects so that the circle is at the origin.
;
x0 = x0 - xc
y0 = y0 - yc
x1 = x1 - xc
y1 = y1 - yc

RETURN, Oneside( x1, y0, y1, r ) + Oneside( y1, -x1, -x0, r ) +$
        Oneside( -x0, -y1, -y0, r ) + Oneside( -y0, x0, x1, r )

END


; ---------------------------------------------------------------------------
; FUNCTION Pixwt( xc, yc, r, x, y )
;
; Compute the fraction of a unit pixel that is interior to a circle.
; The circle has a radius r and is centered at (xc, yc).  The center of
; the unit pixel (length of sides = 1) is at (x, y).
; ---------------------------------------------------------------------------
FUNCTION Pixwt, xc, yc, r, x, y
RETURN, Intarea( xc, yc, float(r), x-0.5, x+0.5, y-0.5, y+0.5 )
END

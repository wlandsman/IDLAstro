pro xyz,date,x,y,z,xvel,yvel,zvel,equinox=equinox

;+
; NAME:
;       XYZ
; PURPOSE:
;       Calculate geocentric X,Y, and Z  and velocity coordinates of the Sun
; EXPLANATION:
;       Calculates geocentric X,Y, and Z vectors and velocity coordinates 
;       (dx, dy and dz) of the Sun.   (The positive X axis is directed towards 
;       the equinox, the y-axis, towards the point on the equator at right 
;       ascension 6h, and the z axis toward the north pole of the equator).
;       Typical position accuracy is <1e-4 AU (15000 km).
;
; CALLING SEQUENCE:
;       XYZ, date, x, y, z, [ xvel, yvel, zvel, EQUINOX = ]
;
; INPUT:
;       date: reduced julian date (=JD - 2400000), scalar or vector
;
; OUTPUT:
;       x,y,z: scalars or vectors giving heliocentric rectangular coordinates
;                 (in A.U) for each date supplied.    Note that sqrt(x^2 + y^2
;                 + z^2) gives the Earth-Sun distance for the given date.
;       xvel, yvel, zvel: velocity vectors corresponding to X, Y and Z.
;
; OPTIONAL KEYWORD INPUT:
;       EQUINOX: equinox of output. Default is 1950.
;
; EXAMPLE:
;       What were the rectangular coordinates and velocities of the Sun on 
;       Jan 22, 1999 0h UT (= JD 2451200.5) in J2000 coords? NOTE:
;       Astronomical Almanac (AA) is in TDT, so add 64 seconds to 
;       UT to convert.
;
;       IDL> xyz,51200.5+64.d/86400.d,x,y,z,xv,yv,zv,equinox = 2000
;
;       Compare to Astronomical Almanac (1999 page C20)
;                   X  (AU)        Y  (AU)     Z (AU)
;       XYZ:      0.51456871   -0.76963263  -0.33376880
;       AA:       0.51453130   -0.7697110   -0.3337152
;       abs(err): 0.00003739    0.00007839   0.00005360
;       abs(err)
;           (km):   5609          11759         8040 
;
;       NOTE: Velocities in AA are for Earth/Moon barycenter
;             (a very minor offset) see AA 1999 page E3
;                  X VEL (AU/DAY) YVEL (AU/DAY)   Z VEL (AU/DAY)
;       XYZ:      -0.014947268   -0.0083148382    -0.0036068577
;       AA:       -0.01494574    -0.00831185      -0.00360365
;       abs(err):  0.000001583    0.0000029886     0.0000032077
;       abs(err)
;        (km/sec): 0.00265        0.00519          0.00557
;
; PROCEDURE CALLS:
;       PRECESS_XYZ
; REVISION HISTORY
;       Original algorithm from Almanac for Computers, Doggett et al. USNO 1978
;       Adapted from the book Astronomical Photometry by A. Henden
;       Written  W. Landsman   STX       June 1989
;       Correct error in X coefficient   W. Landsman HSTX  January 1995
;       Added velocities, more terms to positions and EQUINOX keyword,
;          some minor adjustments to calculations 
;          P. Plait/ACC March 24, 1999
;-

   On_error,2
  
   if (n_params() eq 0) then begin
      print,'Syntax - XYZ, date, x, y, z, [ xvel, yvel, zvel, EQUINOX= ]'
      print,'     (date is REDUCED Julian date (JD - 2400000.0) )'
      return
   endif

   picon = !DPI/180.0d
   t = (date - 15020.0d0)/36525.0d0         ;Relative Julian century from 1900

; NOTE: longitude arguments below are given in *equinox* of date.
;   Precess these to equinox 1950 to give everything an even footing.
;   Compute argument of precession from equinox of date back to 1950
   pp = (1.396041d + 0.000308d*(t + 0.5d))*(t-0.499998d)

; Compute mean solar longitude, precessed back to 1950
   el = 279.696678D + 36000.76892D*t + 0.000303d*t*t - pp

; Compute Mean longitude of the Moon
   c = 270.434164d + 480960.d*t + 307.883142d*t - 0.001133d*t*t - pp

; Compute longitude of Moon's ascending node
   n = 259.183275d - 1800.d*t - 134.142008d*t + 0.002078d*t*t - pp

; Compute mean solar anomaly
   g = 358.475833d + 35999.04975d*t - 0.00015d*t*t

; Compute the mean jupiter anomaly
   j = 225.444651d + 2880.0d*t + 154.906654d*t*t

; Compute mean anomaly of Venus
   v = 212.603219d + 58320.d*t + 197.803875d*t + 0.001286d*t*t

; Compute mean anomaly of Mars
   m = 319.529425d + 19080.d*t + 59.8585d*t + 0.000181d*t*t

; Convert degrees to radians for trig functions
   el = el*picon
   g  = g*picon
   j =  j*picon
   c  = c*picon
   v  = v*picon
   n  = n*picon
   m  = m*picon

; Calculate X,Y,Z using trigonometric series
   X =   0.999860d*cos(el)                          $
       - 0.025127d*cos(g - el)                      $
       + 0.008374d*cos(g + el)                      $
       + 0.000105d*cos(g + g + el)                  $
       + 0.000063d*t*cos(g - el)                    $
       + 0.000035d*cos(g + g - el)                  $
       - 0.000026d*sin(g - el - j)                  $
       - 0.000021d*t*cos(g + el)                    $
       + 0.000018d*sin(2.d*g + el - 2.d*v)          $
       + 0.000017d*cos(c)                           $
       - 0.000014d*cos(c - 2.d*el)                  $
       + 0.000012d*cos(4.d*g + el - 8.d*m + 3.d*j)  $
       - 0.000012d*cos(4.d*g - el - 8.d*m + 3.d*j)  $
       - 0.000012d*cos(g + el - v)                  $
       + 0.000011d*cos(2.d*g + el - 2.d*v)          $
       + 0.000011d*cos(2.d*g - el - 2.d*j)         
  

   Y =   0.917308d*sin(el)                             $
       + 0.023053d*sin(g - el)                         $
       + 0.007683d*sin(g + el)                         $
       + 0.000097d*sin(g + g + el)                     $
       - 0.000057d*t*sin(g - el)                       $
       - 0.000032d*sin(g + g - el)                     $
       - 0.000024d*cos(g - el - j)                     $
       - 0.000019d*t*sin(g + el)                       $
       - 0.000017d*cos(2.d0*g + el - 2.d0*v)           $
       + 0.000016d*sin(c)                              $
       + 0.000013d*sin(c - 2.d0*el )                   $
       + 0.000011d*sin(4.d0*g + el - 8.d0*m + 3.d0*j)  $
       + 0.000011d*sin(4.d0*g - el - 8.d0*m + 3.d0*j)  $
       - 0.000011d*sin(g + el - v)                     $
       + 0.000010d*sin(2.d0*g + el - 2.d0*v )          $
       - 0.000010d*sin(2.d0*g - el - 2.d0*j )         


   Z =   0.397825d*sin(el)        $
       + 0.009998d*sin(g-el)      $
       + 0.003332d*sin(g+el)      $
       + 0.000042d*sin(g+g+el)    $
       - 0.000025d*t*sin(g-el)    $
       - 0.000014d*sin(g+g-el)    $
       - 0.000010d*cos(g-el-j)    

;Precess_to new equator?
   if keyword_set(equinox) then precess_xyz, x, y, z, 1950, equinox

   if N_params() LE 3 then return
   
   XVEL = -0.017200d * sin(el)           $
          -0.000288d * sin(g + el)       $
          -0.000005d * sin(2.d0*g + el)  $
          -0.000004d * sin(c)            $
          +0.000003d * sin(c - 2.d0*el)  $
          +0.000001d *t * sin(g+el)      $
          -0.000001d * sin(2.d0*g-el)           
 
   YVEL =  0.015780 * cos(el)            $
          +0.000264 * cos(g + el)        $
          +0.000005 * cos(2.d0*g + el)   $
          +0.000004 * cos(c)             $
          +0.000003 * cos(c - 2.d0*el)   $
          -0.000001 * t * cos(g + el)    

   ZVEL = 0.006843 * cos(el)             $
         +0.000115 * cos(g  + el)        $
         +0.000002 * cos(2.d0*g + el)    $
         +0.000002 * cos(c)              $
         +0.000001 * cos(c - 2.d0*el)    

;Precess to new equator?

   if keyword_set(equinox) then precess_xyz, xvel, yvel, zvel, 1950, equinox

   return
   end

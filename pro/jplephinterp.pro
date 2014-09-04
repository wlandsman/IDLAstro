;+
; NAME:
;   JPLEPHINTERP
;
; AUTHOR:
;   Craig B. Markwardt, NASA/GSFC Code 662, Greenbelt, MD 20770
;   craigm@lheamail.gsfc.nasa.gov
;   UPDATED VERSIONs can be found on my WEB PAGE: 
;      http://cow.physics.wisc.edu/~craigm/idl/idl.html
;
; PURPOSE:
;   Interpolate position and motion of planetary bodies (JPL Ephemeris)
;
; MAJOR TOPICS:
;   Planetary Orbits, Interpolation
;
; CALLING SEQUENCE:
;   JPLEPHINTERP, INFO, RAWDATA, T, X, Y, Z, [VX, VY, VZ, /EARTH, /SUN,
;         OBJECTNAME=, CENTER=, TBASE=, POSUNITS=, VELUNITS= ]
;
; DESCRIPTION:
;
;   JPLEPHINTERP interpolates the JPL DE200 or DE405 planetary
;   ephemeris to find the positions and motions of planetary bodies.
;
;   This routine is the second stage of a two-stage process to
;   interpolate the JPL ephemeris.  In this first stage, the file is
;   opened using JPLEPHREAD, and the relevant portions of the table
;   are read and stored into the two variables INFO and RAWDATA.  In
;   the second stage, the user actually interpolates the ephemeris for
;   the desired bodies and to the desired ephemeris time using
;   JPLEPHINTERP.
;
;   The only independent variable which must be specified is T, the
;   ephemeris time.  For low to moderate accuracy applications, T is
;   simply the conventional calendar date, expressed in Julian days.
;   See below for high precision applications.
;
;   Upon output, the position components of the desired body are
;   returned in parameters X, Y and Z, and if requested velocity
;   components are returned in parameters VX, VY and VZ.  Coordinates
;   are referred to the ephemeris's coordinate system: FK5 for
;   JPL-DE200 and ICRS for JPL-DE405.  By default, the origin of
;   coordinates is the solar system barycenter (SSB), unless another
;   origin is selected using the CENTER keyword.
;
;   Users must set the VELOCITY keyword to generate body velocities.
;   By default they are not generated.
;
;   Users can select the desired body by using either the EARTH or SUN
;   keywords, or the OBJECTNAME keyword.
;
;   By default, positions are returned in units of KM and velocities
;   in units of KM/DAY.  However, the output units are selectable by
;   setting the POSUNITS and VELUNITS keywords.
;
; High Precision Applications
;
;   If the required precision is finer than a few hundred meters, the
;   user must be aware that the formal definition of the ephemeris
;   time is the coordinate time of a clock placed at the solar system
;   barycenter (SSB).  If the user's time is measured by a clock
;   positioned elsewhere, then various corrections must be applied.
;   Usually, the most significant correction is that from the
;   geocenter to the SSB (see Fairhead & Bretagnon 1990; Fukushima
;   1995).  Not applying this correction creates an error with
;   amplitude ~170 nano-light-seconds ( = 50 m) on the earth's
;   position.  (see TDB2TDT)
;
;   For high precision, the user should also specify the TBASE
;   keyword.  TBASE should be considered a fixed epoch with respect to
;   which T is measured; T should be small compared to TBASE.
;   Internally, subtraction of large numbers occurs with TBASE first,
;   so truncation error is minimized by specifying TBASE.
;
; Nutations and Librations
;
;   This routine also provides information about earth nutations and
;   lunar librations, which are stored in the JPL ephemeris tables.
;   The POSUNITS and VELUNITS keywords do not affect these
;   computations.
;
;   Lunar librations in the form of three Euler angles are returned in
;   X, Y, Z, in units of radians, and their time derivatives are
;   returned in VX, VY, and VZ in units of radians per day.
;
;   The earth nutation angles psi (nutation in longitude) and epsilon
;   (nutation in obliquity) are returned in X and Y, in units of
;   radians.  Their time derivatives are returned in VX and VY
;   respectively.  The quantities returned in Z and VZ are undefined.
;
; Verification
;
;   The precision routine has been verified using JPLEPHTEST, which is
;   similar to the original JPL program EPHTEST.  For years 1950 to
;   2050, JPLEPHINTERP reproduces the original JPL ephemeris to within
;   1 centimeter.
;
; Custom Ephemerides
;
;   It is possible to make custom ephemerides using JPLEPHMAKE, or to
;   augmented an existing ephemeris with additional data.  In the
;   former case JPLEPHINTERP should automatically choose the correct
;   object from the table and interpolate it appropriately.
;
;   For augmented ephemerides, the object can be specified by name,
;   which works as expected, or by number, which has a special
;   behavior.  For augmented files only, the new objects begin at
;   number 100.
;
;
; PARAMETERS: 
;
;   INFO - structure returned by JPLEPHREAD.  Users should not modify
;          this structure.
;
;   RAWDATA - raw data array returned by JPLEPHREAD.  Users should not
;             modify this data array.
;
;   T - ephemeris time(s) of interest, relative to TBASE (i.e. the
;       actual interpolation time is (T+TBASE)).  May be a scalar or
;       vector.
;
;   X, Y, Z - upon return, the x-, y- and z-components of the body
;             position are returned in these parameters.  For
;             nutations and librations see above.
;
;   VX, VY, VZ - upon return, the x-, y- and z-components of the body
;                velocity are returned in these parameters, if the
;                VELOCITY keyword is set.  For nutations and
;                librations see above.
;
;
; KEYWORD PARAMETERS:
;
;   EARTH, SUN - set one of these keywords if the desired body is the
;                earth or the sun.  One of EARTH, SUN or OBJECTNAME
;                must be specified.
;
;   OBJECTNAME - a scalar string or integer, specifies the planetary
;                body of interest.  May take any one of the following
;                integer or string values.
;
;                   1 - 'MERCURY'     9 - 'PLUTO'
;                   2 - 'VENUS'      10 - 'MOON'  (earth's moon)
;                   3 - 'EARTH'      11 - 'SUN'
;                   4 - 'MARS'       12 - 'SOLARBARY' or 'SSB' (solar system barycenter)
;                   5 - 'JUPITER'    13 - 'EARTHBARY' or 'EMB' (earth-moon barycenter)
;                   6 - 'SATURN'     14 - 'NUTATIONS' (see above)
;                   7 - 'URANUS'     15 - 'LIBRATIONS' (see above)
;                   8 - 'NEPTUNE' 
;
;                For custom ephemerides, the user should specify the
;                object name or number.
;
;                For augmented ephemerides, the user should specify
;                the name.  If the number is specified, then numbers
;                1-15 have the above meanings, and new objects are
;                numbered starting at 100.
;
;   CENTER - a scalar string or integer, specifies the origin of
;            coordinates.  See OBJECTNAME for allowed values.
;            Default: 12 (Solar system barycenter)
;
;   VELOCITY - if set, body velocities are generated and returned in
;              VX, VY and VZ.
;              Default: unset (no velocities)
;
;   POSUNITS - a scalar string specifying the desired units for X, Y,
;              and Z.  Allowed values:
;                 'KM' - kilometers  (default)
;                 'CM' - centimeters
;                 'AU' - astronomical units
;                 'LT-S' - light seconds
;               If angles are requested, this keyword is ignored and
;               the units are always 'RADIANS'.
;
;   VELUNITS - a scalar string specifying the desired units for VX, VY
;              and VZ.  Allowed values:
;                 'KM/DAY' - kilometers per day  (default)
;                 'KM/S' - kilometers per second
;                 'CM/S' - centimeters per second
;                 'LT-S/S' or 'V/C' - light seconds per second or
;                     unitless ratio with speed of light, V/C
;                 'AU/DAY' - astronomical units per day
;
;   TBASE - a scalar or vector, specifies a fixed epoch against wich T
;           is measured.  The ephemeris time will be (T+TBASE).  Use
;           this keyword for maximum precision.
;
;
; EXAMPLE:
;
;   Find position of earth at ephemeris time 2451544.5 JD.  Units are
;   in Astronomical Units.
;
;   JPLEPHREAD, 'JPLEPH.200', pinfo, pdata, [2451544D, 2451545D]
;
;   JPLEPHINTERP, pinfo, pdata, 2451544.5D, xearth, yearth, zearth, $
;                 /EARTH, posunits='AU'
;     
;
; REFERENCES:
;
;   AXBARY, Arnold Rots.
;      ftp://heasarc.gsfc.nasa.gov/xte/calib_data/clock/bary/
;
;   HORIZONS, JPL Web-based ephermis calculator (Ephemeris DE406)
;      http://ssd.jpl.nasa.gov/horizons.html
;   
;   Fairhead, L. & Bretagnon, P. 1990, A&A, 229, 240
;
;   Fukushima, T. 1995, A&A, 294, 895
;
;   Standish, E.M. 1982, "Orientation of the JPL Ephemerides,
;      DE200/LE200, to the Dynamical Equinox of J2000", Astronomy &
;      Astrophysics, vol. 114, pp. 297-302.
;
;   Standish, E.M.: 1990, "The Observational Basis for JPL's DE200,
;      the planetary ephemeris of the Astronomical Almanac", Astronomy
;      & Astrophysics, vol. 233, pp. 252-271.    
;
; SEE ALSO
;   JPLEPHREAD, JPLEPHINTERP, JPLEPHTEST, TDB2TDT, JPLEPHMAKE
;   
; MODIFICATION HISTORY:
;   Written and Documented, CM, Jun 2001
;   Corrected bug in name conversion of NUTATIONS and LIBRATIONS, 18
;     Oct 2001, CM
;   Added code to handle custom-built ephemerides, 04 Mar 2002, CM
;   Fix bug in evaluation of velocity (only appears in highest order
;     polynomial term); JPLEPHTEST verification tests still pass;
;     change is of order < 0.5 cm in position, 22 Nov 2004, CM
;   Perform more validity checking on inputs; and more informative
;     outputs, 09 Oct 2008, CM
;   Allow SSB and EMB as shortcuts for solar system and earth-moon
;     bary center, 15 Oct 2008, CM
;   TBASE now allowed to be a vector or scalar, 01 Jan 2009, CM
;   VELFAC keyword gives scale factor between POSUNITS and VELUNITS, 
;     12 Jan 2009, CM
;   Add option VELUNITS='V/C' for unitless ratio with speed of light,
;     2012-10-02, CM;
;
;  $Id: jplephinterp.pro,v 1.19 2012/10/02 11:32:59 cmarkwar Exp $
;
;-
; Copyright (C) 2001, 2002, 2004, 2008, 2009, 2012, Craig Markwardt
; This software is provided as is without any warranty whatsoever.
; Permission to use, copy and distribute unmodified copies for
; non-commercial purposes, and to modify and use for personal or
; internal use, is granted.  All other rights are reserved.
;-

pro jplephinterp_calc, info, raw, obj, t, x, y, z, vx, vy, vz, $
                       velocity=vel, tbase=tbase

  ; '$Id: jplephinterp.pro,v 1.19 2012/10/02 11:32:59 cmarkwar Exp $'

  if n_elements(tbase) EQ 0 then tbase = 0D
  ;; Number of coefficients (x3), number of subintervals, num of rows
  nc = info.ncoeff[obj]
  ns = info.nsub[obj]
  dt = info.timedel
  nr = info.jdrows
  jd0 = info.jdlimits[0] - tbase
  jd1 = info.jdlimits[1] - tbase

  ;; Extract coefficient data from RAW
  if obj EQ 11 then begin
      ;; Nutations have two components
      ii1 = info.ptr[obj]-1
      ii2 = ii1 + nc*ns*2L - 1
      coeffs = reform(dblarr(nc,3,ns,nr), nc,3,ns,nr, /overwrite)
      coeffs[0,0,0,0] = reform(raw[ii1:ii2,*],nc,2,ns,nr, /overwrite)
  endif else begin
      ;; All other bodies are done with three components
      ii1 = info.ptr[obj]-1
      ii2 = ii1 + nc*ns*3L - 1
      coeffs = reform(raw[ii1:ii2,*],nc,3,ns,nr, /overwrite)
  endelse

  ;; Decide which interval and subinterval we are in
  tint = (t-jd0)/dt        ;; Interval number (real)
  ieph = floor(tint)       ;; Interval number (index = int)
  tint = (tint-ieph)*ns    ;; Subinterval number (real)
  nseg = floor(tint)       ;; Subinterval number (index = int)
  ;; Chebyshev "x" (rescaled to range = [-1,1] over subinterval)
  tseg = 2D*(tint - nseg) - 1  

  ;; Below is an optimization.  If the time interval doesn't span an
  ;; ephemeris subinterval, then we can index the coefficient array by
  ;; a scalar, which is much faster.  Otherwise we maintain the full
  ;; vector-level indexing.
  mini = minmax(ieph) & minn = minmax(nseg)
  if mini[0] EQ mini[1] AND minn[0] EQ minn[1] then begin
      ieph = ieph[0]
      nseg = nseg[0]
  endif

  ;; Initialize the first two Chebyshev polynomials, which are P_0 = 1
  ;; and P_1(x) = x
  p0 = 1D
  p1 = tseg
  ;; Initial polynomials for Chebyshev derivatives, V_0 = 0, V_1(x) =
  ;; 1, V_2(x) = 4*x
  v0 = 0D
  v1 = 1D
  v2 = 4D*tseg
  tt = 2D*temporary(tseg)

  x  = 0D & y  = 0D & z  = 0D
  vx = 0D & vy = 0D & vz = 0D
  i0 = ieph*0 & i1 = i0 + 1 & i2 = i1 + 1

  ;; Compute Chebyshev functions two at a time for efficiency
  for i = 0, nc-1, 2 do begin
      if i EQ nc-1 then begin
          p1 = 0
          v1 = 0
      endif
      ii = i0 + i
      jj = i0 + ((i+1) < (nc-1))
      
      x = x + coeffs[ii,i0,nseg,ieph]*p0 + coeffs[jj,i0,nseg,ieph]*p1
      y = y + coeffs[ii,i1,nseg,ieph]*p0 + coeffs[jj,i1,nseg,ieph]*p1
      z = z + coeffs[ii,i2,nseg,ieph]*p0 + coeffs[jj,i2,nseg,ieph]*p1

      if keyword_set(vel) then begin
          vx = vx + coeffs[ii,i0,nseg,ieph]*v0 + coeffs[jj,i0,nseg,ieph]*v1
          vy = vy + coeffs[ii,i1,nseg,ieph]*v0 + coeffs[jj,i1,nseg,ieph]*v1
          vz = vz + coeffs[ii,i2,nseg,ieph]*v0 + coeffs[jj,i2,nseg,ieph]*v1

          ;; Advance to the next set of Chebyshev polynomials. For
          ;; velocity we need to keep the next orders around
          ;; momentarily.
          p2 = tt*p1 - p0
          p3 = tt*p2 - p1
          v2 = tt*v1 - v0 + 2*p1
          v3 = tt*v2 - v1 + 2*p2
          
          p0 = temporary(p2) & p1 = temporary(p3)
          v0 = temporary(v2) & v1 = temporary(v3)
      endif else begin
          ;; Advance to the next set of Chebyshev polynomials.  For no
          ;; velocity, we can re-use old variables.
          p0 = tt*p1 - temporary(p0)
          p1 = tt*p0 - temporary(p1)
      endelse
  endfor

  if keyword_set(vel) then begin
      vfac = 2D*ns/dt
      vx = vx * vfac
      vy = vy * vfac
      vz = vz * vfac
  endif

  return
end

pro jplephinterp_denew, info, raw, obj, t, x, y, z, vx, vy, vz, $
                        velocity=vel, tbase=tbase

  if n_elements(tbase) EQ 0 then tbase = 0D
  dt = info.timedel
  nr = info.jdrows
  jd0 = info.jdlimits[0]
  jd1 = info.jdlimits[1]
  c = info.c / 1000D
  cday = 86400D*info.c/1000D

  ;; Renormalize to fractional and whole days, so fractional
  ;; component is between -.5 and +.5, as needed by barycentering
  ;; approximation code.
  ti  = round(t)      ;; Delta Time: integer
  tbi = round(tbase)  ;; Base: integer
  
  tc = ti + tbi             ;; Total time: integer
  tt = (t-ti) + (tbase-tbi) ;; Total time: fractional

  tc = tc + round(tt)       ;; Re-round: integer
  tt = tt - round(tt)       ;; Re-round: fractional
  t2 = tt*tt                ;; Quadratic and cubic terms
  t3 = t2*tt

  ieph = tc - round(jd0)
  ;; Below is an optimization.  If the time interval doesn't span an
  ;; ephemeris subinterval, then we can index the coefficient array by
  ;; a scalar, which is much faster.  Otherwise we maintain the full
  ;; vector-level indexing.
  mini = minmax(ieph)
  if mini[0] EQ mini[1] then ieph = ieph[0]

  if obj EQ 3 then begin
      ;; Earth, stored as Taylor series coefficients per day
      x = (raw[0,ieph] + raw[3,ieph]*tt + 0.5D*raw[6,ieph]*t2 + $
           (raw[9,ieph]/6D)*t3)
      y = (raw[1,ieph] + raw[4,ieph]*tt + 0.5D*raw[7,ieph]*t2 + $
           (raw[10,ieph]/6D)*t3)
      z = (raw[2,ieph] + raw[5,ieph]*tt + 0.5D*raw[8,ieph]*t2 + $
           (raw[11,ieph]/6D)*t3)
      if keyword_set(vel) then begin
          vx = raw[3,ieph] + raw[6,ieph]*tt + 0.5D*raw[9 ,ieph]*t2
          vy = raw[4,ieph] + raw[7,ieph]*tt + 0.5D*raw[10,ieph]*t2
          vz = raw[5,ieph] + raw[8,ieph]*tt + 0.5D*raw[11,ieph]*t2
      endif
      x = reform(x, /overwrite)
      y = reform(y, /overwrite)
      z = reform(z, /overwrite)
 
  endif else if obj EQ 11 then begin
      ;; Sun, stored as daily components only
      
      x = reform(raw[12,ieph] + tt*0)
      y = reform(raw[13,ieph] + tt*0)
      z = reform(raw[14,ieph] + tt*0)
      if keyword_set(vel) then $
        message, 'ERROR: DENEW format does not provide solar velocity'

  endif else if obj EQ 1000 then begin

      tt = t - (jd0+jd1)/2D
      x = spl_interp(raw[15,*], raw[16,*], raw[17,*], tt)
      return
      
  endif else begin
      message, 'ERROR: DENEW format does not contain body '+strtrim(obj,2)
  endelse
end

pro jplephinterp, info, raw, t, x, y, z, vx, vy, vz, earth=earth, sun=sun, $
                  objectname=obj0, velocity=vel, center=cent, tbase=tbase, $
                  posunits=outunit0, velunits=velunit0, $
                  pos_vel_factor=velfac, $
                  xobjnum=objnum, decode_obj=decode

  if n_params() EQ 0 then begin
      message, 'USAGE: JPLEPHINTERP, info, rawdata, teph, x, y, z, '+$
        'vx, vy, vz, OBJECTNAME="body", /VELOCITY, CENTER="body", '+$
        'POSUNITS="units", VELUNITS="units", /EARTH, /SUN', /info
      return
  endif
  
  ;; The numbering convention for ntarg and ncent is:
  ;;   1 = Mercury            8 = Neptune
  ;;   2 = Venus              9 = Pluto
  ;;   3 = Earth             10 = Moon
  ;;   4 = Mars              11 = Sun
  ;;   5 = Jupiter           12 = Solar system barycenter
  ;;   6 = Saturn            13 = Earth-Moon barycenter
  ;;   7 = Uranus            14 = Nutations (longitude and obliquity; untested)
  ;;                         15 = Librations 
  ;; This numbering scheme is 1-relative, to be consistent with the
  ;; Fortran version.  (units are seconds; derivative units are seconds/day)
  ;;1000 = TDB to TDT offset (s), returned in X component

  sz = size(info)
  if sz[sz[0]+1] NE 8 then message, 'ERROR: INFO must be a structure'
  if ((info.format NE 'JPLEPHMAKE') AND $
      (info.format NE 'BINEPH2FITS') AND $
      (info.format NE 'DENEW')) then begin
      message, 'ERROR: ephemeris type "'+info.format+'" is not recognized'
  endif

  ;; Handle case of custom ephemerides
  if info.format EQ 'JPLEPHMAKE' then begin
      if n_elements(obj0) GT 0 then begin
          sz = size(obj0)
          if sz[sz[0]+1] EQ 7 then begin
              obj = strupcase(strtrim(obj0[0],2))
              wh = where(info.objname EQ obj, ct)
              if ct EQ 0 then $
                message, 'ERROR: '+obj+' is an unknown object'
              obj = wh[0] + 1
          endif else begin
              obj = floor(obj0[0])
              if obj LT 1 OR obj GT n_elements(info.objname) then $
                message, 'ERROR: Numerical OBJNAME is out of bounds'
          endelse

          ;; Interpolate the ephemeris here
          jplephinterp_calc, info, raw, obj-1, t, velocity=vel, $
            tbase=tbase, x, y, z, vx, vy, vz

          goto, COMPUTE_CENTER
      endif
      message, 'ERROR: Must specify OBJNAME for custom ephemerides'
  endif


  ;; ----------------------------------------------------------
  ;; Determine which body or system we will compute
  if n_elements(obj0) GT 0 then begin
      sz = size(obj0)
      if sz[sz[0]+1] EQ 7 then begin
          obj = strupcase(strtrim(obj0[0],2))
          case obj of 
              'EARTH':      obj = 3
              'SOLARBARY':  obj = 12
              'SSB':        obj = 12
              'EARTHBARY':  obj = 13
              'EMB':        obj = 13
              'NUTATIONS':  obj = 14
              'LIBRATIONS': obj = 15
              'TDB2TDT':    obj = 1000
              ELSE: begin
                  wh = where(info.objname EQ obj, ct)
                  if ct EQ 0 then $
                    message, 'ERROR: '+obj+' is an unknown object'
                  obj = wh[0] + 1
                  if obj GT 11 then obj = obj + 100 - 14
              end
          endcase
      endif else begin
          obj = floor(obj0[0])
      endelse
  endif else begin
      if NOT keyword_set(earth) AND NOT keyword_set(sun) then $
        message, 'ERROR: Must specify OBJNAME, EARTH or SUN'
  endelse
  if keyword_set(earth) then obj = 3
  if keyword_set(sun)   then obj = 11

  ;; If the caller is merely asking us to decode the objectnumber,
  ;; then return it now.
  objnum = obj
  if keyword_set(decode) then return
  
  jdlimits = info.jdlimits

  ;; -------------------------------------------------------
  ;; Handle case of de200_new.fits format
  if info.format EQ 'DENEW' then begin
      if objnum NE 3 AND objnum NE 11 AND objnum NE 1000 then $
        message, 'ERROR: DENEW ephemeris table does not support body #'+$
        strtrim(objnum,2)
      
      jplephinterp_denew, info, raw, objnum, t, x, y, z, vx, vy, vz, $
        velocity=vel, tbase=tbase

      if objnum GE 1000 then return
      goto, DO_UNIT
  endif

  ;; -------------------------------------------------------
  ;; Otherwise, construct the ephemeris using the Chebyshev expansion
  case obj of
      3: begin ;; EARTH (translate from earth-moon barycenter to earth)
          ;; Interpolate the earth-moon and moon ephemerides
          jplephinterp_calc, info, raw, 2, velocity=vel, tbase=tbase, $
            t, xem, yem, zem, vxem, vyem, vzem
          jplephinterp_calc, info, raw, 9, velocity=vel, tbase=tbase, $
            t, xmo, ymo, zmo, vxmo, vymo, vzmo
          emrat = info.emrat
          
          ;; Translate from the earth-moon barycenter to earth
          x = xem - emrat * xmo
          y = yem - emrat * ymo
          z = zem - emrat * zmo
          if keyword_set(vel) then begin
              vx = vxem - emrat * vxmo
              vy = vyem - emrat * vymo
              vz = vzem - emrat * vzmo
          endif
          
      end

      10: begin ;; MOON (translate from earth-moon barycenter to moon)
          jplephinterp_calc, info, raw, 9, t, velocity=vel, tbase=tbase, $
            x, y, z, vx, vy, vz
          ;; Moon ephemeris is geocentered.  If the center is
          ;; explicitly earth then return immediately.  Otherwise
          ;; follow the standard path via the solar barycenter.
          if n_elements(cent) GT 0 then begin
              jplephinterp, info, objectname=cent[0], tbase=tbase, $
                xobjnum=cent1, /decode_obj
              if cent1 EQ 3 then goto, DO_UNIT
          endif

          ;; Use solar barycenter via the earth-moon barycenter
          jplephinterp_calc, info, raw, 2, t, velocity=vel, tbase=tbase, $
            xem, yem, zem, vxem, vyem, vzem
          emrat = 1d - info.emrat
          x = xem + emrat * x
          y = yem + emrat * y
          z = zem + emrat * z
          if keyword_set(vel) then begin
              vx = vxem + emrat * vx
              vy = vyem + emrat * vy
              vz = vzem + emrat * vz
          endif              
      end

      12: begin ;; SOLARBARY
          x = t*0D & y = x & z = x
          vx = x   & vy = x & vz = x
      end

      13: begin ;; EARTHBARY
          jplephinterp_calc, info, raw, 2, velocity=vel, tbase=tbase, $
            t, x, y, z, vx, vy, vz
      end
      
      14: begin ;; NUTATIONS
          ;; X = PSI, Y = EPSILON, VX = PSI DOT, VY = EPSILON DOT 
          jplephinterp_calc, info, raw, 11, velocity=vel, tbase=tbase, $
            t, x, y, z, vx, vy, vz
          goto, CLEAN_RETURN
      end

      15: begin ;; LIBRATIONS
          jplephinterp_calc, info, raw, 12, velocity=vel, tbase=tbase, $
            t, x, y, z, vx, vy, vz
          goto, CLEAN_RETURN
      end

      1000: begin ;; TDT to TDB conversion
          x = tdb2tdt(t, deriv=vx, tbase=tbase)
          if n_elements(velunit0) GT 0 then begin
             ;; Special case of unit conversion when user asks for 
             ;; "per second"
             if strpos(strupcase(velunit0[0]),'/S') GE 0 then $
                vx = vx / 86400d
          endif

          goto, CLEAN_RETURN
      end

      else: begin
          ;; Default objects are derived from the index OBJNUM
          if obj GE 1 AND obj LE 11 then begin
              RESTART_OBJ:
              jplephinterp_calc, info, raw, obj-1, t, velocity=vel, $
                tbase=tbase, $
                x, y, z, vx, vy, vz
          endif else begin
              if info.edited AND obj GT 11 then begin
                  ;; Handle case of edited JPL ephemerides - they
                  ;; start at a value of 100, so shift them to the end
                  ;; of the JPL ephemeris columns
                  obj = obj - 100 + 14
                  if obj LE n_elements(info.objname) then $
                    goto, RESTART_OBJ
              endif
              message, 'ERROR: body '+strtrim(obj,2)+' is not supported'
          endelse
      end
  endcase

  ;; -------------------------------------------------------
  ;; Compute ephemeris of center, and compute displacement vector
  COMPUTE_CENTER:
  if n_elements(cent) GT 0 then begin
      jplephinterp, info, raw, t, x0, y0, z0, vx0, vy0, vz0, tbase=tbase, $
        objectname=cent, velocity=vel, posunits='KM', velunits='KM/DAY'
      x = temporary(x) - temporary(x0)
      y = temporary(y) - temporary(y0)
      z = temporary(z) - temporary(z0)
      if keyword_set(vel) then begin
          vx = temporary(vx) - temporary(vx0)
          vy = temporary(vy) - temporary(vy0)
          vz = temporary(vz) - temporary(vz0)
      endif
  endif

  DO_UNIT:
  
  velfac = 1d

  ;; -------------------------------------------------------
  ;; Convert positional units
  if n_elements(outunit0) GT 0 then begin
      pu = strupcase(strtrim(outunit0[0],2))
      case pu of
          'KM': km = 1 ;; Dummy statement
          'CM': begin
              x = x * 1D5
              y = y * 1D5
              z = z * 1D5
              velfac = velfac * 1D5
          end
          'AU': begin
              au = info.au*info.c/1000d
              x = x / au
              y = y / au
              z = z / au
              velfac = velfac / au
          end
          'LT-S': begin
              c = info.c / 1000d
              x = x / c
              y = y / c
              z = z / c
              velfac = velfac / c
          end
          ELSE: message, 'ERROR: Unrecognized position units "'+pu+'"'
      endcase
  endif

  ;; -------------------------------------------------------
  ;; Convert velocity units
  if n_elements(velunit0) GT 0 AND keyword_set(vel) then begin
      vu = strupcase(strtrim(velunit0[0],2))
      case vu of 
          'CM/S': begin
              vx = vx * (1D5/86400D)
              vy = vy * (1D5/86400D)
              vz = vz * (1D5/86400D)
              velfac = velfac / (1D5/86400D)
          end
          'KM/S': begin
              vx = vx * (1D/86400D)
              vy = vy * (1D/86400D)
              vz = vz * (1D/86400D)
              velfac = velfac / (1D/86400D)
          end
          'LT-S/S': begin
              c = info.c / 1000D
              vx = vx / (c*86400D)
              vy = vy / (c*86400D)
              vz = vz / (c*86400D)
              velfac = velfac / (c*86400D)
          end
          'V/C': begin ;; Unitless ratio V/C (same as LT-S/S
              c = info.c / 1000D
              vx = vx / (c*86400D)
              vy = vy / (c*86400D)
              vz = vz / (c*86400D)
              velfac = velfac / (c*86400D)
          end	  
          'KM/DAY': km = 1 ;; Dummy statement
          'AU/DAY': begin
              au = info.au*info.c/1000d
              vx = vx / au
              vy = vy / au
              vz = vz / au
              velfac = velfac * au
          end
          ELSE: message, 'ERROR: Unrecognized velocity units "'+vu+'"'
      endcase
  endif

CLEAN_RETURN:  
  return
end

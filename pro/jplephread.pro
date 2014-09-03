;+
; NAME:
;   JPLEPHREAD
;
; AUTHOR:
;   Craig B. Markwardt, NASA/GSFC Code 662, Greenbelt, MD 20770
;   craigm@lheamail.gsfc.nasa.gov
;   UPDATED VERSIONs can be found on my WEB PAGE: 
;      http://cow.physics.wisc.edu/~craigm/idl/idl.html
;
; PURPOSE:
;   Open and read JPL DE200 or DE405 Ephemeride FITS File
;
; MAJOR TOPICS:
;   Planetary Orbits, Interpolation
;
; CALLING SEQUENCE:
;   JPLEPHREAD, FILENAME, INFO, RAWDATA, JDLIMITS, STATUS=, ERRMSG=
;
; DESCRIPTION:
;
;   JPLEPHREAD opens and reads the JPL DE200 or DE405 planetary
;   ephemerides, as available in FITS format.  The user must have the
;   IDL Astronomy Library installed to use this routine.
;
;   This routine is the initialization stage of a two-stage process to
;   interpolate the JPL ephemeris.  In this first stage, the file is
;   opened, and the relevant portions of the table are read and stored
;   into the two variables INFO and RAWDATA.  In the second stage, the
;   user actually interpolates the ephemeris for the desired bodies
;   and to the desired ephemeris time using JPLEPHINTERP.
;
;   Users must decide ahead of time the approximate dates of interest,
;   and pass this range in the JDLIMITS parameter.  Any date covered
;   by the ephemeris is valid.
;
;   JPLEPHREAD is able to read files of the following format:
;     DE200 - Chebyshev - FITS format - Note 1
;     DE405 - Chebyshev - FITS format - Note 1
;     DE200 - Taylor    - FITS format - Note 2
;
;   Note 1 - Chebyshev formatted FITS files are available in the
;            AXBARY package by Arnold Rots, found here:
;              ftp://heasarc.gsfc.nasa.gov/xte/calib_data/clock/bary/
;            or at the Markwardt FTP site:
;              ftp://cow.physics.wisc.edu/pub/craigm/bary/
;
;   Note 2 - Taylor-series based ephemerides have been available for
;            years in the FTOOLS / LHEASOFT package produced by NASA's
;            Goddard Space Flight Center.  The original file is
;            de200_new.fits, which covers the years 1959-2000,
;            inclusive.  A newer file is named
;            de200_1950-2050_v2.fits, and covers the years 1959-2050.
;            See Markwardt FTP site for these files.
;
; PARAMETERS: 
;
;   FILENAME - name of ephemeris file (scalar string).
;
;   INFO - upon completion, information about the ephemeris data is
;          returned in this parameter in the form of a structure.
;          Users must not modify INFO, although several fields are
;          useful and may be accessed read-only:
;              TSTART/TSTOP (start and stop time of data in Julian
;                            days);
;              C (speed of light in m/s);
;              DENUM (development ephemeris number [200 or 405])
;              AU (1 astronomical unit, in units of light-seconds)
;
;   RAWDATA - upon completion, raw ephemeris data is returned in this
;             parameter.  Users are not meant to access this data
;             directly, but rather to pass it to JPLEPHINTERP.
;
;   JDLIMITS - a two-element vector (optional), describing the desired
;              time range of interest.  The vector should have the
;              form [TSTART, TSTOP], where TSTART and TSTOP are the
;              beginning and ending times of the range, expressed in
;              Julian days.
;              Default: entire table is read (note, this can be
;              several megabytes)
;
;
; KEYWORD PARAMETERS:
;
;   STATUS - upon completion, a value of 1 indicates success, and 0
;            indicates failure.
;
;   ERRMSG - upon completion, an error message is returned in this
;            keyword.  If there were no errors, then the returned
;            value is the empty string, ''.
;
;
; EXAMPLE:
;
;   Find position of earth at ephemeris time 2451544.5 JD.  Units are
;   in Astronomical Units.
;
;   JPLEPHREAD, 'JPLEPH.405', pinfo, pdata, [2451544D, 2451545D]
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
;      http://ssd.jpl.nasa.gov/?horizons
;   
;   JPL Export Ephemeris FTP Site
;      ftp://ssd.jpl.nasa.gov/pub/eph/planets/
;      (ephemeris files are available here, however, they must be
;      converted to FITS format using the "bin2eph" utility found in
;      AXBARY)
;
;   JPL Export Ephemeris CD-ROM - Ordering Information
;      http://www.willbell.com/software/jpl.htm
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
;   JPLEPHREAD, JPLEPHINTERP, JPLEPHTEST
; PROCEDURES USED:
;     FXBCLOSE, FXBOPEN, FXPAR(), 
;   
; MODIFICATION HISTORY:
;   Written and Documented, CM, Jun 2001
;   Use GETTOK() instead of STR_SEP()  W. Landsman  July 2002
;   Add ephemeris file keywords to INFO, Jan 2002, CM
;   Add fields to INFO to be consistent with JPLEPHMAKE, 04 Mar 2002, CM
;   Correction of units for INFO.C (Thanks Mike Bernhardt), 2011-04-11, CM 
;  $Id: jplephread.pro,v 1.10 2011/06/27 18:44:44 cmarkwar Exp $
;
;-
; Copyright (C) 2001, Craig Markwardt
; This software is provided as is without any warranty whatsoever.
; Permission to use, copy and distribute unmodified copies for
; non-commercial purposes, and to modify and use for personal or
; internal use, is granted.  All other rights are reserved.
;-


function jplephpar, header, parname, default=default, fatal=fatal
compile_opt idl2

  ; '$Id: jplephread.pro,v 1.6 2001/07/01 03:32:02 craigm Exp $'

  value = fxpar(header, parname, Count = N_value)
  if N_value EQ 0 then begin
      if keyword_set(fatal) then $
        message, 'ERROR: keyword '+strupcase(parname)+' was not found'
        return, default
  endif
  return, value
end

function jplephval, names, values, name, default=default, fatal=fatal
  wh = where(names EQ strupcase(name), ct)
  if ct EQ 0 then begin
      if keyword_set(fatal) then $
        message, 'ERROR: value '+strupcase(name)+' was not found in file'
      return, default
  endif
  return, values[wh[0]]
end

pro jplephread, filename, info, raw, jdlimits, $
                status=status, errmsg=errmsg

  status = 0
  printerror = 1 - arg_present(errmsg)
  errmsg = ''

  if n_params() EQ 0 then begin
      message, 'USAGE: JPLEPHREAD, filename, info, rawdata, jdlimits', /info
      return
  endif

;  if n_elements(jdlimits) LT 2 then begin
;      errmsg = 'ERROR: You must specify JDLIMITS'
;      return
;  endif

  fxbopen, unit, filename, 1, ephhead, errmsg=errmsg
  if errmsg NE '' then $
            if printerror then message,errmsg else return

  extname = strtrim(fxpar(ephhead, 'EXTNAME'),2)
  ttype1  = strtrim(fxpar(ephhead, 'TTYPE1'),2)

  if (extname EQ 'EPHEM' AND ttype1 EQ 'EARTH') then begin
      ;; This is the DE200_NEW format (standard FTOOLS)

      nrows  = fxpar(ephhead, 'NAXIS2')
      tstart = fxpar(ephhead, 'TSTART')
      tstop  = fxpar(ephhead, 'TSTOP')
      timedel = jplephpar(ephhead, 'TIMEDEL', default=1D) ;; 1-day default

      ;; Constants from XTEBARYCEN.F
      C=2.99792458D+8
      TWOPI=6.28318530717958648D0
      DAYSEC=1.D0/86400.D0
      AULTSC=499.004782D0
      GAUSS=0.01720209895D0
      RSCHW=(GAUSS^2)*(AULTSC^3)*(DAYSEC^2)
      SUNRAD=2.315D0

      if n_elements(jdlimits) GE 2 then begin
          if (min(jdlimits) LT tstart OR $
              max(jdlimits) GT tstop) then begin
              errmsg = 'ERROR: '+filename+$
                ' does not cover the time of interest'
              fxbclose, unit
              return
          endif
          ;; Expand by one row either side
          rowlimits = floor((jdlimits-tstart)/timedel) + [-2,2]
          rowlimits = rowlimits > 1 < nrows
      endif else begin
          jdlimits  = [tstart, tstop]
          rowlimits = [1L, nrows]
      endelse

      ;; Read raw data
      fxbread, unit, cearth, 'EARTH', rowlimits, errmsg=errmsg
      if errmsg EQ '' then $
        fxbread, unit, csun, 'SUN', rowlimits, errmsg=errmsg
      if errmsg EQ '' then $
        fxbread, unit, ctdb2tdt, 'TIMEDIFF', rowlimits, errmsg=errmsg
      fxbclose, unit
      if errmsg NE '' then $
         if printerror then message,errmsg else return

      nr = rowlimits[1]-rowlimits[0]+1
      t0 = dindgen(nr)*timedel - (jdlimits[1]-jdlimits[0])/2D
      dtt = spl_init(t0, ctdb2tdt)
      raw = reform(dblarr(18, nr), 18, nr, /overwrite)
      raw[0 :11,*] = cearth * c/1000D  ;; units of lt-s
      raw[12:14,*] = csun * c/1000D    ;; units of lt-s/day
      raw[15,   *] = t0
      raw[16   ,*] = ctdb2tdt
      raw[17   ,*] = dtt

      jdlimits1 = (rowlimits+[-1,0])*timedel + tstart

      info = {filename: filename, edited: 0L, $
              creation_date: '', author: '', $
              nrows: nrows, tstart: tstart, tstop: tstop, $
              timedel: timedel, format: 'DENEW', $
              denum: 200L, c: c, emrat: 0.012150586D, $
              au: aultsc, msol: rschw, sunrad: sunrad, $
              jdlimits: jdlimits1, jdrows: nr }


  endif else if (extname EQ 'DE1' AND ttype1 EQ 'Cname') then begin
      ;; This is the BINEPH2FITS format (either DE200 or DE405)

      ;; ---------------------------------------------
      ;; First extension contains parameter data
      fxbread, unit, cname, 'Cname'
      fxbread, unit, cvalue, 'Cvalue'
      cname = strtrim(cname,2)

      denum = 0L & clight = 0D & emrat = 0D & au = 0D
      msol = 0D & radsol = 0D

      denum  = round(jplephval(cname, cvalue, 'DENUM', /fatal))
      clight = jplephval(cname, cvalue, 'CLIGHT', /fatal)
      emrat  = jplephval(cname, cvalue, 'EMRAT',  /fatal)
      au     = jplephval(cname, cvalue, 'AU',     /fatal)    ; km
      msol   = jplephval(cname, cvalue, 'GMS',    /fatal)    ; AU^3/day^2
      radsol = jplephval(cname, cvalue, 'RADS', default=-1D) ; km
      if radsol EQ -1D then $
        radsol = jplephval(cname, cvalue, 'ASUN', default=-1D)

      emrat = 1D / (1D + emrat)
        
      if clight EQ 0 then begin
          errmsg = 'ERROR: Could not load physical constants from '+filename
          fxbclose, unit
          return
      endif
 
      x = au / clight                     ;; AU (lt sec)     
      msol = msol * x * x * x / 86400D^2  ;; GM_sun (in lt sec)
      radsol = radsol / clight            ;; Solar radius (lt sec)
      clight = clight * 1000              ;; Speed of light (m/s)
      
      fxbclose, unit

      ;; ---------------------------------------------
      ;; Second extension contains accounting data
      fxbopen, unit, filename, 2, ephhead, errmsg=errmsg
      if errmsg NE '' then $
            if printerror then message,errmsg else return

      extname = strtrim(fxpar(ephhead, 'EXTNAME'),2)
      if extname NE 'DE2' then begin
          errmsg = 'ERROR: '+filename+' is not a JPL ephemeris file'
          fxbclose, unit
          return
      endif

      fxbread, unit, ephobj, 'Object', errmsg=errmsg
      if errmsg EQ '' then $
        fxbread, unit, ephptr, 'Pointer', errmsg=errmsg
      if errmsg EQ '' then $
        fxbread, unit, ephncoeff, 'NumCoeff', errmsg=errmsg
      if errmsg EQ '' then $
        fxbread, unit, ephnsub, 'NumSubIntv', errmsg=errmsg
      fxbclose, unit
      if errmsg NE '' then begin
            errmsg = 'ERROR: could not read '+filename+' extension 2'
            if printerror then message,errmsg else return
      endif

      ;; Trim each object name to first word only
          ephobj = strupcase(gettok(ephobj, ' '))
      
      ;; ---------------------------------------------
      ;; Third extension contains Chebyshev coefficients
      fxbopen, unit, filename, 3, ephhead, errmsg=errmsg
      if errmsg NE '' then return
      extname = strtrim(fxpar(ephhead, 'EXTNAME'),2)
      if extname NE 'DE3' then begin
          errmsg = 'ERROR: '+filename+' is not a JPL ephemeris file'
          fxbclose, unit
          if printerror then message,errmsg else return
      endif
      
      nrows  = fxpar(ephhead, 'NAXIS2')
      tstart = fxpar(ephhead, 'TSTART')
      tstop  = fxpar(ephhead, 'TSTOP')
      timedel = jplephpar(ephhead, 'TIMEDEL', default=32D) ;; 32-day default

      if floor((tstop-tstart + 0.5)/timedel) NE nrows then begin
          errmsg = 'ERROR: Incorrect number of rows in '+filename
          fxbclose, unit
          if printerror then message,errmsg else return
      endif

      if n_elements(jdlimits) GE 2 then begin
          if (min(jdlimits) LT tstart OR $
              max(jdlimits) GT tstop) then begin
              errmsg = 'ERROR: '+filename+$
                ' does not cover the time of interest'
              fxbclose, unit
              if printerror then message,errmsg else return
          endif
          ;; Expand by two rows either side
          rowlimits = floor((jdlimits-tstart)/timedel) + [-2,2]
          rowlimits = rowlimits > 1 < nrows
      endif else begin
          jdlimits  = [tstart, tstop]
          rowlimits = [1L, nrows]
      endelse

      ;; Read raw data
      dims = fxbdimen(unit, 'ChebCoeffs')
      fxbread, unit, coeffs, 'ChebCoeffs', rowlimits, errmsg=errmsg
      fxbclose, unit
      if errmsg NE '' then $
         if printerror then message,errmsg else return


      raw = reform(coeffs, [dims, rowlimits[1]-rowlimits[0]+1], /overwrite)

      jdlimits1 = (rowlimits+[-1,0])*timedel + tstart
      if (abs(min(raw[0,*]) - jdlimits1[0]) GT 1d-6 OR $
          abs(max(raw[1,*]) - jdlimits1[1]) GT 1d-6) then begin
          errmsg = 'ERROR: JDLIMITS and time column do not match'
          if printerror then message,errmsg else return
      endif

      nr = rowlimits[1]-rowlimits[0]+1
     info = {filename: filename, edited: 0L, $
              creation_date: '', author: '', $
              nrows: nrows, tstart: tstart, tstop: tstop, $
              timedel: timedel, format: 'BINEPH2FITS', $
              denum: denum, c: clight, emrat: emrat, $
              au: au*1000/clight, msol: msol, sunrad: radsol, $
              jdlimits: jdlimits1, jdrows: nr, $
              objname: ephobj, ptr: ephptr, ncoeff: ephncoeff, $
              nsub: ephnsub, keywords: cname, keyvalues: cvalue}
;                 aufac: 1D/clight, velfac: 2D/(timedel*86400D), $

  endif else begin
      errmsg = 'ERROR: '+filename+' was not in a recognized format'
      fxbclose, unit
      if printerror then message,errmsg else return
  endelse

  errmsg = ''
  status = 1
  return
end

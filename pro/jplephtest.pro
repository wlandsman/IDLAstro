;+
; NAME:
;   JPLEPHTEST
;
; AUTHOR:
;   Craig B. Markwardt, NASA/GSFC Code 662, Greenbelt, MD 20770
;   craigm@lheamail.gsfc.nasa.gov
;   UPDATED VERSIONs can be found on my WEB PAGE: 
;      http://cow.physics.wisc.edu/~craigm/idl/idl.html
;
; PURPOSE:
;   Test JPLEPHTEST with JPL test data set
;
; MAJOR TOPICS:
;   Planetary Orbits, Interpolation
;
; CALLING SEQUENCE:
;   JPLEPHTEST, EPHFILE, TESTFILE
;
; DESCRIPTION:
;
;   JPLEPHTEST tests the JPLEPHINTERP procedure for precision.  In
;   order to function, you must have a JPL ephemeris test data set.
;   The test data set testpo.405 is available in 
;   ftp://idlastro.gsfc.nasa.gov/pub/data
;
;   The procedure opens and reads the test set, which contains
;   precomputed data.  Every tenth value is printed on the screen.
;   Any deviations that exceed 1.5d-13 AU = 1.5 cm are reported.
;
;   The columns are labelled according to the input file, except for
;   the final column, which is the deviation between the input file
;   and the computed value.
;
;
; PARAMETERS: 
;
;   EPHFILE - a scalar string, specifies the name of the ephemeris
;             file, in FITS format.    JPLEPHTEST will look in the directory
;             $ASTRO_DATA for the file if it is not in the current directory.
;
;   TESTFILE - a scalar string, specifies JPL test data set to compare
;              against.   JPLEPHTEST will look in the directory
;             $ASTRO_DATA for the file if it is not in the current directory.
;
;
; EXAMPLE:
;
;   Test JPL DE200 and DE405 ephemerides.  Assumes files are in the
;   current directory.
;
;   JPLEPHTEST, 'JPLEPH.200', 'testpo.200'
;   JPLEPHTEST, 'JPLEPH.405', 'testpo.405'
;     
;
; REFERENCES:
;
;   JPL Export Ephemeris FTP Site
;      ftp://ssd.jpl.nasa.gov/pub/eph/planets/
;      (see test-data/ for test data sets)
;   
;   HORIZONS, JPL Web-based ephermis calculator (Ephemeris DE406)
;      http://ssd.jpl.nasa.gov/horizons.html
;
;
; SEE ALSO
;   JPLEPHREAD, JPLEPHINTERP, JPLEPHTEST
;   
; MODIFICATION HISTORY:
;   Written and Documented, CM, Jun 2001
;   Removed TRANSREAD, improved output, improved docs, CM, 9 Jul 2001
;
;  $Id: jplephtest.pro,v 1.4 2001/07/20 13:29:53 craigm Exp $
;
;-
; Copyright (C) 2001, Craig Markwardt
; This software is provided as is without any warranty whatsoever.
; Permission to use, copy and distribute unmodified copies for
; non-commercial purposes, and to modify and use for personal or
; internal use, is granted.  All other rights are reserved.
;-

pro jplephtest, ephfile, testfile, pause=pause

  if n_params() EQ 0 then begin
      message, 'USAGE: JPLEPHTEST, EPHFILE, TESTFILE', /info
      return
  endif

  testdata = find_with_def( testfile, 'ASTRO_DATA')
  openr, unit, testdata, /get_lun, error=err
  if err NE 0 then begin
      message, 'ERROR: could not open '+testdata
      return
  endif

  ;; Read header of file, up to and including the EOT line
  repeat begin
      line = ''
      readf, unit, line
  endrep until strupcase(strmid(line,0,3)) EQ 'EOT'

  ;; Read at least 20000 lines from file
  data = replicate({denum:0L, caldate: '', jd: 0D, targ: 0L, $
                    cent: 0L, coord: 0L, value: 0D}, 20000)
  on_ioerror, DONE
  readf, unit, data, format='(I5,A10,D0,I0,I0,I0,D0)'
  DONE:
  rc = floor((fstat(unit)).transfer_count/7)
  on_ioerror, NULL
  free_lun, unit

  if rc LT 10 then begin
      message, 'ERROR: could not read input data'
  endif

  ;; Cull the data out of the structure
  data = data[0:rc-1]
  denum = data.denum & caldate = data.caldate & jd = data.jd 
  targ = data.targ & cent = data.cent & coord = data.coord
  value = data.value
  data = 0
      
  bad = cent*0

  ephdata = find_with_def(ephfile, 'ASTRO_DATA')
  jplephread, ephdata, pinfo, pdata, status=st, errmsg=errmsg
  if st EQ 0 then begin
      message, errmsg
  endif
  if denum[0] NE pinfo.denum then begin
      message, 'ERROR: test file and ephemeris are not of same version'
  endif

  wh = where(jd GE pinfo.tstart AND jd LE pinfo.tstop, totct)
  if totct EQ 0 then begin
      message, 'ERROR: test file and ephemeris do not overlap'
  endif

  j = 0L
  for i = 0L, totct-1 do begin

      if coord[wh[i]] GE 4 then vel = 1 else vel = 0
      if targ[wh[i]] GE 14 then vel = 1  ;; Always for nut. & libr.
      jplephinterp, pinfo, pdata, jd[wh[i]], x, y, z, vx, vy, vz, $
        objectname=targ[wh[i]], center=cent[wh[i]], $
        posunits='AU', velunits='AU/DAY', velocity=vel

      case coord[wh[i]] of 
          1: newval = x
          2: newval = y
          3: newval = z
          4: newval = vx
          5: newval = vy
          6: newval = vz
          else: message, 'ERROR: coordinate '+coord[wh[i]]+' does not exist'
      endcase

      ;; Nutations are handled differently than PLEPH
      if targ[wh[i]] EQ 14 AND coord[wh[i]] GT 2 then begin
          if coord[wh[i]] EQ 3 then newval = vx $
          else                      newval = vy
      endif

      del = abs(newval - value[wh[i]])
      if targ[wh[i]] EQ 15 AND coord[wh[i]] EQ 3 then $
        del = del/(0.23d0*(jd[wh[i]]-2451545.d0))
      if del GE 1.5d-13 OR (i MOD 10) EQ 0 then begin
          if del GE 1.5d-13 then begin
              print, '****** WARNING: Large difference ******'
              bad[wh[i]] = 1
          endif
          if j GT 300 then j = 0L
          if j EQ 0 then $
            print, 'REC#', 'Jul. Day', 'Targ', 'Cent', 'Coor', $
            'Value', 'Deviation', format='(A6,A10,3(A5),1(A20),A22)'
          print, i+1, jd[wh[i]], targ[wh[i]], cent[wh[i]], coord[wh[i]], $
            value[wh[i]], del, $
            format='(I6,D10.1,3(I5),1(D20.13),E22.13)'
      endif

      j = j + 1
  endfor

  if keyword_set(pause) AND total(bad) NE 0 then stop
  wh = where(bad, ct)
  print, ''
  print, '***********************************'
  print, ' Time Range (Julian Days): ', minmax(jd)
  print, ' Number of Records: ', totct
  print, ' Erroneous Records: ', ct

end


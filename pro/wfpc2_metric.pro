 pro wfpc2_metric,xin,yin,xout,yout,  chip, Header = h, GLOBAL = global, $
                                  YEAR = year, FILTER = filter, RADec = radec
;+
; NAME:
;   WFPC2_METRIC
; PURPOSE:
;   Compute the distortion in a WFPC2 image and optionally return coordinates
; EPLANATION:
;   Uses the distortion solution of Anderson & King (2003, PASP, 115, 113)
;   Pixel 424, 424 on each chip remains fixed, and other pixel positions are
;   mapped to remove nonlinearities.   If /GLOBAL is set, then all chips are
;   put on the same reference frame where pixel 424, 424 in the WF3 chip 
;   remains fixed. 
; CALLING SEQUENCE:
;      WFPC2_METRIC, xin, yin, xout, yout, [ChipNum, HEADER=, /GLOBAL
;                                           YEAR =, FILTER=
;                            or
;      WFPC2_METRIC, xin, yin, a, d, HEADER=, /RAdec, /GLOBAL ]
; INPUTS:
;     XIN, YIN - X,Y positions (0-799) on a WFPC2 chip in 
;               IDL convention (first pixel is 0,0), scalar or vectors
; OUTPUTS:
;     XOUT, YOUT - X,Y positions in the undistorted frame, same number of 
;                  elements as XIN, YIN
;                          or if /RADEC is set
;     AA, DD  - Right ascension and declination (in degrees) corresponding 
;               to the input coordinates after distortion correction.
; OPTIONAL INPUT:
;     ChipNum - Integer  1, 2, 3, or 4  specifying the WFPC2 chip number
;             1-PC, 2-WF2, 3-WF3, 4-WF4.   If not supplied, then WFPC2_METRIC
;             will try to read the value from the DETECTOR in the FITS header.
; OPTIONAL INPUTS:
;     /GLOBAL - If set, then positions are returned in a master reference 
;              frame with pixel 424,424 of WF3 remaining fixed.   Thus, 
;              information  concerning the  interchip separation and 
;              orientation (with a weak dependence on time and filter) is 
;              incorporated. 
;     Header - FITS header with astrometry for a particular chip.
;             If both /RADec and /Global are set, then the header must be
;             from the WF3 chip. 
;     /RADec - If set, then astrometry information in the FITS header (which
;             must be supplied as a keyword) is used to convert the output
;             to Right Ascension and declination (both in degrees).
;     FILTER - Filter name needed if /GLOBAL is set, must be either 'F300W'
;             'F336W', 'F439W', 'F555W' or 'F814W'; otherwise the plate scale
;             for F555W is assumed.   WFPC2_METRIC will try to read this 
;             value from the FITS header if not supplied as a keyword.
;     YEAR -  Observation year including fraction (e.g. 1998.56) needed if
;             /GLOBAL is set.  WFPC2_METRIC will try to read this value from 
;             the FITS header if not supplied as a keyword.  The time 
;             correction is currently applied through the year 2002; later 
;             dates will use the year 2002 correction.              
; EXAMPLES:
;     (1) Find the undistorted X,Y coordinates of position 682.3,234.2 on chip 1 
;         (the PC chip).
;          IDL> WFPC2_METRIC, 682.3, 234.2, xout, yout, 1 
;             ==> xout = 681.13   yout = 235.05
;
;     (2) Determine the RA and Dec of position 682.3, 234.2 on chip 1 on the 
;         WFPC2 image U2Z30201T
;         IDL> WFPC2_READ, 'u2z30201t.c0h', im,h   ;Get header for chip 1
;         IDL> WFPC2_METRIC, 682.3, 234.2, aa, dd, header= h,/RADec
;         IDL> print, adstring(aa,dd,2)
;         05 20 53.572  -69 35 18.17
;
;         Note that a chip number did not need to be specified since its value
;         is in the FITS header
;
;     (3) As above, but now compute coordinates in the global frame, needed
;         for example, to compute the distance between stars on two different
;         chips. 
;
;        First get headers for chips 1 and 3
;        IDL> WFPC2_READ, 'u2z30201t.c0h', im1,h1, im3,h3,num=[1,3]   
;        IDL> WFPC2_METRIC, 682.3, 234.2, aa, dd, 1, header=h3,/RADec,/GLOBAL
;        IDL> print, adstring(aa,dd,2)
;         05 20 53.513  -69 35 17.98
;
;        Note that with /GLOBAL set, that the header must be for WF3, even
;        though coordinates are being computed for chip 1.   Also note that
;        the time and filter will be read from the FITS header.   Finally,
;        note that the coordinates given in examples (2) and (3) differ
;        slightly, because the chip separations incorporated in the FITS 
;        headers differ slightly from those in the Anderson & King solution.   
; PROCEDURES USED:
;     LINTERP, SXPAR(), XYAD, YMD2DN()
; REVISION HISTORY:
;     Written     W. Landsman         March 2003
;-
 On_error,2
 compile_opt idl2
 if N_params() LT 4 then begin
     print,'Syntax - WFPC2_METRIC, xin, yin, xout, yout, chip, /GLOBAL, '
     print,'                  /RADec, HEADER =, YEAR=, FILTER = '
     return
 endif

 have_header = N_elements(h) GT 0 
 if N_elements(chip) EQ 0 then if have_header then $
               chip = sxpar(h,'DETECTOR')
 if (chip LT 1) or (chip GT 4) then message, $
     'ERROR - Supplied chip number must be between 1 and 4'
 k = chip-1
       pwfpc2gc  =   $                                             ;Order
   [    0.000, 0.418, 0.000, 0.051, 0.000,-0.028, 0.000, 0.070 , $ ;x
        0.000,-0.016, 0.000,-0.015, 0.000,-0.036, 0.000, 0.059  , $ ;y
       -0.525,-0.280,-0.624,-0.038,-0.349,-0.027,-0.489,-0.050 , $ ;xx
       -0.268,-0.292,-0.411,-0.568,-0.353,-0.423,-0.391,-0.485 , $ ;xy
       -0.249,-0.470,-0.092,-0.444, 0.009,-0.373,-0.066,-0.406 , $ ;yy
       -1.902,-0.011,-1.762, 0.003,-1.791, 0.004,-1.821,-0.015 , $ ;xxx
        0.024,-1.907, 0.016,-1.832, 0.006,-1.848, 0.022,-1.890 , $ ;xxy
       -1.890, 0.022,-1.825, 0.011,-1.841, 0.006,-1.875, 0.022 , $ ;xyy
       -0.004,-1.923, 0.010,-1.730, 0.021,-1.788,-0.006,-1.821  ]  ;yyy

;        APC1  BPC1    AWF2   BWF2   AWF3   BWF3   AWF4   BWF4  
      
  
      x = (xin-424.0)/375.0
      y = (yin-424.0)/375.0

      pwfpc2gc  = reform(pwfpc2gc,2,4,9,/over)
      
      t = pwfpc2gc[0,k,*]
      x2 = x^2
      x3 = x^2*x
      y2 = y^2
      y3 = y2*y
      xout = xin + t[0]*x + $
                   t[1]*y + $
                   t[2]*x2 + $ 
                   t[3]*x*y  + $
                   t[4]*y2 + $
                   t[5]*x3 + $
                   t[6]*x2*y + $
                   t[7]*y2*x + $
                   t[8]*y3 
;     for x use the A coeffs...
     t = pwfpc2gc[1,k,*]
     yout = yin +  t[0]*x + $
                   t[1]*y + $
                   t[2]*x2 + $ 
                   t[3]*x*y  + $
                   t[4]*y2 + $
                   t[5]*x3 + $
                   t[6]*x2*y + $
                   t[7]*y2*x + $
                   t[8]*y3
      if keyword_set(GLOBAL) then begin

             radeg = 180.0d/!Dpi
            if not keyword_set(year) then begin
               if have_header then begin
                   mjd = sxpar(h,'EXPSTART')
                   CALDAT, mjd+2400000.5d, Month, Day, Year
                   dy = ymd2dn(year,month,day)
                   year = year + dy/365.25
               endif else year = 1998.0
            endif
            alphak = [0.45729, 1.00020, 1.0000, 1.00048]
            theta = [180.178, 269.682, 0.0   , 90.551 ]
            x0_94 = [-140.2, +430.1, +425.0, -347.2 ]
            x0_98 = [-139.4, +430.0, +425.0, -346.1]
            x0_02 = [-138.8, +430.3, +425.0, -345.5]         
            y0_94 = [-123.3, -328.4, +425.0, +423.9 ]
            y0_98 = [-121.5, -327.9, +425.0, 424.2 ]
            y0_02 = [-120.8, -327.2, +425.0, 424.4 ]
            linterp,[1994.,1998.,2002.],[x0_94[k],x0_98[k],x0_02[k]],year,x0
            linterp,[1994.,1998.,2002.],[y0_94[k],y0_98[k],y0_02[k]],year,y0
            if N_elements(filter) EQ 0 then begin 
                if have_header then filter = strtrim(sxpar(h,'FILTNAM1'),2) $
                               else filter = 'F555W'
            endif
            alphaf = 1.0
            case strupcase(FILTER) of 
            'F300W': alphaf = 0.99953
            'F336W': alphaf = 0.99953
            'F439W': alphaf = 0.99978
            'F555W': alphaf = 1.0
            'F814W': alphaf = 1.00036
            else:  message,/CON, $
                   'WARNING - No scale factor determined for filter '+filter +$
                   ' using F555W factor'
            endcase
            xnew = xout - 425.0
            ynew = yout - 425.0
            ctheta  = cos(theta[k]/radeg)
            stheta = sin(theta[k]/radeg)
            alpha = alphaf*alphak[k]
            xout = alpha*(xnew*ctheta-ynew*stheta) + x0
            yout = alpha*(xnew*stheta + ynew*ctheta) + y0
      endif

      if  keyword_set(RADec) then begin 
                xyad,h,xout,yout,a,d
                xout = a
                yout = d
      endif
      return
      end 

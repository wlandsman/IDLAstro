pro mphase,jd, k
;+
; NAME:
;       MPHASE
; PURPOSE:
;       Return the illuminated fraction of the Moon at given Julian date(s) 
;
; CALLING SEQUENCE:
;       MPHASE, jd, k
; INPUT:
;       JD - Julian date, scalar or vector, double precision recommended
; OUTPUT:
;       k - illuminated fraction of Moon's disk (0.0 < k < 1.0), same number
;           of elements as jd.   k = 0 indicates a new moon, while k = 1 for
;           a full moon.
; EXAMPLE:
;       Plot the illuminated fraction of the moon for every day in July 
;       1996 at 0 TD (~Greenwich noon).
;
;       IDL> jdcnv, 1996, 7, 1, 0, jd         ;Get Julian date of July 1
;       IDL> mphase, jd+dindgen(31), k        ;Moon phase for all 31 days
;       IDL> plot, indgen(31),k               ;Plot phase vs. July day number
;
; METHOD:
;       Algorithm from Chapter 46 of "Astronomical Algorithms" by Jean Meeus
;       (Willmann-Bell, Richmond) 1991.   SUNPOS and MOONPOS are used to get
;       positions of the Sun and the Moon (and the Moon distance).   The
;       selenocentric elongation of the Earth from the Sun (phase angle)
;       is then computed, and used to determine the illuminated fraction.
; PROCEDURES CALLED:
;       MOONPOS, SUNPOS
; REVISION HISTORY:
;       Written W. Landsman     Hughes STX           June 1996
;       Converted to IDL V5.0   W. Landsman   September 1997
;       Use /RADIAN keywords to MOONPOS, SUNPOS internally  W. Landsman Aug 2000
;-
 On_error,2

 if N_params() LT 2 then begin
        print,'Syntax - MPHASE, jd, k'
        return
 endif
 diss = 1.49598e8         ;Earth-Sun distance (1 AU)

 moonpos, jd, ram, decm, dism, /RADIAN
 sunpos, jd, ras, decs, /RADIAN

; phi - geocentric elongation of the Moon from the Sun
; inc - selenocentric (Moon centered) elongation of the Earth from the Sun

 phi = acos( sin(decs)*sin(decm) + cos(decs)*cos(decm)*cos(ras-ram) )
 inc = atan( diss * sin(phi), dism - diss*cos(phi) )
 k = (1 + cos(inc))/2.

 return
 end

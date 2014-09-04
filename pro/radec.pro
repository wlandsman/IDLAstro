pro radec,ra,dec,ihr,imin,xsec,ideg,imn,xsc, hours = hours
;+
; NAME:
;	RADEC
; PURPOSE:
;	To convert RA and Dec  from decimal to sexagesimal units.
; EXPLANATION: 
;	The conversion is to sexagesimal hours for RA,  and sexagesimal 
;	degrees for declination.
;
; CALLING SEQUENCE:
;	radec, ra, dec, ihr, imin, xsec, ideg, imn, xsc, [/HOURS}
;
; INPUTS:
;	ra   - Right ascension, scalar or vector, in DEGREES unless the
;              /HOURS keyword is set
;	dec  - declination in decimal DEGREES, scalar or vector, same number
;		of elements as RA
;
; OUTPUTS:
;	ihr  - right ascension hours   (INTEGER*2)
;	imin - right ascension minutes (INTEGER*2)
;	xsec - right ascension seconds  (REAL*4 or REAL*8)
;	ideg - declination degrees (INTEGER*2)
;	imn  - declination minutes (INTEGER*2)
;	xsc  - declination seconds (REAL*4 or REAL*8)
;
; OPTIONAL KEYWORD INPUT:
;       /HOURS - if set, then the input righ ascension should be specified in
;              hours instead of degrees.
; RESTRICTIONS:
;	RADEC does minimal parameter checking.
;
; REVISON HISTORY:
;	Written by B. Pfarr, STX, 4/24/87
;	Converted to IDL V5.0   W. Landsman   September 1997
;       Added /HOURS keyword W. Landsman  August 2002
;-
  On_error,2

  if (N_params() LT 2 ) then begin
    print,'Syntax - radec, ra, dec, ihr, imin, xsec, ideg, imn, xsc'
    return
  endif
  
;    Compute RA
  if keyword_set(hours) then begin
      ra = ra mod 24.
      ra = ra + 24*(ra lt 0)
      ihr = fix(ra)
      xmin = abs(ra*60. - ihr*60.)
  endif else begin
      ra = ra mod 360.          ;Make sure between 0 and 24 hours
      ra = ra + 360*(ra lt 0)
      ihr = fix(ra/15.)
      xmin =abs(ra*4.0-ihr*60.0)
  endelse 
      imin = fix(xmin)
      xsec = (xmin-imin)*60.0

;    Compute Dec

  ideg = fix(dec)
  xmn = abs(dec-ideg)*60.0
  imn = fix(xmn)
  xsc = (xmn-imn)*60.0

; Now test for the special case of zero degrees

  zero_deg = ( ideg EQ 0 ) and (dec LT 0)
  imn = imn - 2*imn*fix( zero_deg*(imn NE 0) )
  xsc = xsc - 2*xsc*zero_deg*(imn EQ 0)

  return
  end

pro xy2ad, x, y, astr, a, d
;+
; NAME:
;     XY2AD
;
; PURPOSE:
;     Compute R.A. and Dec from X and Y and a FITS astrometry structure
; EXPLANATION:
;     The astrometry structure must first be extracted by EXTAST from a FITS
;     header.   The offset from the reference pixel is computed and the CD 
;     matrix is applied.     If distortion is present then this is corrected.
;     If a WCS projection (Calabretta & Greisen 2002, A&A, 395, 1077) is 
;     present, then the procedure WCSXY2SPH is used to compute astronomical
;     coordinates.    Angles are returned in  degrees.
;   
;     XY2AD is meant to be used internal to other procedures.  
;     For interactive purposes use XYAD.
;
; CALLING SEQUENCE:
;     XY2AD, x, y, astr, a, d   
;
; INPUTS:
;     X     - row position in pixels, scalar or vector
;     Y     - column position in pixels, scalar or vector
;           X and Y should be in the standard IDL convention (first pixel is
;           0), and not the FITS convention (first pixel is 1). 
;     ASTR - astrometry structure, output from EXTAST procedure containing:
;        .CD   -  2 x 2 array containing the astrometry parameters CD1_1 CD1_2
;               in DEGREES/PIXEL                                   CD2_1 CD2_2
;        .CDELT - 2 element vector giving physical increment at reference pixel
;        .CRPIX - 2 element vector giving X and Y coordinates of reference pixel
;               (def = NAXIS/2)
;        .CRVAL - 2 element vector giving R.A. and DEC of reference pixel 
;               in DEGREES
;        .CTYPE - 2 element vector giving projection types 
;        .LONGPOLE - scalar longitude of north pole
;        .LATPOLE - scalar giving native latitude of the celestial pole
;        .PV2 - Vector of projection parameter associated with latitude axis
;             PV2 will have up to 21 elements for the ZPN projection, up to 3
;             for the SIN projection and no more than 2 for any other
;             projection
;
;     Fields added for version 2:
;      .PV1 - Vector of projection parameters associated with longitude axis
;      .AXES  - 2 element integer vector giving the FITS-convention axis 
;               numbers associated with astrometry, in ascending order. 
;               Default [1,2].
;      .REVERSE - byte, true if first astrometry axis is Dec/latitude
;      .COORDSYS - 1 or 2 character code giving coordinate system, including
;                 'C' = RA/Dec, 'G' = Galactic, 'E' = Ecliptic, 'X' = unknown.
;      .RADECSYS - String giving RA/Dec system e.g. 'FK4', 'ICRS' etc.
;      .EQUINOX  - Double giving the epoch of the mean equator and equinox
;      .DATEOBS  - Text string giving (start) date/time of observations
;      .MJDOBS   - Modified julian date of start of observations.
;      .X0Y0     - Implied offset in intermediate world coordinates if user has
;                  specified a non-standard fiducial point via PV1 and also
;                  has set PV1_0a =/ 0 to indicate that the offset should be
;                  applied in order to place CRVAL at the IWC origin.
;                  Should be *added* to the IWC derived from application of
;                  CRPIX, CDELT, CD to the pixel coordinates.
;
;      .DISTORT - Optional substructure specifying distortion parameters
;                  
;
; OUTPUT:
;     A - R.A. in DEGREES, same number of elements as X and Y
;     D - Dec. in DEGREES, same number of elements as X and Y
;
; RESTRICTIONS:
;       Note that all angles are in degrees, including CD and CRVAL
;       Also note that the CRPIX keyword assumes an FORTRAN type
;       array beginning at (1,1), while X and Y give the IDL position
;       beginning at (0,0).   No parameter checking is performed.
;
; NOTES:
;      XY2AD tests for presence of WCS coordinates by the presence of a dash 
;      in the 5th character position in the value of CTYPE (e.g 'DEC--SIN').       
; PROCEDURES USED:
;       TAG_EXIST(), WCSXY2SPH, SIP_EVAL(), TPV_EVAL()
; REVISION HISTORY:
;       Written by R. Cornett, SASC Tech., 4/7/86
;       Converted to IDL by B. Boothman, SASC Tech., 4/21/86
;       Perform CD  multiplication in degrees  W. Landsman   Dec 1994
;       Understand reversed X,Y (X-Dec, Y-RA) axes,   W. Landsman  October 1998
;       Consistent conversion between CROTA and CD matrix W. Landsman Oct. 2000
;       No special case for tangent projection W. Landsman June 2003
;       Work for non-WCS coordinate transformations W. Landsman Oct 2004
;       Use CRVAL reference point for non-WCS transformation  W.L. March 2007
;       Use post V6.0 notation   W.L. July 2009
;       Some optimisation for large input arrays & use of version 2 astr
;       structure, J. P. Leahy July 2013
;       Evalue TPV distortion (SCAMP) if present W. Landsman   Jan 2014
;       Support IRAF TNX porjection  M. Sullivan U. of Southamptom  Mar 2014
;       No longer check that CDELT[0] NE 1  W. Landsman Apr 2015
;- 
 common Broyden_coeff, pv1, pv2       ;Needed for TPV transformation
 compile_opt idl2

 if N_params() LT 4 then begin
        print,'Syntax -- XY2AD, x, y, astr, a, d'
        return
 endif
 
 Catch, theError
 IF theError NE 0 then begin
     Catch,/Cancel
     void = cgErrorMsg(/quiet)
     RETURN
     ENDIF
 
 cd = astr.cd
 crpix = astr.crpix
 cdelt = astr.cdelt 
         
  cd[0,0] *= cdelt[0] & cd[0,1] *= cdelt[0]
  cd[1,1] *= cdelt[1] & cd[1,0] *= cdelt[1]

 xdif = x - (crpix[0]-1)            
 ydif = y - (crpix[1]-1)
 no_PV1 =0     ;Set if PV1 used by TGV distortion
 
 if tag_exist(astr,'DISTORT') && astr.distort.name EQ 'SIP' then begin
           distort  = astr.distort
           a = distort.a
           b = distort.b
           na = ((size(a,/dimen))[0])
           xdif1 = xdif
           ydif1 = ydif
           
           for i=0,na-1 do begin
               for j=0,na-1 do begin
                  if a[i,j] NE 0.0 then xdif1 +=  xdif^i*ydif^j*a[i,j]            
                  if b[i,j] NE 0.0 then ydif1 +=  xdif^i*ydif^j*b[i,j]
           endfor
           endfor

           xdif = TEMPORARY(xdif1)
           ydif = TEMPORARY(ydif1)
           
  ENDIF 

 astr2 = TAG_EXIST(astr,'AXES') ; version 2 astrometry structure
 
 xsi = cd[0,0]*xdif + cd[0,1]*ydif   ;Can't use matrix notation, in
                                     ;case X and Y are vectors
 eta = cd[1,0]*TEMPORARY(xdif) + cd[1,1]*TEMPORARY(ydif)   

 if tag_exist(astr,'DISTORT') && astr.distort.name EQ 'TPV' then begin
           pv1 = astr.pv1
	   pv2 = astr.pv2
	   result = tpv_eval( [[xsi],[eta]])
	   xsi = reform( result[*,0] )
           eta = reform( result[*,1] )
           no_PV1 = 1
           ctype = strmid(astr.ctype,0,4) + '-TAN'
        endif	   

        if tag_exist(astr,'DISTORT') && astr.distort.name EQ 'TNX' then begin
        pv1=astr.distort.lngcor
        pv2=astr.distort.latcor
        result = tnx_eval( [[xsi],[eta]])
        xsi = reform( result[*,0] )
        eta = reform( result[*,1] )
        no_PV1 = 1
        ctype = strmid(astr.ctype,0,4) + '-TAN'
     endif

 if N_elements(ctype) Eq 0 then ctype = astr.ctype
 crval = astr.crval
 IF astr2 THEN reverse = astr.reverse ELSE BEGIN
     coord = strmid(ctype,0,4)
     reverse = ((coord[0] EQ 'DEC-') && (coord[1] EQ 'RA--')) || $
               ((coord[0] EQ 'GLAT') && (coord[1] EQ 'GLON')) || $
               ((coord[0] EQ 'ELAT') && (coord[1] EQ 'ELON'))
 ENDELSE
 if reverse then begin
     crval = rotate(crval,2)
     temp = TEMPORARY(xsi) & xsi = TEMPORARY(eta) & eta = TEMPORARY(temp)
 endif

 if strmid(ctype[0],4,1) EQ '-' then begin
     if no_PV1 then begin       ;Set default values for tangent projection
           pv1 = [0.0d,0,90.0d,180.0d,90.0d]  & pv2 = [0.0d,0.0d]	   
     endif else begin 
            pv1 = astr.pv1
	    pv2 = astr.pv2
     endelse 	    
     if astr2 THEN $
         WCSXY2SPH, xsi, eta, a, d, CTYPE = ctype[0:1], PV1 = pv1, $
              PV2 = astr.PV2, CRVAL = crval, CRXY = astr.x0y0 $
     ELSE $ 
         WCSXY2SPH, xsi, eta, a, d, CTYPE = ctype[0:1], PV2 = pv2, $
              LONGPOLE = astr.longpole, CRVAL = crval, LATPOLE = astr.latpole
 endif else begin
         a = crval[0] + TEMPORARY(xsi) & d = crval[1] + TEMPORARY(eta)	
 endelse

 return
 end

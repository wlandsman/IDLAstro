pro starast,ra,dec,x,y,cd, righthanded=right,hdr=hdr, projection=projection    
;+
; NAME:
;       STARAST 
; PURPOSE:
;       Compute astrometric solution using positions of 2 or 3 reference stars
; EXPLANATION:
;       Computes an exact astrometric solution using the positions and 
;       coordinates from 2 or 3 reference stars and assuming a tangent 
;       (gnomonic) projection.   If 2 stars are used, then
;       the X and Y plate scales are assumed to be identical, and the
;       axis are assumed to be orthogonal.   Use of three stars will
;       allow a unique determination of each element of the CD matrix.
;
; CALLING SEQUENCE:
;       starast, ra, dec, x, y, cd, [/Righthanded, HDR = h, PROJECTION=]
;
; INPUTS:
;       RA - 2 or 3 element vector containing the Right Ascension in DEGREES
;       DEC- 2 or 3 element vector containing the Declination in DEGREES
;       X -  2 or 3 element vector giving the X position of reference stars
;       Y -  2 or 3 element vector giving the Y position of reference stars
; OUTPUTS:
;       CD - CD (Coordinate Description) matrix (DEGREES/PIXEL) determined 
;               from stellar positions and coordinates.
; OPTIONAL INPUT KEYWORD:
;       /RightHanded - If only 2 stars are supplied, then there is an ambiguity
;               in the orientation of the coordinate system.   By default,
;               STARAST assumes the astronomical standard left-handed system
;               (R.A. increase to the left).   If /Right is set then a 
;               righthanded coordinate is assumed.  This keyword has no effect
;               if 3 star positions are supplied.
;        PROJECTION - Either a 3 letter scalar string giving the projection
;               type (e.g. 'TAN' or 'SIN') or an integer 1 - 25 specifying the
;               projection as given in the WCSSPH2XY procedure.   If not 
;               specified then a tangent projection is computed.
; OPTIONAL INPUT-OUTPUT KEYWORD:
;        HDR - If a FITS header string array is supplied, then an astrometry 
;              solution is added to the header using the CD matrix and star 0
;              as the reference pixel (see example).   Equinox 2000 is assumed.
; EXAMPLE:
;        To use STARAST to add astrometry to a FITS header H;
;
;        IDL> starast,ra,dec,x,y,cd       ;Determine CD matrix
;        IDL> crval = [ra[0],dec[0]]      ;Use Star 0 as reference star
;        IDL> crpix = [x[0],y[0]] +1      ;FITS is offset 1 pixel from IDL
;        IDL> putast,H,cd,crpix,crval     ;Add parameters to header
;
;        This is equivalent to the following command:
;        IDL> STARAST,ra,dec,x,y,hdr=h      
;  
; METHOD:
;       The CD parameters are determined by solving the linear set of equations
;       relating position to local coordinates (l,m)
;
;       For highest accuracy the first star position should be the one closest
;       to the reference pixel.
; REVISION HISTORY:
;       Written, W. Landsman             January 1988
;       Converted to IDL V5.0   W. Landsman   September 1997
;       Added /RightHanded and HDR keywords   W. Landsman   September 2000
;       Write CTYPE values into header   W. Landsman/A. Surkov  December 2002
;       CD matrix was mistakenly transpose in 3 star solution
;       Added projection keyword    W. Landsman   September 2003
;       Test for singular matrix W. Landsman  August 2011 
;-
 On_ERROR,2
 compile_opt idl2

 if N_params() LT 4 then begin
        print,'Syntax - STARAST, ra, dec, x, y, cd, [/Right, HDR =h,Projection=]'
        return                         
 endif

 cdr = !DPI/180.0D 
 map_types=['DEF','AZP','TAN','SIN','STG','ARC','ZPN','ZEA','AIR','CYP',$
            'CAR','MER','CEA','COP','COD','COE','COO','BON','PCO','SFL',$
            'PAR','AIT','MOL','CSC','QSC','TSC']

 iterate = (N_elements(crpix) EQ 2) && (N_elements(crval) EQ 0)
 if N_elements(projection) EQ 0 then projection = 2    ;Default is tangent proj.
 if size(projection,/TNAME) EQ 'STRING' then begin
      map_type  =where(map_types EQ strupcase(strtrim(projection,2)), Ng)
      if Ng EQ 0 then message, $
         'ERROR - supplied projection of ' + projection[0] + ' not recognized'
      map_type = map_type[0]
 endif else map_type = projection

 nstar = min( [N_elements(ra), N_elements(dec), N_elements(x), N_elements(y)])
 if (nstar NE 2) && (nstar NE 3) then $
        message,'ERROR -  Either 2 or 3 star positions required'
 crval1  = [ ra[0], dec[0] ]
 crpix1  = [ x[0], y[0] ]

; Convert RA, Dec to Eta, Xi

 wcssph2xy, crval = crval1, ra[1:*], dec[1:*], eta, xi, map_type, $
         latpole = 0.0
 delx1 = x[1] - crpix1[0] 
 dely1 = y[1] - crpix1[1]     

if nstar EQ 3 then begin

        delx2 = x[2] - crpix1[0] & dely2 = y[2] - crpix1[1]
        b = double([eta[0],xi[0],eta[1],xi[1]])
        a = double( [ [delx1, 0, delx2,    0    ], $
                      [dely1, 0,  dely2,    0  ], $
                      [0. , delx1, 0,    delx2    ], $
                      [0    , dely1   , 0. ,dely2] ] )
endif else begin

        b = double( [eta[0],xi[0]] )
        if keyword_set(right) then  $
              a = double( [ [delx1,dely1], [-dely1,delx1] ] ) else $
              a = double( [ [delx1,-dely1], [dely1,delx1] ] )

endelse

 cd = invert(a,status)#b        ;Solve linear equations
 if status EQ 1 then $
    message,'ERROR - Singular matrix (collinear points)' 
 if nstar EQ 2 then begin
           if keyword_set(right) then $ 
               cd = [ [cd[0],cd[1]],[-cd[1],cd[0]] ] else $
               cd = [ [cd[0],cd[1]],[cd[1],-cd[0]] ] 
 endif else $ 
       cd = transpose(reform(cd,2,2))


;Add parameters to header
 if N_elements(hdr) GT 0 then begin
        proj = map_types[map_type]
        make_astr, astr,CD = cd, crval = crval1, crpix = crpix1+1, $
                   ctype = ['RA---','DEC--'] + proj
        putast, hdr, astr, equi=2000.0,cd_type=2
       
 endif
     
 return
 end

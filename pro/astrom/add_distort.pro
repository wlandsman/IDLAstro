 pro add_distort, hdr, astr
; NAME:
;    ADD_DISTORT
; PURPOSE:
;    Add the distortion parameters in an astrometry structure to a FITS header.
; EXPLANATION:
;    Called by PUTAST to add SIP (http://fits.gsfc.nasa.gov/registry/sip.html )
;    or TNX ( http://fits.gsfc.nasa.gov/registry/tnx.html ) distortion 
;    parameters in an astrometry structure to a FITS header
;     
;    Prior to April 2012, PUTAST did not add distortion parameters so one
;    had to call ADD_DISTORT after PUTAST. 
;
;    IDL> putast,h ,astr0
;    IDL> add_distort,h,astr0
;
; CALLING SEQUENCE:
;     add_distort, hdr, astr    
;
; INPUTS:
;     HDR -  FITS header, string array.   HDR will be updated to contain
;             the supplied astrometry.
;     ASTR - IDL structure containing values of the astrometry parameters
;            CDELT, CRPIX, CRVAL, CTYPE, LONGPOLE, PV2, and DISTORT
;            See EXTAST.PRO for more info about the structure definition
;
; PROCEDURES USED:
;       SXADDPAR, TAG_EXIST()
; REVISION HISTORY:
;       Written by W. Landsman  May 2005
;       Enforce i+j = n for ij coefficients of order n  W. Landsman April 2012
;       Support IRAF TNX distortion M. Sullivan  March 2014
;;-
 npar = N_params()

 if ( npar LT 2 ) then begin    ;Was header supplied?
        print,'Syntax: ADD_DISTORT, Hdr, astr'
        return
 endif

 add_distort = tag_exist(astr,'distort')
 IF(~ add_distort)THEN RETURN
 
 IF(astr.distort.name EQ 'SIP') then begin 
    
    sxaddpar,hdr,'CTYPE1','RA---TAN-SIP' 
    sxaddpar,hdr,'CTYPE2','DEC--TAN-SIP' 
    distort = astr.distort
    a_dimen = size(distort.a,/dimen) 
    b_dimen = size(distort.b,/dimen)
    ap_dimen = size(distort.ap,/dimen) 
    bp_dimen = size(distort.bp,/dimen)
    
    if a_dimen[0] GT 0 then begin
       a_order = a_dimen[0]-1 
       sxaddpar, hdr, 'A_ORDER', a_order, /savec, $
                 'polynomial order, axis 1, detector to sky '
       for i=0, a_order do begin
          for j = 0, a_order-i do begin
             aij = distort.a[i,j]
             if aij NE 0.0 then $
               sxaddpar, hdr, 'A_' + strtrim(i,2)+ '_' + strtrim(j,2), aij, $
                         ' distortion coefficient', /savec
          endfor
       endfor
    endif
    
    if b_dimen[0] GT 0 then begin
       b_order = b_dimen[0]-1 
       sxaddpar, hdr, 'B_ORDER', a_order, /savec , $
                 'polynomial order, axis 2, detector to sky'
       for i=0, b_order do begin
          for j = 0, b_order-i do begin
             bij = distort.b[i,j]
             if bij NE 0.0 then $
               sxaddpar, hdr, 'B_' + strtrim(i,2)+ '_' + strtrim(j,2), bij, $
                         ' distortion coefficient', /savec
          endfor
       endfor
    endif
    
    if ap_dimen[0] GT 0 then begin
       ap_order = ap_dimen[0]-1 
       sxaddpar, hdr, 'AP_ORDER', a_order, /savec, $
                 ' polynomial order, axis 1, sky to detector '
       for i=0, ap_order do begin
          for j = 0, ap_order-i do begin
             apij = distort.ap[i,j]
             if apij NE 0.0 then $
               sxaddpar, hdr, 'AP_' + strtrim(i,2)+ '_' + strtrim(j,2), apij, $
                         ' distortion coefficient', /savec
          endfor
       endfor
    endif
    
    
    if bp_dimen[0] GT 0 then begin
       bp_order = bp_dimen[0]-1 
       sxaddpar, hdr, 'BP_ORDER', a_order, /savec, $
                 ' polynomial order, axis 2, sky to detector '
       for i=0, bp_order do begin
          for j = 0, bp_order-i do begin
             bpij = distort.bp[i,j]
             if bpij NE 0.0 then $
               sxaddpar, hdr, 'BP_' + strtrim(i,2)+ '_' + strtrim(j,2), bpij, $
                         ' distortion coefficient', /savec
          endfor
       endfor
    endif
    
 ENDIF ELSE IF(astr.distort.name EQ 'TNX')THEN BEGIN

    sxaddpar, hdr,'WAT0_001','system=image'

    string1='wtype=tnx axtype=ra lngcor = "3.'
    string1+= ' '+STRN(astr.distort.lngcor.xiorder,FORMAT='(F2.0)')
    string1+= ' '+STRN(astr.distort.lngcor.etaorder,FORMAT='(F2.0)')
    string1+= ' '+STRN(astr.distort.lngcor.xterms,FORMAT='(F2.0)')
    string1+= ' '+STRN(astr.distort.lngcor.ximin,FORMAT='(F19.16)')
    string1+= ' '+STRN(astr.distort.lngcor.ximax,FORMAT='(F19.16)')
    string1+= ' '+STRN(astr.distort.lngcor.etamin,FORMAT='(F19.16)')
    string1+= ' '+STRN(astr.distort.lngcor.etamax,FORMAT='(F19.16)')
    FOR i=0,N_ELEMENTS(astr.distort.lngcor.coeff)-1 DO BEGIN
       string1+=' '+STRN(astr.distort.lngcor.coeff[i],FORMAT='(F19.16)')
    ENDFOR
    string1+= '"'

    string2='wtype=tnx axtype=dec latcor = "3. '
    string2+= ' '+STRN(astr.distort.latcor.xiorder,FORMAT='(F2.0)')
    string2+= ' '+STRN(astr.distort.latcor.etaorder,FORMAT='(F2.0)')
    string2+= ' '+STRN(astr.distort.latcor.xterms,FORMAT='(F2.0)')
    string2+= ' '+STRN(astr.distort.latcor.ximin,FORMAT='(F19.16)')
    string2+= ' '+STRN(astr.distort.latcor.ximax,FORMAT='(F19.16)')
    string2+= ' '+STRN(astr.distort.latcor.etamin,FORMAT='(F19.16)')
    string2+= ' '+STRN(astr.distort.latcor.etamax,FORMAT='(F19.16)')
    FOR i=0,N_ELEMENTS(astr.distort.latcor.coeff)-1 DO BEGIN
       string2+= ' '+STRN(astr.distort.latcor.coeff[i],FORMAT='(F19.16)')
    ENDFOR
    string2+= '"'

    len1=STRLEN(string1)
    n1=len1/70
    IF(len1 MOD 68 GT 0)THEN n1++
    FOR i=0,n1-1 DO BEGIN
       s=STRMID(string1,i*68,68)
;       PRINT,'WAT1_'+STRN(i+1,FORMAT='(I3.3)'),' ',s
       sxaddpar, hdr,'WAT1_'+STRN(i+1,FORMAT='(I3.3)'),s
    ENDFOR
    len2=STRLEN(string2)
    n2=len2/70
    IF(len2 MOD 68 GT 0)THEN n2++
    FOR i=0,n2-1 DO BEGIN
       s=STRMID(string2,i*68,68)
;       PRINT,'WAT1_'+STRN(i+1,FORMAT='(I3.3)'),' ',s
       sxaddpar, hdr,'WAT2_'+STRN(i+1,FORMAT='(I3.3)'),s
    ENDFOR

 ENDIF

 return
 end

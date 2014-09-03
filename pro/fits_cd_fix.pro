pro fits_cd_fix,hdr, REVERSE = reverse
;+
; NAME:
;    FITS_CD_FIX
;
; PURPOSE:
;    Update obsolete representations of the CD matrix in a FITS header   
;
; EXPLANATION:
;    According the paper, "Representations of Celestial Coordinates in FITS"
;    by Calabretta & Greisen (2002, A&A, 395, 1077, available at 
;    http://fits.gsfc.nasa.gov/fits_wcs.html) the rotation of an image from 
;    standard coordinates is represented by a coordinate description (CD) 
;    matrix.    The standard representation of the CD matrix are PCn_m 
;    keywords, but CDn_m keywords (which include the scale factors) are
;    also allowed.    However, earliers drafts of the standard allowed the
;    keywords forms CD00n00m and PC00n00m.      This procedure will convert
;    FITS CD matrix keywords containing zeros into the standard forms 
;    CDn_m and PCn_m containing only underscores.
;
; CALLING SEQUENCE:
;    FITS_CD_FIX, Hdr
;
; INPUT-OUTPUT: 
;       HDR - FITS header, 80 x N string array.   If the header does not
;           contain 'CD00n00m' or 'PC00n00m' keywords then it is left 
;           unmodified.  Otherwise, the keywords containing integers are
;           replaced with those containing underscores.
;   
; OPTIONAL KEYWORD INPUT
;      /REVERSE - this keyword does nothing, but is kept for compatibility with
;            earlier versions.
; PROCEDURES USED:
;    SXADDPAR, SXDELPAR, SXPAR()
; REVISION HISTORY:
;    Written   W. Landsman             Feb 1990
;    Major rewrite                     Feb 1994
;    Converted to IDL V5.0   W. Landsman   September 1997
;    Use double precision formatting of CD matrix   W. Landsman  April 2000
;    Major rewrite to convert only to forms recognized by the Greisen
;       & Calabretta standard   W. Landsman   July 2003
;-
 On_error,2
 compile_opt idl2

 if N_params() LT 1 then begin
        print,'Syntax - FITS_CD_FIX, hdr'
        return
 endif

 cd00 = ['CD001001','CD001002','CD002001','CD002002']
 pc00 = ['PC001001','PC001002','PC002001','PC002002']

  cd_ = ['CD1_1','CD1_2','CD2_1','CD2_2']
  pc_ = ['PC1_1','PC1_2','PC2_1','PC2_2']
 

 for i= 0 ,3 do begin
   pc = sxpar(hdr,pc00[i], COUNT = N)
   if N GE 1 then begin
        sxaddpar,hdr,pc_[i],pc,'',pc00[i]
        sxdelpar,hdr,pc00[i]
        if i EQ 0 then sxaddhist,'FITS_CD_FIX:' + strmid(systime(),4,20) + $
                  ' PC00n00m keywords changed to PCn_m',hdr
 endif  else begin
      
    cd = sxpar(hdr,cd00[i], COUNT = N )
    if N GE 1 then begin
        sxaddpar,hdr,cd_[i],cd,'',cd00[i]
        sxdelpar,hdr,cd00[i]
        if i EQ 0 then sxaddhist,'FITS_CD_FIX:' + strmid(systime(),4,20) + $
                  ' CD00n00m keywords changed to CDn_m',hdr
 endif
 endelse 
 endfor

 
 return
 end
                                

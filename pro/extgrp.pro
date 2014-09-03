pro extgrp,hdr,par
;+
; NAME:
;	EXTGRP
; PURPOSE:
;	Extract the group parameter information out of SXREAD output
; EXPLANATION:
;	This procedure extracts the group parameter information out of a 
;	header and parameter variable obtained from SXREAD.  This allows 
;	astrometry, photometry and other parameters to be easily SXPARed by 
;	conventional methods and allows the image and header to be saved in 
;	a SIMPLE format.
;
; CALLING SEQUENCE:
;	ExtGrp, hdr, par
;
; INPUT:
;	HDR - The header which is to be converted (input and output)
;	PAR - The Parameter string returned from a call to SXREAD
;
; OUTPUT:
;	HDR -  The converted header, string array
;
; OTHER PROCEDURES CALLED:
;	SXPAR(), SXADDPAR, SXGPAR(), STRN()
;
; HISTORY:
;	25-JUN-90 Version 1 written
;	13-JUL-92 Header finally added to this ancient procedure, code spiffed up
;	a bit.  Now 3 times faster.  Added PTYPE comment inclusion.  E. Deutsch
;	Converted to IDL V5.0   W. Landsman   September 1997
;-

  arg=n_params(0)
  if (arg lt 2) then begin
    print,'Call: IDL> EXTGRP,header,params_string'
    print,"e.g.: IDL> EXTGRP,h,par"
    return
    endif

  h=hdr
  pcount=sxpar(h,'PCOUNT')
  if (pcount le 0) then begin
    print,'[EXTGRP] Error: PCOUNT not >0 in header'
    return
    endif

  htmp=h & ih=0
  while (strmid(h[ih],0,4) ne 'PTYP') do ih=ih+1
  itmp=ih & stbyt=0
  hquick=strarr(4) & hquick[3]='END        '	; tiny temp. header for speed

  for t2=0,pcount-1 do begin
    hquick=h[ih+3*t2:ih+3*t2+2]

    pty=sxpar(hquick,'PTYPE'+strn(t2+1))
    comment=strmid(hquick[0],30,50)
    pdty=sxpar(hquick,'PDTYPE'+strn(t2+1))
    psz=sxpar(hquick,'PSIZE'+strn(t2+1))/8
    pvl=sxgpar(h,par,pty,pdty,stbyt,psz)

    sz=size(pvl) & stbyt=stbyt+psz
    if (sz[1] eq 7) then pvl="'"+strn(pvl,length=18)+"'"
    tmp=pty+'='+strn(pvl,length=21)+comment

    htmp[itmp]=tmp
    itmp=itmp+1
    endfor

  while (strmid(h[ih],0,1) eq 'P') do ih=ih+1

  while (strmid(h[ih],0,3) ne 'END') do begin
    htmp[itmp]=h[ih]
    itmp=itmp+1
    ih=ih+1
    endwhile		

  htmp[itmp]=h[ih]
  hdr=htmp[0:itmp]

  sxaddpar,hdr,'SIMPLE','T',' Group Parameters extracted'
  sxaddpar,hdr,'PCOUNT',0,' All group parameters extracted'
  sxaddpar,hdr,'PSIZE',0,' All group parameters extracted'
  sxaddpar,hdr,'GROUPS','T'
  sxaddpar,hdr,'GCOUNT',1,' Number of groups'

  return
end

pro GSSSExtAst, h, astr, noparams
;+
; NAME: 
;      GSSSEXTAST
;
; PURPOSE:
;      Extract IDL astrometry structure from a ST Guide Star Survey FITS header
;
; EXPLANATION:
;      This procedure extracts the astrometry information from a ST Guide
;      Star Survey FITS header and places it in an IDL structure for 
;      subsequent use with GSSSxyad and GSSSadxy.
;
; CALLING SEQUENCE:
;      GSSSExtast, hdr, astr, noparams
; INPUT:
;      h - the GSSS FITS header
; OUTPUT:
;      astr  - Structure containing the GSSS Astrometry information
;               .CTYPE  =  ['RA---GSS','DEC--GSS'] 
;               .CRVAL = plate center Ra, Dec (from PLTRAH, PLTRAM etc.)
;               .XLL,.YLL = offsets lower lefthand corner
;               .AMDX, .AMDY = 12 transformation coefficients
;               .XSZ,.YSZ = X and Y pixel size in microns
;               .PLTSCL = plate scale in arc sec/mm
;               .PPO3, .PPO6 - orientation coefficients
; NOTES:
;      Most users should use EXTAST rather than this procedure.   EXTAST will
;      call GSSSEXTAST if supplied with GSSS FITS header.
;
; PROCEDURES CALLED:
;      SXPAR() - Extract parameter values from a FITS header
; HISTORY:
;       01-JUL-90 Version 1 written by Eric W. Deutsch
;       Code derived from Software by Brian McLean
;       20-AUG-91 Modified to Double Precision Variables.  E. Deutsch
;       June 94 Change astrometry tags to better agree with EXTAST  W. Landsman
;       Converted to IDL V5.0   W. Landsman   September 1997
;       29-JUN-99 Added support for AMD[X,Y]1[2-3] for DSS images by E. Deutsch
;       Eliminate use of obsolete !ERR  W. Landsman    February 2000
;-
 
 On_error,2

 if N_params() lt 2 then begin
    print,'Syntax - GSSSExtAst, header, GSSS_astrometry_structure, noparams'
    return
 endif

 noparams = -1

  astr = {gsss_astrometry, CTYPE: strarr(2), XLL:0, YLL:0, XSZ:0.0D, YSZ:0.0D, $
                 PPO3:0.0D, PPO6:0.0D, CRVAL: dblarr(2), PLTSCL:0.0D, $
                 AMDX:dblarr(13), AMDY:dblarr(13) }

;Older GSSS headers used CRPIX1 instead of CRPIXN

  astr.xll = sxpar(h,'CNPIX1', Count = N)
        if N EQ 0 then begin            
                astr.xll = sxpar(h, 'CRPIX1')
                astr.yll = sxpar(h, 'CRPIX2')
        endif else astr.yll = sxpar(h,'CNPIX2')

  astr.xsz = sxpar(h,'XPIXELSZ')
  astr.ysz = sxpar(h,'YPIXELSZ')
  astr.ppo3 = sxpar(h,'PPO3')
  astr.ppo6 = sxpar(h,'PPO6', Count = N)

  if (N Eq 0) then message,'Header does not contain GSSS astrometry'

  astr.pltscl = sxpar(h,'PLTSCALE')

  pltrah = sxpar( h, 'PLTRAH' )
  pltram = sxpar( h, 'PLTRAM' )
  pltras = sxpar( h, 'PLTRAS' )
  pltdecsn = sxpar( h, 'PLTDECSN' )
  pltdecd = sxpar( h, 'PLTDECD' )
  pltdecm = sxpar( h, 'PLTDECM' )
  pltdecs = sxpar( h, 'PLTDECS' )

  astr.crval[0] = (pltrah + pltram/60.0d + pltras/3600.0D)*15
  astr.crval[1] = pltdecd + pltdecm/60.0d + pltdecs/3600.0d

  if (strtrim(PLTDECSN,2) EQ '-') then astr.crval[1] = -astr.crval[1]

  ii = strtrim(indgen(13)+1,2)
  for i = 0,12 do begin

    astr.amdx[i] = sxpar(h, 'AMDX' + ii[i] )
    astr.amdy[i] = sxpar(h, 'AMDY' + ii[i] )
 
 endfor

  astr.ctype = ['RA---GSS','DEC--GSS'] 

  noparams = 0             ;Successful Extraction of GSSS astrometry params

  return
 end

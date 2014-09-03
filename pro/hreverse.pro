pro hreverse, oldim, oldhd, newim, newhd, subs, SILENT = silent, ERRMSG= errmsg
;+
; NAME:
;       HREVERSE
; PURPOSE:
;       Reverse an image about either dimension and update FITS astrometry
; EXPLANATION:
;       Reverse an image about either the X or Y axis, and create a new 
;       header with updated astrometry for the reversed image.
;
; CALLING SEQUENCE:
;       HREVERSE,oldim,oldhd, [ subs, /SILENT ]   ;Update input image and header
;               or
;       HREVERSE, oldim, oldhd, newim, newhd, [ subs, /SILENT ]   
;
; INPUTS:
;       OLDIM - the original image array
;       OLDHD - the original image header
;
; OPTIONAL INPUTS:
;       SUBS - Subs equals 1 to reverse the order of the X dimension,
;               2 to reverse Y order.  If omitted, then HREVERSE will
;               prompt for this scalar parameter.
;
; OPTIONAL OUTPUTS:
;       NEWIM - the rotated image, with the same dimensions as Oldim 
;       NEWHD - header for newim containing updated astrometry info
;               If output parameters are not supplied, the program
;               will modify the input parameters OLDIM and OLDHD
;               to contain the rotated image and updated header.
;
; OPTIONAL KEYWORD INPUT:
;       SILENT - if set and non-zero, then informative messages are suppressed.
;
; OPTIONAL KEYWORD OUTPUT:
;       ERRMSG - If this keyword is supplied, then any error mesasges will be
;               returned to the user in this parameter rather than depending on
;               on the MESSAGE routine in IDL.   If no errors are encountered
;               then a null string is returned.               
;
; SIDE EFFECTS:
;       A right-handed coordinate system is converted into a left-
;       handed one, and vice-versa.
;
; PROCEDURE:
;       The User's Library procedure REVERSE is used to reverse the image.
;       The CD and CRPIX header parameters are updated for the new header.
;       For AIPS type astrometry, the CDELT parameters are also updated.
;       A history record is also added to the header
;
; PROCEDURES USED:
;       CHECK_FITS, EXTAST, REVERSE(), STRN(), SXADDPAR 
; MODIFICATION HISTORY:
;       Written, Aug. 1986 W. Landsman, STI Corp.
;       Error modifying CROTA angles corrected     9-23-88
;       Added format keyword, J. Isensee, July, 1990
;       Work for ST Guide Star images, W. Landsman   HSTX, May 1995
;       Compute CRPIX1 correctly for X reversal   W. Landsman HSTX August 1995
;       Converted to IDL V5.0   W. Landsman   September 1997
;       Added ERRMSG, Use double precision formatting, W. Landsman April 2000
;       Recognize PC00n00m astrometry matrix   W. Landsman   December 2001
;       Use V6.0 notation W. Landsman October 2012
;- 
 On_error, 2
 npar = N_params()
 if npar LE 1 then begin
     print,'Syntax: HREVERSE, oldim, oldhd, [ subs, /SILENT, ERRMSG = ]'
     print,'    or  HREVERSE, oldim, oldhd, newim, newhd, [ subs, /SILENT]'
     return
 endif 

 save_err = arg_present(errmsg)     ;Does user want error msgs returned?
;                                    Check for valid 2-D image & header
 check_FITS, oldim, oldhd, dimen, /NOTYPE, ERRMSG = errmsg
  if errmsg NE '' then begin
        if ~save_err then message,'ERROR - ' + errmsg,/CON
        return
  endif

  if N_elements(dimen) NE 2 then begin 
        errmsg =  'ERROR - Input image array must be 2-dimensional'
        if ~save_err then message,'ERROR - ' + errmsg,/CON
        return
 endif

  xsize = dimen[0]  &  ysize = dimen[1]

 if npar EQ 3 then subs = newim 
 READSUBS: if (npar NE 3) && (npar NE 5) then $
 read,'Enter 1 to reverse X dimension, 2 to reverse Y dimension: ',subs
 if  ( subs NE 2 ) && ( subs NE 1 ) then begin
        message,'ERROR - Illegal Value of Subs parameter',/CON
        if npar then npar = npar -1     ;Make npar even
        goto, READSUBS    
 endif

 newhd = oldhd
 axis_name = ['X','Y']
 if ~keyword_set(SILENT) then message, /INF, $
'Now reversing ' + strn(xsize) + ' by ' + strn(ysize) + ' image about ' + $
    axis_name[subs-1] + ' dimension'

if npar GE 4 then newim = reverse( oldim,subs ) else $
                  oldim = reverse( oldim,subs )

 label = 'HREVERSE: ' + strmid(systime(),4,20)
 sxaddpar, newhd, 'HISTORY', label+ $ 
        ' Reversed About '+ axis_name[SUBS-1] + ' Dimension'

; Update astrometry info if it exists

 extast, oldhd, astr, noparams
 if noparams LT 0 then goto, DONE

  if subs EQ 1 then begin

         if strmid( astr.ctype[0],5,3) EQ 'GSS' then begin
                cnpix = -astr.xll -xsize
                sxaddpar, newhd, 'CNPIX1', cnpix
                sxaddpar, newhd, 'XPIXELSZ', -astr.xsz
         endif else begin
                 crpix1 = xsize  - (astr.crpix[0]-1)
                 sxaddpar, newhd, 'CRPIX1', crpix1

         if (noparams LT 2) || (noparams EQ 3) then $
                sxaddpar, newhd, 'CDELT1', -astr.cdelt[0] $

         else begin           ;If so, then convert them

                 sxaddpar, newhd, 'CD1_1', -astr.cd[0,0]
                 sxaddpar, newhd, 'CD2_1', -astr.cd[1,0]

         endelse 
 endelse

 endif else  begin

         if strmid(astr.ctype[0],5,3) EQ 'GSS' then begin

                cnpix = -astr.yll -ysize
                sxaddpar, newhd, 'CNPIX2', cnpix
                sxaddpar, newhd, 'YPIXELSZ', -astr.ysz

         endif else begin
                 crpix2 = ysize  - (astr.crpix[1]-1)
                 sxaddpar, newhd, 'CRPIX2', crpix2

         if (noparams LT 2) or (noparams EQ 3) then $      
                sxaddpar, newhd, 'CDELT2', -astr.cdelt[1] $

         else begin           ;If so, then convert them

                 sxaddpar, newhd, 'CD1_2', -astr.cd[0,1]
                 sxaddpar, newhd, 'CD2_2', -astr.cd[1,1]

         endelse 
         endelse

 endelse

DONE:
 if npar LE 3 then oldhd = newhd                ;update old header

return
end

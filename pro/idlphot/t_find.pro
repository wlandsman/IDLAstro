pro t_find,image, im_hdr, fitsfile, hmin, fwhm, sharplim, roundlim,$
           PRINT = print, SILENT = silent
;+
; NAME:
;        T_FIND
; PURPOSE:
;       Driver procedure (for FIND) to locate stars in an image.
; EXPLANATION:
;       Finds positive brightness perturbations (i.e stars) in a 
;       2 dimensional image.  Output is to a FITS ASCII table.
;
; CALLING SEQUENCE:
;       T_FIND, image, im_hdr, [ fitsfile, hmin, fwhm, sharplim, roundlim, 
;                                       PRINT = , /SILENT ]
; INPUTS:
;       image - 2 dimensional image array (integer or real) for which one
;               wishes to identify the stars present
;       im_hdr - FITS header associated with image array
;
; OPTIONAL INPUTS: 
;       T_FIND will prompt for these parameters if not supplied
;
;       fitsfile - scalar string specifying the name of the output FITS ASCII
;               table file
;       fwhm - FWHM to be used in the convolving filter
;       hmin - Threshold intensity for a point source - should generally
;               be 3 or 4 sigma above background level
;       sharplim - 2 element vector giving low and high Limit for 
;               sharpness statistic (Default: [0.2,1.0] )
;       roundlim - 2 element vector giving low and high Limit for
;               roundness statistic (Default: [-1.0,1.0] )
;
; OPTIONAL INPUT KEYWORDS:
;       /PRINT - if set and non-zero then NSTAR will also write its results to
;               a file find.prt.   One can specify the output file name by
;               setting PRINT = 'filename'.
;       /SILENT -   If this keyword is set and non-zero, then FIND will work
;               silently, and not display each star found
;
; OUTPUTS:
;       None
;
; PROCEDURES CALLED:
;       CHECK_FITS, FDECOMP, FIND, FTADDCOL, FTCREATE, SXADDHIST, SXADDPAR, 
;       SXDELPAR, SXPAR(), WRITEFITS
;
; REVISION HISTORY:
;       Written W. Landsman, STX  May, 1988
;       Added phpadu, J. Hill, STX, October, 1990
;       New calling syntax output to disk FITS table, W. Landsman    May 1996
;       Work with more than 32767 stars  W. Landsman August 1997
;       Converted to IDL V5.0   W. Landsman   September 1997
;       Remove obsolete !ERR system variable   W. Landsman   May 2000
;-
 On_error,2               ;Return to caller

 if N_params() LT 2 then begin                                  
    print,'Syntax -  ' + $
       'T_FIND, image, hdr, [fitsfile, hmin, fwhm, sharplim, roundlim '
    print,'                       PRINT = ,/SILENT ]'
    return
 endif

 if not keyword_set( SILENT ) then silent = 0

 check_FITS, image, im_hdr, /NOTYPE, ERRMSG = errmsg
 if ERRMSG NE ''  then begin
       message,'ERROR - ' + errmsg, /CON 
       return
 endif
 
 if N_elements(fitsfile) EQ 0 then begin
        fitsfile = ''
        read,'Enter name of output FITS ASCII table file: ', fitsfile
 endif

 find, image, x, y, flux, sharp, round, hmin, fwhm, roundlim, sharplim, $ 
                PRINT = print, SILENT = silent

 nstar = N_elements(x)
 if nstar EQ 0 then message,'No FITS table created'

 ftcreate, 80, nstar, h, tab

 name = sxpar( im_hdr, 'IMAGE', Count = N_name )
 if N_name GT 0 then sxaddpar, h, 'IMAGE',name

 sxaddpar, h, 'EXTNAME', 'IDL DAOPHOT: FIND',' Last DAOPHOT stage'
 sxaddpar, h, 'HMIN', hmin, 'Threshold Above Background'
 sxaddpar, h, 'FWHM', fwhm, 'FIND FWHM'
 sxaddpar, h, 'ROUNDLO', roundlim[0], ' Roundness Limit: Low '
 sxaddpar, h, 'ROUNDHI', roundlim[1], ' Roundness Limit: High'
 sxaddpar, h, 'SHARPLO', sharplim[0], ' Sharpness Limit: Low '
 sxaddpar, h, 'SHARPHI', sharplim[1], ' Sharpness Limit: High'

 bscale = sxpar( im_hdr, 'BSCALE', Count = N_bscale )
 if N_bscale EQ 0 then sxaddpar, h, 'BSCALE', bscale, 'Calibration Const'
 phpadu = sxpar( im_hdr, 'PHPADU', Count = N_phpadu )
 if N_phpadu EQ 0 then sxaddpar, h, 'PHPADU', phpadu, 'Photons Per ADU'

 ftaddcol, h, tab, 'STAR_ID', 4, 'I5'
 ftput, h, tab, 1, 0, lindgen(nstar)+1
 ftaddcol, h, tab, 'X', 8, 'F7.2', 'PIX'
 ftput, h, tab, 2, 0, x+1.              ;Position written in FORTRAN convention
 ftaddcol, h, tab, 'Y', 8, 'F7.2', 'PIX'
 ftput, h, tab, 3, 0, y+1.              
 ftaddcol, h, tab, 'FLUX', 8, 'F8.1', 'ADU'
 ftput, h, tab, 4, 0, flux
 ftaddcol, h, tab, 'SHARP', 8, 'F6.3'
 ftput, h, tab, 5, 0, sharp
 ftaddcol, h, tab, 'ROUND', 8, 'F6.3'
 ftput, h, tab, 6, 0, round
 sxaddhist, 'T_FIND: ' + systime(),h

 hprimary = im_hdr                ;Primary FITS header
 sxdelpar,hprimary,['NAXIS1','NAXIS2']
 sxaddpar,hprimary,'NAXIS',0
 sxaddpar,hprimary,'SIMPLE','T'
 sxaddpar,hprimary,'EXTEND','T',after='NAXIS'

 sxaddpar, h, 'NAXIS1', 80
 message,'Creating FITS ASCII table ' + fitsfile, /INF
 writefits, fitsfile, 0, hprimary
 writefits, fitsfile, tab,h,/append

 return
 end

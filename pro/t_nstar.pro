pro t_nstar,image,fitsfile,psfname,groupsel,SILENT=silent,PRINT=print, $
                NEWTABLE = newtable, VARSKY = varsky, DEBUG = debug
;+
; NAME:
;       T_NSTAR
; PURPOSE:
;       Driver procedure (for NSTAR) for simultaneous PSF fitting.  
; EXPLANATION:
;       Input and output are to disk FITS ASCII tables.
;
; CALLING SEQUENCE:
;       T_NSTAR, image, fitsfile, [psfname, groupsel, /SILENT, /PRINT
;                               NEWTABLE = , /VARSKY ]
; INPUTS:
;       IMAGE - 2-d image array
;       FITSFILE  - scalar string giving name of disk FITS ASCII table.  Must 
;               contain the keywords 'X','Y' (from T_FIND) 'AP1_MAG','SKY'
;               (from T_APER) and 'GROUP_ID' (from T_GROUP).   This table
;               will be updated with the results of T_NSTAR, unless the 
;               keyword NEWTABLE is supplied.   
;
; OPTIONAL INPUTS:
;       PSFNAME - Name of the FITS file created by T_GETPSF containing
;               PSF residuals, scalar string
;       GROUPSEL - Scalar or vector listing the groups to process.  For
;               example, to process stars in groups 2 and 5 set
;               GROUPSEL = [2,5].  If omitted, or set equal to -1,
;               then NSTAR will process all groups.
;
; OPTIONAL KEYWORD INPUTS:
;       VARSKY - If this keyword is set and non-zero, then the mean sky level
;               in each group of stars, will be fit along with the brightness
;               and positions.
;       /SILENT - if set and non-zero, then NSTAR will not display its results
;               at the terminal
;       /PRINT - if set and non-zero then NSTAR will also write its results to
;               a file NSTAR.PRT.   One can specify the output file name by
;               setting PRINT = 'filename'.
;       NEWTABLE  - Name of output disk FITS ASCII table to contain the results
;               of NSTAR.   If not supplied, then the input FITSFILE will be 
;               updated.  
;       DEBUG - if this keyword is set and non-zero, then the result of each
;               fitting iteration will be displayed.
;
; PROCEDURES CALLED:
;       FTADDCAL, FTINFO, FTGET(), FTPUT, NSTAR, SXADDHIST, 
;       SXADDPAR, SXPAR(), READFITS(), WRITEFITS
; REVISION HISTORY:
;       Written        W. Landsman         STX Co.    May, 1988
;       Check for CCDGAIN, ATODGAIN keywords to get PHPADU  W. Landsman May 1997
;       Fixed typo preventing compilation, groupsel parameter W.L. July 1997
;       Converted to IDL V5.0   W. Landsman   September 1997
;       Update for new FTINFO call    W. Landsman   May 2000
;-
 On_error,2

 if N_params() LT 2 then begin
        print, 'Syntax - T_NSTAR, image, fitsfile, [ psfname, groupsel, '
        print,'                          /VARSKY, NEWTABLE = ,/SILENT, PRINT=]'
        return
 endif

 if not keyword_set(NEWTABLE) then newtable = fitsfile

 dummy = readfits(fitsfile, hprimary, /SILENT)
 tab = readfits(fitsfile, h, /ext)

 ftinfo, h, ft_str
 ttype = strtrim(ft_str.ttype,2)

 idg = where(ttype EQ 'GROUP_ID', Nid)
 if Nid EQ 0 then begin
        message,'T_NSTAR: ERROR - Field GROUP_ID not found in header',/CON
        message,'Procedure T_GROUP must be run before T_NSTAR',/CON
        return
 endif else group = ftget(ft_str,tab,idg[0] + 1)

 if N_params() EQ 4 then begin
        nsel = N_elements(groupsel)
           if groupsel[0] LT 0 then select = indgen(N_elements(group)) $ 
        else begin
           select = where(group EQ groupsel[0])
           if nsel GT 1 then $ 
              for i=1,nsel-1 do select = [select,where(group eq groupsel[i])]
        endelse
 endif else select = indgen(N_elements(group)) 
 group = group[select]

 id = ftget( ft_str, tab, 'STAR_ID', select )
 x = ftget( ft_str, tab, 'X', select )-1.
 y = ftget( ft_str, tab, 'Y', select )-1.
 mags = ftget( ft_str, tab, 'AP1_MAG', select )          
 sky = ftget( ft_str, tab, 'SKY', select )

;Try to get read-out noise from header
 ronois = sxpar(hprimary,'RONOIS', Count = Nronois) 
 if Nronois EQ 0 then begin
    read,'Enter the read-out noise in ADU per pixel: ',ronois
    print,'Storing readout noise  of ',ronois,' in header'
    sxaddpar,hprimary,'RONOIS',ronois,' Read out noise (ADU/pixel)', $
                before='HISTORY'
 endif

 phpadu = sxpar( hprimary, 'PHPADU', COUNT = n )  ;Try to get photons per ADU
 if n EQ 0 then begin
       phpadu = sxpar( hprimary, 'GAIN', Count  = n)
       if n EQ 0 then phpadu = sxpar( hprimary, 'CCDGAIN', Count = n)
       if n EQ 0 then phpadu = sxpar( hprimary, 'ATODGAIN', Count = n)
       if n EQ 0 then begin
            read,'Enter photons per ADU (CCD Gain):  ',phpadu
      sxaddpar,hprimary,'PHPADU',phpadu,' Photons Per ADU',before = 'HISTORY'
 endif
 endif

 message,'Using photon/ADU (CCD Gain) value of ' + strtrim(phpadu,2),/INF

 nstar, image, id, x, y, mags, sky, group, phpadu, ronois, psfname, errmag,$
         iter, chisq,peak,PRINT = print, SILENT = silent, VARSKY = varsky, $
         DEBUG = debug

 id = id-1

 sxaddpar,h,'EXTNAME','IDL DAOPHOT: NSTAR','DAOPHOT stage'

 g = where(ttype EQ 'X_PSF', Ng)
 if Ng EQ 0 then ftaddcol,h,tab,'X_PSF',8,'F7.2','PIX'
 ftput,h,tab,'X_PSF',id,x+1.

 g = where(ttype EQ 'Y_PSF', Ng)
 if Ng EQ 0 then ftaddcol,h,tab,'Y_PSF',8,'F7.2','PIX'
 ftput,h,tab,'Y_PSF',id,y+1.

 g = where(ttype EQ 'PSF_MAG', Ng)
 if Ng EQ 0 then ftaddcol,h,tab,'PSF_MAG',8,'F7.3','MAG'
 ftput,h,tab,'PSF_MAG',id,mags

 g = where(ttype EQ 'ERR_PSF', Ng)
 if Ng EQ 0 then ftaddcol,h,tab,'ERR_PSF',8,'F5.3','MAG'
 ftput,h,tab,'ERR_PSF',id,errmag

 g = where(ttype EQ 'ITER', Ng)
 if Ng EQ 0 then ftaddcol,h,tab,'ITER',4,'I2'
 ftput,h,tab,'ITER',id,iter

 g = where(ttype EQ 'CHI', Ng)
 if Ng EQ 0 then ftaddcol,h,tab,'CHI',8,'F5.2'
 ftput,h,tab,'CHI',id,chisq

 g = where(ttype EQ 'PEAK', Ng)
 if Ng EQ 0 then ftaddcol,h,tab,'PEAK',8,'F7.3'
 ftput,h,tab,'PEAK',id,peak

 sxaddhist,'T_NSTAR: ' + systime(), h

 writefits, newtable, 0, hprimary
 writefits, newtable, tab,h,/append

 return
 end

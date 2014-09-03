pro t_aper,image,fitsfile,apr,skyrad,badpix,PRINT=print,SILENT=silent, $
        NEWTABLE = newtable, SETSKYVAL = setskyval,EXACT = Exact
;+
; NAME:
;       T_APER
; PURPOSE:
;       Driver procedure (for APER) to compute concentric aperture photometry.
; EXPLANATION:
;       Data is read from and written to disk FITS ASCII tables.   
;       Part of the IDL-DAOPHOT photometry sequence
;
; CALLING SEQUENCE:
;       T_APER, image, fitsfile, [ apr, skyrad, badpix, PRINT=, NEWTABLE=, 
;                       /EXACT, /SILENT, SETSKYVAL = ]
;
; INPUTS:
;       IMAGE   - input data array
;       FITSFILE  - disk FITS ASCII table name (from T_FIND).  Must contain
;               the keywords 'X' and 'Y' giving the centroid of the source
;               positions in FORTRAN (first pixel is 1) convention.   An
;               extension of .fit is assumed if not supplied.
;
; OPTIONAL INPUTS:
;       User will be prompted for the following parameters if not supplied.
;
;       APR    -  Vector of up to 12 REAL photometry aperture radii.
;       SKYRAD  - Two element vector giving the inner and outer radii
;               to be used for the sky annulus
;       BADPIX  - Two element vector giving the minimum and maximum
;               value of a good pixel (Default [-32765,32767])
;
; OPTIONAL KEYWORDS INPUTS:
;       /EXACT - If this keyword is set, then intersection of the circular
;               aperture is computed exactly (and slowly) rather than using
;               an approximation.   See APER for more info.
;       /PRINT - if set and non-zero then NSTAR will also write its results to
;               a file aper.prt.   One can specify a different output file 
;               name by setting PRINT = 'filename'.
;       /SILENT - If this keyword is set and non-zero, then APER will not
;               display photometry results at the screen, and the results 
;               will be automatically incorporated in the FITS table without
;               prompting the user
;       NEWTABLE  - Name of output disk FITS ASCII table, scalar string.   
;               If not supplied, then the input FITSFILE will be updated with 
;               the aperture photometry results.
;       SETSKYVAL - Use this keyword to force the sky to a specified value 
;               rather than have APER compute a sky value.    SETSKYVAL 
;               can either be a scalar specifying the sky value to use for 
;               all sources, or a 3 element vector specifying the sky value, 
;               the sigma of the sky value, and the number of elements used 
;               to compute a sky value.   The 3 element form of SETSKYVAL
;               is needed for accurate error budgeting.
;
; PROMPTS:
;       T_APER requires the number of photons per analog digital unit
;       (PHPADU), so that it can compute Poisson noise statistics to assign
;       photometry errors.    It first tries to find the PHPADU keyword in the
;       original image header, and if not found will look for the GAIN, 
;       CCDGAIN and finally ATODGAIN keywords.   If still not found, T_APER 
;       will prompt the user for this value.
;
; PROCEDURES:
;       APER, FTADDCOL, FTGET(), FTINFO, FTPUT, READFITS(), SXADDPAR, 
;       SXPAR(), WRITEFITS 
; REVISON HISTORY:
;       Written   W. Landsman   ST Systems Co.            May 1988
;       Store results as flux or magnitude                August 1988
;       Added SILENT keyword  W. Landsman                 Sep. 1991
;       Changed ERR SKY to ERR_SKY W. Landsman   March 1996
;       Replace TEXTOUT keyword with PRINT keyword  W. Landsman  May 1996
;       Check CCDGAIN or ATODGAIN keywords to find phpadu W. Landsman May 1997
;       Converted to IDL V5.0   W. Landsman   September 1997
;       Updated for new FTINFO calling sequence   W. Landsman  May 2000
;       Added /EXACT keyword                      W. Landsman  June 2000
;       
;-
 On_error,2                         ;Return to caller

 if N_params() LT 2 then begin
    print,'Syntax - T_APER, image, fitsfile, [ apr, skyrad, badpix'
    print,'              /EXACT, SETSKY = ,PRINT = , NEWTABLE = ,/SILENT ]'
  return
 endif

 newfile = keyword_set(NEWTABLE)
 if not keyword_set(NEWTABLE) then newtable = fitsfile

 dummy = readfits( fitsfile, hprimary, /SILENT )
 tab = readfits( fitsfile, h, /exten)

 ftinfo,h,ft_str
 ttype = strtrim(ft_str.ttype,2)
 xc = ftget( ft_str, tab, 'X' ) - 1.      ;Subtract to conv from FORTRAN to IDL
 yc = ftget( ft_str, tab, 'Y' ) - 1.

 phpadu = sxpar( hprimary, 'PHPADU', Count = n )  ;Try to get photons per ADU
 if n EQ 0 then begin
        phpadu = sxpar( hprimary, 'GAIN', Count  = n)
        if n EQ 0 then phpadu = sxpar( hprimary, 'CCDGAIN', Count = n)
        if n EQ 0 then phpadu = sxpar( hprimary, 'ATODGAIN', Count = n)
        if n EQ 0 then begin
        read,'Enter photons per ADU (CCD Gain):  ',phpadu
        message,'Storing photon/ADU value of ' + strtrim(phpadu,2) + $
               ' in header',/INF
       sxaddpar,hprimary,'PHPADU',phpadu,'Photons Per ADU',before = 'HISTORY'
       endif
 endif

 message,'Using photon/ADU value of ' + strtrim(phpadu,2),/INF

 aper, image, xc, yc, mags, errap, sky, skyerr, phpadu, apr, skyrad,$
     badpix, PRINT = print, SILENT=silent, SETSKYVAL = setskyval, EXACT = exact

 ans=''
 if NOT keyword_set(SILENT) and (NOT newfile) then read, $
    'T_APER: Update table with current results [Y]? ',ans

 if strupcase(ans) NE 'N' then begin   
    sxaddpar,h,'EXTNAME','IDL DAOPHOT: APER',' Last DAOPHOT step'
    sxaddpar,h,'SKYIN',skyrad[0],' Inner Sky Radius','TTYPE1' 
    sxaddpar,h,'SKYOUT',skyrad[1],' Outer Sky Radius','TTYPE1'
    sxaddpar,h,'BADPIX1',badpix[0],' Bad Pixel Value: LOW','TTYPE1'
    sxaddpar,h,'BADPIX2',badpix[1],' Bad Pixel Value: HIGH','TTYPE1'

    gsky = where(ttype EQ 'SKY', N_sky)
    if N_sky EQ 0 then ftaddcol,h,tab,'SKY',8,'F8.3'
    ftput,h,tab,'SKY',0,sky
    
    gskyerr = where(ttype EQ 'ERR_SKY', N_skyerr)
    if N_skyerr EQ 0 then ftaddcol,h,tab,'ERR_SKY',8,'F8.3'
    ftput,h,tab,'ERR_SKY',0,skyerr
    nstars = N_elements(xc)
    name = 'MAG'       &  e_name = 'ERR_AP'
    units = ' MAG'
    f_format = 'F7.3'  &  e_format ='F6.3'

    for i = 1,N_elements(apr) do begin    
       ii = strtrim(i,2)
      apsize = 'APR' + ii
      sxaddpar,h,apsize,apr[i-1],' Aperture ' + ii + ' Size','TTYPE1'
      field = 'AP' + ii + '_' + name                
      efield = e_name + ii 
      gap = where(ttype EQ field, Nap)
     
      if Nap EQ 0 then begin            ;Create new columns?
           ftaddcol,h,tab,field,8,f_format,units
           ftaddcol,h,tab,efield,8,e_format,units
      endif
      ftput,h,tab,field,0,fltarr(nstars) + mags[i-1,*]
      ftput,h,tab,efield,0,fltarr(nstars) + errap[i-1,*]
    endfor     

    sxaddhist,'T_APER: '+ systime(),h
 endif

 writefits, newtable, 0, hprimary
 writefits, newtable, tab,h,/append

 return
 end

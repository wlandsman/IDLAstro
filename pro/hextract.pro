pro hextract, oldim, oldhd, newim, newhd, x0, x1, y0, y1, SILENT = silent, $
    ERRMSG = errmsg,ALT = alt  
;+
; NAME:
;       HEXTRACT
; PURPOSE:
;       Extract a subimage from an array and update astrometry in FITS header
; EXPLANATION:
;       Extract a subimage from an array and create a new FITS header with
;       updated astrometry for the subarray
; CALLING SEQUENCE:
;       HEXTRACT, Oldim, Oldhd, [ Newim, Newhd, x0, x1, y0, y1, /SILENT ]
;               or
;       HEXTRACT, Oldim, Oldhd, [x0, x1, y0, y1, /SILENT, ERRMSG =  ]    
;
; INPUTS:
;       Oldim - the original image array
;       Oldhd - the original image header
;
; OPTIONAL INPUTS:
;       x0, x1, y0, y1 - respectively, first and last X pixel, and first and
;       last Y pixel to be extracted from the original image, integer scalars.
;       HEXTRACT will convert these values to long integers. 
;       If omitted,  HEXTRACT will prompt for these parameters
;
; OPTIONAL OUTPUTS:
;       Newim - the new subarray extracted from the original image 
;       Newhd - header for newim containing updated astrometry info
;               If output parameters are not supplied or set equal to
;               -1, then the HEXTRACT will modify the input parameters 
;               OLDIM and OLDHD to contain the subarray and updated header.
;
; OPTIONAL INPUT KEYWORD:
;      ALT - Single character 'A' through 'Z' or ' ' specifying which astrometry
;          system to modify in the FITS header.    The default is to use the
;          primary astrometry or ALT = ' '.    See Greisen and Calabretta (2002)
;          for information about alternate astrometry keywords.
;      /SILENT - If set and non-zero, then a message describing the extraction
;               is not printed at the terminal.   This message can also be 
;               suppressed by setting !QUIET.
; OPTIONAL KEYWORD OUTPUT:
;       ERRMSG - If this keyword is supplied, then any error mesasges will be
;               returned to the user in this parameter rather than depending on
;               on the MESSAGE routine in IDL.   If no errors are encountered
;               then a null string is returned.               
;
; PROCEDURE:
;       The FITS header parameters NAXIS1, NAXIS2, CRPIX1, and CRPIX2 are
;       updated for the extracted image.
;
; EXAMPLE:  
;       Read an image from a FITS file 'IMAGE', extract a 512 x 512 subimage 
;       with the same origin, and write to a new FITS file 'IMAGENEW'
;
;       IDL> im = READFITS( 'IMAGE', hdr )      ;Read FITS files into IDL arrays
;       IDL> hextract, im, h, 0, 511, 0, 511    ;Extract 512 x 512 subimage
;       IDL> writefits, 'IMAGENEW', im ,h       ;Write subimage to a FITS file
;
; PROCEDURES CALLED
;       CHECK_FITS, STRN(), SXPAR(), SXADDPAR, SXADDHIST
; MODIFICATION HISTORY:
;       Written, Aug. 1986 W. Landsman, STX Corp.
;       Use astrometry structure,   W. Landsman      Jan, 1994
;       Minor fix if bad Y range supplied   W. Landsman    Feb, 1996
;       Added /SILENT keyword              W. Landsman     March, 1997
;       Added ERRMSG keyword    W. Landsman   May 2000
;       Work for dimensions larger than 32767   W.L., M.Symeonidis Mar 2007
;       Added ALT keyword  W.L. April 2007
;       Use V6.0 notation W.L.  October 2012
;       Fix for SFL projection W.L.   September 2015
;- 
 On_error, 2
 compile_opt idl2
 npar = N_params()

 if (npar EQ 3) || (npar LT 2) then begin       ;Check # of parameters
    print,'Syntax - HEXTRACT, oldim, oldhd, [ newim, newhd, x0, x1, y0, y1]'
    print,'   or    HEXTRACT, oldim, oldhd, x0, x1, y0, y1, [/SILENT, ERRMSG=]'
    return
 endif
 
 save_err = arg_present(errmsg)      ;Does user want to return error messages?
;                                    Check for valid 2-D image & header
  check_FITS, oldim, oldhd, dimen, /NOTYPE, ERRMSG = errmsg
  if errmsg NE '' then begin
        if ~save_err then message,'ERROR - ' + errmsg,/CON
        return
  endif

  if N_elements(dimen) NE 2 then begin 
           errmsg = 'Input image array must be 2-dimensional'
           if ~save_err then message,'ERROR - ' + errmsg,/CON
           return
  endif

  xsize = dimen[0]  &  ysize = dimen[1]


 if ( npar LT 4 ) then Update = 1 else Update = 0     ;Update old array?

 if ( npar EQ 6 ) then begin                 ;Alternative calling sequence ?

     if ( N_elements(newim) EQ 1 ) && ( N_elements(newhd) EQ 1 ) && $
        ( N_elements(x0) EQ 1 ) && ( N_elements(x1) EQ 1 ) then begin
              y0 = x0   &  y1 = x1
              x0 = newim   &   x1 = newhd
              Update = 1
      endif 

 endif

 RDX: 
 if ( npar LE 5 )  then begin

      message, /INF, $ 
           'Original array size is ' + strn(xsize) + ' by ' + strn(ysize) 
      x0 = 0l & x1 = 0l
      read,'% HEXTRACT: Enter first and last X pixel to be extracted: ',x0,x1

 endif

 if ( x1 LT x0 ) || ( x0 LT 0 ) || ( x1 GE xsize ) then begin

     message,'ERROR - Illegal pixel range: X direction',  /CON
     print, ' '
     message, /INF,   $
     ' Legal Range is 0 < First Pixel < Last Pixel < ' + strn(xsize-1)
     if update then npar = npar < 2 else npar = npar < 4
     goto, RDX 

 endif

 RDY: if (~update && ( npar LE 7 )) || (update && (npar LT 6) ) then $ 
    read,'% HEXTRACT: Enter first and last Y pixel to be extracted: ',y0,y1

 if ( y1 LT y0 ) || ( y0 LT 0 ) || ( y1 GE ysize ) then begin

     message,'ERROR - Illegal pixel range: Y direction', /CON
     message, /INF,     $ 
      'Legal Range is 0 < First Pixel < Last Pixel < ' + strn(ysize-1)
     if update then npar = npar < 4 else npar = npar < 6 
     goto, RDY

 endif

 x0 = long(x0) & x1 = long(x1)
 y0 = long(y0) & y1 = long(y1)                                          

 naxis1 = x1 - x0 + 1 
 naxis2 = y1 - y0 + 1   ;New dimensions

 if ~keyword_set(SILENT) then message, /INF,        $
      'Now extracting a '+ strn(naxis1) + ' by ' + strn(naxis2) + ' subarray'

  if Update then oldim = oldim[ x0:x1,y0:y1 ]        $
            else newim = oldim[ x0:x1,y0:y1 ]

 newhd = oldhd
 sxaddpar, newhd, 'NAXIS1', naxis1                                   
 sxaddpar, newhd, 'NAXIS2', naxis2
 label = 'HEXTRACT: ' + systime(0)

 hist = [label,'Original image size was '+ strn(xsize) + ' by ' + strn(ysize), $
         'Extracted Image: [' + strn(x0) + ':'+ strn(x1) +  $
         ',' + strn(y0) + ':'+ strn(y1) + ']'  ]

 sxaddhist, hist, newhd


;GSSS image uses CNPIX instead of CRPIX
   cnpix1 = sxpar( oldhd, 'CNPIX1', COUNT = Ncnpix1)
         if ( Ncnpix1 EQ 1 ) then begin   ;Shift position of reference pixel

                sxaddpar, newhd, 'CNPIX1', cnpix1+x0
                cnpix2 = sxpar( oldhd, 'CNPIX2' )
                sxaddpar, newhd, 'CNPIX2', cnpix2+y0
        endif

; Update astrometry info if it exists

  if N_elements(alt) EQ 0 then alt = ''
  extast, newhd, astr, noparams, ALT = alt

  if noparams GE 0 then begin
;Handle SFL projection separately in case it was originally GLS  
  if astr.projection EQ 'SFL' then begin     
       crpix = sxpar(newhd,'CRPIX*')       
       sxaddpar,newhd,'CRPIX1'+alt,crpix[0]-x0
       sxaddpar,newhd,'CRPIX2'+alt,crpix[1]-y0
  endif else begin     
       sxaddpar, newhd, 'CRPIX1'+alt, astr.crpix[0]-x0
       sxaddpar, newhd, 'CRPIX2'+alt, astr.crpix[1]-y0
  endelse 

 endif 
 if Update then begin

      oldhd = newhd
      newim = x0 & newhd = x1
      x0 = y0 & x1 = y1

 endif

 return
 end

pro ftab_delrow,filename,rows,EXTEN_NO=exten_no, NEWFILE = newfile                      
;+
; NAME:
;       FTAB_DELROW
; PURPOSE:
;       Delete rows of data from a FITS ASCII or binary table extension
;
; CALLING SEQUENCE:
;       ftab_delrow, filename, rows, EXTEN_NO =, NEWFILE = ] 
;
; INPUTS-OUPUTS
;       filename - scalar string giving name of the FITS file containing an
;               ASCII or binary table extension. 
; 
;       rows  -  scalar or vector, specifying the row numbers to delete
;               First row has index 0.   If a vector, it will be sorted and
;               duplicates will be removed
;
; OPTIONAL KEYWORD INPUTS:
;       EXTEN_NO - scalar integer specifying which extension number to process
;               Default is to process the first extension
;       NEWFILE - scalar string specifying the name of the new output FITS file
;               FTAB_DELROW will prompt for this parameter if not supplied
;
; EXAMPLE:
;       Compress the first extension of a FITS file 'test.fits' to include 
;       only non-negative values in the 'FLUX' column
;
;       ftab_ext,'test.fits','flux',flux       ;Obtain original flux vector
;       bad = where(flux lt 0)                 ;Find negative fluxes
;       ftab_delrow,'test.fits',bad,new='test1.fits'  ;Delete specified rows
;
; RESTRICTIONS:
;       Does not work for variable length binary tables
;
; PROCEDURES USED:
;       FITS_CLOSE, FITS_OPEN, FITS_READ, FITS_WRITE, FTDELROW, TBDELROW        
;
; REVISION HISTORY:                                           
;       Written   W. Landsman        STX Co.     August, 1997
;       Converted to IDL V5.0   W. Landsman   September 1997
;       Use COPY_LUN if V5.6 or later     W. Landsman   February 2003
;       Assume since V5.6, COPY_LUN available   W. Landsman   Sep 2006
;- 
; On_error,2

 if N_params() LT 2 then begin
     print,'Syntax - FTAB_DELROW, filename, rows, [EXTEN_NO= , NEWFILE= ] '
     return                                                  
 endif

 if not keyword_set(exten_no) then exten_no = 1

 fits_open,filename,fcb_in
 nextend = fcb_in.nextend

 if fcb_in.nextend EQ 0 then $
        message,'ERROR - FITS file contains no table extensions
 if fcb_in.nextend LT exten_no then $
        message,'ERROR - FITS file contains only ' + strtrim(nextend,2) + $
                ' extensions'


 if (N_elements(newfile) EQ 0) then begin
        newfile = ''
        read,prompt='Enter name of updated FITS file: ',newfile
 endif

; Make sure specified extension contains a table and determine if it is ASCII
; or binary

 fits_read,fcb_in, tab, htab, exten_no = exten_no, /NO_PDU
 case fcb_in.xtension[exten_no] of
 'A3DTABLE': binary = 1b
 'BINTABLE': binary = 1b
 'TABLE': binary = 0b
 else: message,'ERROR - Extension type of ' + $
                ext_type + 'is not a FITS table format'
 endcase
 if binary then tbdelrow,htab,tab,rows else $
                ftdelrow,htab,tab,rows  

; Copy primary header and data array unchanged to output file

 fits_open, newfile, fcb_out, /write
 filestat = fstat(fcb_in.unit)
 hstart = fcb_in.start_header
 point_lun,fcb_in.unit,0               ;Back to the start of the file
          copy_lun, fcb_in.unit, fcb_out.unit,hstart[1] 
          fcb_out.nextend = fcb_out.nextend+1       ;flag that primary header is written

 for i = 1, Nextend  do begin
        if i EQ exten_no then begin
                fits_write, fcb_out, tab,htab
        endif else begin
          if i EQ Nextend then nbyte = filestat.size - hstart[i] $
                          else nbyte = hstart[i+1] - hstart[i]
          point_lun,fcb_in.unit,hstart[i]
          copy_lun, fcb_in.unit, fcb_out.unit,nbyte 
         endelse
 endfor
 fits_close,fcb_in
 fits_close,fcb_out

 return  
 end

pro fits_add_checksum, hdr, im, no_timestamp = no_timestamp, $
              FROM_IEEE=from_IEEE
;+
; NAME:
;    FITS_ADD_CHECKSUM
; PURPOSE:
;    Add or update the CHECKSUM and DATASUM keywords in a FITS header
; EXPLANATION: 
;     Follows the May 2002 version of the FITS checksum proposal at 
;     http://fits.gsfc.nasa.gov/registry/checksum.html 
; CALLING SEQUENCE:
;     FITS_ADD_CHECKSUM, Hdr, [ Data, /No_TIMESTAMP, /FROM_IEEE ]
; INPUT-OUTPUT:
;     Hdr - FITS header (string array), it will be updated with new 
;           (or modified) CHECKSUM and DATASUM keywords 
; OPTIONAL INPUT:
;     Data - data array associated with the FITS header.   If not supplied, or
;           set to a scalar, then the program checks whether there is a 
;           DATASUM keyword already in the FITS header containing the 32bit
;           checksum for the data.  If there is no such keyword then there 
;           assumed to be no data array associated with the FITS header.
; OPTIONAL INPUT KEYWORDS:
;    /FROM_IEEE - If this keyword is set, then the input is assumed to be in 
;             big endian format (e.g. an untranslated FITS array).    This 
;             keyword only has an effect on little endian machines (e.g. 
;             a Linux box).
;    /No_TIMESTAMP - If set, then a time stamp is not included in the comment
;             field of the CHECKSUM and DATASUM keywords.   Unless the 
;             /No_TIMESTAMP keyword is set, repeated calls to FITS_ADD_CHECKSUM
;             with the same header and data will yield different values of 
;             CHECKSUM (as the date stamp always changes).   However, use of the
;             date stamp is recommended in the checksum proposal. 
; PROCEDURES USED:
;     CHECKSUM32, FITS_ASCII_ENCODE(), GET_DATE, SXADDPAR, SXPAR()
; REVISION HISTORY:
;     W. Landsman    SSAI    December 2002
;     Fix problem with images with a multiple of 2880 bytes.  W.L. May 2008
;     Avoid conversion error when DATASUM is an empty string  W.L.  June 2008
;     Don't update DATASUM if not already present and no data array supplied 
;                       W.L. July 2008 
;     Make sure input header array has 80 chars/line  W.L. Aug 2009
;-
 On_error,2
 compile_opt idl2
 
 if N_params() EQ 0 then begin 
     print,'Syntax - FITS_ADD_CHECKSUM, Hdr, Data, /No_TIMESTAMP, /FROM_IEEE'
     return
 endif

 datasum = sxpar(hdr,'DATASUM', Count = N_DATASUM)
 Nim = N_elements(im)
 datasum_update = 1b
 if Nim GT 1 then begin
     checksum32,im, dsum,FROM_IEEE = from_IEEE
     remain = Nim mod 2880
     if remain GT 0 then begin
         exten = sxpar( hdr, 'XTENSION', Count = N_exten)
         if N_exten GT 0 then if exten EQ 'TABLE   ' then $
                 checksum32,[dsum,replicate(32b,2880-remain)],dsum
    endif
    sdsum = strtrim(dsum,2)
    dsum_exist= 1b
 endif else begin 
        if N_datasum EQ 0 then begin      ;Don't update DATASUM keyword 
	      datasum_update = 0b     
 	      sdsum = '         0' 
	 endif else begin
	   if strtrim(datasum,2) EQ '' then dsum=0 else dsum = ulong(datasum)
           sdsum = strtrim(dsum,2)
       endelse   
 endelse 
 
 if keyword_set(no_timestamp) then tm = '' else Get_date,tm,/timetag

; Do the Checksum keywords already exist?

  if N_DATASUM GT 0 then verb = 'updated ' else verb = 'created '
  if datasum_update then sxaddpar,hdr,'DATASUM', sdsum,  $
    ' data unit checksum ' + verb + tm

 test = sxpar(hdr,'CHECKSUM', Count = N_CHECKSUM)
 if N_CHECKSUM GT 0 then verb = 'updated ' else verb = 'created '
 sxaddpar,hdr,'CHECKSUM','0000000000000000', $
       ' HDU checksum ' + verb + tm   ;Initialize CHECKSUM keyword
;Make sure each line in header is 80 characters
 if ~array_equal(strlen(hdr),80) then begin
     n = N_elements(hdr)
     bhdr = replicate(32b,80,n )
     for i=0, n-1 do bhdr[0,i] = byte(hdr[i])
 endif else bhdr = byte(hdr)

 remain = N_elements(bhdr) mod 2880 
 if remain  NE 0 then $
       bhdr = [reform(bhdr,N_elements(bhdr)), replicate(32b, 2880 - remain) ]
 checksum32,bhdr, hsum, /NoSAVE
 if N_elements(dsum) GT 0 then checksum32, [dsum,hsum], hdusum $
                        else hdusum = hsum
 
 ch = FITS_ASCII_ENCODE(not hdusum) ;ASCII encode the complement of the checksum 
 sxaddpar,hdr,'CHECKSUM',ch

 return
 end

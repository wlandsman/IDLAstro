 function fits_test_checksum,hdr, data, ERRMSG = errmsg,FROM_IEEE=from_ieee
;+
; NAME:
;    FITS_TEST_CHECKSUM()
; PURPOSE:
;    Verify the values of the CHECKSUM and DATASUM keywords in a FITS header 
; EXPLANATION: 
;     Follows the 2007 version of the FITS checksum proposal at 
;     http://fits.gsfc.nasa.gov/registry/checksum.html
; 
; CALLING SEQUENCE:
;    result = FITS_TEST_CHECKSUM(HDR, [ DATA, ERRMSG=, /FROM_IEEE ])
; INPUTS:
;    HDR - FITS header (vector string)
; OPTIONAL DATA:
;    DATA - data array associated with the FITS header.   An IDL structure is 
;           not allowed.    If not supplied, or
;           set to a scalar, then there is assumed to be no data array 
;           associated with the FITS header.
; RESULT:
;     An integer -1, 0 or 1 indicating the following conditions:
;           1 - CHECKSUM (and DATASUM) keywords are present with correct values
;           0 - CHECKSUM keyword is not present
;          -1 - CHECKSUM or DATASUM keyword does not have the correct value
;               indicating possible data corruption.
; OPTIONAL INPUT KEYWORD:
;    /FROM_IEEE - If this keyword is set, then the input is assumed to be in 
;             big endian format (e.g. an untranslated FITS array).    This 
;             keyword only has an effect on little endian machines (e.g. 
;             a Linux box).
; OPTIONAL OUTPUT KEYWORD:
;     ERRMSG - will contain a scalar string giving the error condition.   If
;              RESULT = 1 then ERRMSG will be an empty string.   If this 
;              output keyword is not supplied, then the error message will be
;              printed at the terminal.
; NOTES:
;     The header and data must be *exactly* as originally written in the FITS 
;     file.  By default, some FITS readers may alter keyword values (e.g. 
;     BSCALE) or append information (e.g. HISTORY or an inherited primary 
;     header) and this will alter the checksum value.           
; PROCEDURES USED:
;    CHECKSUM32, FITS_ASCII_ENCODE(), SXPAR()
; EXAMPLE:
;     Verify the CHECKSUM keywords in the primary header/data unit of a FITS 
;     file 'test.fits'
;
;     FITS_READ,'test.fits',data,hdr,/no_PDU,/NoSCALE
;     print,FITS_TEST_CHECKSUM(hdr,data)
;
;     Note the use of the /No_PDU and /NoSCALE keywords to avoid any alteration 
;     of the FITS header
; REVISION HISTORY:
;     W. Landsman  SSAI               December 2002
;     Return quietly if CHECKSUM keywords not found W. Landsman May 2003
;     Add /NOSAVE to CHECKSUM32 calls when possible W. Landsman Sep 2004
;-
  On_error,2 
  compile_opt idl2 
  
  if N_Params() LT 1 then begin
      print,'Syntax - result = FITS_TEST_CHECKSUM(Hdr, [Data,' +  $
                               ' ERRMSG=, /FROM_IEEE ])'
      return, 0
  endif
  result = 1
  printerr = ~arg_present(errmsg)
  checksum = sxpar(hdr,'CHECKSUM', Count = N_checksum)
  datasum = sxpar(hdr,'DATASUM', Count = N_datasum)
  if (N_checksum EQ 0) then begin
      errmsg = 'CHECKSUM keyword not present in FITS header'
      if printerr then message,/con, errmsg
      return, 0
  endif 
  if N_datasum EQ 0 then datasum = '0' 
  ch  = shift(byte(checksum),-1)
  checksum32,ch-48b, sum32, /NOSAVE
  bhdr = byte(hdr)
  remain = N_elements(bhdr) mod 2880 
  if remain  NE 0 then $
       bhdr = [reform(bhdr,N_elements(bhdr)), replicate(32b, 2880 - remain) ]
  checksum32,bhdr, hsum, FROM_IEEE = from_ieee, /NOSAVE
  Ndata = N_elements(data)
  if Ndata GT 1 then begin 
           checksum32, data, dsum, FROM_IEEE= from_ieee
           remain = Ndata mod 2880
           if remain GT 0 then begin
              exten = sxpar( hdr, 'XTENSION', Count = N_exten)
              if N_exten GT 0 then if exten EQ 'TABLE   ' then $
                      checksum32,[dsum,replicate(32b,2880-remain)],dsum,/NOSAVE
           endif
           checksum32, [dsum, hsum], hdusum, /NOSAVE
           dsum = strtrim(dsum,2)
           if dsum NE datasum then begin
                  result = 1
                  errmsg = 'Computed Datasum: ' + dsum + $
                           ' FITS header value: ' + datasum
                  if printerr then message,/Con, errmsg 
           endif
  endif else hdusum = hsum

  csum = FITS_ASCII_ENCODE(not hdusum)
  if csum NE '0000000000000000' then begin
                  result = -1
                  errmsg = 'Computed Checksum: ' + csum + $
                           ' FITS header value: ' + checksum
                   if printerr then message,/Con, errmsg 
  endif
  return, result
  end

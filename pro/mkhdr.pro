pro mkhdr, header, im, naxisx, IMAGE = image, EXTEND = extend
;+
; NAME:
;       MKHDR
; PURPOSE:
;       Make a minimal primary (or IMAGE extension) FITS header
; EXPLANATION:
;       If an array is supplied,  then the created FITS header will be 
;       appropriate to the supplied array.  Otherwise, the user can specify 
;       the dimensions and datatype.
;
;       To update an *existing* FITS header with a new image array, instead 
;       use check_FITS, /Update 
;
; CALLING SEQUENCE:
;       MKHDR, header                   ;Prompt for image size and type
;               or
;       MKHDR, header, im, [ /IMAGE, /EXTEND ]
;               or
;       MKHDR, header, type, naxisx, [/IMAGE, /EXTEND ]         
;
; OPTIONAL INPUTS:
;       IM - If IM is a vector or array then the header will be made
;               appropriate to the size and type of IM.  IM does not have
;               to be the actual data; it can be a dummy array of the same
;               type and size as the data.    Set IM = '' to create a dummy
;               header with NAXIS = 0. 
;       TYPE - If 2 parameters are supplied, then the second parameter
;               is interpreted as an integer giving the IDL datatype e.g. 
;               1 - Byte, 2 - 16 bit integer, 4 - float, 3 - Long
;       NAXISX - Vector giving the size of each dimension (NAXIS1, NAXIS2, 
;               etc.).  
;
; OUTPUT:
;       HEADER - image header, (string array) with required keywords
;               BITPIX, NAXIS, NAXIS1, ... Further keywords can be added
;               to the header with SXADDPAR. 
;
; OPTIONAL INPUT KEYWORDS:
;       /IMAGE   = If set, then a minimal header for a FITS IMAGE extension
;               is created.    An IMAGE extension header is identical to
;               a primary FITS header except the first keyword is 
;               'XTENSION' = 'IMAGE' instead of 'SIMPLE  ' = 'T'
;       /EXTEND  = If set, then the keyword EXTEND is inserted into the file,
;               with the value of "T" (true).    The EXTEND keyword can 
;               optionally be included in a primary header, if the FITS file 
;               contains extensions.
;
; RESTRICTIONS:
;       (1)  MKHDR should not be used to make an STSDAS header or a FITS
;               ASCII or Binary Table extension header.   Instead use
;
;               SXHMAKE - to create a minimal STSDAS header
;               FXBHMAKE - to create a minimal FITS binary table header
;               FTCREATE - to create a minimal FITS ASCII table header
;
;       (2)  Any data already in the header before calling MKHDR
;               will be destroyed.
; EXAMPLE:
;       Create a minimal FITS header, Hdr, for a 30 x 40 x 50 INTEGER*2 array
;
;             IDL> mkhdr, Hdr, 2, [30,40,50]
;
;       Alternatively, if the array already exists as an IDL variable, Array,
;
;              IDL> mkhdr, Hdr, Array
;
; PROCEDURES CALLED:
;       SXADDPAR, GET_DATE
;
; REVISION HISTORY:
;       Written November, 1988               W. Landsman
;       May, 1990, Adapted for IDL Version 2.0, J. Isensee
;       Aug, 1997, Use SYSTIME(), new DATE format  W. Landsman
;       Allow unsigned data types    W. Landsman   December 1999
;       Set BZERO = 0 for unsigned integer data  W. Landsman January 2000
;       EXTEND keyword must immediately follow last NAXISi W. Landsman Sep 2000
;       Add FITS definition COMMENT to primary headers W. Landsman Oct. 2001
;       Allow (nonstandard) 64 bit integers   W. Landsman  Feb. 2003
;       Add V6.0 notation W. Landsman July 2012
;       Support unsigned 64 bit integers W. Landsman January 2018 
;-                          
 compile_opt idl2

 npar = N_params()
 if npar LT 1 then begin
   print,'Syntax:  MKHDR, header, [ im, /IMAGE, /EXTEND ]'
   print,'    or   MKHDR, header, [ type, naxisx, /IMAGE, /EXTEND ]'
   print,'   header - output FITS header to be created'
   return
 endif
 
 Catch, theError
IF theError NE 0 then begin
	Catch,/Cancel
	void = cgErrorMsg(/quiet)
	RETURN
ENDIF

 if (npar eq 1) then begin               ;Prompt for keyword values
    read,'Enter number of dimensions (NAXIS): ',naxis
    s = lonarr(naxis+2)
    s[0] = naxis
    if ( naxis GT 0 ) then begin       ;Make sure not a dummy header
    for i = 1,naxis do begin       ;Get dimension of each axis
      keyword = 'NAXIS' + strtrim(i,2)
      read,'Enter size of dimension '+ strtrim(i,2) + ' ('+keyword+'): ',nx
      s[i] = nx                            
    endfor
  endif

  print,'Allowed datatypes are (1) Byte, (2) 16 bit integer, (3) 32 bit integer'
  print,'                      (4) 32bit floating, (5) 64 bit double precision' 
  print,'                  or (14) 64bit integer'
  read,'Enter datatype: ',stype
  s[s[0] + 1] = stype

 endif else $
     if ( npar EQ 2 ) then s = size(im) $  ;Image array supplied
          else  s = [ N_elements(naxisx),naxisx, im ] ;Keyword values supplied

 stype = s[s[0]+1]              ;Type of data    
        case stype of
        0:      message,'ERROR: Input data array is undefined'
        1:      bitpix = 8  
        2:      bitpix = 16  
        3:      bitpix = 32  
        4:      bitpix = -32 
        5:      bitpix = -64 
        6:      message,'Complex types not allowed as FITS primary arrays'  
        7:      bitpix = 8
       12:      bitpix = 16
       13:      bitpix = 32
       14:      bitpix = 64
       15:      bitpix = 64
        else:   message,'ERROR: Illegal Image Datatype'
        endcase

 header = strarr(s[0] + 7) + string(' ',format='(a80)')      ;Create empty array
 header[0] = 'END' + string(replicate(32b,77))

 if keyword_set( IMAGE) then $
    sxaddpar, header, 'XTENSION', 'IMAGE   ',' IMAGE extension' $
 else $
    sxaddpar, header, 'SIMPLE', 'T',' Written by IDL:  '+ systime()

 sxaddpar, header, 'BITPIX', bitpix, ' Number of bits per data pixel'
 sxaddpar, header, 'NAXIS', S[0],' Number of data axes'       ;# of dimensions

 if ( s[0] GT 0 ) then begin
   for i = 1, s[0] do sxaddpar,header,'NAXIS' + strtrim(i,2),s[i]
 endif

 if keyword_set( IMAGE) then begin
     sxaddpar, header, 'PCOUNT', 0, ' No Group Parameters'
     sxaddpar, header, 'GCOUNT', 1, ' One Data Group'
 endif else begin
     if keyword_set( EXTEND) or (s[0] EQ 0) then $
          sxaddpar, header, 'EXTEND', 'T', ' FITS data may contain extensions'
     Get_date, dte                       ;Get current date as CCYY-MM-DD
     sxaddpar, header, 'DATE', dte, $
           ' Creation UTC (CCCC-MM-DD) date of FITS header'
  endelse

 if stype EQ 12 then sxaddpar, header,'O_BZERO',32768, $
            ' Original Data is Unsigned Integer'
 if stype EQ 13 then sxaddpar, header,'O_BZERO',2147483648, $
            ' Original Data is Unsigned Long'
 if stype EQ 15 then sxaddpar, header,'O_BZERO',ulong64(2)^63, $
            ' Original Data is Unsigned 64 bit Long'           
 header = header[0:s[0]+7]

 if ~keyword_set(IMAGE) then begin   ;Add FITS definition for primary header
     sxaddpar,header,'COMMENT ', $
      "FITS (Flexible Image Transport System) format is defined in 'Astronomy"
     sxaddpar,header,'COMMENT ', $
      "and Astrophysics', volume 376, page 359; bibcode 2001A&A...376..359H"
 endif 
 end

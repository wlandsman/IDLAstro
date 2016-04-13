pro check_FITS, im, hdr, dimen, idltype, UPDATE = update, NOTYPE = notype, $
                   SDAS = sdas, FITS = fits, SILENT = silent, ERRMSG = errmsg
;+
; NAME:
;       CHECK_FITS
; PURPOSE:
;       Check that keywords in a FITS header array match the associated data  
; EXPLANATION:
;       Given a FITS array IM, and a associated FITS header HDR, this
;       procedure will check that
;               (1) HDR is a string array, and IM is defined and numeric   
;               (2) The NAXISi values in HDR are appropriate to the dimensions 
;                   of IM
;               (3) The BITPIX value in HDR is appropriate to the datatype of IM
;       If the /UPDATE keyword is present, then the FITS header will be 
;       modified, if necessary, to force agreement with the image array
;
; CALLING SEQUENCE:
;       check_FITS, im, hdr, [ dimen, idltype, /UPDATE, /NOTYPE, /SILENT
;                              ERRMSG = ]'
;
; INPUT PARAMETERS:
;       IM -  FITS array, e.g. as read by READFITS
;       HDR - FITS header (string array) associated with IM
;
; OPTIONAL OUTPUTS:
;       dimen - vector containing actual array dimensions
;       idltype- data type of the FITS array as specified in the IDL SIZE
;               function (1 for BYTE, 2 for 16 bit integer, 3 for 32 bit integer, etc.)
;
; OPTIONAL KEYWORD INPUTS:
;       /NOTYPE - If this keyword is set, then only agreement of the array
;               dimensions with the FITS header are checked, and not the 
;               data type.
;       /UPDATE - If this keyword is set then the BITPIX, NAXIS and NAXISi
;               FITS keywords will be updated to agree with the array
;       /FITS, /SDAS -  these are obsolete keywords that now do nothing 
;       /SILENT - If keyword is set and nonzero, the informational messages 
;               will not be printed
; OPTIONAL KEYWORD OUTPUT:
;       ERRMSG  = If this keyword is present, then any error messages will be
;                 returned to the user in this parameter rather than
;                 depending on the MESSAGE routine in IDL.  If no errors are
;                 encountered, then a null string is returned.  
;
; PROCEDURE:
;       Program checks the NAXIS and NAXISi keywords in the header to
;       see if they match the image array dimensions, and checks whether
;       the BITPIX keyword agrees with the array type.
;
; PROCEDURE CALLS:
;       FXADDPAR, FXPAR(), SXDELPAR
; MODIFICATION HISTORY:
;       Written, December 1991  W. Landsman Hughes/STX to replace CHKIMHD
;       No error returned if NAXIS=0 and IM is a scalar   W. Landsman  Feb 93
;       Fixed bug for REAL*8 STSDAS data W. Landsman July 93
;       Make sure NAXIS agrees with NAXISi  W. Landsman  October 93
;        Converted to IDL V5.0   W. Landsman   September 1997
;       Allow unsigned data types   W. Landsman December 1999
;       Allow BZERO = 0 for unsigned data types   W. Landsman January 2000
;       Added ERRMSG keyword, W. Landsman February 2000
;       Use FXADDPAR to put NAXISi in proper order   W. Landsman August 2000
;       Improper FXADDPAR call for DATATYPE keyword  W. Landsman December 2000
;       Remove explicit setting of obsolete !err W. Landsman February 2004
;       Remove SDAS support   W. Landsman       November 2006
;       Fix dimension errors introduced Nov 2006
;       Work again for null arrays W. Landsman/E. Hivon May 2007
;       Use V6.0 notation  W.L.  Feb. 2011 
;- 
 compile_opt idl2
 On_error,2

 if N_params() LT 2 then begin
    print,'Syntax - CHECK_FITS, im, hdr, dimen, idltype, '
    print,'            [ /UPDATE, /NOTYPE, ERRMSG=, /SILENT ]'
    return
 endif

 if arg_present(errmsg) then errmsg = ''       

 if size(hdr,/TNAME) NE 'STRING' then begin        ;Is hdr of string type?
        message= 'FITS header is not a string array'
        if  N_elements(ERRMSG) GT 0 then errmsg = message else $
             message, 'ERROR - ' + message, /CON
             return 
 endif

 im_info = size(im,/struc)
 ndimen = im_info.n_dimensions
 if ndimen GT 0 then dimen = im_info.dimensions[0:ndimen-1]
 idltype = im_info.type

 
 nax = fxpar( hdr, 'NAXIS', Count = N_naxis ) 
 if N_naxis EQ 0 then begin
        message = 'FITS header missing NAXIS keyword'
        if  N_elements(errmsg) GT 0 then errmsg = message else $
             message,'ERROR - ' + message,/CON 
             return 
 endif
        
 if ndimen EQ 0  then $             ;Null primary array
     if nax EQ 0 then return else begin
         message = 'FITS array is not defined'
         if  N_elements(errmsg) GT 0 then errmsg = message else $
             message,'ERROR - ' +message,/con 
             return 
     endelse

 
 naxis = fxpar( hdr, 'NAXIS*')
 naxi = N_elements( naxis )
 if nax GT naxi then begin                 ;Does NAXIS agree with # of NAXISi?
        if keyword_set( UPDATE) then begin
                fxaddpar, hdr, 'NAXIS', naxi
                if ~keyword_set(SILENT) then message, /INF, $
        'NAXIS changed from ' + strtrim(nax,2) + ' to ' + strtrim(naxi,2)
        endif else begin 
                message =  'FITS header has NAXIS = ' + strtrim(nax,2) + $
                ', but only ' + strtrim(naxi, 2) + ' axes defined'
                if  N_elements(ERRMSG) GT 0 then errmsg = message else $
                    message, 'ERROR - ' + message
                return
        endelse
 endif

 last = naxi-1                        ;Remove degenerate dimensions
 while ( (naxis[last] EQ 1) && (last GE 1) ) do last--
 if last NE nax-1 then begin
     naxis = naxis[ 0:last]
 endif 

 if ( ndimen NE last + 1 ) then begin
    if ~keyword_set( UPDATE) THEN begin
        message = $
        '# of NAXISi keywords does not match # of array dimensions'
        if  N_elements(ERRMSG) GT 0 then errmsg = message else $
                                     message,'ERROR - ' + message,/CON 
        return 
 
     endif else goto, DIMEN_ERROR
 endif

 for i = 0,last do begin
      if naxis[i] NE dimen[i] then begin
      if ~keyword_set( UPDATE ) then begin
          message =  'Invalid NAXIS' + strtrim( i+1,2 ) + $
	             ' keyword value in header'
          if  N_elements(ERRMSG) GT 0 then errmsg = message else $ 
                                       message,'ERROR - ' + message,/CON
          return 
      endif else goto, DIMEN_ERROR
    endif
 endfor

BITPIX:     

 if ~keyword_set( NOTYPE ) then begin

 
  bitpix = fxpar( hdr, 'BITPIX')
  
    case idltype of

     1: if bitpix NE 8 then goto, BITPIX_ERROR
     2: if bitpix NE 16 then goto, BITPIX_ERROR  
     4: if bitpix NE -32 then goto, BITPIX_ERROR       
     3: if bitpix NE 32 then goto, BITPIX_ERROR 
     5: if bitpix NE -64 then goto, BITPIX_ERROR 
     12:if bitpix NE 16 then goto, BITPIX_ERROR
     13: if bitpix NE 32 then goto, BITPIX_ERROR
     
     else: begin
              message = 'Data array is not a valid FITS datatype'
             if  N_elements(ERRMSG) GT 0 then errmsg = message else $
                                          message,'ERROR - ' + message,/CON
             return 
      end

   endcase

 endif

 return

BITPIX_ERROR:
    if keyword_set( UPDATE ) then begin
    bpix = [0, 8, 16, 32, -32, -64, 32, 0, 0, 0, 0, 0, 16,32 ]
    comm = ['',' Character or unsigned binary integer', $
               ' 16-bit twos complement binary integer', $
               ' 32-bit twos complement binary integer', $
               ' IEEE single precision floating point', $
               ' IEEE double precision floating point', $
               ' 32-bit twos complement binary integer','','','','','', $
               ' 16-bit unsigned binary integer', $
               ' 32-bit unsigned binary integer' ]
    bitpix = bpix[idltype]
    comment = comm[idltype]
    if ~keyword_set(SILENT) then message, /INF, $
        'BITPIX value of ' + strtrim(bitpix,2) +  ' added to FITS header'
    fxaddpar, hdr, 'BITPIX', bitpix, comment
    return

  endif else begin 
       message = 'BITPIX value of ' + strtrim(bitpix,2) + $
                 ' in FITS header does not match array'
      if  N_elements(ERRMSG) GT 0  then errmsg = message else  $
          message,'ERROR - ' + message,/CON
      return
 endelse

DIMEN_ERROR:
   if keyword_set( UPDATE ) then begin
        fxaddpar, hdr, 'NAXIS', ndimen, before = 'NAXIS1'
	naxis = 'NAXIS' + strtrim(indgen(ndimen)+1,2)
        for i = 1, ndimen do fxaddpar, hdr, naxis[i-1], dimen[i-1], $
                'Number of positions along axis ' + strtrim(i,2), $
                after = 'NAXIS' + strtrim(i-1,2)          
        if naxi GT ndimen then begin
                for i = ndimen+1, naxi do sxdelpar, hdr, 'NAXIS'+strtrim(i,2)
        endif
        if ~keyword_set(SILENT) then message, /INF, $
                'NAXIS keywords in FITS header have been updated'
        goto, BITPIX
   endif

 end

pro mrd_hread, unit, header, status, SILENT = silent, FIRSTBLOCK = firstblock, $
    ERRMSG = errmsg,SKIPDATA=skipdata,NO_BADHEADER=no_badheader
;+
; NAME: 
;     MRD_HREAD
;
; PURPOSE: 
;     Reads a FITS header from an opened disk file or Unix pipe
; EXPLANATION:
;     Like FXHREAD but also works with compressed Unix files
;
; CALLING SEQUENCE: 
;     MRD_HREAD, UNIT, HEADER  [, STATUS, /SILENT, ERRMSG =, /FIRSTBLOCK ]
; INPUTS: 
;     UNIT    = Logical unit number of an open FITS file
; OUTPUTS: 
;     HEADER  = String array containing the FITS header.
; OPT. OUTPUTS: 
;     STATUS  = Condition code giving the status of the read.  Normally, this
;                 is zero, but is set to -1 if an error occurs, or if the
;                 first byte of the header is zero (ASCII null).
; OPTIONAL KEYWORD INPUT:
;      /FIRSTBLOCK - If set, then only the first block (36 lines or less) of 
;                the FITS header are read into the output variable.   If only
;                size information (e.g. BITPIX, NAXIS) is needed from the
;                header, then the use of this keyword can save time.  The 
;                file pointer is still positioned at the end of the header,
;                even if the /FIRSTBLOCK keyword is supplied.
;      /SILENT - If set, then warning messages about any invalid characters in
;                the header are suppressed.
;      /SKIPDATA - If set, then the file point is positioned at the end of the
;                HDU after the header is read, i.e. the following data block
;                is skipped.   Useful, when one wants to the read the headers
;                of multiple extensions.
;      /NO_BADHEADER - if set, returns if FITS header has illegal characters
;                By default, MRD_HREAD replaces bad characters with blanks,
;                issues a warning, and continues.
; OPTIONAL OUTPUT PARAMETER:
;       ERRMSG  = If this keyword is present, then any error messages will be
;                 returned to the user in this parameter rather than
;                 depending on the MESSAGE routine in IDL.  If no errors are
;                 encountered, then a null string is returned.
; RESTRICTIONS: 
;      The file must already be positioned at the start of the header.  It
;      must be a proper FITS file.
; SIDE EFFECTS: 
;       The file ends by being positioned at the end of the FITS header, unless
;       an error occurs.
; REVISION HISTORY:
;      Written,  Thomas McGlynn                     August 1995
;      Modified, Thomas McGlynn		     January 1996
;         Changed MRD_HREAD to handle Headers which have null characters
;          A warning message is printed out but the program continues.
;          Previously MRD_HREAD would fail if the null characters were
;          not in the last 2880 byte block of the header.  Note that
;          such characters are illegal in the header but frequently
;          are produced by poor FITS writers.
;      Added /SILENT keyword   W. Landsman   December 2000
;      Added /FIRSTBLOCK keyword  W. Landsman   February 2003
;      Added ERRMSG, SKIPDATA keyword W. Landsman          April 2009
;      Close file unit even after error message   W.L.  October 2010
;      Added /NO_BADHEADER  Zarro (ADNET), January 2012
;-
  On_error,2
  compile_opt idl2
  printerr = ~arg_present(errmsg)
  errmsg = ''

  block = string(replicate(32b, 80, 36))
		
  Nend = 0                  ;Signal if 'END     ' statement is found
  nblock = 0

  while Nend EQ 0 do begin
		
; Shouldn't get eof in middle of header.
       if eof(unit) then begin
                errmsg = 'EOF encountered in middle of FITS header'
		if printerr then message,errmsg,/CON
		free_lun, unit
		status = -1
		return
		endif
		
	on_ioerror, error_return
	readu, unit, block
	on_ioerror, null

; Check that there aren't improper null characters in strings that are causing 
; them to be truncated.   Issue a warning but continue if problems are
; found (unless /NO_BADHEADER is set)

	w = where(strlen(block) ne 80, Nbad)
	if (Nbad GT 0) then begin
                warning='Warning-Invalid characters in header'
		if ~keyword_set(SILENT) then message,warning,/INF
                if keyword_set(NO_BADHEADER) then begin
                  status=-1 & errmsg=warning & free_lun,unit & return
                endif
		block[w] = string(replicate(32b, 80))
	endif	       
	w = where(strmid(block, 0, 8) eq 'END     ', Nend)
        if nblock EQ 0 then begin
               header = Nend GT 0 ?  block[ 0:w[0] ] : block
	       nblock =1
        endif else $
	       if ~keyword_set(firstblock) then $
	         header = Nend GT 0 ? [header,block[0:w[0]]] : [header, block]
			
	endwhile
		
        if keyword_set(skipdata) then begin 
                bitpix = fxpar(header,'bitpix')
                naxis  = fxpar(header,'naxis')
                gcount = fxpar(header,'gcount') 
                if gcount eq 0 then gcount = 1
                pcount = fxpar(header,'pcount')
               
                if naxis gt 0 then begin 
                        dims = fxpar(header,'naxis*')           ;read dimensions
  			ndata = product(dims,/integer)
                endif else ndata = 0
                
                nbytes = long64(abs(bitpix) / 8) * gcount * (pcount + ndata)
	        mrd_skip, unit, nbytes
	endif	
	status = 0
	return
error_return:
        status = -1
	errmsg = 'END Statement not found in FITS header'
        if printerr then message, 'ERROR ' + errmsg	
	return
end
			

pro sxdelpar, h, parname
;+
; NAME:
;	SXDELPAR
; PURPOSE:
;	Procedure to delete a keyword parameter(s) from a FITS header
;
; CALLING SEQUENCE:
;	sxdelpar, h, parname
;
; INPUTS:
;	h - FITS header, string array
;	parname - string or string array of keyword name(s) to delete
;
; OUTPUTS:
;	h - updated FITS header, If all lines are deleted from 
;		the header, then h is returned with a value of 0
;
; EXAMPLE:
;	Delete the astrometry keywords CDn_n from a FITS header, h
;
;	IDL> sxdelpar, h, ['CD1_1','CD1_2','CD2_1','CD2_2']
;
; NOTES:
;	(1)  No message is returned if the keyword to be deleted is not found
;	(2)  All appearances of a keyword in the header will be deleted
; HISTORY:
;   version 1  D. Lindler Feb. 1987
;	Test for case where all keywords are deleted    W. Landsman Aug 1995 
;   Allow for headers with more than 32767 lines W. Landsman Jan. 2003
;   Use ARRAY_EQUAL, cleaner syntax  W. L.  July 2009
;------------------------------------------------------------------
 On_error,2
 compile_opt idl2

 if N_Params() LT 2 then begin
      print,'Syntax - SXDELPAR, h, parname'
      return
 endif

; convert parameters to string array of upper case names of length 8 char


 if size(parname,/type) NE 7 then $
         message,'Keyword name(s) must be a string or string array'
 par = strtrim( strupcase(parname),2 ) 

 sz = size(h,/structure)
 if (sz.N_dimensions NE 1) || (sz.type NE 7) then $
	message,'FITS header (1st parameter) must be a string array'

 nlines = sz.N_elements		;number of lines in header array
 pos = 0L		;position in compressed header with keywords removed

; loop on header lines

 keyword = strtrim( strmid(h,0,8), 2 )
 for i = 0L, nlines-1 do begin
        if array_equal(keyword[i] NE par, 1b) then begin   
 	   h[pos] = h[i]		;keep it
	   pos++		;increment number of lines kept
	   if keyword[i] eq 'END' then break  	;end of header
        endif 
 endfor

 if pos GT 0 then h = h[0:pos-1] else h = 0	      ;truncate

 return
 end

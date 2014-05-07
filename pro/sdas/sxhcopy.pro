pro sxhcopy, h, keyword1, keyword2, hout
;+
; NAME:
;	SXHCOPY                            
; PURPOSE:
;	Copies selected portions of one header to another
;
; CALLING SEQUENCE:
;	sxhcopy, h, keyword1, keyword2, hout
;
; INPUTS:
;	h - input header
;	keyword1 - first keyword to copy
;	keyword2 - last keyword to copy
;
; INPUT/OUTPUT:
;	hout - header to copy the information to.
;
; METHOD:
;	the headers lines from keyword1 to keyword2 are copied to
;	the end of the output header.  No check is made to verify
;	that a keyword value already exists in the output header.
;
; HISTORY:
;	version 1  D. Lindler    Sept. 1989
;	Converted to IDL V5.0   W. Landsman   September 1997
;-
;--------------------------------------------------------------------------
;
; make keywords 8 characters long (upper case)
;
	key1 = strmid(strupcase(keyword1+'        '),0,8)
	key2 = strmid(strupcase(keyword2+'        '),0,8)
;
; get header lengths
;
	n = n_elements(h)
	nout = n_elements(hout)
;
; find position of first keyword in h
;
	i1 = 0

	while i1 lt n do begin
		key = strmid(h[i1],0,8)
		if key1 eq key then goto,found1
		if key eq 'END     ' then begin
			print,'SXHCOPY -- keyword '+key1+' not found in header.'
			print,'           Nothing copied to output header.'
			return
		endif
		i1 = i1+1
	endwhile
found1:
;
; find position of second keyword
;
	i2 = i1
	while i2 lt n do begin
		key = strmid(h[i2],0,8)
		if key eq 'END     ' then begin
			i2 = i2-1		;do not copy 'END     '
			goto,found2
		endif
		if key2 eq key then goto,found2
		i2 = i2+1
	endwhile
found2:
;
; find end of output header
;
	i = 0
	while i lt nout do begin
		if strmid(hout[i],0,8) eq 'END     ' then goto,found
		i = i+1
	endwhile
	message,'No END keyword found in output header'
found:
;
; create new output header
;
	if i gt 0 then hout=[hout[0:i-1],h[i1:i2],hout[i]] $
		  else hout=[h[i1:i2],hout[i]]
return
end

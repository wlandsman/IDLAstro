function dbtitle,c,f
;+
; NAME:
;	DBTITLE
; PURPOSE:
;	function to create title line for routine dbprint
;
; CALLING SEQUENCE:
;	result = dbtitle( c, f )
;
; INPUTS:
;	c = string array of titles for each item
;	f = field length of each item
;
; OUTPUT:
;	header string returned as function value
;
; OPERATIONAL NOTES:
;	this is a subroutine of DBPRINT.
;
; HISTORY:
;	version 1  D. Lindler  Sept 86
;	Converted to IDL V5.0   W. Landsman   September 1997
;-
;------------------------------------------------------------
n=n_elements(c)
h=' '
com = strtrim(c,0)              ;header for item with trailing blanks removed
ncom = strlen(com)
for i=0,n-1 do begin		;loop on items
	flen=f[i]		;field length
	st=string(replicate(byte(32),flen+1));blank field
	ipos=((flen-ncom[i]+1)/2)>1	;starting position in field for comment
	strput,st,com[i],ipos	;insert into field
	h=h+st			;add to header
end; loop on items
return,h			;return header
end

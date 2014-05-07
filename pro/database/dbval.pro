function dbval,entry,item
;+
; NAME:
;	DBVAL
; PURPOSE:
;	procedure to extract value(s) of the specified item from
;	a data base file entry.
;
; CALLING SEQUENCE:
;	result = dbval( entry, item )
;
; INPUTS:
;	entry - byte array containing the entry, or a scalar entry number
;	item - name (string) or number (integer) of the item
;
; OUTPUT:
;	the value(s) will be returned as the function value
;
; EXAMPLE:
;	Extract a flux vector from entry 28 of the database FARUV
;	==> flux = dbval(28,'FLUX')
;
; HISTORY:
;   version 2  D. Lindler Nov, 1987	(new db format)
;	Converted to IDL V5.0   W. Landsman   September 1997
;-
;-------------------------------------------------------------------
;
; get item info
;
db_item,item,itnum,ival,idltype,sbyte,numvals,nbytes
;
; check to see if entry is a valid array
;
s=size(entry)
if s[0] gt 0 then begin		;array supplied
	if(s[0] ne 1) then begin	;is entry a 1-d array
		print,'entry must be a 1-d byte array, dbval aborting'
		retall
	endif
	if(s[2] ne 1) then begin	;check if byte array
		print,'entry must be a byte array, dbval aborting'
		retall
	endif
	return,dbxval(entry,idltype[0],numvals[0],sbyte[0],nbytes[0])
end else begin			;scalar supplied (assume entry number)
	dbrd,entry,e		;read entry
	return,dbxval(e,idltype[0],numvals[0],sbyte[0],nbytes[0]);return value(s)
end
end

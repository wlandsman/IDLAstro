pro dbxput,val,entry,idltype,sbyte,nbytes
;+
; NAME:
;	DBXPUT
; PURPOSE:
;	routine to replace value of an item in a data base entry
;
; CALLING SEQUENCE:	
;	dbxput, val, entry, idltype, sbyte, nbytes
;
; INPUT:
;	val - value(s) to be placed into entry, string values might be
;		truncated to fit number of allowed bytes in item
;	entry - entry or entries to be updated
;	idltype - idl data type for item (1-7)
;	sbyte - starting byte in record
;	nbytes - total number of bytes in value added
;
; OUTPUT:
;	entry - (updated)
;
; OPERATIONAL NOTES:
;	This routine assumes that the calling procedure or user knows what he 
;	or she is doing.  String items are truncated or padded to the fixed 
;	size specified by the database but otherwise no validity checks are 
;	made.
;
; HISTORY:
;	version 1, D. Lindler   Aug, 1986
;	converted to IDL Version 2.  M. Greason, STX, June 1990.
;	Work with multiple element string items   W. Landsman  August 1995
;	Really work with multiple element string items   
;			R. Bergman/W. Landsman  July 1996
;	Work with multiple entries, R. Schwartz, GSFC/SDAC August 1996
;	Use /overwrite with REFORM() W. Landsman May 1997
;	Converted to IDL V5.0   W. Landsman   September 1997
;-
;-------------------------------------------------------
;
nentry = n_elements(entry[0,*])
case idltype of		;case of data type

   7: begin			;string
	numvals = N_elements(val)                   ;Number of input values
	nbyte = nbytes/numvals                      ;Number of bytes/value
	val = strmid(val,0,nbyte)                   ;Truncate string
	temp = replicate( 32b, nbyte, numvals, nentry)	    ;Array of blanks
	for i = 0, numvals-1 do temp[0,i,0] = byte(val[i,*])     ;Fill with values
	entry[sbyte:sbyte+nbytes-1,*] = reform(temp,nbytes,nentry, /over)  
      end
   1: entry[sbyte:sbyte+nbytes-1,*]=val
   else: entry[sbyte:sbyte+nbytes-1,*] = byte(val,0,nbytes,nentry)

endcase
return
end

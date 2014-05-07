pro dbclose,dummy
;+
; NAME:
;       DBCLOSE
; PURPOSE:
;       procedure to close a data base file
;
; CALLING SEQUENCE:  
;       dbclose
;
; INPUTS:
;       None
;
; OUTPUTS
;       None
;
; SIDE EFFECTS:
;       the data base files currently opened are closed
;
; PROCEDURE CALLS:
;       DB_INFO()
; HISTORY:
;       version 2  D. Lindler  Oct. 1987
;       For IDL version 2      August 1990
;       William Thompson, GSFC/CDS (ARC), 30 May 1994
;                Added support for external (IEEE) data format
;       Remove call to HOST_TO_IEEE   W. Landsman June 2013
;-
;------------------------------------------------------------------------
 On_error,2
 common db_com, QDB, QITEMS, QDBREC         ;Database common - see DBOPEN

 if N_elements(qdb) LT 120 then return	;No db opened
 ndb = db_info('NUMBER')		;number of data bases opened
 update = db_info('UPDATE',0)		;opened for update?

; If database open for update, write total number of entries in zeroeth record

 if update EQ 1 then begin		;update header
	output = [db_info('entries',0), db_info('seqnum',0)]
	if qdb[119] eq 1 then $
	     swap_endian_inplace, output, /Swap_if_little ;External format?
        qdbrec[0] = byte(output,0,8)
 endif

 for i = 0, ndb-1 do begin		;loop on units (2 per data base)
        unit1 = qdb[96,i]			;unit numbers
        unit2 = qdb[97,i]			;unit numbers
	if unit1 gt 0 then free_lun,unit1       ;Is it opened?
	if unit2 gt 0 then free_lun,unit2       ;Is it opened?
 endfor

 qdb=0					;mark as closed

 return                                                              
 end

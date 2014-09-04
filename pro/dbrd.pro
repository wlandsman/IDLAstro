pro dbrd,enum,entry,available,dbno, noconvert=noconvert
;+
; NAME:
;	DBRD
; PURPOSE:
;	procedure to read an entry from a data base file or from
;	linked multiple databases.
;
; CALLING SEQUENCE:
;	dbrd, enum, entry, [available, dbno, /NoConvert]
;
; INPUTS:
;	enum - entry number to read, integer scalar
;
; OUTPUT:
;	entry - byte array containing the entry
;
; OPTIONAL OUTPUT:
;	available - byte array with length equal to number of data
;		bases opened.  available(i) eq 1 if an entry (pointed
;		to) is available.  It always equals 1 for the first 
;		data base, otherwise it is an error condition.
;
; OPTIONAL  INPUT:
;	dbno - specification of the data base number to return.  If
;		supplied, only the record for the requested data base
;		number is returned in entry.  Normally this input should
;		not be supplied.  dbno is numbered for 0 to n-1 and gives
;		the number of the data base opened.  The data bases are 
;		numbered in the order supplied to dbopen.  If dbno is supplied 
;		then the entry number refers to that data base and not the
;		primary or first data base. If set to -1, then it means all
;		data bases opened (same as not supplying it)
; OPTIONAL INPUT KEYWORD:
;	noconvert - if set then don't convert external to host format.
;		Assumes that calling program will take care of this
;		requirement.
; OPERATIONAL NOTES:
;	If multiple data base files are opened, the records are
;	concatenated with each other
; HISTORY
;	version 2  D. Lindler  Nov. 1987
;	William Thompson, GSFC/CDS (ARC), 1 June 1994
;		Added support for external (IEEE) representation.
;	Version 3, Richard Schwartz, GSFC/SDAC, 23-Aug-1996
;			Add noconvert keyword
;
;	Converted to IDL V5.0   W. Landsman   September 1997
;       Version 4, 2 May 2003, W. Thompson
;               Use BSWAP keyword to DBXVAL instead of calling IEEE_TO_HOST.
;-
;
;-----------------------------------------------------------------------
On_error,2

 if N_params() LT 2 then begin
     print,'Syntax - dbrd, enum, entry, [available, dbno, /NoConvert]'
     return
 endif

 COMMON db_com,qdb,qitems,qdbrec

; Find out if databases are in external format.
 externali= db_info('EXTERNAL')
 external = externali * (1-keyword_set(noconvert))
 if N_params() LT 4 then dbno = -1

 if dbno GE 0 then begin		;get only requeseted data base entry
	available = bytarr(1)+1b
    if dbno EQ 0 then begin
	entry = qdbrec[enum]
	if external[0] then db_ent2host, entry, 0
      end else begin
	len = db_info( 'LENGTH', dbno)
	unit = db_info( 'UNIT_DBF', dbno)
	p = assoc(unit,bytarr(len, /NOZERO), enum)
	entry = p[0]		;read entry
	if external[dbno] then db_ent2host, entry, dbno
    end
    return
 end

; get info on open data bases

 len = db_info( 'LENGTH' )	;record lengths
 units = db_info( 'UNIT_DBF' ) 	;unit numbers
 n = N_elements(len)		;number of db's opened
 entry = qdbrec[enum]		;read entry for first db
 if external[0] then db_ent2host, entry, 0
 irec = enum			;record number
 available = bytarr(n)+1B		;entry available

 if n GT 1 then begin
	for i = 1,n-1 do begin	;loop on db's
		pointer = db_info('pointer',i)		;what points to it
		db_item, pointer,itnum,ival,dtype,sb,nv,nb
		
		;Make sure irec is in internal format!
		if externali[db_item_info('dbnumber',itnum[0])] and keyword_set(noconvert) $
			 then bswap=1 else bswap=0
		irec = dbxval(entry,dtype[0],1,sb[0],nb[0],bswap=bswap)
		if irec GT 0 then begin
			p = assoc( units[i], bytarr( len[i],/NOZERO ))
			tmp = p[irec]
			if external[i] then db_ent2host, tmp, i
			entry = [ entry, tmp ]	;add to end
		   end else begin
			available[i] = 0B
			entry = [ entry, bytarr(len[i])]
		end
	end
 end

 return
 end

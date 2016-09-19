pro dbwrt,entry,index,append,noconvert=noconvert
;+
; NAME:
;	DBWRT
; PURPOSE:
;	procedure to update or add a new entry to a data base
;
; CALLING SEQUENCE:
;	dbwrt, entry, [ index, append, /NoConvert ]
;
; INPUTS:
;	entry - entry record to be updated or added if first
;		item (entry number=0)
;
; OPTIONAL INPUTS:
;	index - optional integer flag,  if set to non zero then index
;		file is  updated. (default=0, do not update index file)
;		(Updating the index file is time-consuming, and should
;		normally be done after all changes have been made.
;	append - optional integer flag, if set to non-zero the record
;		is appended as a new entry, regardless of what the
;		entry number in the record is.  The entry number will
;		be reset to the next entry number in the file.
; OUTPUTS:
;	data base file is updated.                    
;	If index is non-zero then the index file is updated.
; OPTIONAL INPUT KEYWORD:
;	NoConvert - If set then don't convert to host format with an external
;		database.    Useful when the calling program decides that
;		conversion isn't needed (i.e. on a big-endian machine), or 
;		takes care of the conversion itself.
; OPERATIONAL NOTES:
;	!PRIV must be greater than 1 to execute
; HISTORY:
;	version 2  D. Lindler  Feb. 1988 (new db format)
;	converted to IDL Version 2.  M. Greason, STX, June 1990.
;	William Thompson, GSFC/CDS (ARC), 28 May 1994
;		Added support for external (IEEE) representation.
;	Faster handling of byte swapping  W. L.  August 2010
;	Updates for indexing on the fly, D.Lindler, June, 2016
;-
;-------------------------------------------------------------------
 COMMON db_com,qdb,qitems,qdbrec

 if N_params() LT 2 then index=0
 if N_params() LT 3 then append=0

; Byte swapping is needed if database is in external format, and user is on 
; a little endian machine, and /noconvert is not st 

 bswap = (qdb[119] eq 1) && ~keyword_set(noconvert) && ~is_ieee_big()

 
; get some info on the data base

 update = db_info( 'UPDATE' )   
 if update EQ 0 then message,'Database opened for read only'

 len = db_info( 'LENGTH', 0 )	;record length
 qnentry = db_info( 'ENTRIES', 0 )

; determine if entry is correct size

 s = size(entry)
 if s[0] NE 1 then message,'Entry must be a 1-dimensional array'

 if s[1] NE len then $
	message,'Entry not the proper length of '+strtrim(len,2)+' bytes'

 if s[2] NE 1 then $
        message,'Entry vector (first parameter) must be a byte array'

; get entry number

 enum = append ? 0 : dbxval(entry,3,1,0,4)
 if ( enum GT qnentry ) || ( enum LT 0 ) then $
    message,'Invalid entry number of '+strtrim(enum,2)+' (first value in entry)'

 if enum EQ 0 then begin		;add new entry
	qnentry = qnentry+1
	qdb[84] = byte(qnentry,0,4)
	enum = qnentry
	dbxput,long(enum),entry,3,0,4
        newentry = 1b
 endif else newentry =0b
 if bswap then begin
      tmp = entry 
      db_ent2ext, tmp
      qdbrec[enum]=tmp
  endif else qdbrec[enum] =  entry
 
; update index file if necessary

 if index EQ 0 then return
 nitems = db_info( 'ITEMS', 0 )                    ;Total number of items
 indextype = db_item_info( 'INDEX', indgen(nitems))  ;Which ones are indexed?
 indexed = where(indextype,nindex)
 if nindex LE 0 then return            ;If no indexed items, then we are done
 indextype = indextype[indexed]        ;Now contains only indexed items
 unit = db_info( 'UNIT_DBX', 0 )
 reclong = assoc(unit,lonarr(2),0)
 h = reclong[0]
 maxentries = h[1]
 if bswap then swap_endian_inplace, maxentries
 if newentry then $
   if (maxentries LT qnentry) then begin   ;Enough room for new indexed items?
     print,'DBWRT -- maxentries too small'
     print,'Rerun DBCREATE with maxentries in .dbd file at least ',qnentry
     return
 endif

 reclong = assoc(unit,lonarr(7,nindex),8)
 header = reclong[0]
 if bswap then swap_endian_inplace,header
 hitem = header[0,*]            ;indexed item number
 hblock = header[3,*]
 sblock = header[4,*]  & sblock = sblock[*]
 iblock = header[5,*]  & iblock = iblock[*]
 ublock = header[6,*]  & ublock = ublock[*]
 db_item, indexed, itnum, ivalnum, idltype, startbyte, numvals, nbytes
 pos = where(hitem EQ itnum ) 
 for i = 0, nindex-1 do begin
     v = dbxval( entry, idltype[i], numvals[i], startbyte[i], nbytes[i] )
     sbyte = nbytes[i] * (enum-1)  
     isort = (indextype[i] EQ 3) || (indextype[i] EQ 4)

     datarec = dbindex_blk(unit, sblock[pos[i]], 512, sbyte, idltype[i])
     reclong = assoc(unit,lonarr(1),(iblock[pos[i]]*512L))
     if idltype[i] eq 7 then v = byte(v)                    ; convert string to a byte array
     if n_elements(v) eq 1 then v = reform(v,1,/overwrite)  ; make scale into an array
     case indextype[i] of

	1:  datarec[0] = bswap ? swap_endian(v) : v
	    

	2:  begin
	      datarec[0] = bswap ? swap_endian(v) : v
	      if (qnentry mod 512) EQ 0 then begin        ;Update
	      nb = qnentry/512
              hbyte = nbytes[i] * nb
              datarec = dbindex_blk(unit,hblock[pos[i]],512,hbyte,idltype[i])
	      datarec[0] = bswap ? swap_endian(v) : v
              endif
      end
	3: begin                          ;SORT

	   datarec = dbindex_blk(unit,sblock[pos[i]],512,0,idltype[i])
	   values = datarec[0:(qnentry-1)]                  ;Read in old values
	   if bswap then swap_endian_inplace, values
	   reclong = dbindex_blk(unit,iblock[pos[i]],512,0,3)
	   sub = reclong[0:(qnentry-1)]                     ;Read in old indices
	   if bswap then swap_endian_inplace, sub
	   if enum lt qnentry then begin       		;Change an old value?
	       sort_index = where(sub EQ enum)          ;Which value to change
	       sort_index = sort_index[0]
	       if values[sort_index] EQ v $      ;Value remains the same so
                   then isort =0  $          ;don't bother sorting again
	        else values[sort_index] = v            ;Update with new value
	   endif else values = [values,v]            ;Append a new value
	   end

	4: begin                          ;SORT/INDEX

	   values = datarec[qnentry-1,ublock*512]    ;Update index record
	   if bswap then swap_endian_inplace, values
	   if enum lt qnentry then begin
	        if values[enum-1] EQ v then isort = 0 else values[enum-1] = v 
 	   endif else  values = [values,v]
	   datarec = dbindex_blk(unit,ublock[pos[i]],512,sbyte,idltype[i])
	   datarec[0] = bswap ? swap_endian(v) : v
	   end

	else:

	endcase

 if isort then begin                  ;resort values?
	sub = bsort(values)
	values = values[sub]
	nb = (qnentry + 511)/512
	ind = indgen(nb)*512L
	sval = values[ind]
;
	datarec = dbindex_blk(unit, hblock[pos[i]], 512, 0, idltype[i])
	datarec[0] = bswap ? swap_endian(sval) : sval
;
	datarec = dbindex_blk(unit, sblock[pos[i]], 512, 0, idltype[i])
	datarec[0] = bswap ?swap_endian(values) : values
;
	reclong = dbindex_blk(unit, iblock[pos[i]], 512, 0, 3)
	reclong[0] = bswap ?swap_endian(sub+1) : sub+1
 endif

 endfor

 return
 end

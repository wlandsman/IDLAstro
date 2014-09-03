pro dbext_dbf,list,dbno,sbyte,nbytes,idltype,nval,v1,v2,v3,v4,v5,v6, $
        v7,v8,v9,v10,v11,v12,v13,v14,v15,v16,v17,v18, item_dbno=item_dbno
	
;+
; NAME:
;       DBEXT_DBF
; PURPOSE:
;       Subroutine of DBEXT to extract values of up to 18 items from a database 
; EXPLANATION:
;       This is a subroutine of DBEXT, which is the routine a user should 
;       normally use.
;
; CALLING SEQUENCE:
;       dbext_dbf,list,dbno,sbyte,nbytes,idltype,nval,v1,[ v2,v3,v4,v5,v6,v7,
;                  v8,v9,v10,v11,v12,v13,v14,v15,v16,v17,v18 ITEM_DBNO = ]
;
; INPUTS:
;       list - list of entry numbers to extract desired items.   It is the 
;               entry numbers in the primary data base unless dbno is greater 
;               than or equal to -1.  In that case it is the entry number in 
;               the specified data base.
;       dbno - number of the opened db file
;               if set to -1 then all data bases are included
;       sbyte - starting byte in the entry.  If single data base then it must 
;               be the starting byte for that data base only and not the 
;               concatenation of db records 
;       nbytes - number of bytes in the entry
;       idltype - idl data type of each item to be extracted
;       nval - number of values per entry of each item to be extracted
;
; OUTPUTS:
;       v1...v18 - the vectors of values for up to 18 items
;
; OPTIONAL INPUT KEYWORD:
;       item_dbno - A vector of the individual database numbers for each item.
;               Simplifies the code for linked databases
; PROCEDURE CALLS:
;       DB_INFO(), DB_ITEM_INFO(), DBRD, DBXVAL(), IS_IEEE_BIG(), IEEE_TO_HOST
; HISTORY
;       version 1  D. Lindler  Nov. 1987
;       Extract multiple valued entries    W. Landsman   May 1989
;       William Thompson, GSFC/CDS (ARC), 1 June 1994
;               Added support for external (IEEE) representation.
;       Work with multiple element string items  W. Landsman  August 1995
;       Increase speed for external databases on IEEE machines WBL August 1996
;       IEEE conversion implemented on blocks of entries using BIG
;       Added keyword ITEM_DBNO     R. Schwartz, GSFC/SDAC, August 1996
;       Return a vector even if only 1 value W. Thompson  October 1996
;       Change variable name of BYTESWAP to BSWAP  W. Thompson Mar 1997
;       Use /OVERWRITE with reform   W. Landsman   May 1997
;       Increase maximum number of items to 18  W. Landsman  November 1999
;       2 May 2003, W. Thompson, Use DBXVAL with BSWAP instead of IEEE_TO_HOST.
;       Avoid EXECUTE() for V6.1 or later  W. Landsman Jan 2007 
;       Assume since V6.1  W. Landsman June 2009
;       Change arrays to LONG to support entries >32767 bytes WL Oct 2010
;-
;
 compile_opt idl2
;*****************************************************************
;
COMMON db_com,qdb,qitems,qdbrec
nitems=n_elements(sbyte)                                ;number of items
external = db_info('external')                          ;External format?
bswap = external * (~IS_IEEE_BIG() )              ;Need to byteswap?
if dbno ge 0 then bswap = bswap[dbno] + bytarr(nitems) else $
        if n_elements(item_dbno) eq nitems then bswap=bswap[item_dbno] $
        else begin
        sbyte1 = db_item_info('bytepos')
        itnums = intarr(nitems)
        for i=0,nitems-1 do itnums[i] = (where( sbyte[i] eq sbyte1))[0]
        dbno1  = db_item_info('dbnumber', itnums)
        bswap  = bswap[dbno1]
endelse
        
scalar=0
if n_elements(list) eq 1 then begin
        scalar=1
        savelist=list
        list=lonarr(1)+list
        if list[0] eq -1 then list=lindgen(db_info('entries',0))+1
end
nlist=n_elements(list)
;
; create a big array to hold all extracted values in
; byte format
;
totbytes=total(nbytes)
big=bytarr(totbytes,nlist)
;
; generate vector of bytes in entries to extract
;
index=lonarr(totbytes)
ipos=0
for i=0,nitems-1 do begin
     for j=0,nbytes[i]-1 do index[ipos+j]=sbyte[i]+j
     ipos=ipos+nbytes[i]
endfor
;
; generate vector of byte positions in big for each item
;
bpos=lonarr(nitems)
if nitems gt 1 then for i=1,nitems-1 do bpos[i]=bpos[i-1]+nbytes[i-1]
;
; loop on records and extract info into big
;
if dbno ge 0 then begin
        ;
        ; bypass dbrd for increased performance
        ;
        if dbno eq 0 then begin
                for i=0L,nlist-1 do begin
                    if list[i] ge 0 then begin
                        entry=qdbrec[list[i]]
                        big[0,i] = entry[index]
                    endif
                endfor
            end else begin      ;mapped I/O
                unit=db_info('unit_dbf',dbno)
                rec_size=db_info('length',dbno)
                for i=0L,nlist-1 do begin
                    if list[i] ge 0 then begin
                        p=assoc(unit,bytarr(rec_size,/nozero),rec_size*list[i])
                        entry=p[0]
                        big[0,i] = entry[index]
                    end
                endfor
        end
   end else begin
        for i = 0L, nlist-1 do begin
           if list[i] GE 0 then begin
                dbrd,list[i],entry, /noconvert
                big[0,i] = entry[index]
            endif
        end
end
;
; now extract each value and convert to correct type
;
last = bpos + nbytes -1

for i = 0,nitems-1 do begin
    item = dbxval(big, idltype[i], nval[i], bpos[i], nbytes[i], bswap=bswap[i])
    st = 'v' + strtrim(i+1,2)
    if nlist GT 1 then $
       (SCOPE_VARFETCH(st)) = reform(item,/overwrite) else $
       (SCOPE_VARFETCH(st)) = [item]

  endfor;for i loop on items
;
if scalar then list=savelist    ;restore scalar value
return
end

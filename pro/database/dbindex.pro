pro dbindex,items
;+                      
; NAME:
;       DBINDEX
; PURPOSE:
;       Procedure to create index file for data base 
;
; CALLING SEQUENCE:     
;       dbindex, [ items ]
;
; OPTIONAL INPUT:
;       items - names or numbers of items to be index -- if not supplied,
;               then all indexed fields will be processed.  
;
; OUTPUT:
;       Index file <name>.dbx is created on disk location ZDBASE:
;
; OPERATIONAL NOTES:
;       (1) Data base must have been previously opened for update
;       by DBOPEN 
;
;       (2) Only 18 items can be indexed at one time.   If the database has
;       more than 18 items, then two separate calls to DBINDEX are needed.
; PROCEDURES CALLED:
;       DBINDEX_BLK, DB_INFO(), DB_ITEM, DB_ITEM_INFO(), IEEE_TO_HOST, 
;       IS_IEEE_BIG()
; HISTORY:
;       version 2  D. Lindler  Nov 1987 (new db format)
;       W. Landsman    added optional items parameter Feb 1989 
;       William Thompson, GSFC/CDS (ARC), 30 May 1994
;               Added support for external (IEEE) data format
;       Test if machine is bigendian  W. Landsman     May, 1996
;       Change variable name of BYTESWAP to BSWAP  W. Thompson  Mar, 1997
;       Increased number of fields to 15   W. Landsman   June, 1997
;       Increase number of items to 18     W. Landsman  November 1999
;       Allow multiple valued (nonstring) index items W. Landsman November 2000
;       Use 64 bit integers for V5.2 or later  W. Landsman February 2001
;       Do not use EXECUTE() for V6.1 or later, improve efficiency 
;                W. Landsman   December 2006
;       Automatically enlarge .dbx file if needed, fix major bug in last
;             update    W. Landsman Dec 2006
;       Assume since V6.1    W. Landsman   June 2009
;       Allow sorted string items   W. Landsman   October 2009
;-                                         
;*****************************************************************
 On_error,2                ;Return to caller
 compile_opt idl2

; Check to see if data base is opened for update

 if db_info('UPDATE') EQ 0 then message, $
        'Database must be opened for update'

; Extract index items from data base

 if N_params() EQ 1 then db_item,items,itnum else begin 
      nitems = db_info('ITEMS',0)
      itnum = indgen(nitems)
 endelse

 indextype = db_item_info('INDEX',itnum)
 indexed = where(indextype, Nindex)                 ;Select only indexed items
 if Nindex LE 0 then begin
        message,'Database has no indexed items',/INF
        return
 endif else if Nindex GT 18 then begin
        message,'ERROR - Only 18 items can be indexed at one time',/INF
        return
 endif

 indextype = indextype[indexed]
 if N_params() EQ 1 then indexed = itnum[indexed]

; get info on indexed items

 nbytes = db_item_info('NBYTES',indexed)         ;Number of bytes
 idltype = db_item_info('IDLTYPE',indexed)       ;IDL type
 sbyte = db_item_info('SBYTE',indexed)           ;Starting byte
 nval = db_item_info('NVALUES',indexed)          ;Number of values per entry

; get db info

 nentries = db_info('ENTRIES',0)
 if nentries EQ 0 then begin
  message, 'ERROR - database contains no entries',/INF
  return
 endif
 unit = db_info('UNIT_DBX',0)                      ;unit number of index file
 external = db_info('EXTERNAL',0)                  ;external format?
 bswap = external ? not IS_IEEE_BIG() : 0

; read header info of index file (mapped file)

 reclong = assoc(unit,lonarr(2),0)
 h = reclong[0]  ;first two longwords
 if bswap then ieee_to_host,h
 maxentries = h[1]      ;max allowed entries
; If necessary, enlarge the size of the .dbx file.    All indexed items must
; then be reindexed.
 if maxentries lt nentries then begin
        message,'Enlarging index (.dbx) file to support ' +  $
	         strtrim(nentries,2) + ' entries',/INF
	dbname = db_info('name',0)	 
        dbcreate,dbname,1,maxentry=nentries,external=db_info('external')
	dbopen, dbname, 1
        nitems = db_info('ITEMS',0)
        itnum = indgen(nitems)   
 endif
 
 nindex2 = h[0] ;number of indexed items
 if nindex2 LT nindex then goto, NOGOOD   
 reclong = assoc(unit,lonarr(7,nindex2),8)
 header = reclong[0]            ;index header
 if bswap then ieee_to_host,header
 hitem = header[0,*]            ;indexed item numbers
 hindex = header[1,*]           ;index type
 htype = header[2,*]            ;idl data type
 hblock = header[3,*]           ;starting block of header
 sblock = header[4,*]           ;starting block of data values
 iblock = header[5,*]           ;starting block of indices (type=3)
 ublock = header[6,*]           ;starting block of unsorted data (type=4)

; extract index items...maximum of 18 indexed fields.

 list = lindgen(nentries)+1l
 dbext_dbf,list,0,sbyte,nbytes*nval,idltype,nval, $
               v1,v2,v3,v4,v5,v6,v7,v8,v9,v10,v11,v12,v13,v14,v15,v16,v17,v18

 for i = 0,nindex-1 do begin
        ;
        ; place item in variable v
        ;
        v = (scope_varfetch('v' + strtrim(i+1,2))) 
        pos = where(hitem EQ indexed[i], N_found)
        if N_found LE 0 then goto, NOGOOD    
        pos = pos[0]
        if hindex[pos] NE indextype[i] then goto, NOGOOD  
        if ( idltype[i] EQ 7 ) then v = byte(v)
;
; process according to index type ---------------------------------------
;
        reclong = assoc(unit,lonarr(1),(iblock[pos]*512LL))
        case indextype[i] of
 
        1: begin                                ;indexed (unsorted)

                datarec = dbindex_blk(unit, sblock[pos], 512, 0, idltype[i])
   		datarec[0] =  bswap ? swap_endian(v,/swap_if_little) : v
           end
; 
        2: begin                                ;values are already sorted

                nb=(nentries+511L)/512          ;number of 512 value blocks
                ind=indgen(nb)*512LL             ;position at start of each block
                sval=v[ind]                     ;value at start of each block
;
                datarec = dbindex_blk(unit, hblock[pos], 512, 0, idltype[i])
                datarec[0] = bswap ? swap_endian(sval,/swap_if_little) : sval
 ;
                datarec = dbindex_blk(unit, sblock[pos], 512, 0, idltype[i])
   		datarec[0] =  bswap ? swap_endian(v,/swap_if_little) : v
           end
 
        3: begin                                ; sort item before storage
                
                if idltype[i] EQ 7 then begin 
		    svv = string(v)
		    sub= bsort(svv) 
		    v = byte(svv[sub])
		endif     else begin 
		   sub=bsort(v)                    ;sort values
                   v=v[sub]
                endelse
		nb=(nentries+511)/512           ;number of 512 value blocks
                ind=l64indgen(nb)*512LL             ;position at start of each block
                if idltype[i] EQ 7 then sval=v[*,ind] else sval = v[ind] 
		                    ;value at start of each block
                datarec = dbindex_blk(unit, hblock[pos], 512, 0, idltype[i])
 		datarec[0] = bswap ? swap_endian(sval,/swap_if_little) : sval
;
                datarec = dbindex_blk(unit, sblock[pos], 512, 0, idltype[i])
  		datarec[0] =  bswap ? swap_endian(v,/swap_if_little) : v
                reclong[0] = bswap ? swap_endian(sub+1,/swap_if_little) : sub+1                ;indices
           end
        4: begin                                ; sort item before storage
                
                datarec = dbindex_blk(unit, ublock[pos], 512, 0, idltype[i])
 		datarec[0] =  bswap ? swap_endian(v,/swap_if_little) : v
                if idltype[i] EQ 7 then begin 
		    svv = string(v)
		    sub= bsort(svv) 
		    v = byte(svv[sub])
		endif     else begin 
		   sub=bsort(v)                    ;sort values
                   v=v[sub]
                endelse
   
   
                  nb=(nentries+511)/512           ;number of 512 value blocks
                ind=l64indgen(nb)*512LL             ;position at start of each block
                if idltype[i] EQ 7 then sval=v[*,ind] else sval = v[ind] 
		                    ;value at start of each block
                datarec = dbindex_blk(unit, hblock[pos], 512, 0, idltype[i])
                datarec[0] = bswap ? swap_endian(sval,/swap_if_little) : sval
 ;
 		datarec = dbindex_blk(unit, sblock[pos], 512, 0, idltype[i])
		datarec[0] =  bswap ? swap_endian(v,/swap_if_little) : v
;
                 reclong[0] = bswap ?swap_endian(sub+1,/swap_if_little) : sub+1                ;indices
	   end
        endcase
endfor
return
NOGOOD:    
        print,'DBINDEX-- Inconsistency in .dbh and .dbx file'
        print,'Run dbcreate to create a new index file'
        return
end

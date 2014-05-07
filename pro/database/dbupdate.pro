pro dbupdate,list,items,v1,v2,v3,v4,v5,v6,v7,v8,v9,v10,v11,v12,v13,v14, $
             NOINDEX = noindex
;+
; NAME:
;	DBUPDATE
; PURPOSE:
;	Update columns of data in a database  -- inverse of DBEXT
; EXPLANATION:
;	Database must be open for update before calling DBUPDATE
;
; CALLING SEQUENCE:
;	dbupdate, list, items, v1, [ v2, v3, v4......v14 ]
;
; INPUTS:
;	list - entries in database to be updated, scalar or vector
;		If list=-1 then all entries will be updated
;	items -standard list of items that will be updated.  
;	v1,v2....v14 - vectors containing values for specified items.  The
;		number of vectors supplied must equal the number of items
;		specified.   The number of elements in each vector should be
;		the same.
;
; OPTIONAL KEYWORD INPUT:
;       /NOINDEX - If set, then DBUPDATE will not update the index file.   This
;               keyword is useful to save if additional updates will occur,
;               and the index file need only be updated on the last call.
;            
; EXAMPLES:
;	A database STAR contains RA and DEC in radians, convert to degrees
;
;	IDL> !PRIV=2 & dbopen,'STAR',1          ;Open database for update
;	IDL> dbext,-1,'RA,DEC',ra,dec          ;Extract RA and DEC, all entries 
;	IDL> ra = ra*!RADEG & dec=dec*!RADEG    ;Convert to degrees
;	IDL> dbupdate,-1,'RA,DEC',ra,dec        ;Update database with new values
;
; NOTES:
;	It is quicker to update several items simultaneously rather than use
;	repeated calls to DBUPDATE.  
; 
;	It is possible to update multiple valued items.  In this case, the
;	input vector should be of dimension (NVAL,NLIST) where NVAL is the
;	number of values per item, and NLIST is the number of entries to be
;	updated.  This vector will be temporarily transposed by DBUPDATE but
;	will be restored before DBUPDATE exits.
;
; REVISION HISTORY
;	Written W. Landsman      STX       March, 1989
;	Work for multiple valued items     May, 1991
;	String arrays no longer need to be fixed length      December 1992
;	Transpose multiple array items back on output        December 1993
;	Faster update of external databases on big endian machines November 1997
;	Converted to IDL V5.0   W. Landsman 24-Nov-1997
;       Added /NOINDEX keyword  W. Landsman  July 2001
;-
 On_error,2                             ;Return to caller

 if N_params() LT 3 then begin
    print,'Syntax - dbupdate, list, items, v1, [ v2, v3, v4, v5,...v14 ]'
    return
 endif
                                      ;Get number of entries to update
 nlist = N_elements(list)
 if nlist EQ 0 then message, $
      'ERROR - no entry values supplied'

 nentries = db_info( 'ENTRIES' )      ;Number of entries in database
 external = db_info( 'EXTERNAL', 0 )
 if external then noconvert = is_ieee_big() else noconvert = 1b

 if list[0] LT 0  then begin           ;If LIST = -1, then update all entries
       nlist = nentries[0]
       list = lindgen(nlist) + 1
 endif 

 db_item, items, itnum, ivalnum, idltype, sbyte, numvals, nbyte
 nitem = N_elements(itnum)            ;Number of items in database
 if N_params() LT nitem+2 then $
    message,'ERROR - ' + strtrim(nitem,2) + ' items specified, but only ' + $
             strtrim(N_params()-2,2) + ' input variables supplied'

;  Make sure user supplied enough values for all desired entries

 for i = 0,nitem-1 do begin

    ii = strtrim(i+1,2)
    test = execute('good = N_elements(v' + ii +') EQ nlist*numvals[i]')
    if good NE 1 then $
        message,'Supplied values for item ' + $
           strtrim(db_item_info('name',itnum[i]),2) + ' must contain '+ $
                              strtrim(nlist*numvals[i],2)+' elements'  

    test = execute('s=size(v' + ii +')' )
    if s[s[0] + 1] NE idltype[i] then $
         message,'Item ' + strtrim(db_item_info('name',itnum[i]),2)+ $
           ' has an incorrect data type'

    if numvals[i] GT 1 then begin
         test = execute('v'+ ii + '= transpose(v'+ ii + ')' )
    endif

 endfor

 nitems = (nitem GT indgen(14) )
 nbyte = nbyte*numvals

 for i = 0l,nlist-1 do begin

   dbrd,list[i],entry,noconvert=noconvert
   dbxput,v1[i,*],entry,idltype[0],sbyte[0],nbyte[0]
     if nitems[1] then begin
        dbxput,v2[i,*],entry,idltype[1],sbyte[1],nbyte[1]
     if nitems[2] then begin 
        dbxput,v3[i,*],entry,idltype[2],sbyte[2],nbyte[2]
     if nitems[3] then begin 
        dbxput,v4[i,*],entry,idltype[3],sbyte[3],nbyte[3]
     if nitems[4] then begin 
        dbxput,v5[i,*],entry,idltype[4],sbyte[4],nbyte[4]
     if nitems[5] then begin 
        dbxput,v6[i,*],entry,idltype[5],sbyte[5],nbyte[5]
     if nitems[6] then begin 
        dbxput,v7[i,*],entry,idltype[6],sbyte[6],nbyte[6]
     if nitems[7] then begin 
        dbxput,v8[i,*],entry,idltype[7],sbyte[7],nbyte[7]
     if nitems[8] then begin 
        dbxput,v9[i,*],entry,idltype[8],sbyte[8],nbyte[8]
     if nitems[9] then begin 
        dbxput,v10[i,*],entry,idltype[9],sbyte[9],nbyte[9]
     if nitems[10] then begin 
        dbxput,v11[i,*],entry,idltype[10],sbyte[10],nbyte[10]
     if nitems[11] then begin 
        dbxput,v12[i,*],entry,idltype[11],sbyte[11],nbyte[11]
     if nitems[12] then begin 
        dbxput,v13[i,*],entry,idltype[12],sbyte[12],nbyte[12]
     if nitems[13] then $
        dbxput,v14[i,*],entry,idltype[13],sbyte[13],nbyte[13]
   endif & endif & endif & endif & endif & endif & endif & endif & endif
   endif & endif & endif 
   dbwrt,entry, noconvert = noconvert

 endfor

; Transpose back any multiple value items

 for i = 0,nitem-1 do begin           
    if numvals[i] GT 1 then begin
	ii = strtrim(i+1,2)
        test = execute('v'+ ii + '= transpose(v'+ ii + ')' )
    endif
 endfor

;   Check if the indexed file needs to be updated

 if keyword_set(NOINDEX) then return

 indextype = db_item_info( 'INDEX', itnum)
 index = where( indextype, nindex)                  ;Indexed items
 if nindex GT 0 then begin
     message, 'Now updating indexed file', /INFORM     
     dbindex, itnum[index]
 endif

 return
 end

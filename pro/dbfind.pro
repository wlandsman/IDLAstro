function dbfind,spar,listin,SILENT=silent,fullstring = Fullstring,      $
        errmsg=errmsg, Count = count
;+
; NAME: 
;    DBFIND()
; PURPOSE:      
;     Search data base for entries with specified characteristics
; EXPLANATION:  
;     Function to search data base for entries with specified
;     search characteristics.
;
; CALLING SEQUENCE:     
;     result = dbfind(spar,[ listin, /SILENT, /FULLSTRING, ERRMSG=, Count = ])
;
; INPUTS:       
;     spar - search_parameters (string)...each search parameter 
;               is of the form:
;
;               option 1) min_val < item_name < max_val
;               option 2) item_name = value
;               option 3) item_name = [value_1, value_10]
;                       Note: option 3 is also the slowest.
;               option 4) item_name > value
;               option 5) item_name < value
;               option 6) item_name = value(tolerance) ;eg. temp=25.0(5.2)
;               option 7) item_name                     ;must be non-zero
;
;               Multiple search parameters are separated by a comma.
;               eg.     'cam_no=2,14<ra<20'
;
;               Note: < is interpreted as less than or equal.
;                     > is interpreted as greater than or equal.
;       
;               RA and DEC keyfields are stored as floating point numbers 
;               in the data base may be entered as HH:MM:SEC and
;               DEG:MIN:SEC. Where:
;
;                       HH:MM:SEC   equals  HH + MM/60.0  + SEC/3600.
;                       DEG:MIN:SEC equals DEG + MIN/60.0 + SEC/3600.
;                       
;               For example:
;                       40:34:10.5 < dec < 43:25:19 , 8:22:1.0 < ra < 8:23:23.0
;
;               Specially encoded date/time in the data base may
;               be entered by  CCYY/DAY:hr:min:sec which is
;               interpreted as  
;                       CCYY*1000+DAY+hr/24.0+min/24.0/60.+sec/24.0/3600.
;               If a two digit year is supplied and YY GE 40 then it is 
;               understood to refer to year 1900 +YY;  if YY LT 40 then it is 
;               understood to refer to year 2000 +YY

;               For example
;                       1985/201:10:35:30<date_time<1985/302:10:33:33.4
;               would specify all entries between:
;                       year 1985 day 201 at 10:35:30 to
;                       day 302 at 10:33:33.4
;               The date/time may also be encoded as:
;                       DD-MMM-YEAR HH::MM:SS.SS        
;                       eg.  12-JUL-86 10:23:33.45
;               (this is the format of system variable !stime)
;
;               Multiple search parameters may be stored in a string
;               array (one parameter per array element) instead of
;               concatenating them with commas in a single string.
;               Example:
;                       input_array = strarr(2)
;                       input_array[0] = '14<ra<16'   ; 14-16 hrs of ra.
;                       input_array[1] = '8<dec<20'   ; + 8-20 deg. decl.
;
; OPTIONAL INPUT:       
;       listin - gives list of entries to be searched.  If not supplied or 
;               set to -1 then all entries are searched.
;
; OUTPUT:       
;       List of ENTRY numbers satisfying search characteristics
;               is returned as the function value.
;
; OPTIONAL INPUT KEYWORDS:      
;       /SILENT  - If the keyword SILENT is set and non-zero, then DBFIND
;               will not print the number of entries found.
;
;       /FULLSTRING - By default, one has a match if a search string is 
;               included in any part of a database value (substring match).   
;               But if /FULLSTRING is set, then all characters in the database
;               value must match the search string (excluding leading and 
;               trailing blanks).    Both types of string searches are case
;               insensitive.
;
;       ERRMSG   = If defined and passed, then any error messages will
;                  be returned to the user in this parameter rather
;                  than depending on the MESSAGE routine in IDL.  If no
;                  errors are encountered, then a null string is
;                  returned.  In order to use this feature, ERRMSG must
;                  be defined first, e.g.
;
;                       ERRMSG = ''
;                       DB_ITEM, ERRMSG=ERRMSG, ...
;                       IF ERRMSG NE '' THEN ...;
;
; OPTIONAL OUTPUT KEYWORD:
;       COUNT - Integer scalar giving the number of valid matches
; PROCEDURE CALLS:
;       DB_INFO, DB_ITEM, DB_ITEM_INFO, DBEXT, DBEXT_IND, DBFIND_ENTRY,
;       DBFIND_SORT, DBFPARSE, DBRD, DBSEARCH, ZPARCHECK,IS_IEEE_BIG
;
; RESTRICTIONS: 
;       The data base must be previously opened with DBOPEN.
;
; SIDE EFFECTS: 
;       The obsolete system variable !ERR is set to number of entries found
;
; REVISION HISTORY:
;       Written     :   D. Lindler, GSFC/HRS, November 1987
;       Version 2, Wayne Landsman, GSFC/UIT (STX), 1 April 1994
;                       Added FULLSTRING keyword.
;       Version 3, William Thompson, GSFC, 1 April 1994
;                       Added check for empty database
;       Version 4, William Thompson, GSFC, 5 April 1994
;                       Changed so that !ERR is zero when database is empty,
;                       and LISTIN is returned, based on discussion with Wayne
;                       Landsman.
;       Version 5, Wayne Landsman, GSFC/UIT (STX), 26 May 1994
;                       Added error message when database is empty.
;       Version 6, William Thompson, GSFC, 14 March 1995
;                       Added FULLSTRING keyword to DBFIND_SORT call
;       Version 7, Richard Schwartz, GSFC/SDAC 23 August 1996
;                       Move external to host conversion from DBRD to
;                       operation on extracted values only.
;       Version 8, William Thompson, GSFC, 3 December 1996
;                       Renamed BYTESWAP variable to BSWAP--appeared to be
;                       conflicting with function of same name.
;       Version 9, William Thompson, GSFC, 17-Mar-1997
;                       Added keyword ERRMSG
;       Version 10, July, 1997  W. Landsman, added CATCH errors
;       Converted to IDL V5.0   W. Landsman   October 1997
;       Update documentation for new Y2K compliant DBFPARSE W. Landsman Nov 1998
;       Suppress empty database message with /SILENT, W. Landsman Jan 1999
;       Added COUNT keyword, deprecate !ERR        W. Landsman March 2000
;       Added new unsigned & 64bit datatypes       W. Landsman July 2001
;       Fix possible floating illegand operand error W. Landsman July 2009
;       Change arrays to LONG to support entries >32767 bytes W.L. Oct. 2010
;       Delay warning now for 10000 instead of 2000 entries W.L. Aug 2014
;-
;
; ---------------------------------------------------------------------

On_error,2                          ;return to caller
;
; Check parameters.  If LISTIN supplied, make sure all entry values are
; less than total number of entries.
;
 count = 0
 zparcheck,'dbfind',spar,1,7,[0,1],'search parameters'

 catch, error_status
 if error_status NE 0 then begin 
        print,!ERR_STRING
        if N_elements(listin) NE 0 then return,listin else return, -1
 endif
 nentries = db_info( 'ENTRIES',0 )              ;number of entries
 if ( N_params() LT 2 ) then listin = -1  else begin
      zparcheck,'dbfind',listin,2,[1,2,3],[0,1],'entry list'
      maxlist = max(listin)
      if ( maxlist GT nentries ) then begin
         message = 'Entry list values (second parameter) must be less than '+ $
                strtrim(nentries,2)
         goto, handle_error
      endif
 endelse
 if nentries eq 0 then begin                    ;Return if database is empty
        !err = 0 
        if not keyword_set(SILENT) then message, $
            'ERROR - No entries in database ' + db_info("NAME",0),/INF
        return,listin
 endif
;
; parse search parameter string
;
 dbfparse,spar,items,stype,search_values
 nitems = N_elements(items)             ;number of items
;
; set up initial search list
;
list  = listin
s=size(list) & ndim=s[0]
if ndim EQ 0 then list=lonarr(1)+list
;
; get some item info
;
db_item,items,it,ivalnum,idltype,sbyte,numvals,nbytes,errmsg=errmsg
IF N_ELEMENTS(ERRMSG) NE 0 THEN IF ERRMSG NE '' THEN BEGIN
        MESSAGE = ERRMSG
        GOTO, HANDLE_ERROR
ENDIF
index = db_item_info('INDEX',it)                        ;index type
dbno = db_item_info('DBNUMBER',it)                      ;data base number
                                                        ; particular db.
;
; get info on the need to byteswap item by item
;
external = db_info('external')                          ;External format?
bswap = external * (not IS_IEEE_BIG() )              ;Need to byteswap?
dbno1  = db_item_info('dbnumber', it)
bswap  = bswap[dbno1]

done=bytarr(nitems)                                     ;flag for completed
                                                        ; items
;----------------------------------------------------------------------
; ENTRY number is a search parameter?
;
for pos = 0,nitems-1 do begin
    if (it[pos] eq 0) then begin
        dbfind_entry,stype[pos],search_values[pos,*],nentries,list,count=count
        done[pos]=1                           ;flag as done
        if count LT 1 then goto, FINI            ;any found
     end
end     
;----------------------------------------------------------------------
;
; perform search on sorted items in the first db
;

for pos=0,nitems-1 do begin
     if(not done[pos]) and (dbno[pos] eq 0) and $
        (index[pos] ge 2) then begin
                dbfind_sort,it[pos],stype[pos],search_values[pos,*],list, $
                        fullstring=fullstring, Count = count
                if !err ne -2 then begin
                        if count lt 1 then goto,FINI 
                        done[pos]=1
                end
     end
end
; ------------------------------------------------------------------------
; Perform search on items in lookup file (indexed items) in first db
;
if total(done) eq nitems then goto,FINI
for pos=0,nitems-1 do begin
    if(not done[pos]) and (dbno[pos] eq 0) and (index[pos] ne 0) then begin
            dbext_ind,list,it[pos],0,values
            dbsearch, stype[pos], search_values[pos,*], values, good, $
                Fullstring = fullstring, Count = count
            if !err eq -2 then begin 
                print,'DBFIND - Illegal search value for item ', $
                       db_item_info('name',it[pos])
                       return,listin
            endif
            if count lt 1 then goto, FINI        ;any found
            if list[0] ne -1 then list=list[good] else list=good+1
            done[pos]=1                         ; DONE with that item
    end
end

;------------------------------------------------------------------------
;
; search index items in other opened data bases (if any)
;
found=where( (index gt 0) and (dbno ne 0 ), Nfound)
if Nfound gt 0 then begin
      db = dbno[ where(dbno NE 0) ]
      for i = 0, n_elements(db)-1 do begin
;
; find entry numbers of second database corresponding to entry numbers
; in the first data base.
;
        pointer=db_info('pointer',db[i])        ;item which points to it
;
        dbext,list,pointer,list2        ;extract entry numbers in 2nd db
        good=where(list2 ne 0,ngood)    ;is there a valid pointer
        if ngood lt 1 then goto, FINI 
        if list[0] eq -1 then list=good+1 else list=list[good]
        list2=list2[good]
        for pos=0,nitems-1 do begin
            if (not done[pos]) and (dbno[pos] eq db[i]) and (index[pos] ne 0) $
                              and (index[pos] ne 3) then begin
                    dbext_ind,list2,it[pos],dbno[pos],values
                    dbsearch, stype[pos], search_values[pos,*], values, good, $
                        fullstring = fullstring, count = count
                    if !err eq -2 then begin
                       message = 'Illegal search value for item ' + $
                               db_item_info('name',it[pos])
                       goto, handle_error
                    endif
                    if count lt 1 then goto, FINI        ;any found
                    if list[0] ne -1 then list=list[good] else list=good+1
                    list2=list2[good]
                    done[pos]=1                         ; DONE with that item
            endif
        endfor
     endfor
endif           
;---------------------------------------------------------------------------
; search remaining items
;

  if list[0] eq -1 then list= lindgen(nentries)+1       ;Fixed WBL Feb. 1989
  count = N_elements(list)
  !err = count
  if total(done) eq nitems then goto, FINI      ;all items searched

  nlist     = N_elements(list)        ;number of entries to search
  if nlist GT 10000 then begin
        print,'Non-indexed search on ',strtrim(nlist,2),' entries'
        print,'Expect Delay'
  end
;
; Create array to hold values of all remaining items...a big one.
;
  left = where( done EQ 0, N_left )           ;items left
  nbytes = nbytes[left]
  sbyte = sbyte[left]
  idltype = idltype[left]
  bswap = bswap[left]
  totbytes  = total(nbytes)           ;total number of bytes to extract
  big  = bytarr(totbytes,nlist)   ;array to store values of the items
;
; generate starting position in big for each item
;
  bpos  = lonarr(N_left)        ;starting byte in bpos of each item
  if N_left GT 1 then for i=1,N_left-1 do bpos[i] = bpos[i-1]+nbytes[i-1]

  index = lonarr(totbytes)      ;indices of bytes to extract
  ipos  = 0                     ;position in index array
  for i = 0,N_left-1 do begin   ;loop on items
    for j=0,nbytes[i]-1 do index[ipos+j]=sbyte[i]+j     ;position in entry
    ipos = ipos + nbytes[i]
  end;for

;
; loop on entries and extract info
;
  for ii = 0L, nlist-1L do begin
    dbrd,list[ii],entry, /noconvert                 ;read entry
    big[0,ii]= entry[index]
  endfor

;
; now extract values for each item and search for valid ones
;
  stillgood  = lindgen( nlist )

  for i = 0l,N_left-1 do begin
        if i Eq 0 then val = big[ bpos[i]:bpos[i]+nbytes[i]-1, 0:nlist-1 ] else $
        val = big[ bpos[i]:bpos[i]+nbytes[i]-1, stillgood ]
        if bswap[i] then ieee_to_host, val, idltype=idltype[i]
       case idltype[i] of
                1: v = byte(val,0,nlist)        ;byte
                2: v = fix(val,0,nlist)         ;i*2
                3: v = long(val,0,nlist)        ;i*4
                4: v = float(val,0,nlist)       ;r*4
                5: v = double(val,0,nlist)      ;r*8
                7: v = string(val)               ;string
                12: v = uint(val,0,nlist)         ;u*2
               13: v = ulong(val,0,nlist)        ;u*4
               14: v = long64(val,0,nlist)       ;i*8
               15: v = ulong64(val,0,nlist)      ;u*8
         endcase
        dbsearch, stype[left[i]], search_values[left[i],*], v, good, $
                Fullstring = fullstring, count = count
        if count LT 1 then goto, FINI 
        stillgood=stillgood[good]
	nlist = count
  endfor
  list = list[stillgood]
  count = N_elements(list) & !ERR = count

FINI:
if not keyword_set(SILENT) then begin
  print,' ' & print,' '
  if count LE 0  then $
        print,'No entries found by dbfind in '+ db_info('name',0) $
  else $
        print,count,' entries found in '+ db_info('name',0)
endif
if count LE 0 then return,intarr(1) else return,list[sort(list)]
;
;  Error handling point.
;
HANDLE_ERROR:
        IF N_ELEMENTS(ERRMSG) NE 0 THEN ERRMSG = 'DBFIND: ' + MESSAGE $
                ELSE MESSAGE, MESSAGE
end

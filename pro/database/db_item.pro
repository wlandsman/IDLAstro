pro db_item,items,itnum,ivalnum,idltype,sbyte,numvals,nbytes,errmsg=errmsg
;+
; NAME: 
;       DB_ITEM
; PURPOSE:      
;       Returns the item numbers and other info. for an item name.
; EXPLANATION:  
;       Procedure to return the item numbers and other information
;       of a specified item name
;
; CALLING SEQUENCE:     
;       db_item, items, itnum, ivalnum, idltype, sbyte, numvals, nbytes
;
; INPUTS:       
;       items - item name or number
;               form 1  scalar string giving item(s) as list of names
;                       separated by commas
;               form 2  string array giving list of item names
;               form 3  string of form '$filename' giving name
;                       of text file containing items (one item per
;                       line)
;               form 4  integer scalar giving single item number or
;                         integer vector list of item numbers
;               form 5  Null string specifying interactive selection
;                       Upon return items will contain selected items
;                       in form 1
;               form 6  '*'     select all items
;
; OUTPUTS:      
;       itnum - item number
;       ivalnum - value(s) number from multiple valued item
;       idltype - data type(s) (1=string,2=byte,4=i*4,...)
;       sbyte - starting byte(s) in entry
;       numvals - number of data values for item(s)
;               It is the full length of a vector item unless
;               a subscript was supplied
;       nbytes - number of bytes for each value
;    All outputs are vectors even if a single item is requested
;
; OPTIONAL INPUT KEYWORDS:      
;       ERRMSG   = If defined and passed, then any error messages will
;               be returned to the user in this parameter rather than depending
;               on the MESSAGE routine in IDL.  If no errors are encountered, 
;               then a null string is returned.  In order to use this feature, 
;               ERRMSG must be defined first, e.g.
;
;                               ERRMSG = ''
;                               DB_ITEM, ERRMSG=ERRMSG, ...
;                               IF ERRMSG NE '' THEN ...
;
; PROCEDURE CALLS:
;       DB_INFO, GETTOK, SELECT_W
;
; REVISION HISTORY:
;       Written:   D. Lindler, GSFC/HRS, October 1987
;       Version 2, William Thompson, GSFC, 17-Mar-1997
;                       Added keyword ERRMSG
;       Use STRSPLIT instead of GETTOK to parse form 1, W. Landsman July 2002
;       Assume since V5.4 use FILE_EXPAND_PATH() instead of SPEC_DIR()
;               W. Landsman April 2006
;       Support new DB format allowing entry lengths > 32767 bytes WL Oct 2010
;       Ignore blank lines in .items file WL February 2011
;-
;
;------------------------------------------------------------------------
 compile_opt idl2
 On_error,2
 if N_params() LT 2 then begin
    print,'Syntax - DB_ITEM,items,itnum,ivalnum,idltype,sbyte,numvals,nbytes'
    return
 endif 
; data base common block
;
common db_com,QDB,QITEMS,QLINK
;
; QDB[*,i] contains the following for each data base opened
;
;       bytes
;         0-18   data base name character*19
;         19-79  data base title character*61
;         80-81  number of items (integer*2)
;         82-83  record length of DBF file (integer*2) old DB format
;         84-87  number of entries in file (integer*4)
;         88-89  position of first item for this file in QITEMS (I*2)
;         90-91  position of last item for this file (I*2)
;         92-95  Last Sequence number used (item=SEQNUM) (I*4)
;         96     Unit number of .DBF file
;         97     Unit number of .dbx file (0 if none exists)
;         98-99  Index number of item pointing to this file (0 for first db)
;         100-103 Number of entries with space allocated
;         104    Update flag (0 open for read only, 1 open for update)
;         105-108  record length of DBF file (integer*4) 
;         118    Equals 1 if database can store records larger than 32767 bytes
;         119    Equals 1 if external data representation (IEEE) is used
;
;  QITEMS[*,i] contains a description of item number i with following
;  byte assignments:
;
;       0-19    item name (character*20)
;       20-21   IDL data type (integet*2)
;       22-23   Number of values for item (1 for scalar) (integer*2)
;       24-25   Starting byte position in original DBF record (integer*2)
;       26-27   Number of bytes per data value (integer*2)
;       28      Index type
;       29-97   Item description
;       98-99   Print field length
;       100     Flag set to one if pointer item
;       101-119 Data base this item points to
;       120-125 Print format
;       126-170 Print headers
;       171-172 Starting byte in record returned by DBRD, old DB format
;       173-174 Data base number in QDB
;       175-176 Data base number this item points to
;       177-178 Item number within the specific data base
;       179-182 Number of values for item (1 for scalar) (integer*4)
;       183-186 Starting byte position in original DBF record (integer*4)
;       187-190 Starting byte in record returned by DBRD
;
;
; QLINK[i] contains the entry number in the second data base
;       corresponding to entry i in the first data base.
;-------------------------------------------------------------------------
if n_elements(items) eq 0 then items = ''
;
; check if data base open
;
if n_elements(qdb) lt 120 then begin
        message = 'data base file not open'
        goto, handle_error
endif

;
; determine type of item list -------------------------------------------
;
vector=1                                        ;vector output flag
newdb = qdb[118,0] EQ 1
s=size(items,/str)
ndim = s.n_dimensions
if s.type_name eq 'STRING' then begin                     ;string(s)
        if ndim eq 0 then begin                         ;string scalar?
            if strtrim(items) eq '' then form=5 else $  ;null string   - form 5
            if strmid(items,0,1) eq '$' then form=3  $  ;filename      - form 3
                else form=1                             ;scalar list   - form 1
            if strtrim(items) eq '*' then form=6        ;all items '*' - form 6
         end else form=2                                ;string vector - form 2
   end else begin                                       ;non-string
        form=4                                          ;integer       - form 4
end
s=size(qitems)
if s[0] ne 2 then begin
        message = 'No data base opened'
        goto, handle_error
endif
qnumit=s[2]

;-----------------------------------------------------------------------------
;       CONVERT INPUT ITEMS TO INTEGER LIST OR STRING LIST
;
;
; Form 4 ------------------ Integer
;
If form eq 4 then begin
        if ndim eq 0 then begin
                itnum=intarr(1)+items
                ivalnum=intarr(1)
                ivalflag=intarr(1)
                goto,scalar                     ;speedy method
            end else begin
                itnum=items
                nitems=n_elements(itnum)
                ivalflag=bytarr(nitems)
                ivalnum=intarr(nitems)
                if (min(itnum) lt 0) or (max(itnum) ge qnumit) then begin
                        message = 'Invalid item number specified'
                        goto, handle_error
                endif
                goto,vector
        end
end

;
; Form 3 ----------------- File name
;
if form eq 3 then begin
        item_names=strarr(200)          ;input buffer
        if strlen(items) gt 1 then filename=strmid(items,1,strlen(items)-1) $
                               else filename=strtrim(db_info('name',0))+'.items'
        if ~file_test(filename) then begin
            message = 'Unable to locate file ' + FILE_EXPAND_PATH(filename) +  $
                    ' with item list'
            goto, handle_error
        endif
 	nlines = file_lines(filename)
        item_names = strarr(nlines)
        openr,unit,filename,/get_lun    ;open file
        readf,unit,item_names
	free_lun,unit
	item_names = strtrim(item_names,2) 
; Remove any blank lines 	
	good = where(strlen(item_names) GT 0, Nitems) 
	if Nitems LT Nlines then item_names = item_names[good]	
end
;
; form 1 ----------------- scalar string list  'item1,item2,item3...'
;
 if form eq 1 then begin
     item_names = strsplit(items,',',/EXTRACT) 
     nitems = N_elements(item_names)                     
 endif
;
; form 2 -------------------------- string array
;
if form eq 2 then begin
        item_names=items
        nitems = N_elements(items)
endif
;
; form 5 -------------------------- null string (interactive input)
;
if form eq 5 then begin
        names=strtrim(qitems[0:19,*],2)
        desc=string(qitems[29:78,*])
        select_w,names,itnum,desc,'Select List of Items',count=count
        if count le 0 then begin
                message = 'No items selected'
                goto, handle_error
        endif
;
        nitems=n_elements(itnum)
        items = strtrim(names[itnum[0]],2)
        if nitems gt 1 then for i=1,nitems-1 do $
                  items = items +','+strtrim(names[itnum[i]],2)
        ivalflag=bytarr(nitems)
        ivalnum=intarr(nitems)   
        goto,vector
end
;
; Form 4 ------------------ '*'  select all items
;
If form eq 6 then begin
        nitems=db_info('items')         ;number of items
        itnum=indgen(nitems)
        ivalflag=bytarr(nitems)
        ivalnum=intarr(nitems)
        goto,vector
end
;
;-------------------------------------------------------------------------
;   CONVERT STRING LIST TO INTEGER LIST AND PULL OFF SUBSCRIPT IF SUPPLIED
;
;
        names=strtrim(qitems[0:19,*],2) ;all possible item names
        ivalnum=intarr(nitems)          ;selection of multi-value items
        ivalflag=bytarr(nitems)         ;Flag for subscripted items
        itnum=intarr(nitems)            ;integer item numbers
;
; loop on item names supplied
;
        for i=0,nitems-1 do begin       ;loop on items
            st=strtrim(item_names[i],2)         ;get item
            name=gettok(st,'(')         ;get name
;
;     subscript supplied
;
            if st ne '' then begin      ;number supplied?
                ivalnum[i]=fix(gettok(st,')'))  ;get number
                ivalflag[i]=1
            end;
;
;     data base name supplied
;
            if strpos(name,'.') ge 0 then begin ;data base name supplied
                dbname=gettok(name,'.')         ;  form is 'dbname.itemname'
                i1=db_info('item1',dbname)      ;first item for the db
                i2=db_info('item2',dbname)      ;last item for the db
             end else begin                     ;search all items
                i1=0 & i2=qnumit-1
            end
;
;    search for item name
;
            name=strupcase(name)                ;convert to upper case
            j = where(names[i1:i2] eq name,nmatch)
            if nmatch eq 0 then begin
                    message = 'Item '+ name +' is invalid'
                    goto, handle_error
            endif
itnum[i] =j[0] +i1                              ;save item number
endfor;i loop on items
if nitems eq 1 then goto,scalar                 ;speedy method

;
;---------------------------------------------------------------------------
;  We now have
;       1) integer list of item numbers of length nitems
;       2) we have list of ivalnum (subscripts) with
;               flag(s) ivalflag if subscript supplied
; EXTRACT OTHER PARAMETERS
;

vector:                                         ;---- vector processing
 idltype = fix(qitems[20:21,*],0,qnumit)
 numvals = newdb ? long(qitems[179:182,*],0,qnumit) : $
                  fix(qitems[22:23,*],0,qnumit)
 sbyte =  newdb ? long(qitems[187:190,*],0,qnumit) : $
                  fix(qitems[171:172,*],0,qnumit)
 nbytes = fix(qitems[26:27,*],0,qnumit)
 idltype = idltype[itnum]
 numvals = numvals[itnum]
 sbyte = sbyte[itnum]
 nbytes = nbytes[itnum]
;
; add offset for subscripted variables
;
sbyte=sbyte+ivalnum*nbytes
;
; if ivalflag is set we have subscripted item and don't want all
;  values in vector
;
pos=where(ivalflag, Npos)
if Npos GT 0 then numvals[pos]=1
return
;
; -----------------------
scalar:                                         ;------- scalar processing
it=itnum[0]
if (it lt 0) or (it ge qnumit) then begin
        message = 'Invalid item number '+strtrim(it,2)+' specified'
        goto, handle_error
endif
;
idltype = fix(qitems[20:21,it],0,1)
numvals = newdb ? long(qitems[179:182,it],0,1) : $
                  fix(qitems[22:23,it],0,1)
sbyte = newdb ? long(qitems[187:190,it],0,1) : $
             fix(qitems[171:172,it],0,1)
nbytes = fix(qitems[26:27,it],0,1)
sbyte = sbyte+nbytes*ivalnum
if ivalflag[0] then numvals[0]=1
return
;
;  Error handling point.
;
HANDLE_ERROR:
        IF N_ELEMENTS(ERRMSG) NE 0 THEN ERRMSG = 'DB_ITEM: ' + MESSAGE $
                ELSE MESSAGE, MESSAGE
end

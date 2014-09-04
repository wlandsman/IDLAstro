pro dbedit_basic,list,items
;+
; NAME:
;       DBEDIT_BASIC
; PURPOSE:
;       Subroutine of DBEDIT_BASIC to edit a database on a dumb terminal.
; EXPLANATION:
;       Interactively edit specified fields in a database.  The
;       value of each field is displayed, and the user has the option
;       of changing or keeping the value.
;
; CALLING SEQUENCE:
;       dbedit_basic, list, [ items ]
;
; INPUTS:
;       list - scalar or vector of database entry numbers.  Set LIST=0
;               to interactively add a new entry to a database.
;
; OPTIONAL INPUTS
;       items - list of items to be edited.  If not supplied, then the
;               value of every field will be displayed.
;
; NOTES:
;       (1) Database must be opened for update (dbopen,<dbname>,1) before
;       calling DBEDIT_BASIC.  User must have write privileges on the database
;       files.
;       (2) User gets a second chance to look at edited values, before
;       they are actually written to the database
;
; PROMPTS:
;       The item values for each entry to be edited are first displayed
;       User is the asked "EDIT VALUES IN THIS ENTRY (Y(es), N(o), or Q(uit))?
;       If user answers 'Y' or hits RETURN, then each item is displayed
;       with its current value, which the user can update.  If user answered
;       'N' then DBEDIT_BASIC skips to the next  entry.   If user answers 'Q'
;       then DBEDIT will exit, saving all previous changes.
;
; EXAMPLE:
;       Suppose V magnitudes (V_MAG) in a database STARS with unknown values 
;       were assigned a value of 99.9.  Once the true values become known, the
;       database can be edited
;
;       IDL> !PRIV=2 & dbopen,'STARS',1         ;Open database for update
;       IDL> list =  dbfind('V_MAG=99.9')       ;Get list of bad V_MAG values
;       IDL> dbedit,list,'V_MAG'       ;Interactively insert good V_MAG values
;
; REVISION HISTORY:
;       Written  W. Landsman     STX        April, 1989
;       Rename DBEDIT_BASIC from DBEDIT            July, 1993
;       Converted to IDL V5.0   W. Landsman   September 1997
;       Change DATATYPE() to size(/TNAME)  W. Landsman   November 2001
;-
 On_error,2

 zparcheck, 'DBEDIT_BASIC', list, 1, [1,2,3], [0,1], 'Database entry numbers'

 dbname = db_info( 'NAME', 0 )                ;Database name
 if not db_info( 'UPDATE' ) then $
     message, 'Database ' + dbname + ' must be opened for update

 if ( N_params() LT 2 ) then begin         ;Did user specify items string?
     nitems = db_info( 'ITEMS', 0 ) -1     ;If not then use every item but ENTRY
     items = indgen(nitems) + 1
 endif 

 nlist = N_elements(list)

 if ( list[0] EQ -1 ) then begin            ;Edit all entries?
    nlist = db_info( 'ENTRIES', 0 )         ;Get number of entries
    list = lindgen(nlist) + 1
 endif

 db_item, items, itnum, ivalnum, dtype, sbyte, numvals, nbytes

 nitems = N_elements(itnum)                ;Number of items to be edited
 names = db_item_info( 'NAME', itnum )     ;Get names of each item
 newflag = bytarr(nlist,nitems)        ;Keeps track of fields actually updated
 yesno = ''

for i = 0, nlist-1 do begin            ;Loop over each entry to be edited
    ll = list[i]

    if ll GT 0 then begin             ;Existing entry?
      dbprint,ll,'*',TEXT = 1
      read,'Edit values in this entry (Y(es),N(o),Q(uit), def=Y)? ',yesno
      yesno = strupcase(strmid(yesno,0,1))
      if yesno eq 'Q' then goto, UPDATE $
        else if yesno EQ 'N' then goto, ENTRY_DONE   
    endif else message,'Adding new entry to database '+dbname,/inform

    print,'Hit [RETURN] to leave values unaltered'
    READVAL:  dbrd,ll,entry
    for j = 0,nitems - 1 do begin
        val = ''
        name = strtrim(names[j],2)
        curval = dbxval( entry, dtype[j], numvals[j], sbyte[j], nbytes[j] )
;       Convert byte to integer to avoid string conversion problems
        if (dtype[j] EQ 1) and ( N_elements(curval) EQ 1 ) then $ 
            curval = fix(curval)       
        if ( numvals[j] EQ 1 ) then oldval = strtrim(curval,2) else $
                                oldval = strtrim(curval[0],2) + '...'
        read,name+' New Value (' + oldval + '): ',val
        TESTVAL: 
           if ( val NE '' ) then begin
           oldval = make_array( size = [1,numvals[j],dtype[j],numvals[j]] )
           On_IOerror, BADVAL 
           oldval[0] = val
           On_IOerror, NULL 
           newflag[i,j] = 1
           dbxput, oldval, entry, dtype[j], sbyte[j], nbytes[j]
        endif    
    endfor

    if ( total(newflag[i,*]) GT 0 ) then begin
    print,'' & print,'Updated Values' & print,''

    for j = 0,nitems-1 do begin
         name = strtrim(names[j],2)
         print,name,': ',dbxval( entry,dtype[j],numvals[j],sbyte[j],nbytes[j] )
    endfor
         print,''
         yesno = ''
         read,' Are these values correct [Y]? ', yesno
         if ( strupcase(yesno) NE 'N' ) then begin
            if ( ll EQ 0 ) then begin 
                dbwrt,entry,0,1 
                ll = db_info('entries',0) + 1
            endif else dbwrt,entry
            print,'' & print,'Entry ',strtrim(ll,2), ' now updated   
         endif else begin 
            newflag[i,*] = 0
            goto, READVAL
         endelse
    endif else print,'No values updated for entry',ll
    ENTRY_DONE:     
endfor

UPDATE: 
 newitem = total(newflag, 1)
 indexnum = where(newitem, nindex)

 if ( nindex GT 0 ) then begin                          ;Any mods made?
      indexnum = itnum[indexnum]
      indextype = db_item_info('INDEX',indexnum)  ;Index type of modified fields 
      good = where(indextype GE 1, ngood)         ;Which fields are indexed?
      if ngood GT 0 then dbindex,indexnum[good]
      dbopen,dbname,1
      dbprint,list,[0,itnum],TEXT=1
 endif
 return
BADVAL:  
  print,'Item '+name+ ' must be of type '+ size(oldval[0],/TNAME)
         val = ''
         j = j-1
         goto, TESTVAL      

 end

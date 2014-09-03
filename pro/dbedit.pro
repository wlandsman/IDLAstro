;+
; NAME:
;      DBEDIT
;
; PURPOSE:
;       Interactively edit specified fields in an IDL database. 
; EXPLANATION:
;       The value of each field is displayed, and the user has the option
;       of changing or keeping the value.  Widgets will be used if they
;       are available.
;
; CALLING SEQUENCE:
;       dbedit, list, [ items ]
;
; INPUTS:
;       list - scalar or vector of database entry numbers.  Set list = 0 to 
;       interactively add a new entry to a database.  Set list = -1 to edit 
;       all entries.
;
; OPTIONAL INPUTS:
;       items - list of items to be edited.  If omitted, all fields can be 
;               edited.      
;
; KEYWORDS:
;       BYTENUM = If set, treat byte variables as numbers instead of
;                 characters.
;
; COMMON BLOCKS:
;       DB_COM -- contains information about the opened database.
;       DBW_C -- contains information intrinsic to this program.
;
; SIDE EFFECTS:
;       Will update the database files.
;
; RESTRICTIIONS:
;       Database must be opened for update prior to running
;       this program.  User must be running DBEDIT from an 
;       account that has write privileges to the databases.  
;
;       If one is editing an indexed item, then after all edits are complete,
;       DBINDEX will be called to reindex the entire item.    This may
;       be time consuming.
;
;       Cannot be used to edit items with multiple values
;
; EXAMPLE:
;       Suppose one had new parallaxes for all stars fainter than 5th magnitude
;       in the Yale Bright Star Catalog and wanted to update the PRLAX and
;       PRLAX_CODE fields with these new numbers
;
;       IDL> !priv=2                    
;       IDL> dbopen, 'yale_bs', 1            ;Open catalog for update
;       IDL> list = dbfind( 'v>5')     ;Find fainter than 5th magnitude
;       IDL> dbedit, list, 'prlax, prlax_code'   ;Manual entry of new values
;
; PROCEDURE:
;       (1) Use the cursor and point to the value you want to edit.   
;       (2) Type the new field value over the old field value.
;       (3) When you are done changing all of the field values for each entry
;       save the entry to the databases by pressing 'SAVE ENTRY TO DATABASES'.
;       Here all of the values will be checked to see if they are the correct
;       data type.  If a field value is not of the correct data type, it will
;       not be saved.  
;
;       Use the buttons "PREV ENTRY" and "NEXT ENTRY" to move between entry 
;       numbers.  You must save each entry before going on to another entry in 
;       order for your changes to be saved.
;
;       Pressing "RESET THIS ENTRY" will remove any unsaved changes to the 
;       current entry.
;
;REVISION HISTORY:
;       Adapted from Landsman's DBEDIT
;       added widgets,  Melissa Marsh, HSTX, August 1993
;       do not need to press return after entering each entry,
;                       fixed layout problem on SUN,
;                       Melissa Marsh, HSTX, January 1994
;       Only updates the fields which are changed. Joel Offenberg, HSTX, Mar 94
;       Corrected test for changed fields  Wayne Landsman  HSTX, Mar 94
;       Removed a couple of redundant statements W. Landsman HSTX Jan 96
;       Converted to IDL V5.0   W. Landsman   September 1997
;       Replace DATAYPE() with size(/TNAME)   W. Landsman   November 2001
;       Work for entry numbers > 32767     W. Landsman   December 2001
;       Added /BYTENUM  William Thompson        13-Mar-2006
;       Use DIALOG_MESSAGE for error messages  W. Landsman  April 2006
;       Assume since V5.5, remove VMS support  W. Landsman  Sep 2006
;-

;----------------------------------------------------------------


;event handler for main part of program

pro widgetedit_event,event

common db_com,qdb,QITEMS,QDBREC

common dbw_c,liston,main,holder,widlabel,widtext,middle,nitems,names,$
        it,itnum,dtype,numvals,sbyte,nbytes,buts,prevbut,but2,resetbut,$
        endbut,nextbut,mid,minlist,maxlist,savebut,bigmid,entry,wid_warn,$
        holder0,widtext0,widlabel0,thislist,nlist,wereat,newflag,bytenum

CASE event.id OF

    endbut: widget_control,event.top,/destroy ;destory main widget--end session

    prevbut:begin       ;go to previous entry
        if wereat ne 0 then wereat= wereat-1
        liston = thislist[wereat]
        widedit
    end

    nextbut:begin       ;go to next entry
        if wereat lt nlist-1 then wereat = wereat+1 else $
              widget_control,event.top,/destroy          ;end session
        liston = thislist[wereat]
        widedit
    end

    resetbut:begin      ;reset this entry
        liston = liston
        widedit 
    end

    savebut: begin      ;save entry to databases
          ;update database
        for i = 0, nitems -1 do begin
          widget_control,widtext[i],get_value=val
          ;test value
          valid = 0
           oldval = dbxval(entry,dtype[i],numvals[i],sbyte[i],nbytes[i])

          on_ioerror,BADVAL
          IF (strtrim(oldval[0],2) ne (strtrim(val[0],2))) THEN BEGIN
              oldval[0] = strtrim(val,2)
              valid = 1
              dbxput,oldval,entry,dtype[i],sbyte[i],nbytes[i]
              print,strcompress('Entry ' + string(liston) +':  '  + $
              names[i] + ' = ' + string(val))
              newflag[ wereat, i ] = 1b
    BADVAL:     if (not valid) then begin
                result = dialog_message(title='Bad Value',/ERROR, $
                   'Item '+ strcompress(names[i],/rem) + $ 
                        ' must be of type ' + size(oldval[0],/TNAME) )
                str = dbxval(entry,dtype[i],numvals[i],sbyte[i],nbytes[i])
                if (dtype[i] eq 1) and keyword_set(bytenum) then str=fix(str)
                str = '    '+string(str[0])
                widget_control,widtext[i],set_value=str         
                endif
          endIF 
          on_ioerror,NULL
        endfor
        
        if (liston EQ 0) then begin
                 dbwrt,entry,0,1        ;new entry
        endif else begin
                 dbwrt,entry
        endelse
        widedit
        ;create widget telling the user that the changes have been made.
    end

    else: ;donothing
   
     endcase
end

;--------------------------------------------------------------------
pro widedit
;program that makes "middle" of main widget (field values)


common db_com,qdb,QITEMS,QDBREC
                           

common dbw_c,liston,main,holder,widlabel,widtext,middle,nitems,names,$
        it,itnum,dtype,numvals,sbyte,nbytes,buts,prevbut,but2,resetbut,$
        endbut,nextbut,mid,minlist,maxlist,savebut,bigmid,entry,wid_warn,$
        holder0,widtext0,widlabel0,thislist,nlist,wereat,newflag,bytenum


;get entry number
 dbrd, liston, entry

;get field values for this entry
 widget_control, widtext0, set_value=string(liston)
 for i = 0,nitems-1 do begin
        str = dbxval(entry,dtype[i],numvals[i],sbyte[i],nbytes[i])
        if (dtype[i] eq 1) and keyword_set(bytenum) then str=fix(str)
        str = '    '+string(str[0])
        widget_control,widtext[i],set_value=str
 endfor

;check to see if this entry is the minimum or maximum entry 
 if (liston EQ minlist) then widget_control,prevbut,sensitive=0 else $
                widget_control,prevbut,sensitive=1 
 if (liston EQ maxlist) then widget_control,nextbut,sensitive=0 else $
                widget_control,nextbut,sensitive=1

 end
;-------------------------------------------------------------------------
;main program

pro dbedit,list,items,bytenum=k_bytenum

 compile_opt idl2
common db_com,qdb,QITEMS,QDBREC

;Nitems - Number elements in input list
;Thislist - Sorted list of entry numbers
;Minlist - Minimum input entry number
;Maxlist - Maximum input entry number
;Liston - The current entry number being edited (scalar)
;wereat - The index of ThisList vector being edited, i.e. Thislist(wereat)=LIston
;dtype - data type(s) (1=string,2=byte,4=i*4,...)
;sbyte - starting byte(s) in entry
;numvals - number of data values for item(s)
;    NOTE: dtype, sbyte, numvals are dimensioned for *all* entries 

common dbw_c,liston,main,holder,widlabel,widtext,middle,nitems,names,$
        it,itnum,dtype,numvals,sbyte,nbytes,buts,prevbut,but2,resetbut,$
        endbut,nextbut,mid,minlist,maxlist,savebut,bigmid,entry,wid_warn,$
        holder0,widtext0,widlabel0,thislist,nlist,wereat,newflag,bytenum
                          
 On_error,2
 if N_params() LT 1 then begin
        print,'Syntax - dbedit, list, [ items ]'
        return
 endif
        
;Set the value of bytenum
bytenum = keyword_set(k_bytenum)

;make sure widgets are available
 if (!D.FLAGS AND 65536) EQ 0 then begin  
        dbedit_basic, list, items
        return
 endif

;check to make sure database is open
    ;first check to see if there is an open database
    s = size(qdb)
    if (s[0] EQ 0) then begin
    
           result = dialog_message(/ERROR, title='NOT OPEN FOR UPDATE', $
	        'No database has been opened')
            goto, PROEND  
    endif
;check to make sure the database is opened for update
    dbname = db_info('NAME',0)
    if not db_info('UPDATE') then begin

        result = dialog_message(/ERROR, title='NOT OPEN FOR UPDATE', $
	        'Database ' + dbname + ' must be opened for update.')
        goto,PROEND

    endif


    ;check parameters
    zparcheck, 'DBEDIT', list, 1, [1,2,3], [0,1], 'Database entry numbers'

    ;get items.  If items not specified use all items except ENTRY
    if ( N_params() LT 2 ) then begin       
        nitems = db_info('ITEMS',0) -1       
        items = indgen(nitems) + 1
    endif

    nlist = N_elements(list)

    if nlist gt 1 then begin ;sort entry numbers

        sar = sort(list)
        thislist = list[sar]

    endif else begin

        thislist = lonarr(1) 
        thislist[0] = list

    endelse

    ;edit all entries?  get number of entries
    if ( list[0] EQ -1 ) then begin          
        nlist = db_info('ENTRIES',0)           
        if nlist le 0 then begin
           print,'Empty database cannot be edited. Use list=0 to add new entry'
           goto, PROEND
        endif
        thislist = lindgen(nlist) + 1
    endif

    minlist = min(thislist, max = maxlist)


    nentry = db_info('ENTRIES',0)
    if (maxlist gt nentry) then begin
        result = dialog_message(title='INVALID ENTRY NUMBER',/ERROR, $
           dbname + ' entry numbers must be less than ' + strtrim(nentry+1,2) )
         goto, PROEND
    endif

    nitems = db_info('ITEMS',0) -1
    allitems = indgen(nitems) + 1

    ;get information about items
    db_item,allitems,itnum,ivalnum,dtype,sbyte,numvals,nbytes
    nvalues = db_item_info('nvalues')

    db_item,items,it

    nit = n_elements(it)                      ;Number of items to be edited
    names = db_item_info('name',itnum)        ;Get names of each item
    newflag = bytarr(nlist,nitems)  ;Keeps track of fields actually updated

    wereat = 0
    liston = thislist[wereat]
    dbrd,liston,entry

    ;create widget and display
    main = widget_base(/COLUMN,title='Widgetized Database Editor')
    w1 = widget_label(main,value='******  '  + dbname + '  ******')
    bigmid = widget_base(main,/column,x_scroll_size=325,y_scroll_size=650)


    butbase = widget_base(main,/column,/frame)
    savebut = widget_button(butbase,value='SAVE THIS ENTRY')
    buts = widget_base(butbase,/row)
    prevbut = widget_button(buts,value='<- PREV ENTRY')
    but2 = widget_base(buts,/column)
    resetbut = widget_button(but2,value='RESET THIS ENTRY')
    endbut = widget_button(but2,value='END SESSION')
    nextbut = widget_button(buts,value='NEXT ENTRY ->')

    widlabel = lonarr(nitems+1)
    widtext = lonarr(nitems+1)
    holder = lonarr(nitems+1)

    mid = widget_base(bigmid,/column)

    holder0 = widget_base(mid,/row)
    widlabel0 =widget_label(holder0,value='  ENTRY NUMBER  ',/frame)
    num = string(liston)
    widtext0 = widget_label(holder0,value=num)

    middle = widget_base(mid,/column)

    for i = 0,nitems-1 do begin
        ed = 'N'
        str1 = names[i]

        for j = 0, N_elements(it)-1 do begin
                if it[j] EQ itnum[i] then ed = 'Y'
        endfor

        str = dbxval(entry,dtype[i],numvals[i],sbyte[i],nbytes[i])
        if (dtype[i] eq 1) and keyword_set(bytenum) then str=fix(str)
        str = '    ' + string(str[0])
        if ed eq 'Y' then  begin
                holder[i] = widget_base(middle,/row)
                widlabel[i] = widget_label(holder[i],value = str1,/frame)
                widtext[i] = widget_text(holder[i],/frame,value=str,/edit)
        endif else begin
                holder[i] = widget_base(middle,/row)
                widlabel[i] = widget_label(holder[i],value = str1,/frame)
                widtext[i] = widget_label(holder[i],value=str)
        endelse 
    endfor

    if (liston EQ minlist) then widget_control,prevbut,sensitive=0 else $
                widget_control,prevbut,sensitive=1
    if (liston EQ maxlist) then widget_control,nextbut,sensitive=0 else $
                widget_control,nextbut,sensitive=1

    widget_control,main,/realize
    xmanager,'widgetedit',main

 newitem = total(newflag, 1)
 indexnum = where(newitem, nindex)

   if ( nindex GT 0 ) then begin                          ;Any mods made?
      indexnum = itnum[indexnum]
      indextype = db_item_info('INDEX',indexnum);Index type of modified fields 
      good = where(indextype GE 1, Ngood)         ;Which fields are indexed?
      if Ngood GT 0 then begin 
        message, 'Now updating index file', /INF
        dbindex, indexnum[good]
      endif
      dbopen,strlowcase(dbname),1
    endif

PROEND:

 return
 end

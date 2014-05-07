pro dbcompare,list1,list2, items, TEXTOUT=textout, DIFF = diff
;+
; NAME:
;     DBCOMPARE
; PURPOSE:
;     Display two entries in an IDL database side by side in a column format
;
; CALLING SEQUENCE:     
;     dbcompare, list1, list2, [items, TEXTOUT= , /DIFF]  
;
; INPUTS:
;     list1 - Integer scalar giving first entry number to be compared.
;     list2 - Integer scalar giving second entry number to be compared.
;
; OPTIONAL INPUT-OUTPUT:
;     items - items to be compared, if not supplied then all items will be
;          compared.    The items can be specified in any of the following ways:
;
;             form 1  scalar string giving item(s) as list of names
;                     separated by commas
;             form 2  string array giving list of item names
;             form 3  string of form '$filename' giving name
;                     of text file containing items (one item per line)                      line)
;             form 4  integer scalar giving single item number or
;                     integer vector list of item numbers
;             form 5  Null string specifying interactive selection.   This
;                     is the default if 'items' is not supplied
;             form 6  '*'     select all items (= default)
;
;            If items was undefined or a null string on input, then
;            on output it will contain the items interactively selected.
;
; OPTIONAL INPUT KEYWORDS:
;     /DIFF - If this keyword is set and non-zero, then only the items 
;             in the database that differ will be printed
;
;     TEXTOUT -  Scalar Integer (1-7) Used to determine output device.   See
;               TEXTOPEN for more info.
;
; SYSTEM VARIABLES:
;     Output device controlled by non-standard system variable !TEXTOUT, if 
;     TEXTOUT keyword is not used.    
;
; EXAMPLE:
;     Display entries 3624 and 3625 in column form showing only the items
;     that differ.
;               IDL> dbcompare,3624,3625,/diff
;
; PROCEDURES USED:
;     DB_INFO(), DB_ITEM, DB_ITEM_INFO(), DBRD, DBXVAL()
;     TEXTOPEN, TEXTCLOSE
; HISTORY:
;     Written,  W. Landsman            July 1996
;     Fix documentation, add Syntax display    W. Landsman   November 1998   
;     Replace DATATYPE() with size(/TNAME)   W. Landsman    November 2001
;     Assume since V5.5, remove VMS call  W. Landsman       September 2006
;     Fix problem with multiple values when /DIFF set W. Landsman April 2007
;-
;
 On_error,2                                ;Return to caller
 compile_opt idl2
 if N_params() LT 2 then begin
       print,'Syntax - DBCOMPARE, list1, list2, [items, TEXTOUT= ,/DIFF]'  
       return
 endif
 
; Make list a vector

 dbname = db_info( 'NAME', 0 )

 nentry = db_info( 'ENTRIES', 0)
 if list1[0] GT nentry then message, dbname + $
     ' LIST1 entry number must be between 1 and ' + strtrim( nentry, 2 )

 if list2[0] GT nentry then message, dbname + $
     ' LIST2 entry number must be between 1 and ' + strtrim( nentry, 2 )


; Determine items to print

 if N_elements(items) EQ 0 then items = '*'
 db_item,items, it, ivalnum, dtype, sbyte, numvals, nbytes
 nvalues = db_item_info( 'NVALUES', it )        ;number of values in item
 nitems = N_elements( it )                      ;number of items requested
 qnames = db_item_info( 'NAME', it )
 qtitle = db_info( 'TITLE', 0 )                 ;data base title

; Open output text file

 if not keyword_set(TEXTOUT) then textout = !textout  ;use default output dev.

 textopen, dbname, TEXTOUT = textout
 if size(TEXTOUT,/TNAME) EQ 'STRING' then text_out = 5 else $
        text_out = textout <!TEXTUNIT

; Create table listing of each item specified. -------------------------

      dbrd, list1, entry1                         ; read an entry.
      dbrd, list2, entry2                         ; read an entry.
      printf, !TEXTUNIT, ' '                        ; print  blank line.

; display name and value for each entry 

      for k = 0, nitems-1  do begin
         ;
         ; only print entries of reasonable size... < 5 values in item.
         ;
         if nvalues[k] LT 5 then begin
                value1 = dbxval(entry1,dtype[k],nvalues[k],sbyte[k],nbytes[k])
                value2 = dbxval(entry2,dtype[k],nvalues[k],sbyte[k],nbytes[k])
                if dtype[k] EQ 1 then begin
                        value1 = fix(value1)
                        value2 = fix(value2)
                endif
                value1 = strtrim(value1,2)
                value2 = strtrim(value2,2)
                if keyword_set(diff) then $
		       doprint = total(value1 NE value2) GT 0  $
                                      else doprint = 1
                if doprint then printf,!textunit,it[k],') ',qnames[k],  $
                        f = '(i,a,a,a,t55,a)',  value1,value2
         endif                                          ;display name,value
       endfor   ; k


 printf,!textunit,' '                         ;Added 11/90
 
 textclose, TEXTOUT = textout                   ;close text file

 return
 end

function dbget,item,values,listin,SILENT=silent, FULLSTRING = fullstring, $
               Count = count
;+
; NAME:
;       DBGET
; PURPOSE:
;       Find entry numbers which contain specified values of a given item.
; EXPLANATION:
;       DBGET() is useful as an alternative to DBFIND() when the desired 
;       search values are not easily expressed as a string.  
;
; CALLING SEQUENCE:
;       list = dbget( item, values, [ listin ], /SILENT, /FULLSTRING )
;
; INPUTS:
;       item - Item name or number
;       values -  scalar or vector containing item values to search for.
;
; OPTIONAL INPUTS:
;       listin - list of entries to be searched.  If not supplied, or
;               set to -1, then all entries are searched
;
; OUTPUT:
;       list - vector giving the entry number of entries containing desired
;               item values.  The number of elements in  LIST may be different 
;               from that of VALUE, since a value might be located zero, once, 
;               or many times in the database.  Use the function DBMATCH if a 
;               one to one correspondence is desired between VALUES and LIST. 
; OPTIONAL INPUT KEYWORDS:
;       /SILENT - If this keyword is set, then DBGET will not display
;               the number of entries found
;       /FULLSTRING - By default, one has a match if a search string is 
;               included in any part of a database value (substring match).   
;               But if /FULLSTRING is set, then all characters in the database
;               value must match the search string (excluding leading and 
;               trailing blanks).    Both types of string searches are case
;               insensitive.
; OPTIONAL OUTPUT KEYWORD:
;       COUNT - Integer scalar giving the number of valid matches
;
; RESTRICTIONS:
;       When linked databases are opened together, DBGET can only be used to
;       search on items in the primary database.
; EXAMPLE:
;       Get info on selected HD stars in Bright Star catalogue
;
;       IDL> dbopen, 'YALE_BS' 
;       IDL> hdno = [1141,2363,3574,4128,6192,6314,6668]    ;Desired HD numbers
;       IDL> list = dbget( 'HD', hdno )        ;Get corresponding entry numbers
;
; SYSTEM VARIABLES:
;       The obsolete system variable !ERR is set to number of entries found
; REVISION HISTORY:
;       Written,    W. Landsman      STX     February, 1989
;       William Thompson, GSFC, 14 March 1995 Added keyword FULLSTRING
;       Converted to IDL V5.0   W. Landsman   September 1997
;       Added COUNT keyword, deprecate !ERR        W. Landsman March 2000
;       Fix bug introduced March 2000              W. Landsman November 2000
;       Fix possible bug when sublist supplied    W. Landsman August 2008
;-
;
 On_error,2                                   ;Return to caller
 compile_opt idl2

 if N_params() LT 2 then begin
   print,'Syntax --  list = ' + $
          'DBGET( item, values, [listin, /SILENT, /FULLSTRING, Count=]'
   return,-1
 endif
   
 if N_params() LT 3 then listin = lonarr(1)-1

 nvals = N_elements(values)

 if nvals EQ 0 then message,'No search values supplied'

 db_item, item, itnum
 index = db_item_info( 'INDEX', itnum)
 list = listin
 
 if nvals EQ 1 then val = [values,values] $  ;Need at least 2 elements
               else val = values 

 if index[0] GE 2 then begin                              ;Sorted item
    if N_elements(list) EQ 1 then list = lonarr(1) + list
    dbfind_sort, itnum[0], nvals, val, list, $
            FULLSTRING = fullstring, Count =count

 endif else begin                                        ;Non-sorted item
    dbext, list, itnum, itvals
    dbsearch, nvals, val, itvals, good, FULLSTRING = fullstring, Count = count
    if count GT 0 then $     ;Updated Aug 2008
        if list[0] NE -1 then list = list[good] else list = good+1
 endelse

 if count LE 0 then begin
     if not keyword_set(SILENT) then $
         print, 'No entries found by DBGET in ' + db_info( 'NAME',0 )
     list = intarr(1)  

 endif else  if not keyword_set( SILENT ) then $
          print,count,' entries found in '+db_info('name',0)

 return, list[ sort(list) ]

 end

function dbsort,list,items,REVERSE = rev
;+
; NAME:
;       DBSORT
; PURPOSE:
;       Routine to sort list of entries in data base
;
; CALLING SEQUENCE: 
;       result = dbsort( list, items , [ REVERSE = ])
;
; INPUTS:
;       list - list of entry numbers to sort
;               -1 to sort all entries
;       items - list of items to sort (up to 9 items)
;
; OUTPUT:
;       result - numeric vector giving input list sorted by items
;
; OPTIONAL KEYWORD INPUT:
;       REVERSE - scalar or vector with the same number of elements as the
;         the number of items to sort.  If the corresponding element of REVERSE 
;         is non-zero then that item is sorted in descending rather than 
;         ascending order.
;
; EXAMPLE:
;       Sort an astronomical catalog with RA as primary sort, and declination
;       as secondary sort (used when RA values are equal)
;
;          IDL> NEWLIST = DBSORT( -1, 'RA,DEC' )
;
;       If for some reason, one wanted the DEC sorted in descending order, but
;       the RA in ascending order
;
;          IDL> NEWLIST = DBSORT( -1, 'RA,DEC', REV = [ 0, 1 ] )
;
; METHOD:
;       The list is sorted such that each item is sorted into
;       asscending order starting with the last item.
; COMMON BLOCKS:
;       DBCOM
; PROCEDURES USED:
;       ZPARCHECK, BSORT, DBEXT, DB_ITEM
; HISTORY
;       VERSION 1  D. Lindler  Oct. 86
;       Added REVERSE keyword   W. Landsman        August, 1991
;       Avoid use of EXECUTE() for V6.1 or later   W. Landsman Dec 2006
;       Assume since V6.1   W. Landsman   June 2009
;       Add TEMPORARY call  W. Lnadsman  July 2009
;-
 On_error,2
 compile_opt idl2
 if N_params() LT 2 then begin
     print,'Syntax: newlist = dbsort( list, items, [ REVERSE = ] )'
     return, -1
 endif
;---------------------------------------------------------
; data base common block, see DBOPEN for meanings

 common db_com,QDB,QITEMS,QLINK

; check parameters

 zparcheck, 'DBSORT', list, 1, [1,2,3], [0,1], 'entry list'
 zparcheck, 'DBSORT', items, 2, [1,2,3,7], [0,1], 'item list'

; extract values of items

 db_item, items, it
 nitems = N_elements(it)                    ;Number of items
 if nitems GT 9 then message, $
        'ERROR -  Can only sort on nine items or less'

                                            ;Verify REVERSE vector
 if not keyword_set(REV) then rev = bytarr(nitems) else $
         if N_elements(rev) NE nitems then $
             message,'ERROR - REVERSE vector must contain ' + $
                   strtrim(nitems,2) + ' elements'

; make list vector

 qnentry = long(qdb,84)
 if list[0] EQ -1 then vlist = lindgen(qnentry)+1 else vlist = list

; create line to execute in the form:
;       dbext, vlist, it, v1,v2,...,v(nitems)
 case nitems of 
        1: dbext, vlist, it, v1
        2: dbext, vlist, it, v1, v2
        3: dbext, vlist, it, v1, v2, v3
        4: dbext, vlist, it, v1, v2, v3, v4
        5: dbext, vlist, it, v1, v2, v3, v4, v5
        6: dbext, vlist, it, v1, v2, v3, v4, v5, v6
        7: dbext, vlist, it, v1, v2, v3, v4, v5, v6, v7
        8: dbext, vlist, it, v1, v2, v3, v4, v5, v6, v7, v8
        9: dbext, vlist, it, v1, v2, v3, v4, v5, v6, v7, v8, v9
 endcase

; sort on each item

 sub = lindgen(N_elements(vlist))               ;list of subscripts
 for i = 0,nitems-1 do begin

; get item

        j = nitems-i
        vv = 'v' + strtrim(j,2) 
        v = temporary(scope_varfetch(vv, level=0))

; perform previous sorts on item

        if i GT 0 then v = v[sub]
         
; sort item

        sub = sub[ bsort( v, REVERSE = rev[j-1] ) ]

 end

; return sorted list

 return, vlist[sub]
 end

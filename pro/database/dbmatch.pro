function dbmatch, item, values, listin, FULLSTRING = fullstring
;+
; NAME:
;       DBMATCH
; PURPOSE:
;       Find the entry number in a database for each element of item values
; EXPLANATION:
;       DBMATCH() is especially useful for finding a one-to-one 
;       correspondence between entries in different databases, and thus to 
;       create the vector needed for database pointers.
;
; CALLING SEQUENCE:
;       list = DBMATCH( item, values, [ listin, /FULLSTRING ] )
;
; INPUTS:
;       ITEM - Item name or number, scalar
;       VALUES -  scalar or vector containing item values to search for.
;
; OPTIONAL INPUTS:
;       LISTIN - list of entries to be searched.  If not supplied, or
;               set to -1, then all entries are searched
; OUTPUT:
;       LIST - vector of entry numbers with the same number of elements as 
;               VALUES.  Contains a value of 0 wherever the corresponding item
;               value was not found.
;
; OPTIONAL INPUT:
;       /FULLSTRING - By default, one has a match if a search string is 
;               included in any part of a database value (substring match).   
;               But if /FULLSTRING is set, then all characters in the database
;               value must match the search string (excluding leading and 
;               trailing blanks).    Both types of string searches are case
;               insensitive.
;
; NOTES:
;       DBMATCH is meant to be used for items which do not have duplicate values
;       in a database (e.g. catalog numbers).  If more than one entry is found
;       for a particular item value, then only the first one is stored in LIST.
;
;       When linked databases are opened together, DBMATCH can only be 
;       used to search on items in the primary database.
;
; EXAMPLE:
;       Make a vector which points from entries in the Yale Bright Star catalog
;       to those in the Hipparcos catalog, using the HD number
;
;       IDL> dbopen, 'yale_bs'            ;Open the Yale Bright star catalog
;       IDL> dbext, -1, 'HD', hd          ;Get the HD numbers
;       IDL> dbopen, 'hipparcos'          ;Open the Hipparcos catalog
;       IDL> list = dbmatch( 'HD', HD)    ;Get entries in Hipparcos catalog 
;                                         ;corresponding to each HD number.
; PROCEDURE CALLS:
;       DB_ITEM, DB_ITEM_INFO(), DBEXT, DBFIND_SORT()
; REVISION HISTORY:
;       Written,    W. Landsman      STX     February, 1990
;       Fixed error when list in parameter used May, 1992
;       Faster algorithm with sorted item when listin parameter supplied 
;       Added keyword FULLSTRING,check for empty database, William Thompson, 
;               GSFC, 15 March 1995
;       Work for more than 32767 values, added CATCH W. Landsman   July 1997
;       Change some loop variables to type LONG,  W. Landsman  July 1999
;       Remove loop for substring searches (faster)  W. landsman August 1999
;       Replace DATATYPE() with size(/TNAME)  W. Landsman  November 2001
;       Fixed typo when search on sorted items W. Landsman February 2002
;       Fixed bug from Nov 2001 where /FULLSTRING was always set.  W.L Feb 2007
;-
 On_error,2

 if N_params() LT 2 then begin
     print,'Syntax --  list = DBMATCH( item, values, [ listin, /FULLSTRING] )'
     return,-1
 endif 


 catch, error_status
 if error_status NE 0 then begin 
        print,!ERR_STRING
        if N_elements(listin) NE 0 then return,listin else return, -1
 endif

 nvals = N_elements( values )
 if nvals EQ 0 then message, $ 
       'ERROR - No search values (second parameter) supplied'

 if N_params() LT 3 then listin = lonarr(1) - 1

 db_item,item,itnum
 index = db_item_info( 'INDEX', itnum)           ;Get index type of item
 list = lonarr( nvals )

 nentries = db_info('entries')
 if nentries[0] eq 0 then begin                 ;Return if database is empty
        message,'ERROR - No entries in database ' + db_info("NAME",0),/INF
        return,listin*0
 endif 

 if index[0] GE 2 then begin                      ;Sorted item

    if listin[0] NE -1 then min_listin = min( listin, MAX = max_listin)

    for i = 0l,nvals-1 do begin

        val = [values[i],values[i]]

;       We don't supply the LISTIN parameter directly to DBFIND_SORT.  Since
;       we know that we need only 1 match for each item value, we can do
;       the restriction to the LISTIN values faster than DBFIND_SORT can

        tmplist = -1
        dbfind_sort,itnum[0],1,val, tmplist, $    ;Search all entries to start
                fullstring=fullstring, Count = Nmatch_sort
 
           if ( listin[0] NE -1 ) then begin

                if Nmatch_sort EQ 0 then goto, FOUND_MATCH

                good = where( ( tmplist LE max_listin ) and $ 
                              ( tmplist GE min_listin ), Ngood)

                if ( Ngood EQ 0 ) then goto, FOUND_MATCH

                tmplist = tmplist[good]

                for j = 0L, Ngood - 1  do begin
                   test = where( listin EQ tmplist[j], Nfound ) 
                   if Nfound GE 1 then begin
                         list[i] = tmplist[j]
                         goto, FOUND_MATCH
                   endif
                endfor 

         endif else if ( Nmatch_sort GT 0 ) then list[i] = tmplist[0]
 
        FOUND_MATCH:
   endfor

  endif else begin                                 ;Non-sorted item

    if listin[0] EQ -1 then tmplist = lindgen( nentries[0] )+1 else $
                            tmplist = listin
    dbext, tmplist, itnum, itvals
    typ = size(itvals,/TNAME)
    if typ EQ 'STRING' then begin
                itvals = strupcase( strtrim(itvals,2) )
                vals   = strupcase( strtrim(values,2) )
    endif else vals = values
    for i=0L,nvals-1 do begin             
       if typ NE 'STRING' then begin                  ;Fixed Feb 2007
               good = where( itvals EQ vals[i], Nfound ) 
               if Nfound GT 0 then list[i] = tmplist[ good[0] ]  ;Fixed May-92

        endif else begin                 ;Can't use WHERE on string arrays
                                         ;unless FULLSTRING is set

               if keyword_set(fullstring) then begin
                   good = where( itvals EQ vals[i], Nfound)
                   if Nfound GT 0 then list[i] = tmplist[ good[0] ]
                end else begin
                      good = where(strpos( itvals, vals[i]) GE 0, Nfound) 
                      if Nfound GT 0 then begin
                             list[i] = tmplist[good[0]]
                             goto, DONE
                       endif
                    
                endelse
             endelse
    DONE:       
    endfor
endelse

return,list

end

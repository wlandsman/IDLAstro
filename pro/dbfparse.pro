pro dbfparse, spar, items, stype, values
;+
; NAME:
;     DBFPARSE
; PURPOSE:
;     Parse the search string supplied to DBFIND.   Not a standalone routine
;
; CALLING SEQUENCE:
;     DBFPARSE, [ spar, items, stype, values ]
;
; INPUTS:
;     spar - search parameter specification, scalar string
;
; OUTPUTS:
;     items - list of items to search on
;     stype - search type, numeric scalar
;               0    item=values[j,0]
;               -1   item>values[j,0]
;               -2   item<values[j,1]
;               -3   values[j,0]<item<values[j,1]
;               -4   item is non zero
;               -5   item=values[j,0] within tolerance values[j,1]
;               0<   items in list values[j,i] for i=0,stype-1
;     values - search values, 20 x 10 string array, can parse a string
;               with up to 20 items specifications, each item can have 10
;               values
;
; REVISION HISTORY:  
;     D. Lindler NOV, 1987
;     Check for valid numeric values before assuming a date string
;     W. Landsman                    July, 1993
;     Accept four digit years when in ccyy/doy format W. Landsman   October 1998
;     Don't do DATE/Time test for string items  W. Landsman   July 2006
;     No tolerance search allowed for strings, so allow parenthesis within string
;         W. Landsman Feb 2015
;-
;--------------------------------------------------------------
 On_error,2
;
; parse string array search parameters into a single string.
;
  par  = strjoin( strtrim( spar, 2),',')    ;Make into a scalar if necessary
 
  items = strarr(20)                 ;array of items
  values = strarr(20,10)              ;range limited to 10 elements/item.
  stype = intarr(20)                  ;search type for item j
                                        ;   0    item=values(j,0)
                                        ;   -1   item>values(j,0)
                                        ;   -2   item<values(j,1)
                                        ;   -3   values(j,0)<item<values(j,1)
                                        ;   -4   item is non zero
                                        ;   -5   item=values(j,0) within
                                        ;         tolerance values(j,1)
                                        ;   0<   items in list values(j,i)
                                        ;             for i=0,stype-1
;
; parse par
;
nitems  = 0
while par ne '' do begin
               
  ;
  ;  Concatenated array. A normal seach involves using comma's as
  ;    delimiter. For concatenation array, the brackets must be
  ;    found (both beginning and end) prior to extracting item
  ;    search information. This is done once at a time as each
  ;    search item is deciphered.
  ;
    strparam = strpos(par,'[')
    if (strparam lt strpos(par,',')) and (strparam gt 0) then begin
       next = gettok(par,']')             ; just the concatenation portion.
       next = next + ']'                  ; put it back.
       par=strtrim(par,2)                 ; trim blanks
       par  = strmid(par,1,strlen(par)-1) ; eat next comma.
    end else next=gettok(par,',')         ; get next search item
    par=strtrim(par,2)                    ;trim blanks

    case 1 of

    ;
    ;    Concatenation array...
    ;       item=[value1,value2,...]
    ;
    (strpos(next,'[') gt 0): begin       ; explicit range.
             items[nitems]=gettok(next,'='); get item name
           ;
           ; that leaves brackets and indices.
           ;
             junk = gettok( next, '[' )
             vals = gettok( next, ']' )
             nvals=0
             while vals ne '' do begin
                values[nitems,nvals]=gettok(vals,',')
                nvals++
                if nvals GE 10 then message, $ 
    'No more than 10 values/item allowed; use DBMATCH or DBGET instead'
             endwhile
             stype[nitems] = nvals
             end
             
    ;
    ;  item=value(tolerance) 
    ;  Updated Feb 2015 to allow parenthesis within string searches 
    ;
    (strpos(next,'=') gt 0): begin      ; equality specified
             items[nitems]=gettok(next,'='); get item name
             db_item,items[nitems],itnum,ivalnum,idltype
             if idltype EQ 7 then begin
               values[nitems,0] = next
               stype[nitems] = 0
             endif else begin  
             values[nitems,0]=gettok(next,'('); value for item
             stype[nitems]=0
  
             if next ne '' then begin   ;tolerance supplied
                values[nitems,1]=gettok(next,')')
                stype[nitems] = -5
             endif
             endelse
             end
    ;
    ; minimum supplied?   item>value
    ;
    (strpos(next,'>') gt 0): begin
             items[nitems]=gettok(next,'>');get item name
             values[nitems,0]=next         ;get minimum value
             stype[nitems]=-1
             end
    ;
    ;  Range specified or maximum specified.
    ;
      (strpos(next,'<') gt 0): begin    ; form is min<item<max
             ltpos=strpos(next,'<')
             if strpos(next,'<',ltpos+1) ge 0 then begin
        ;
        ;  range specified   value1<item<value2
        ;
                values[nitems,0]  = gettok(next,'<')    ;minimum value
                items[nitems] = gettok(next,'<')        ; get item name.
                values[nitems,1]=next                   ;whats left is max.
                stype[nitems]=-3
               end else begin
        ;
        ;  maximum specified
        ;
                items[nitems] = gettok(next,'<')
                values[nitems,1]=next
                stype[nitems]=-2
             end
           end
        ;
        ; non zero value specified  item not equal to 0
        ;               
      else: begin
                items[nitems]=next
                stype[nitems]=-4
                end
      endcase
      nitems=nitems+1
  end; while

;
; truncate arrays down to proper number of items.
;
  items  = items[0:nitems-1]
  values = values[0:nitems-1,*]

; convert data/time and ra, dec to real numbers (special user mode).

 n = N_elements(values)
 db_item,items,it,ivalnum,idltype
 idltype = rebin(idltype,n)
; loop on elements in vals

 for i = 0,n-1 do begin
        if idltype[i] NE 7 then begin
        v = strtrim(values[i])

; is it of the form DD-MMM-YYYY hh:mm:ss.ss

        if (strpos(v,':') gt 0) and (strpos(v,'-') gt 0) then begin
                val = date_conv(v)
                v = string(val,'(d22.14)')
        end

; is it of form ccyy/ddd/hh:mm:sss?   (Two digit years are interpreted as 
; 1900 + YY if YY GT 40, and 2000 + YY if YY LE 40.)

        if strpos(v,'/') gt 0 then begin
                v1 = v
                val = 0.0d0
                yr = strtrim( gettok( v1,'/'), 2 )
                if yr EQ '' then goto, DATE
                if strnumber( yr, num) then begin
                        if num LT 40 then num = num + 2000 else $
                        if ((num GT 40) and (num LT 100)) then num = num + 1900
                        val = val + num*1000d0
                        day = strtrim(gettok(v1,':'),2)
                        if day EQ '' then goto,DATE
                        if strnumber(day,num) then begin  
                           val = val + num
                           hr = strtrim(gettok( v1,':'),2)
                           if hr EQ '' then goto,DATE
                           if strnumber( hr, num) then begin
                                val = val + num/24.0d0
                                mn = strtrim( gettok(v1,':'),2)
                                if mn EQ '' then goto,DATE
                                if strnumber( mn, num) then begin
                                        val = val + num/24.0d0/60.0
                                        sc = strtrim(v1,2)
                                        if sc EQ '' then goto, DATE
                                        if strnumber(sc,num) then begin 
                                           val = val + num/24.0d0/3600.0
                                           goto, DATE
                                         endif
                                endif
                            endif
                         endif
                      endif
                 goto, NOT_DATE
DATE:           v = string(val,'(d22.14)')
        endif
NOT_DATE:
;
; is it of form hh:min:sec or deg:min:sec
;
        if strpos(v,':') gt 0 then begin
                val  =0.0d0           
                val = val+gettok(v,':')
                sign = 1
                if(val lt 0.0) then sign = (-1)
                val = val+gettok(v,':')/60.0*sign
                val = val+strtrim(v)/3600.0d0*sign
                v = val
        endif
        values[i]=v
 endif
 endfor
 return
 end

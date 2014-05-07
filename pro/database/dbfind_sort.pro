pro dbfind_sort,it,type,svals,list, FULLSTRING = fullstring, COUNT = number
;+
; NAME:
;       DBFIND_SORT   
; PURPOSE:
;       Subroutine of DBFIND to perform a search using sorted values 
; EXPLANATION:
;       This is a subroutine of dbfind and is not a standalone procedure
;       It is used to limit the search using sorted values  
;
; CALLING SEQUENCE:
;       dbfind_sort, it, type, svals, list, [/FULLSTRING, COUNT = ]
;
; INPUT: 
;       it - item number, scalar
;       type - type of search (output from dbfparse)
;       svals - search values (output from dbfparse)
;
; INPUT/OUTPUT:
;       list - found entries
;
; OPTIONAL INPUT KEYWORD:
;       /FULLSTRING - By default, one has a match if a search string is 
;               included in any part of a database value (substring match).   
;               But if /FULLSTRING is set, then all characters in the database
;               value must match the search string (excluding leading and 
;               trailing blanks).    Both types of string searches are case
;               insensitive.
; OPTIONAL OUTPUT KEYWORD
;       Count - Integer scalar giving the number of matches found
; SYSTEM VARIABLES:
;       The obsolete system variable !err is set to number of good values
;       !ERR = -2 for an invalid search
; PROCEDURES CALLED:
;       DB_INFO(), DB_ITEM_INFO(), DBSEARCH() 
; REVISION HISTORY:
;       D. Lindler  July,1987
;       William Thompson, GSFC/CDS (ARC), 30 May 1994
;               Added support for external (IEEE) data format
;       William Thompson, GSFC, 14 March 1995 Added keyword FULLSTRING
;       Minimize use of obsolete !ERR variable   W. Landsman  February 2000
;       Added COUNT keyword, deprecate !ERR W. Landsman  March 2000
;       Use 64 bit integers V5.2 or later
;       Include new IDL unsigned & 64 bit integer datatypes W.Landsman July 2001
;       Make sure returned list vector is LONG  W. Landsman August 2001
;       Work on string items   W. Landsman November 2009
;       Don't use VALUE_LOCATE on a single value  W. Landsman November 2009
;       Use VALUE_LOCATE even for equal values W. Landsman December 2009
;       Fix bug allowing negative FIRST values, William Thompson, 10 May 2010
;-
;----------------------------------------------------------------------------
;       READ EVERY 512TH VALUE IN SORTED VALUES
;
; get item info
;
itnum = db_item_info('itemnumber',it)   ;item number in this dbno
index_type = db_item_info('index',it)
;
; get unit number of index file and read header info
;
unit = db_info('UNIT_DBX',0)
external = db_info('EXTERNAL',0)
pi = assoc(unit,lonarr(2))
h = pi[0]
if external then swap_endian_inplace,h,/swap_if_little
pi = assoc(unit,lonarr(7,h[0]),8)
header = pi[0]
if external then swap_endian_inplace,header,/swap_if_little
items = header[0,*]
pos = where(items EQ itnum) & pos=pos[0]
; 
; find starting location to read
;
sblock = header[3,pos]
sbyte = 512LL*sblock
nv = (db_info('ENTRIES',0)+511)/512
nbytes = db_item_info('NBYTES',it)
;
; create mapped i/o variable
;
dtype = db_item_info('IDLTYPE',it)
if dtype NE 7 then  $
  p = assoc(unit,make_array( size=[1,nv,dtype[0],0],/NOZERO), sbyte) else $
    p = assoc(unit,make_array( size=[2,nbytes,nv,1,0],/NOZERO), sbyte)
    
numbyte = [0,1,2,4,4,8,0,nbytes,16,0,0,0,2,4,8,8]
num_bytes = numbyte[ dtype[0] ]
;
; read values from file (for every 512th entry)
;

values=p[0]
if dtype EQ 7 then values = string(values) else $
if external then swap_endian_inplace,values,/swap_if_little
;
;------------------------------------------------------------------
; CONVERT INPUT SVALS TO CORRECT DATA TYPE
;
; determine data type of values to be searched
;
s=size(values) & nv = N_elements(values)
;
; convert svals
;
nvals = type>2
sv=replicate(values[0],nvals)
for i=0L,nvals-1 do sv[i]=strtrim(svals[i],2)
sv0 = sv[0] & sv1 = sv[1]

;
;--------------------------------------------------------------------------
; FIND RANGE OF VALID SUBSCRIPTS IN LIST
;
;
if nv EQ 1 then begin 
    first = 0 & last = 1
endif else begin     

case type of
 
        0: begin                                ;value=sv0
               first = value_locate(values,sv0) > 0  
	       last = (first +1) < nv 
	       while values[first] EQ sv0 do begin 
		    if first EQ 0 then break
	            first = first-1
	       endwhile	
	                 
           end

        -1: begin                               ;value>sv0
                first = value_locate(values,sv0) > 0                
                last = nv
	        while values[first] EQ sv0 do begin 
		    if first EQ 0 then break
	            first = first-1
	        endwhile	
            end

        -2: begin                               ;value<sv1
                first = 0
		last = (value_locate(values,sv1) + 1) < nv > first 
	        while values[first] EQ sv0 do begin 
		    if first EQ 0 then break
	            first = first-1
	        endwhile	
             end

        -3: begin                               ;sv0<value<sv1
           

            if sv1 LT sv0 then begin
                temp = sv0
                sv0 = sv1
                sv1 = temp
            end
                 first = value_locate(values,sv0) > 0                
 		 last = (value_locate(values,sv1) + 1) < nv > 0
	         while values[first] EQ sv0 do begin 
		    if first EQ 0 then break
	            first = first-1
	         endwhile	
  
             end 
        -5: begin                               ;sv1 is tolerance

            minv = sv0-abs(sv1)
            maxv = sv0+abs(sv1)
                good = where(values LT minv, N)
                if N LT 1 then first=0 else first=N-1
                good = where(values GT maxv, N)
                if N LT 1 then last=nv else last=good[0]
	       while values[first] EQ sv0 do begin 
		    if first EQ 0 then break
	            first = first-1
	       endwhile	
            end

        -4: begin                       ;non-zero
                if values[0] EQ 0 then begin
                        good=where(values EQ 0, N)
                        first=N-1
                        last=nv
                 end else begin ;not allowed
                        !err=-2
                        return
                end
           end
        else: begin                             ;set of values
              sv0 = min(sv[0:type-1]) & sv1 = max(sv[0:type-1])
                good=where(values LT sv0, N)
                if N LT 1 then first=0 else first=N-1
                good=where(values GT sv1, N)
                if N LT 1 then last=nv else last=good[0]
              end
endcase
endelse
;-----------------------------------------------------------------------------
; we now know valid values are between index numbers first*512 to last*512
;
if first EQ last then begin
        !err=0
        return
end
;
; extract data values for blocks first to last
;
sblock=header[4,pos]            ;starting block for sorted data
sbyte=512LL*sblock               ;starting byte
first=first*512L+1
last=(last*512L) < db_info('entries',0)
number=last-first+1
if dtype NE 7 then $
p = assoc(unit,make_array(size=[1,number,dtype,0],/nozero), $
                                             sbyte+(first-1)*num_bytes) else $
    p = assoc(unit,make_array( size=[2,nbytes,number,1,0],/NOZERO), $
			      sbyte+(first-1)*num_bytes)
			      
values=p[0]

if dtype EQ 7 then values = string(values) else $
if external then swap_endian_inplace,values,/swap_if_little
;
; if index type is 2, data base is sorted on this item, first and last
; give range of valid entry numbers
;

if index_type EQ 2 then begin
        if list[0] EQ -1 then begin
                list=lindgen(number)+first
           end else begin
                good=where((list ge first) and (list le last), number)
                if number GT  0 then begin
                         list=list[good]
                         values=values[list-first]
                endif
        end
;
; if index type wasn't 2 the item was sorted and index numbers must
;       be read
;

end else begin
;
; find starting location to read
;
        sblock=header[5,pos]
        sbyte=512LL*sblock
;
; read values from file
;
p = assoc(unit,make_array(size=[1,number,3,0],/nozero),sbyte+(first-1)*4)
        if list[0] EQ -1 then begin
                list=p[0]
                if external then byteorder,list, /NTOHL
           end else begin
                list2=p[0]
                if external then byteorder,list2,/NTOHL   ;Fixed typo Jan 2010
                match,list,list2,suba,subb, Count = number
                if number GT 0 then begin
                         list=list[suba]
                        values=values[subb]
                end
        end
end
;
; now search indiviual entries
;
if number GT 0 then begin
        dbsearch,type,svals,values,good,fullstring=fullstring, Count = number
        if number GT 0 then list=list[good]
end
!err=number
return
end

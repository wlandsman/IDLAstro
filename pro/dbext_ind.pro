pro dbext_ind,list,item,dbno,values
;+
; NAME:
;       DBEXT_IND
; PURPOSE:
;       routine to read a indexed item values from index file
;
; CALLING SEQUENCE:  
;       dbext_ind,list,item,dbno,values
;
; INPUTS:
;       list - list of entry numbers to extract values for
;               (if it is a scalar, values for all entries are extracted)
;       item - item to extract
;       dbno - number of the opened data base
;
; OUTPUT:
;       values - vector of values returned as function value
; HISTORY:
;       version 1  D. Lindler  Feb 88
;       Faster processing of string values    W. Landsman   April, 1992
;       William Thompson, GSFC/CDS (ARC), 30 May 1994
;               Added support for external (IEEE) data format
;       Allow multiple valued (nonstring) index items W. Landsman  November 2000      
;       Use 64bit integer index for large databases W. Landsman  February 2001
;       Fix sublisting of multiple valued index items W. Landsman  March 2001
;       Check whether any supplied entries are valid W. Landsman Jan 2009
;       Remove IEEE_TO_HOST    W. Landsman  Apr 2016
;-
On_error,2
compile_opt idl2
;
if N_params() LT 4 then begin
     print,'Syntax - DBEXT_IND, list, item, dbno, values'
     return
endif

; Determine first and last block to extract
;
s=size(list) & ndim=s[0]
if (ndim GT 0) then if (list[0] EQ -1) then ndim=0
zeros = 0                               ;flag if zero's present in list
if ndim EQ 0 then begin
        minl = 1
        maxl = db_info('ENTRIES',dbno)
    end else begin
        minl = min(list)
        if minl EQ 0 then begin ;any zero values in list
                zeros = 1
                nonzero = where(list GT 0, Ngood, comp=bad)
		if Ngood EQ 0 then message,'ERROR - No valid entry numbers supplied'
                minl = min(list[nonzero])
        endif
        maxl=max(list)
 end
;
; get item info
;
db_item,item,it,ivalnum,dtype,sbyte,numvals,nbytes
nbytes = nbytes[0]
if N_elements(it) GT 1 then $
        message,'ERROR - Only one item can be extracted by dbext_ind'

itnum = db_item_info('itemnumber',it[0])  ;item number in this dbno
;
; determine if indexed
;
index_type = db_item_info('index',it[0])
if index_type EQ 0  then $
        message,'ERROR - Requested item is not indexed'

if index_type EQ 3 then $
        message,'ERROR - Unsorted values of item not recorded in index file'
;
; get unit number of index file and read header info
;
 unit=db_info('UNIT_DBX',dbno)
 external = db_info('EXTERNAL',dbno)     ;External (IEEE) data format?
 p=assoc(unit,lonarr(2))
 h=p[0]
 if external then swap_endian_inplace,h,/swap_if_little
 p = assoc(unit,lonarr(7,h[0]),8)
 header = p[0]
 if external then swap_endian_inplace,header,/swap_if_little
 items = header[0,*]
 pos = where(items EQ itnum, Nindex) & pos=pos[0]
 if Nindex LT 1 then $
        message,'Item not indexed, DBNO may be wrong'

;
; find starting location to read
;
if index_type NE 4 then sblock=header[4,pos] else sblock=header[6,pos]
;
numvals = numvals[0]
sbyte = 512LL*sblock
sbyte = sbyte+(minl-1L)*nbytes*numvals
nv = (maxl-minl+1L) ;number of bytes to extract
;            
; create mapped i/o variable
;
dtype = dtype[0]

if dtype NE 7 then begin
   if numvals GT 1 then $ 
   p = assoc(unit, make_array(size=[2,numvals,nv,dtype,0],/NOZERO), sbyte ) else $
   p = assoc(unit, make_array(size=[1,nv,dtype,0],/NOZERO), sbyte ) 
 endif else  p = assoc(unit, make_array(size=[2,nbytes,nv,1,0],/NOZERO), sbyte )

;
; read values from file
; Modified, April 92 to delay conversion to string until the last step WBL
;
values = p[0]
if external then swap_endian_inplace,values,/swap_if_little
;
; if subset list specified perform extraction
;

if ndim NE 0 then begin
        if zeros then begin                     ;zero out bad values
                if dtype NE 7 then begin        ;not a string?
                        if numvals EQ 1 then begin
                             values = values[(list-minl)>0 ]
                             values[bad]=0
                        endif else begin 
                             values = values[*,(list-minl)>0 ]
                             values[*,bad] = intarr(numvals)
                        endelse
                   end else begin                       ;string 
                        values = values[*, (list-minl)>0 ]
                        if N_elements(bad) EQ 1 then bad = bad[0]
                        values[0,bad] = replicate( 32b, nbytes )
                   endelse
           end else begin
                  if (dtype EQ 7) || (numvals GT 1) then  $
                            values = values[*, list-minl] $
                      else  values = values[ list-minl ]
        end
end
if dtype EQ 7 then values = string(values)
return
end

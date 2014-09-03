pro dbext,list,items,v1,v2,v3,v4,v5,v6,v7,v8,v9,v10,v11,v12
;+
; NAME:
;       DBEXT
; PURPOSE:
;       Extract values of up to 12 items from an IDL database 
; EXPLANATION:
;       Procedure to extract values of up to 12 items from
;       data base file, and place into IDL variables
;
; CALLING SEQUENCE:
;       dbext,list,items,v1,[v2,v3,v4,v5,v6,v7,v8,v9,v10,v11,v12]
;
; INPUTS:
;       list - list of entry numbers to be printed, vector or scalar
;               If list = -1, then all entries will be extracted.
;               list may be converted to a vector by DBEXT 
;       items - standard item list specification.  See DBPRINT for 
;               the 6 different ways that items may be specified. 
;
; OUTPUTS:
;       v1...v12 - the vectors of values for up to 12 items.
;
; EXAMPLE:
;       Extract all RA and DEC values from the currently opened database, and
;       place into the IDL vectors, IDLRA and IDLDEC.
;
;               IDL> DBEXT,-1,'RA,DEC',idlra,idldec
;
; HISTORY
;       version 2  D. Lindler  NOV. 1987
;       check for INDEXED items   W. Landsman   Feb. 1989
;       Converted to IDL V5.0   W. Landsman   September 1997
;       Avoid EXECUTE() call for V6.1 or later  W. Landsman   December 2006
;       Assume since V6.1   W. Landsman June 2009
;-
;*****************************************************************
 On_error,2
 compile_opt idl2

 if N_params() lt 3 then begin
        print,'Syntax - dbext, list, items, v1, [ v2, v3....v12 ]'
        return
 endif

 zparcheck,'DBEXT',list,1,[1,2,3,4,5],[0,1],'Entry List'

 db_item,items,it,ivalnum,idltype,sbyte,numvals,nbytes

 nitems = N_elements(it)
 nentries = db_info('entries')
 if max(list) GT nentries[0] then $
         message,db_info('name',0)+' entry numbers must be between 1 and ' + $
         strtrim(nentries[0],2)
 if nitems GT N_params()-2 then $
        message,'Insufficient output variables supplied'
 if nitems LT N_params()-2 then message, /INF, $
        'WARNING - More output variables supplied than items specified'

; get item info.

 dbno = db_item_info('dbnumber',it)
 if max(dbno) eq 0 then dbno=0 $                ;flag that it is first db only
                  else dbno=-1
 index = db_item_info('index',it)
 ind = where( (index ge 1) and (index ne 3), Nindex ) 

 if (Nindex eq nitems) and (dbno eq 0) then begin     ;All indexed items?

        if N_elements(list) eq 1 then list = lonarr(1) + list
        for i=0,nitems - 1 do begin                         ;Get indexed items
          itind = it[ind[i]]
   	  dbext_ind,list,itind,dbno,scope_varfetch('v' + strtrim(ind[i]+1,2))
       endfor

 endif else begin     

         nvalues = db_item_info('nvalues',it)
         dbext_dbf,list,dbno,sbyte,nbytes*nvalues,idltype,nvalues, $
                    v1,v2,v3,v4,v5,v6,v7,v8,v9,v10,v11,v12

 endelse

 return
 end

pro dbsearch,type,svals,values,good, FULLSTRING = fullstring, COUNT = count
;+
; NAME:
;	DBSEARCH
; PURPOSE:
;	Subroutine of DBFIND() to search a vector for specified values
;
; CALLING SEQUENCE:
;	dbsearch, type, svals, values, good, [ /FULLSTRING, COUNT = ] 
;
; INPUT: 
;	type - type of search (output from dbfparse)
;	svals - search values (output from dbfparse)
;	values - array of values to search
;
; OUTPUT:
;	good - indices of good values
;
; OPTIONAL INPUT KEYWORD:
;	/FULLSTRING - By default, one has a match if a search string is 
;		included in any part of a database value (substring match).   
;		But if /FULLSTRING is set, then all characters in the database
;		value must match the search string (excluding leading and 
;		trailing blanks).    Both types of string searches are case
;		insensitive.
; OPTIONAL OUTPUT KEYWORD:
;       COUNT  - Integer scalar giving the number of valid matches
;  SIDE EFFECTS:
;	The obsolete system variable !ERR is set to number of good values
; REVISION HISTORY:
;	D. Lindler  July,1987
;       Added COUNT keyword, deprecate !ERR   W. Landsman   March 2000
;      Some speed improvements W.L. August 2008
;       Add compound operators, slightly faster WL November 2009
;       D. Lindler  Aug 2013, added strtrim on values for a string search
;       Fix problem with "less than" string searches WL November 2014
;       November 2014 fix actually broke things, reverting  WL January 2015
;-
;-----------------------------------------------------------
 On_error,2
 compile_opt idl2
 
 svals = strupcase(svals)
;
; determine data type of values to be searched
;
 datatype=size(values,/type) & nv = N_elements(values)
 
;
; convert svals to correct data type
;
 nvals = type>2
 if datatype NE 7 then sv = replicate(values[0],nvals) else $
                      sv = replicate(' ',nvals)
 On_ioerror, BADVAL              ;Trap any type conversions
 sv[0]= svals[0:nvals-1]
 On_ioerror, NULL
 sv0=sv[0] & sv1=sv[1]
;
; -----------------------------------------------------------
;      STRING SEARCHES (Must use STRPOS to search for substring match)
;
if datatype EQ 7 then begin
    values = strupcase(strtrim(values))
    case type of
						
         0: if keyword_set(FULLSTRING) then $            ;Exact string match?
	    valid = strtrim(values,2) EQ strtrim(sv0,2) else $
	    valid = strpos(values,strtrim(sv0,2)) GE 0   ;substring search
        -1: valid = values GE sv0                        ;greater than
        -2: valid = values LE sv1                        ;less than
	    -3: valid = (values GE sv0) and (values LE sv1)  ;in range
	    -4: valid = strtrim(values) NE ''       ;non zero (i.e. not null)
        -5: message, $                                  ;Tolerance value
               ' Tolerance specification for strings is not valid'
         else:  begin
                sv = strtrim(sv,2)
		        sv = sv[uniq(sv,sort(sv))]     ;Remove duplicates
		        type = N_elements(sv)
                valid = bytarr(nv)

		        if keyword_set(FULLSTRING) then begin
		           values = strtrim(values,2)
                   for ii = 0l,type-1 do valid OR= (values EQ sv[ii]) 

                endif else begin

                for ii=0L,type-1 do begin               ;within set of substring
             		valid OR= (strpos(values,sv[ii]) GE 0)		
                endfor

		        endelse
                end
	endcase
	good = where(valid, count)
	return
end
;
;---------------------------------------------------------------------
;		ALL OTHER DATA TYPES

case type of
 
	 0: good = where( values EQ sv0, count )               ;value=sv0
	-1: good = where( values GE sv0, count )		;value>sv0
	-2: good = where( values LE sv1, count )		;value<sv1
	-3: begin				;sv0<value<sv1
	    if sv1 lt sv0 then begin
	        temp=sv0
		    sv0=sv1
		    sv1=temp
	    endif
	    good=where((values GE sv0) and (values LE sv1), count)
	    end 	
	-5: begin				;sv1 is tolerance
	    minv=sv0-abs(sv1)
	    maxv=sv0+abs(sv1)
	    good=where( (values GE minv) and (values LE maxv), count)
	    end
	-4: good=where(values, count)		;non-zero
	else: begin				;set of values	
            sv = sv[uniq(sv,sort(sv))]     ;Remove duplicates
	      type = N_elements(sv)
	      valid = bytarr(nv) 

	      for i=0L,type-1 do begin		;loop on possible values  
	         valid OR= (values EQ sv[i])
	      endfor
	      good = where(valid, count) 	    
  

              if count EQ 0 then good = intarr(1)-1   ;Make sure good is defined
	      !err=count
	      end
endcase
return
BADVAL: !ERR=-2       ;Illegal search value supplied
return
end

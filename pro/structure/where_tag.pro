function where_Tag, Struct, Nfound,	TAG_NAME=Tag_Name,	$
					TAG_NUMBER=Tag_Num,	$
					ISELECT=ipart, NOPRINT=noprint,	$
					RANGE=range, VALUES=values
;+
; NAME:
;	WHERE_TAG
; PURPOSE:
;	Like WHERE but works on structure tag names
; EXPLANATION:
;	Obtain subscripts of elements in structure array for which
;	a particular Tag has values in a range or matching specified values.
;	Like the WHERE function but for use with structures
; CATEGORY:
;			Structures
; CALLING SEQUENCE:
;	 w = where_tag( struct, [ Nfound,  TAG_NAME=, TAG_NUMBER = , RANGE =, 
;				VALUES =, RANGE =, ISELECT =, /NOPRINT ]
;
; INPUTS:
;	Struct = structure array to search.
;
; INPUT KEYWORDS:
;	User *must* specify (1) TAG_NAME or TAG_NUMBER to search, and (2)
;		the VALUES or RANGE to search on
;
;	TAG_NAME = Scalar string specifying Tag Name
;	TAG_NUMBER = otherwise give the Tag Number,
;	RANGE = [min,max] range to search for in Struct,
;	VALUES = one or array of numbers to match for in Struct,
;	ISELECT= specifies indices to select only part of structure array,
;		(use it to recycle subscripts from previous searches).
;	/NOPRINT = suppress informational messages about nothing found.
;
; OUTPUTS:
;	Nfound = # of occurrences found.
;
; RESULT:
;	Function returns subscripts (indices) to desired elements.
;
; EXAMPLES:
;	Suppose STR is a structure with tags CAT_NO:indgen(10), and 
;		NAME:strarr(10).   Find the indices where STR.CAT_NO is
;		between 3 and 5.
;
;	IDL> print, WHERE_TAG( str, TAG_NAME = 'CAT_NO', VALUE = [3,4,5] )  ;or
;	IDL> print, WHERE_TAG( str, TAG_NUM = 0, RANGE = [3,5]) 
;
; PROCEDURE:
;	Get tag number and apply the WHERE function appropriately.
;
; MODIFICATION HISTORY:
;	written 1990 Frank Varosi STX @ NASA/GSFC
;	Stop printing "Tag <xxx> not found" with /NOPRINT, CD Pike 8-Jun-93
;       Use STRJOIN for display  W.L. July 2009
;-
;First check required parameters...

        On_Error,2
	compile_opt idl2
	Ntag = N_tags( Struct )

	if (Ntag LE 1) then begin
		message,"expecting a Structure Array, try again...",/CONTIN
		return,[-1]
	   endif

	if (N_elements( Tag_Num ) NE 1) AND $
	   (N_elements( Tag_Name ) NE 1) then begin
		message,"specify TAG_NAME= or TAG_NUMBER= to search",/CONTIN
		return,[-1]
	   endif

	Tags = Tag_names( Struct )

	if N_elements( Tag_Name ) EQ 1 then begin
		Tag_Name = strupcase( Tag_Name )
		Tag_Num = where( Tags EQ Tag_Name )
		Tag_Num = Tag_Num[0]
		if (Tag_Num LT 0) then begin
		 if ~keyword_set( noprint ) then $
			message,"Tag <"+Tag_Name+"> not found",/CONTIN
			return,[-2]
		   endif
	   endif

	if (Tag_Num LT 0) OR (Tag_Num GE Ntag) then begin
		message,"Tag# " + strtrim(Tag_Num,2) + " exceeds Max Tag# " $
			+ strtrim(Ntag-1,2) + " in structure",/CONTIN
		return,[-1]
	   endif

	if N_elements( ipart ) GT 0 then begin		;check if any searching	
							;on a subset of input.
		w = where( ipart GE 0, nf )
		if (nf LE 0) then return,[-1]
		if (nf LT N_elements( ipart )) then ipart = ipart[w]
	   endif

;Now find out where for RANGE :

	if N_elements( range ) EQ 2 then begin

		if N_elements( ipart ) GT 0 then begin

		     w = where( (Struct[ipart].(Tag_Num) GE range[0]) AND $
				(Struct[ipart].(Tag_Num) LE range[1]), Nfound )

		     if (Nfound GT 0) then windex = ipart[w] else windex = w

		 endif $
		  else 	windex = where( (Struct.(Tag_Num) GE range[0]) AND $
					(Struct.(Tag_Num) LE range[1]), Nfound )

		if (Nfound LE 0) && (~keyword_set( noprint ) ) then begin
			strnums = strtrim( range, 2 )
			string = strnums[0] + "," + strnums[1]
			message," NO values of <" + Tags[Tag_num] + $
				"> found in the Range [" + string + "]",/CONTIN
		   endif
;where Values:

	 endif else if N_elements( values ) GE 1 then begin

		Nval = N_elements( values )
		vals = [values]
		Nfound = 0

		if N_elements( ipart ) GT 0 then begin

		    for v=0,Nval-1 do begin
			w = where( Struct[ipart].(Tag_Num) EQ vals[v], Nf )
			if (Nf GT 0) then begin
				if (Nfound GT 0) then ww = [ww,w] else ww = w
			   endif
			Nfound = Nfound + Nf
		      endfor

		    if (Nfound GT 0) then windex = ipart[ww[sort( ww )]] $
				     else windex = w

		 endif else begin

		    for v=0,Nval-1 do begin
			w = where( Struct.(Tag_Num) EQ vals[v], Nf )
			if (Nf GT 0) then begin
				if (Nfound GT 0) then ww = [ww,w] else ww = w
			   endif
			Nfound = Nfound + Nf
		      endfor

		    if (Nfound GT 0) then windex = ww[sort( ww )] $
				     else windex = w

		  endelse

		if (Nfound LE 0) && (~keyword_set( noprint ) ) then begin
			string = strjoin( strtrim(vals,2) ,',') 
			message," NO values of <" + Tags[Tag_num] + $
				"> found Equaling [" + string + "]",/CONTIN
		   endif

	   endif else begin

		message,"must specify a RANGE=[#,#]  or VALUES=#('s)",/CONTIN
		windex=[-1]
	    endelse

return, windex
end

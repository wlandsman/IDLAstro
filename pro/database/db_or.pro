function db_or,list1,list2
;+
; NAME:
;	DB_OR
; PURPOSE:
;	Combine two vectors of entry numbers, removing duplicate values.
; EXPLANATION:
;	DB_OR can also be used to remove duplicate values from any longword 
;	vector
;
; CALLING SEQUENCE:
;	LIST = DB_OR( LIST1 )          ;Remove duplicate values from LIST1
;		or
;	LIST = DB_OR( LIST1, LIST2 )   ;Concatenate LIST1 and LIST2, remove dups
;
; INPUTS:
;	LIST1, LIST2 - Vectors containing entry numbers, must be non-negative
;			integers or longwords.
; OUTPUT:
;	LIST - Vector containing entry numbers in either LIST1 or LIST2
;  
; METHOD
;	DB_OR returns where the histogram of the entry vectors is non-zero
;
; PROCEDURE CALLS
;	ZPARCHECK - checks parameters  
; REVISION HISTORY:
;	Written,     W. Landsman             February, 1989
;	Check for degenerate values  W.L.    February, 1993
;	Converted to IDL V5.0   W. Landsman   September 1997
;-
  if N_params() EQ 0 then begin
       print,'Syntax - list = db_or( list1, [ list2] )
       return, -1
  endif

  zparcheck, 'DB_OR', list1, 1, [1,2,3], [1,2], 'First Entry Vector'

  if N_params() eq 1 then begin
       minlist1 = min( list1, max = maxlist1 )
       if ( minlist1 EQ maxlist1 ) then return, minlist1  else $
                   return, where( histogram( list1 ) GT 0 ) + minlist1
  endif

  zparcheck, 'DB_OR', list1, 1, [1,2,3], [1,2], 'Second Entry Vector'

  list = [list1, list2]
  minlist = min( list, max = maxlist )
  if ( minlist EQ maxlist ) then return, minlist  else $
                return,where( histogram( list ) GT 0 ) + minlist

  end

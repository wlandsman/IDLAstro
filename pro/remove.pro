pro remove,index, v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, $
     v15, v16, v17, v18, v19, v20, v21, v22, v23, v24, v25
;+
; NAME:
;       REMOVE
; PURPOSE:
;       Contract a vector or up to 25 vectors by removing specified elements   
; CALLING SEQUENCE:
;       REMOVE, index, v1,[ v2, v3, v4, v5, v6, ... v25]     
; INPUTS:
;       INDEX - scalar or vector giving the index number of elements to
;               be removed from vectors.  Duplicate entries in index are
;               ignored.    An error will occur if one attempts to remove
;               all the elements of a vector.     REMOVE will return quietly
;               (no error message) if index is !NULL or undefined.
;
; INPUT-OUTPUT:
;       v1 - Vector or array.  Elements specifed by INDEX will be 
;               removed from v1.  Upon return v1 will contain
;               N fewer elements, where N is the number of distinct values in
;               INDEX.
;
; OPTIONAL INPUT-OUTPUTS:
;       v2,v3,...v25 - additional vectors containing
;               the same number of elements as v1.  These will be
;               contracted in the same manner as v1.
;
; EXAMPLES:
;       (1) If INDEX = [2,4,6,4] and V = [1,3,4,3,2,5,7,3] then after the call
;
;               IDL> remove,index,v      
;
;       V will contain the values [1,3,3,5,3]
;
;       (2) Suppose one has a wavelength vector W, and three associated flux
;       vectors F1, F2, and F3.    Remove all points where a quality vector,
;       EPS is negative
;
;               IDL> bad = where( EPS LT 0, Nbad)
;               IDL> if Nbad GT 0 then remove, bad, w, f1, f2, f3
;
; METHOD:
;       If more than one element is to be removed, then HISTOGRAM is used
;       to generate a 'keep' subscripting vector.    To minimize the length of 
;       the subscripting vector, it is only computed between the minimum and 
;       maximum values of the index.   Therefore, the slowest case of REMOVE
;       is when both the first and last element are removed.
;
; REVISION HISTORY:
;       Written W. Landsman        ST Systems Co.       April 28, 1988
;       Cleaned up code          W. Landsman            September, 1992
;       Major rewrite for improved speed   W. Landsman    April 2000
;       Accept up to 25 variables, use SCOPE_VARFETCH internally
;              W. Landsman   Feb 2010
;       Fix occasional integer overflow problem  V. Geers  Feb 2011
;       Quietly return if index is !null or undefined W.L. Aug 2011
;             
;-
 On_error,2
 compile_opt idl2,strictarrsubs

 npar = N_params()
 nvar = npar-1
 if npar LT 2 then begin
      print,'Syntax - remove, index, v1, [v2, v3, v4,..., v25]'
      return
 endif

 if N_elements(index) EQ 0 then return

  vv = 'v' + strtrim( indgen(nvar)+1, 2) 
  npts = N_elements(v1)
   
  max_index = max(index, MIN = min_index)

 if ( min_index LT 0 ) || (max_index GT npts-1) then message, $
             'ERROR - Index vector is out of range'

 if ( max_index Eq min_index ) then begin   ;Remove only 1 element?
     Ngood = 0  
    if npts EQ 1 then message, $ 
         'ERROR - Cannot delete all elements from a vector'
  endif else begin 
         

;  Begin case where more than 1 element is to be removed.   Use HISTOGRAM
;  to determine then indices to keep

 nhist = max_index - min_index +1 

 hist = histogram( index)      ;Find unique index values to remove
 keep = where( hist EQ 0, Ngood ) + min_index

 if ngood EQ 0 then begin 
    if ( npts LE nhist ) then message, $
          'ERROR - Cannot delete all elements from a vector'
  endif 
 endelse

 imin = min_index - 1
 imax = max_index + 1
 i0 = (min_index EQ 0) + 2*(max_index EQ npts-1) 
 case i0 of 
 3: begin
    for i=0, nvar-1 do  $
         (SCOPE_VARFETCH(vv[i],LEVEL=0)) = $
	 (SCOPE_VARFETCH(vv[i],LEVEL=0))[keep]
     return	 
     end

 1:  ii = Ngood EQ 0 ? imax + lindgen(npts-imax) : $
                      [keep, imax + lindgen(npts-imax) ]
 2:  ii = Ngood EQ 0 ? lindgen(imin+1)               :  $
                       [lindgen(imin+1), keep ]
 0:   ii = Ngood EQ 0 ? [lindgen(imin+1), imax + lindgen(npts-imax) ]  : $
                      [lindgen(imin+1), keep, imax + lindgen(npts-imax) ]
 endcase 

      for i=0,nvar-1 do  $
         (SCOPE_VARFETCH(vv[i],LEVEL=0)) =    $
	        (SCOPE_VARFETCH(vv[i],LEVEL=0))[ii]
 
 return
 end

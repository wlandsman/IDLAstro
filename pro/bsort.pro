function Bsort, Array, Asort, INFO=info, REVERSE = rev
;+
; NAME:
;       BSORT
; PURPOSE:
;       Function to sort data into ascending order, like a simple bubble sort.
; EXPLANATION:
;       Original subscript order is maintained when values are equal (stable sort).
;       (This differs from the IDL SORT routine alone, which may rearrange 
;       order for equal values)
;
;       A faster algorithm (radix sort) for numeric data is described at 
;http://www.harrisgeospatial.com/Support/SelfHelpTools/HelpArticles/HelpArticles-Detail/TabId/2718/ArtMID/10220/ArticleID/16724/An-LSD-radix-sort-algorithm-in-IDL.aspx
;       and available at
;       https://github.com/mgalloy/mglib/blob/master/src/analysis/mg_sort.pro
; CALLING SEQUENCE:  
;       result = bsort( array, [ asort, /INFO, /REVERSE ] )
;
; INPUT:
;       Array - array to be sorted
;
; OUTPUT:
;       result - sort subscripts are returned as function value
;
; OPTIONAL OUTPUT:
;       Asort - sorted array
;
; OPTIONAL KEYWORD INPUTS:
;       /REVERSE - if this keyword is set, and non-zero, then data is sorted
;                 in descending order instead of ascending order.
;       /INFO = optional keyword to cause brief message about # equal values.
;
; HISTORY
;       written by F. Varosi Oct.90:
;       uses WHERE to find equal clumps, instead of looping with IF ( EQ ).
;       compatible with string arrays, test for degenerate array 
;       20-MAY-1991     JKF/ACC via T AKE- return indexes if the array to 
;                       be sorted has all equal values.
;       Aug - 91  Added  REVERSE keyword   W. Landsman      
;       Always return type LONG    W. Landsman     August 1994
;       Converted to IDL V5.0   W. Landsman   September 1997
;-
        N = N_elements( Array )
        if N lt 1 then begin
                print,'Input to BSORT must be an array'
                return, [0L]
           endif

        if N lt 2 then begin
            asort = array       ;MDM added 24-Sep-91
            return,[0L]    ;Only 1 element
        end
;
; sort array (in descending order if REVERSE keyword specified )
;
        subs = sort( Array )
        if keyword_set( REV ) then subs = rotate(subs,5)  
        Asort = Array[subs]
;
; now sort subscripts into ascending order
; when more than one Asort has same value
;
             weq = where( (shift( Asort, -1 ) eq Asort) , Neq ) 

        if keyword_set( info ) then $
                message, strtrim( Neq, 2 ) + " equal values Located",/CON,/INF

        if (Neq EQ n) then return,lindgen(n) ;Array is degenerate equal values

        if (Neq GT 0) then begin

                if (Neq GT 1) then begin              ;find clumps of equality

                        wclump = where( (shift( weq, -1 ) - weq) GT 1, Nclump )
                        Nclump++

                  endif else Nclump = 1

                if (Nclump LE 1) then begin
                        Clump_Beg = 0
                        Clump_End = Neq-1
                  endif else begin
                        Clump_Beg = [0,wclump+1]
                        Clump_End = [wclump,Neq-1]
                   endelse

                weq_Beg = weq[ Clump_Beg ]              ;subscript ranges
                weq_End = weq[ Clump_End ] + 1          ; of Asort equalities.

                if keyword_set( info ) then message, strtrim( Nclump, 2 ) + $
                                " clumps of equal values Located",/CON,/INF

                for ic = 0L, Nclump-1 do begin          ;sort each clump.

                        subic = subs[ weq_Beg[ic] : weq_End[ic] ]
                        subs[ weq_Beg[ic] ] = subic[ sort( subic ) ]
                  endfor

                if N_params() GE 2 then Asort = Array[subs]     ;resort array.
           endif

return, subs
end

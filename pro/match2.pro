;+
; NAME:
;       MATCH2
; PURPOSE:
;       Routine to cross-match values in two vectors (including non-matches)
; EXPLANATION:
;       MATCH2 reports matching elements of two arrays.

;       This procedure *appears* similar to MATCH of the IDL astronomy
;       library.  However, this routine is quite different in that it
;       reports an index value for each element of the input arrays.
;       In other words, while MATCH reports the *existence* of
;       matching elements in each array, MATCH2 reports explicitly
;       *which* elements match.
;
;       Furthermore, while MATCH reports only unique matching
;       elements, MATCH2 will always report a cross-match for every
;       element in each array, even if it is a repeat.
;
;       In cases where no match was found, an index of -1 is
;       reported.  
;
; CALLING SEQUENCE:
;       match2, a, b, suba, subb
;
; INPUTS:
;       a,b - two vectors to match elements, numeric or string data
;             types.  (See below for RESTRICTIONS on A and B)
;
;
; OUTPUTS:
;       suba - vector with same number of elements as A, such that
;              A EQ B[SUBA], except non-matches which are indicated
;              by SUBA EQ -1
;       subb - vector with same number of elements as B, such that
;              B EQ A[SUBB], except non-matches which are indicated
;              by SUBB EQ -1
;
;
; RESTRICTIONS:
; 
;       The vectors A and B are allowed to have duplicates in them,
;       but for matching purposes, only the first one found will
;       be reported.
;
;       If A and B are string arrays, then non-printable ASCII values
;       1B and 2B will confuse the algorithm.  Don't use these
;       non-printable characters in strings.
;
; EXAMPLE:
;      A = [0,7,14,23,24,30]
;      B = [7,8,14,25,14]
;      IDL> match2, a, b, suba, subb
;     --> suba = [ -1 ,  0,  4,  -1, -1, -1 ]
;     (indicates that A[1] matches B[1] and A[3] matches B[2])
;     --> subb = [  1 , -1,  2,  -1,  2 ]
;     (indicates that B[1] matches A[1] and B[2] matches A[3])
;
;  Compare to the results of the original MATCH procedure,
;    
;      IDL> match, a, b, suba, subb
;     --> suba = [  1,  3]
;  (indicates that A[1] and A[3] match elements in B, but not which ones)
;     --> subb = [  1,  2]
;  (indicates that B[1] and B[2] match elements in A, but not which ones)
;
; MODIFICATION HISTORY
;   Derived from the IDL Astronomy Library MATCH, 14 Feb 2007
;   Updated documentation, 17 Jul 2007
;   More updated documentation (example), 03 Sep 2007
;   Bug fix for string arrays with numerical contents; the subset
;   string is now 1B and 2B; this is now documented, 2014-10-20 CM
;   
; 
;-
;-------------------------------------------------------------------------
pro match2, a, b, suba, subb

 On_error,2
 compile_opt idl2

 if N_params() LT 3 then begin
     print,'Syntax - match2, a, b, suba, subb'
     print,'    a,b -- input vectors for which to match elements'
     print,'    suba,subb -- match index lists'
     return
 endif

 da = size(a,/type) & db =size(b,/type)
 
 na = N_elements(a)              ;number of elements in a
 nb = N_elements(b)             ;number of elements in b
 suba = lonarr(na)-1 & subb = lonarr(nb)-1

; Check for a single element array

 if (na EQ 1) or (nb EQ 1) then begin
        if (nb GT 1) then begin
            wh = where(b EQ a[0], nw)
            if nw GT 0 then begin
                subb[wh] = 0L
                suba[0]  = wh[0]
            endif
        endif else begin
            wh = where(a EQ b[0], nw)
            if nw GT 0 then begin
                suba[wh] = 0L
                subb[0]  = wh[0]
            endif
        endelse
        return
 endif
        
 c = [ a, b ]                   ;combined list of a and b
 ind = [ lindgen(na), lindgen(nb) ]       ;combined list of indices
 vec = [ intarr(na), replicate(1,nb) ]  ;flag of which vector in  combined 
                                         ;list   0 - a   1 - b

; sort combined list

 if da EQ 7 OR db EQ 7 then begin
     vecstr = [string(1b), string(2b)]
     ;; String sort (w/ double key)
     sub = sort(c+vecstr[vec])
 endif else begin
     ;; Number sort (w/ double key)
     eps = (machar(/double)).eps
     sub = sort(double(c)*(1d + vec*eps))
 endelse

 c = c[sub]
 ind = ind[sub]
 vec = vec[sub]
 
 n = na + nb                    ;total elements in c
 wh = where( c[1:*] NE c, ct)
 if ct EQ 0 then begin
     whfirst = [0]
     whlast  = [n-1]
 endif else begin
     whfirst = [0, wh+1]
     whlast  = [wh, n-1]
 endelse
 
 vec0 = vec[whfirst]
 vec1 = vec[whlast]
 ;; 0 = present in A but not B
 ;; 1 = can't occur (since the array was sorted on 'VEC')
 ;; 2 = present in both
 ;; 3 = present in B but not A
 matchtype = vec0 + vec1*2

 nm = n_elements(matchtype)
 mm = ind*0L & wa = mm & wb = mm
 for i = 0, nm-1 do begin
     mm[whfirst[i]:whlast[i]] = matchtype[i]
     wa[whfirst[i]:whlast[i]] = ind[whfirst[i]]
     wb[whfirst[i]:whlast[i]] = ind[whlast[i]]
 endfor

 suba = lonarr(na)-1 & subb = lonarr(nb)-1

 wh = where(mm EQ 2 AND vec EQ 0, ct)
 if ct GT 0 then suba[ind[wh]] = wb[wh]
 wh = where(mm EQ 2 AND vec EQ 1, ct)
 if ct GT 0 then subb[ind[wh]] = wa[wh]

 return
end

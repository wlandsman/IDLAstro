pro match, a, b, suba, subb, COUNT = count, SORT = sort, epsilon=epsilon
  ;+
  ; NAME:
  ;       MATCH
  ; PURPOSE:
  ;       Routine to match values in two vectors.
  ;
  ; CALLING SEQUENCE:
  ;       match, a, b, suba, subb, [ COUNT =, /SORT, EPSILON =  ]
  ;
  ; INPUTS:
  ;       a,b - two vectors to match elements, numeric or string data types
  ;
  ; OUTPUTS:
  ;       suba - subscripts of elements in vector a with a match
  ;               in vector b
  ;       subb - subscripts of the positions of the elements in
  ;               vector b with matchs in vector a.
  ;
  ;       suba and subb are ordered such that a[suba] equals b[subb]
  ;       suba and subb are set to !NULL if there are no matches (or set to -1
  ;		if prior to IDL Version 8.0)
  ;
  ; OPTIONAL INPUT KEYWORD:
  ;       /SORT - By default, MATCH uses two different algorithm: (1) the
  ;               /REVERSE_INDICES keyword to HISTOGRAM is used for integer data,
  ;               while (2) a sorting algorithm is used for non-integer data.  The
  ;               histogram algorithm is usually faster, except when the input
  ;               vectors are sparse and contain very large numbers, possibly
  ;               causing memory problems.   Use the /SORT keyword to always use
  ;               the sort algorithm.
  ;       epsilon - if values are within epsilon, they are considered equal. Used only
  ;               only for non-integer matching.  Note that input vectors should
  ;               be unique to within epsilon to provide one-to-one mapping..
  ;               Default=0.
  ;
  ; OPTIONAL KEYWORD OUTPUT:
  ;       COUNT - set to the number of matches, integer scalar
  ;
  ; SIDE EFFECTS:
  ;       The obsolete system variable !ERR is set to the number of matches;
  ;       however, the use !ERR is deprecated in favor of the COUNT keyword
  ;
  ; RESTRICTIONS:
  ;       The vectors a and b should not have duplicate values within them.
  ;       You can use rem_dup function to remove duplicate values
  ;       in a vector
  ;
  ; EXAMPLE:
  ;       If a = [3,5,7,9,11]   & b = [5,6,7,8,9,10]
  ;       then
  ;               IDL> match, a, b, suba, subb, COUNT = count
  ;
  ;       will give suba = [1,2,3], subb = [0,2,4],  COUNT = 3
  ;       and       a[suba] = b[subb] = [5,7,9]
  ;
  ;
  ; METHOD:
  ;       For non-integer data types, the two input vectors are combined and
  ;       sorted and the consecutive equal elements are identified.   For integer
  ;       data types, the /REVERSE_INDICES keyword to HISTOGRAM of each array
  ;       is used to identify where the two arrays have elements in common.
  ; HISTORY:
  ;       D. Lindler  Mar. 1986.
  ;       Fixed "indgen" call for very large arrays   W. Landsman  Sep 1991
  ;       Added COUNT keyword    W. Landsman   Sep. 1992
  ;       Fixed case where single element array supplied   W. Landsman Aug 95
  ;       Use a HISTOGRAM algorithm for integer vector inputs for improved
  ;             performance                W. Landsman         March 2000
  ;       Work again for strings           W. Landsman         April 2000
  ;       Use size(/type)                  W. Landsman         December 2002
  ;       Work for scalar integer input    W. Landsman         June 2003
  ;       Assume since V5.4, use COMPLEMENT to WHERE() W. Landsman Apr 2006
  ;       Added epsilon keyword            Kim Tolbert         March 14, 2008
  ;       Fix bug with Histogram method with all negative values W. Landsman/
  ;       R. Gutermuth, return !NULL for no matches  November 2017
  ;       Added epsilon test in na=1||nb=1 section (missed that when added
  ;             epsilon in 2008)           Kim Tolbert         July 10, 2018
  ;
  ;-
  ;-------------------------------------------------------------------------
  compile_opt idl2
  Catch, theError
  IF theError NE 0 then begin
    Catch,/Cancel
    void = cgErrorMsg(/quiet)
    RETURN
  ENDIF


  if N_elements(epsilon) EQ 0 then epsilon = 0

  if N_params() LT 3 then begin
    print,'Syntax - match, a, b, suba, subb, [ COUNT =, EPSILON=, /SORT]'
    print,'    a,b -- input vectors for which to match elements'
    print,'    suba,subb -- output subscript vectors of matched elements'
    return
  endif

  da = size(a,/type) & db =size(b,/type)
  if keyword_set(sort) then hist = 0b else $
    hist = (( da LE 3 ) || (da GE 12)) &&  ((db LE 3) || (db GE 12 ))

  if ~hist then begin           ;Non-integer calculation

    na = N_elements(a)              ;number of elements in a
    nb = N_elements(b)             ;number of elements in b

    ; Check for a single element array

    if (na EQ 1) || (nb EQ 1) then begin
      if (nb GT 1) then begin
        if epsilon eq 0. then subb = where(b EQ a[0], nw) else $
          subb = where(abs(b - a[0]) lt epsilon, nw)
        if (nw GT 0) then suba = replicate(0,nw) else suba = [-1]
      endif else begin
        if epsilon eq 0. then suba = where(a EQ b[0], nw) else $
          suba = where(abs(a - b[0]) lt epsilon, nw)
        if (nw GT 0) then subb = replicate(0,nw) else subb = [-1]
      endelse
      count = nw
      return
    endif

    c = [ a, b ]                   ;combined list of a and b
    ind = [ lindgen(na), lindgen(nb) ]       ;combined list of indices
    vec = [ bytarr(na), replicate(1b,nb) ]  ;flag of which vector in  combined
    ;list   0 - a   1 - b

    ; sort combined list

    sub = sort(c)
    c = c[sub]
    ind = ind[sub]
    vec = vec[sub]

    ; find duplicates in sorted combined list

    n = na + nb                            ;total elements in c
    if epsilon eq 0. then $
      firstdup = where( (c EQ shift(c,-1)) and (vec NE shift(vec,-1)), Count ) $
    else $
      firstdup = where( (abs(c - shift(c,-1)) lt epsilon) and (vec NE shift(vec,-1)), Count )

    if Count EQ 0 then begin               ;any found?
      suba = lonarr(1)-1
      subb = lonarr(1)-1
      return
    end

    dup = lonarr( Count*2 )                     ;both duplicate values
    even = lindgen( N_elements(firstdup))*2     ;Changed to LINDGEN 6-Sep-1991
    dup[even] = firstdup
    dup[even+1] = firstdup+1
    ind = ind[dup]                         ;indices of duplicates
    vec = vec[dup]                         ;vector id of duplicates
    subb = ind[ where( vec, complement = vzero) ]             ;b subscripts
    suba = ind[ vzero]

  endif else begin             ;Integer calculation using histogram.

    minab = min(a, MAX=maxa) > min(b, MAX=maxb) ;Only need intersection of ranges
    maxab = maxa < maxb

    ;If either set is empty, or their ranges don't intersect:
    ;  result = NULL (which is denoted by integer = -1)
    !ERR = -1
    if !VERSION.RELEASE GE '8.0' then begin
      suba = !NULL
      subb = !NULL
    endif else begin
      suba = -1
      subb = -1
    endelse
    COUNT = 0L
    if maxab lt minab then return       ;No overlap

    ha = histogram([a], MIN=minab, MAX=maxab, reverse_indices=reva)
    hb = histogram([b], MIN=minab, MAX=maxab, reverse_indices=revb)

    r = where((ha ne 0) and (hb ne 0), count)

    if count gt 0 then begin
      suba = reva[reva[r]]
      subb = revb[revb[r]]
    endif
  endelse

  return

end

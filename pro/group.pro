PRO GROUP, X, Y, RCRIT, NGROUP
;+
; NAME:
;      GROUP
; PURPOSE:
;      Assign stars with non-overlapping PSF profiles into distinct groups
; EXPLANATION:
;      Part of the IDL-DAOPHOT sequence
;
; CALLING SEQUENCE:
;      GROUP, X, Y, RCRIT, NGROUP
;
; INPUTS:
;      X - vector, giving X coordinates of a set of stars.
;      Y - vector, giving Y coordinates of a set of stars.
;           If X and Y are input as integers, then they will be converted to 
;           floating point
;      RCRIT - scalar, giving minimum distance between stars of two
;               distinct groups.  Stars less than this distance from
;               each other are always in the same group.    Stetson suggests
;               setting the critical distance equal to the PSF radius +
;               the Fitting radius.
;
; OUTPUTS:
;      NGROUP - integer vector, same number of elements as X and Y,
;               giving a group number for each star position.  Group
;               numbering begins with 0.
;
; METHOD:
;      Each position is initially given a unique group number.  The distance
;      of each star is computed against every other star.   Those distances
;      less than RCRIT are assigned the minimum group number of the set.   A
;      check is then made to see if any groups have merged together.
;
; PROCEDURES USED:
;      REM_DUP()
;
; REVISION HISTORY:
;      Written W. Landsman  STX                  April, 1988
;      Major revision to properly merge groups together  W. Landsman   Sep 1991
;      Work for more than 32767 points    W. Landsman  March 1997
;      Converted to IDL V5.0   W. Landsman   September 1997
;      Avoid overflow if X and Y are integers      W. Landsman  Feb. 1999   
;-
 On_error,2                                   ;Return to caller

 if N_params() LT 4 then begin
    print,'Syntax - group, x, y, rcrit, ngroup'
    print,'   x,y -   Input position vectors'
    print,'   rcrit - Minimum radius between stars of different groups'
    print,'   ngroup - Output vector of group indices'
    return
 endif

 rcrit2 = rcrit^2                            ;Don't bother taking square roots
 npts = min( [N_elements(x), N_elements(y)] )    ;Number of stars

 if npts LT 2 then message, $
    'ERROR - Input position X,Y vectors must contain at least 2 points'

 x = 1.0*x  &  y = 1.0*y   ;Make sure at least floating point
 ngroup =  lindgen(npts)   ;Initially each star in a separate group

;  Whenever the positions between two stars are less than the critical
;  distance, assign both stars the minimum group id.   The tricky part
;  is to recognize when distinct groups have merged together.

 for i = 0l,npts-2 do begin
      dis2 = (x[i] - x[i+1:*])^2 + (y[i] - y[i+1:*])^2
      good =  where( dis2 LE rcrit2, ngood)
      if ngood GT 0 then begin             ;Any stars within critical radius?

                good = [i,good+i+1]
                groupval = ngroup[good]
                mingroup = min( groupval )
                    if ( mingroup LT i ) then begin      ;Any groups merge?
                       groupval = groupval[ where( groupval LT i, nval) ]
                       if nval GT 1 then $
                         groupval = groupval[ rem_dup(groupval) ]
                         nval = N_elements(groupval)

                           if nval GE 2 then for j= 1, nval-1 do begin  
                             redo = where ( ngroup EQ groupval[j], ndo )
                             if ndo GT 0 then ngroup[redo] = mingroup
                           endfor

                    endif
                ngroup[good] = mingroup
      endif
endfor
;
; Star are now placed in distinct groups, but they are not ordered
; consecutively.  Remove gaps in group ordering
;
 if max(ngroup) EQ 0 then return               ;All stars in one group ?

 ghist = histogram(ngroup,min=0)
 gmax = max(ghist)
 val = where(ghist GE 1, ngood)  
 if ( ngood GT 0 ) then $ 
          for i = 0, ngood-1 do ngroup[ where( ngroup EQ val[i] ) ] = i

 message,'Number of Groups: '+ strtrim(ngood,2), /INF 
 message,'Largest group size '+ strtrim(gmax,2) + ' stars',/INF

 return
 end

;+
; NAME:
;	PERMUTE
;
; PURPOSE:
;	This function returns an array containing the numbers
;	[0, ..., N-1] in random order.  They are useful as indices
;	when permuting a dataset, for example in a balanced bootstrap
;	Monte Carlo algorithm.      
;
; CATEGORY:
;	Statistics.
;
; CALLING SEQUENCE:
;
;	Result = PERMUTE(N)
;
; INPUTS:
;	N:	The number of items to be permuted.
;
; OPTIONAL INPUTS:
;	SEED:	A random number seed, see RANDOMU.
;
; OUTPUTS:
;	This function returns an N-element array containing a random
;	permutation of the integers from 0 through N-1.
;
; SIDE EFFECTS:
;	Unless Seed is specified, IDL's global random number
;	seed is changed. 
;
; PROCEDURE:
;	This is an in-place swapping algorithm.  It starts with an
;	index array.  For each position in the array, it swaps the
;	occupant of that position with the occupant of a random
;	position from there (inclusive) to the end of the array.  The
;	last iteration is not necessary to compute, since it swaps
;	with itself.
;
;	See http://www.techuser.net/randpermgen.html for a proof.  The
;	2-line code there has been optimized for IDL's vector
;	architecture.  This is a linear-time algorithm.
;
; EXAMPLE:
; Show some permutations of 6 numbers:
;	print, permute(6)
;	    0           2           1           3           4           5
;	print, permute(6)
;           2           4           3           5           1           0
;	print, permute(6)
;	    0           4           3           1           2           5
;
; Permute the array [2, 4, 6, 8]
;	a = [2, 4, 6, 8]
;	print, a[permute(4)]
;       4       8       6       2
;
; Test randomness (results should be close to k):
; m = 6l
; k = 10000l
; n = m * k
; a = lonarr(m, n)
; for i = 0l, n-1, 1 do a[*, i] = permute(m)
; for i = 0l, m-1, 1 do print, histogram(a[i, *])
;         9885       10062       10051        9915       10028       10059
;        10096       10087       10094        9913        9933        9877
;        10041       10013        9968        9958        9911       10109
;         9880        9858       10166       10049       10081        9966
;        10093        9915        9800       10166        9969       10057
;        10005       10065        9921        9999       10078        9932
;
; Time the algorithm:
; maxn = 7
; t = dblarr(maxn)
; n = 10L^(indgen(maxn)+1)
; for i = 0, maxn-1, 1 do begin &$
;   t1 = systime(/s) &$
;   print, n[i] &$
;   a = permute(n[i]) &$
;   t2 = systime(/s) &$
;   t[i] = t2-t1 &$
; endfor
; print, '        Elements         Seconds   Elements Per Second'
; print, transpose([[n], [t], [t/n]])
; 
;         Elements         Seconds   Elements Per Second
;        10.000000   0.00012397766   1.2397766e-05
;        100.00000   0.00015020370   1.5020370e-06
;        1000.0000    0.0011651516   1.1651516e-06
;        10000.000     0.018178225   1.8178225e-06
;        100000.00      0.13504505   1.3504505e-06
;        1000000.0       1.3817160   1.3817160e-06
;        10000000.       14.609985   1.4609985e-06
;
; These times are for a 2.071 GHz AMD Athlon 2800+ CPU.
;
; MODIFICATION HISTORY:
; 	Written by:	Joseph Harrington, Cornell.  2006-03-22
;			jh@alum.mit.edu
;-
function PERMUTE, N, Seed

; Don't stop here!
on_error, 2

; test inputs
if n eq 1 then return, 0L
if n lt 1 then message, 'N = ' + strtrim(n, 2) + ', must be 1 or more.'

ar  = lindgen(n)
rar = reverse(ar[0 : n - 2]) + 2
r   = (n - 1) - long( randomu(seed, n - 1) * rar )

for i = 0L, n - 2, 1 do begin
  t = ar[i]
  ar[i] = ar[r[i]]
  ar[r[i]] = t
endfor

return, ar
end


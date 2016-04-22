;+
; NAME:
;       REPCHR
; PURPOSE:
;       Replace one character if a string with another
;
; CALLING SEQUENCE:
;       res = repchr(s, old, [new])
;
; INPUTS:
;       s   - Original string
;       old - Character to be replaced
;       new - Replacing character
;
; REVISION HISTORY:
;       Rewritten from scratch, Christer Sandin, 2015
;
;-
function repchr, str, c, d
  if size(str, /type) ne 7 then $
    message, 'The first argument (STR) must be set to a string.'
  ret = str

  if size(c, /type) ne 7 then $
    message, 'The second argument (C) must be set to a string.'
  nc = n_elements(c)

  if ~n_elements(d) then $
    d = nc gt 1L ? strarr(nc) + ' ' : ' '

  ;; Looping over all characters in C:
  for i = 0L, nc - 1L do begin
    pos = 0L
    opos = - 1L
    while pos ne - 1L do begin
      pos = strpos(ret, c[i], opos)
      if pos ne - 1L then begin
        ret = (~pos ? '' : strmid(ret, 0L, pos)) + $
              d[i] + strmid(ret, pos + strlen(c[i]))
        opos = pos + strlen(d[i])
      endif
    endwhile
  endfor

  return, ret
end

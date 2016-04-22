;+
; NAME:
;       ISARRAY
; PURPOSE:
;       Test if the argument is an array or not.
;
; CALLING SEQUENCE:
;       res = isarray(a)
;
; INPUTS:
;       a - argument
;
; REVISION HISTORY:
;       Rewritten from scratch, Ole Streicher, 2015
;
;-
FUNCTION isarray, a
  res = size(a)
  return, res[0] ne 0
END

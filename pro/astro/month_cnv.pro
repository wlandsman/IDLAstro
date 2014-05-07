function month_cnv, MonthInput, Up=Up, Low=Low, Short=Short
;\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\
;+
; NAME:
;       MONTH_CNV
; PURPOSE:
;       Convert between a month name and  the equivalent number 
; EXPLANATION: (e.g.,
;       For example, converts from 'January' to 1  or vice-versa.
; CALLING SEQUENCE:
;       Result = MONTH_CNV( MonthInput, [/UP, /LOW, /SHORT ] )
; INPUTS:
;       MonthInput - either a string ('January', 'Jan', 'Decem', etc.) or
;               an number from 1 to 12.  Scalar or array. 
; OPTIONAL KEYWORDS:
;       UP - if set and if a string is being returned, it will be in all
;               uppercase letters.
;       LOW - if set and if a string is being returned, it will be in all 
;               lowercase letters.
;       SHORT - if set and if a string is being returned, only the first
;               three letters are returned.
;       
; OUTPUTS:
;       If the input is a string, the output is the matching month number.If
;               an input string isn't a valid month name, -1 is returned.
;       If the input is a number, the output is the matching month name.  The
;               default format is only the first letter is capitalized.
; EXAMPLE:
;       To get a vector of all the month names:
;               Names = month_cnv(indgen(12)+1)
;
; MODIFICATION HISTORY:
;       Written by:     Joel Wm. Parker, SwRI, 1998 Dec 9
;-
;/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/

NumElem = n_elements(MonthInput)

MonthNames = ['  ', 'January', 'February', 'March', 'April', 'May', 'June', $
          'July', 'August', 'September', 'October', 'November', 'December']
MonthShort = strupcase(strmid(MonthNames,0,3))


if size(MonthInput,/TNAME) EQ 'STRING' then begin
   Result = intarr(NumElem) - 1
   ShortInput = strupcase(strmid(strtrim(MonthInput,2),0,3))
   for N=1,12 do begin
      Mask = where(MonthShort[N] eq ShortInput)
      if (Mask[0] ne -1) then Result[Mask] = N
   endfor
endif else begin
   if ( (min(MonthInput) lt 1) or (max(MonthInput) gt 12) ) then begin
      message, /CON, "Bad input values.  Month numbers must be 1-12."
      Result = ''
   endif else begin
      Result = MonthNames[MonthInput]
      if keyword_set(Short) then Result = strmid(Result,0,3)
      if keyword_set(Up) then Result = strupcase(Result)
      if keyword_set(Low) then Result = strlowcase(Result)
   endelse
endelse

if (NumElem eq 1) then Result = Result[0]

return, Result
end   ;   function MONTH_CNV

   

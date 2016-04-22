pro GET_COORDS, Coords, PromptString, NumVals, InString=InString, Quiet=Quiet
;*******************************************************************************
;+
; NAME:
;       GET_COORDS
;
; PURPOSE:
;       Converts a string with angular coordinates  to floating point values.  
; EXPLANATION:
;       Although called by ASTRO.PRO, this is a general purpose routine.
;       The user may input as floating point or sexagesimal.  If user inputs 
;       calling procedure's job to convert hours to degrees if needed.
;       Since the input string is parsed character-by-character, ANY character
;       that is not a digit, minus sign or decimal point may be used as a 
;       delimiter, i.e. acceptable examples of user input are:
;
;       1:03:55 -10:15:31
;       1 3 55.0 -10 15 31
;       1*3 55              -10abcd15efghij31
;       1.065278  hello   -10.25861
;
; CALLING SEQUENCE:
;       GET_COORDS, Coords, [ PromptString, NumVals, INSTRING =, /QUIET ]
;
; OPTIONAL INPUT:
;       PromptString - A string to inform the user what data are to be entered
;
; OPTIONAL KEYWORD INPUT:
;       InString - a keyword that, if set, is assumed to already contain the
;               input data string to be parsed.  If this keyword is set, then
;               the user is not prompted for any input.
;       /Quiet - if set the program won't printout any error messages, but bad
;               input is still flagged by Coords=[-999,-999].
;
; OUTPUT:
;       Coords - a 2 element floating array containing the coordinates.  The
;               vector [-999,-999] is returned if there has been an error.
;
; OPTIONAL OUTPUT:
;       NumVals - the number of separate values entered by the user:  2 if the
;               user entered the coordinates as floating point numbers, 6 if 
;               the user entered the coordinates as sexagesimal numbers.  Some
;               calling procedures might find this information useful (e.g., to
;               to print some output in the same format as the user's input).
;
; REVISION HISTORY:
;       Written by Joel Parker, 5 MAR 90
;       Included InString and Quiet keywords.  Cleaned up some of the code and
;       comments.  JWmP,  16 Jun 94
;
;*******************************************************************************
;       Converted to IDL V5.0   W. Landsman   September 1997
;-

On_error,2

if (N_params() eq 0) then begin
      print,'Syntax - ' + $
         'GET_COORDS, Coords, [PromptString, NumVals, INSTRING=, /QUIET]'
      return
endif 

;
;   Define some parameters and variables.
;
if (N_Params() lt 2) then PromptString = " Please input the coordinates"
Bell     = string(7B)
Minus    = 45      ; ascii of "-"
Decimal  = 46      ; ascii of "."
Zero     = 48      ; ascii of "0"
Nine     = 57      ; ascii of "9"
ValArr   = dblarr(6)
SignArr  = intarr(6) + 1
NumVals  = 0
StartPos = -1

;
;    If the InString keyword is not set, then prompt the user for input.  If 
; nothing is entered, return [-999,-999] as a warning flag to the calling 
; procedure.
;
if keyword_set(InString) then begin
   Coords = InString
endif else begin
   Coords = ""
   print,form =  "(1X,A,$)", + PromptString + " {RETURN to exit} "
   read, Coords
endelse

Coords = strtrim(Coords) + " "  ; The final space is needed for parsing purposes
if (Coords eq " ") then begin
   Coords = [-999,-999]
   return
endif

;
;   All's well.  Get the byte values for the characters in the input string.
;
BCoords = byte(Coords)

;
;   Begin the loop that parses the input string.
;   Start by loading the byte value of the next character into the BC variable.
; Check to see if the character is a minus sign (if so, set the flag in the 
; SignArr array to -1).  Check to see if the character is a numeral between 0-9 
; or a decimal (if so, then the NumFlag is set to 1).
;
for N = 0,(strlen(Coords)-1) do begin
   BC = BCoords[N]
   if (BC eq Minus) then SignArr[NumVals] = -1
   NumFlag = ((BC ge Zero) and (BC le Nine)) or (BC eq Decimal)

;
;   If the number flag is set, but StartPos = -1, then we are starting a new 
; value.  Load the character's position in StartPos.
;
   if (NumFlag and (StartPos eq -1)) then StartPos = N

;
;     If the number flag is NOT set, but StartPos > -1, then we have just 
; finished reading a number.  Read the number from StartPos to the current 
; position, and reset StartPos to -1.
;   Put the resulting number in the ValArr.
;
   if (~(NumFlag) && (StartPos gt -1)) then begin
      if (NumVals lt 6) then begin
         ValArr[NumVals] = float(strmid(Coords, StartPos, (N - StartPos)))
      endif
      StartPos = -1
      NumVals  = NumVals + 1
   endif
endfor

;
;   Coords should be a 2 or 6 element vector {depending on the type of input}.
; It is converted to a 2 element vector such that Coords = [RA/Long, Dec/Lat].
;
case NumVals of

   2 : Coords = (ValArr * SignArr)[0:1]

   6 : begin
      Temp = where(SignArr[0:2] eq -1)
      if (Temp[0] eq -1) then XSign = 1 else XSign = -1
      Temp = where(SignArr[3:5] eq -1)
      if (Temp[0] eq -1) then YSign = 1 else YSign = -1
      X = (ValArr[0] + (ValArr[1] / 60.) + (ValArr[2] / 3600.)) * XSign
      Y = (ValArr[3] + (ValArr[4] / 60.) + (ValArr[5] / 3600.)) * YSign
      Coords = [X,Y]
   end

   else : begin
      Coords = [-999,-999]
      if ~keyword_set(Quiet) then begin
         print, Bell
         print, "ERROR - Invalid Input!"
         print, "Coordinates must be input as 2 or 6 values."
         print, "For example:  1.568 -10.343   or  1 34 4.8  10 20 34.8"
      endif
   endelse

endcase

return
end   ;   procedure GET_COORDS   by   Joel Parker   16 Jun 94
